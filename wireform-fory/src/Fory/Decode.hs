{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
-- | Apache Fory xlang value decoder.
--
-- Mirrors 'Fory.Encode.encode'. Wire-compatible with @pyfory@
-- 0.17 for the value shapes documented on the encode side; see
-- "Fory.Encode" for the exact subset.
--
-- Internally the decoder runs in 'IO' against a 'Decoder'
-- record that holds the input 'ByteString' (and its raw 'Ptr'
-- base), a mutable 'IORef Int' read cursor, and 'IORef'-backed
-- dedup pools (meta-string, ref-id, TypeDef). Errors are
-- thrown as 'DecodeError' exceptions and caught at the
-- top-level boundary to produce 'Either String'. This keeps
-- the per-byte read cost down to one @readIORef + peek +
-- writeIORef@ — there is no state-monad bind chain.
--
-- The pure-looking 'decode' / 'decodeWith' API uses
-- 'unsafeDupablePerformIO' to wrap the IO action; the buffer
-- and pools are local to one decode call so this is safe.
module Fory.Decode
  ( decode
  , decodeWith
  , decodeValueSlot
  , decodeTypedPayload
  , decodePayloadFor
  , DecodeM (DecodeM, runDM)
  , Decoder (..)
  , runDecodeM
  , runDecodeMWith
    -- * Read primitives (for typed decoders in 'Fory.Direct')
  , readByteD
  , readBytesD
  , readWord16D
  , readWord32D
  , readWord64D
  , readInt16D
  , readInt32D
  , readInt64D
  , readFloat32D
  , readFloat64D
  , readVaruint32D
  , readVaruint64D
  , readVarint32D
  , readVarint64D
  , readForyString
  , decodeMetaStringWith
  , failD
    -- * Raw-pointer read primitives (for typed batched decoders)
  , readSameTypeBatch
  , readSameTypeBatchList
  , peekByteRaw
  , peekWord16LERaw
  , peekWord32LERaw
  , peekWord64LERaw
  , peekInt32LERaw
  , peekInt64LERaw
  , peekVaruint64Raw
  , peekVarint64Raw
  ) where

import Control.Exception (Exception, throwIO, try)
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.ByteString.Internal as BSI
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.IORef
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.HashMap.Strict as HM

import qualified Fory.Bulk as B
import qualified Fory.MetaString as MS
import qualified Fory.MetaString.Encoder as MSE
import qualified Fory.Options as Opt
import qualified Fory.Struct as ST
import qualified Fory.TypeId as T
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Decoder state (mutable cursor + dedup pools)
-- ---------------------------------------------------------------------------

-- | The 'Decoder' record is built once per @decode@ call. The
-- 'ByteString' field keeps the underlying 'ForeignPtr' alive
-- so that 'decBase' (the raw 'Ptr') remains valid for the
-- duration of the decode. The 'IORef'-backed pools mutate as
-- meta-strings, refs, and TypeDefs are seen.
data Decoder = Decoder
  { decBs       :: !ByteString
  , decBase     :: {-# UNPACK #-} !(Ptr Word8)
  , decLen      :: {-# UNPACK #-} !Int
  , decPos      :: {-# UNPACK #-} !(IORef Int)
  , decStrPool  :: {-# UNPACK #-} !(IORef (IntMap Text))
  , decStrNext  :: {-# UNPACK #-} !(IORef Int)
  , decRefPool  :: {-# UNPACK #-} !(IORef (IntMap VV.Value))
  , decRefNext  :: {-# UNPACK #-} !(IORef Int)
  , decTdPool   :: {-# UNPACK #-} !(IORef (IntMap TypeDef))
  , decTdNext   :: {-# UNPACK #-} !(IORef Int)
  , decOptions  :: !Opt.DecodeOptions
  }

data TypeDef = TypeDef
  { tdNamespace  :: !Text
  , tdTypeName   :: !Text
  , tdFieldNames :: !(Vector Text)
  } deriving (Show, Eq)

newDecoder :: Opt.DecodeOptions -> ByteString -> IO Decoder
newDecoder !opts !bs@(BSI.BS fp len) = do
  -- We hold the ForeignPtr alive in the ByteString and use
  -- 'withForeignPtr' to derive the raw 'Ptr Word8'. The pointer
  -- stays valid as long as 'bs' (and hence 'fp') is reachable
  -- from the Decoder, which it is until the decode action
  -- finishes.
  pos <- newIORef 0
  sp  <- newIORef IM.empty
  sn  <- newIORef 0
  rp  <- newIORef IM.empty
  rn  <- newIORef 0
  tp  <- newIORef IM.empty
  tn  <- newIORef 0
  withForeignPtr fp $ \p ->
    pure $! Decoder bs p len pos sp sn rp rn tp tn opts
{-# INLINE newDecoder #-}

-- | Decoder errors are thrown as exceptions and caught at the
-- top-level boundary. Cheaper than a plain @Either@ in the
-- inner loop because GHC doesn't have to thread the @Left@
-- short-circuit through every bind.
newtype DecodeError = DecodeError String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | The decoder monad. A reader of 'Decoder' returning 'IO'.
newtype DecodeM a = DecodeM { runDM :: Decoder -> IO a }

instance Functor DecodeM where
  fmap f (DecodeM g) = DecodeM $ \d -> fmap f (g d)
  {-# INLINE fmap #-}

instance Applicative DecodeM where
  pure x = DecodeM $ \_ -> pure x
  {-# INLINE pure #-}
  DecodeM f <*> DecodeM x = DecodeM $ \d -> f d <*> x d
  {-# INLINE (<*>) #-}

instance Monad DecodeM where
  DecodeM m >>= k = DecodeM $ \d -> do
    a <- m d
    runDM (k a) d
  {-# INLINE (>>=) #-}

-- ---------------------------------------------------------------------------
-- Top-level runners
-- ---------------------------------------------------------------------------

runDecodeM :: DecodeM a -> ByteString -> Either String a
runDecodeM = runDecodeMWith Opt.defaultDecodeOptions

runDecodeMWith
  :: Opt.DecodeOptions -> DecodeM a -> ByteString -> Either String a
runDecodeMWith !opts (DecodeM m) !bs = unsafeDupablePerformIO $ do
  d <- newDecoder opts bs
  r <- try (m d)
  case r of
    Left (DecodeError e) -> pure (Left e)
    Right a -> do
      pos <- readIORef (decPos d)
      if pos == decLen d
        then pure (Right a)
        else pure (Left $ "Fory.Decode: " ++ show (decLen d - pos)
                          ++ " trailing bytes")
{-# NOINLINE runDecodeMWith #-}

-- ---------------------------------------------------------------------------
-- Cursor primitives
-- ---------------------------------------------------------------------------

failD :: String -> DecodeM a
failD msg = DecodeM $ \_ -> throwIO (DecodeError msg)
{-# INLINE failD #-}

ensureBytes :: Decoder -> Int -> Int -> IO ()
ensureBytes !d !pos !need
  | pos + need > decLen d =
      throwIO (DecodeError $
        "Fory.Decode: need " ++ show need ++ " bytes at offset "
          ++ show pos ++ " but buffer only has "
          ++ show (decLen d - pos))
  | otherwise = pure ()
{-# INLINE ensureBytes #-}

readByteD :: DecodeM Word8
readByteD = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 1
  b <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 1)
  pure b
{-# INLINE readByteD #-}

readBytesD :: Int -> DecodeM ByteString
readBytesD n = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos n
  -- 'BSU.unsafeTake' / 'unsafeDrop' skip the bounds-check
  -- branches in the safe variants. We just verified bounds
  -- via 'ensureBytes', so they're equivalent and compile to
  -- a single 'PS' constructor allocation instead of two.
  let !slice = BSU.unsafeTake n (BSU.unsafeDrop pos (decBs d))
  writeIORef (decPos d) (pos + n)
  pure slice
{-# INLINE readBytesD #-}

readWord16D :: DecodeM Word16
readWord16D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 2
  w <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 2)
  pure w
{-# INLINE readWord16D #-}

readWord32D :: DecodeM Word32
readWord32D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 4
  w <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 4)
  pure w
{-# INLINE readWord32D #-}

readWord64D :: DecodeM Word64
readWord64D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 8
  w <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 8)
  pure w
{-# INLINE readWord64D #-}

readInt16D :: DecodeM Int16
readInt16D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 2
  n <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 2)
  pure n
{-# INLINE readInt16D #-}

readInt32D :: DecodeM Int32
readInt32D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 4
  n <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 4)
  pure n
{-# INLINE readInt32D #-}

readInt64D :: DecodeM Int64
readInt64D = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  ensureBytes d pos 8
  n <- peekByteOff (decBase d) pos
  writeIORef (decPos d) (pos + 8)
  pure n
{-# INLINE readInt64D #-}

readFloat32D :: DecodeM Float
readFloat32D = castWord32ToFloat <$> readWord32D
{-# INLINE readFloat32D #-}

readFloat64D :: DecodeM Double
readFloat64D = castWord64ToDouble <$> readWord64D
{-# INLINE readFloat64D #-}

-- | varuint32 decode in IO. Reads up to 5 bytes.
readVaruint32D :: DecodeM Word32
readVaruint32D = DecodeM $ \d -> do
  pos0 <- readIORef (decPos d)
  let goVu :: Int -> Int -> Word32 -> IO (Word32, Int)
      goVu !shft !pos !acc
        | shft >= 35 =
            throwIO (DecodeError
              "Fory.Decode.readVaruint32: too many continuation bytes")
        | otherwise = do
            ensureBytes d pos 1
            b <- peekByteOff (decBase d) pos :: IO Word8
            let !chunk = (fromIntegral (b .&. 0x7F) :: Word32) `shiftL` shft
                !next  = acc .|. chunk
            if b .&. 0x80 == 0
              then pure (next, pos + 1)
              else goVu (shft + 7) (pos + 1) next
  (!v, !pos1) <- goVu 0 pos0 0
  writeIORef (decPos d) pos1
  pure v

-- | varuint64 decode in IO. Reads up to 9 bytes; pyfory's wire
-- shape is 1–8 high-bit-tagged bytes followed by an optional
-- 9th raw byte (no continuation flag) — we mirror that here.
readVaruint64D :: DecodeM Word64
readVaruint64D = DecodeM $ \d -> do
  pos0 <- readIORef (decPos d)
  let go :: Int -> Int -> Word64 -> IO (Word64, Int)
      go !i !pos !acc
        | i >= 8 = do
            ensureBytes d pos 1
            b <- peekByteOff (decBase d) pos :: IO Word8
            let !chunk = (fromIntegral b :: Word64) `shiftL` (7 * 8)
                !next  = acc .|. chunk
            pure (next, pos + 1)
        | otherwise = do
            ensureBytes d pos 1
            b <- peekByteOff (decBase d) pos :: IO Word8
            let !chunk = (fromIntegral (b .&. 0x7F) :: Word64) `shiftL` (7 * i)
                !next  = acc .|. chunk
            if b .&. 0x80 == 0
              then pure (next, pos + 1)
              else go (i + 1) (pos + 1) next
  (!v, !pos1) <- go 0 pos0 0
  writeIORef (decPos d) pos1
  pure v

readVarint32D :: DecodeM Int32
readVarint32D = do
  v <- readVaruint32D
  let !i = fromIntegral (v `shiftR` 1) :: Int32
      !s = fromIntegral (v .&. 1) :: Int32
  pure (i `xor` (-s))
{-# INLINE readVarint32D #-}

readVarint64D :: DecodeM Int64
readVarint64D = do
  v <- readVaruint64D
  let !i = fromIntegral (v `shiftR` 1) :: Int64
      !s = fromIntegral (v .&. 1) :: Int64
  pure (i `xor` (-s))
{-# INLINE readVarint64D #-}

-- ---------------------------------------------------------------------------
-- Raw-pointer read primitives (for tight batched loops)
-- ---------------------------------------------------------------------------
--
-- These take a base 'Ptr Word8' + current offset (no
-- 'IORef' touch), read a value, and return the new offset.
-- Used by the same-type collection decoder's batched path.

{-# INLINE peekByteRaw #-}
peekByteRaw :: Ptr Word8 -> Int -> IO (Word8, Int)
peekByteRaw !p !pos = do
  b <- peekByteOff p pos
  pure (b, pos + 1)

{-# INLINE peekWord16LERaw #-}
peekWord16LERaw :: Ptr Word8 -> Int -> IO (Word16, Int)
peekWord16LERaw !p !pos = do
  w <- peekByteOff p pos
  pure (w, pos + 2)

{-# INLINE peekWord32LERaw #-}
peekWord32LERaw :: Ptr Word8 -> Int -> IO (Word32, Int)
peekWord32LERaw !p !pos = do
  w <- peekByteOff p pos
  pure (w, pos + 4)

{-# INLINE peekWord64LERaw #-}
peekWord64LERaw :: Ptr Word8 -> Int -> IO (Word64, Int)
peekWord64LERaw !p !pos = do
  w <- peekByteOff p pos
  pure (w, pos + 8)

{-# INLINE peekInt32LERaw #-}
peekInt32LERaw :: Ptr Word8 -> Int -> IO (Int32, Int)
peekInt32LERaw !p !pos = do
  n <- peekByteOff p pos
  pure (n, pos + 4)

{-# INLINE peekInt64LERaw #-}
peekInt64LERaw :: Ptr Word8 -> Int -> IO (Int64, Int)
peekInt64LERaw !p !pos = do
  n <- peekByteOff p pos
  pure (n, pos + 8)

{-# INLINE peekVaruint64Raw #-}
peekVaruint64Raw :: Ptr Word8 -> Int -> IO (Word64, Int)
peekVaruint64Raw !p !pos0 = go (0 :: Int) pos0 (0 :: Word64)
  where
    -- Explicit type sigs on the loop variables are
    -- important: without them GHC defaults the @0@ literal
    -- on @i@ to 'Integer', producing a @\$wgo :: Integer
    -- -> Int# -> Word64# -> ...@ loop with a 3-case
    -- @IS x | IP x | IN x@ pattern-match per iteration on
    -- the byte counter.
    go :: Int -> Int -> Word64 -> IO (Word64, Int)
    go !i !pos !acc
      | i >= 8 = do
          b <- peekByteOff p pos :: IO Word8
          let !chunk = (fromIntegral b :: Word64) `shiftL` (7 * 8)
              !next  = acc .|. chunk
          pure (next, pos + 1)
      | otherwise = do
          b <- peekByteOff p pos :: IO Word8
          let !chunk = (fromIntegral (b .&. 0x7F) :: Word64) `shiftL` (7 * i)
              !next  = acc .|. chunk
          if b .&. 0x80 == 0
            then pure (next, pos + 1)
            else go (i + 1) (pos + 1) next

{-# INLINE peekVarint64Raw #-}
peekVarint64Raw :: Ptr Word8 -> Int -> IO (Int64, Int)
peekVarint64Raw !p !pos = do
  (v, pos') <- peekVaruint64Raw p pos
  let !i = fromIntegral (v `shiftR` 1) :: Int64
      !s = fromIntegral (v .&. 1) :: Int64
  pure (i `xor` (-s), pos')

-- ---------------------------------------------------------------------------
-- State pool accessors (typed, no full DecodeState snapshot)
-- ---------------------------------------------------------------------------

getOptions :: DecodeM Opt.DecodeOptions
getOptions = DecodeM $ \d -> pure (decOptions d)
{-# INLINE getOptions #-}

lookupRefValue :: Int -> DecodeM (Maybe VV.Value)
lookupRefValue rid = DecodeM $ \d -> do
  m <- readIORef (decRefPool d)
  pure (IM.lookup rid m)
{-# INLINE lookupRefValue #-}

-- | Allocate a new ref id, run @inner@ to compute the value
-- bound to that id, register the @(id, value)@ pair, and
-- return a 'VV.RefVal' wrapping both. Used when the wire bytes
-- begin a fresh ref-tracked value.
withFreshRef :: DecodeM VV.Value -> DecodeM VV.Value
withFreshRef inner = DecodeM $ \d -> do
  rid <- readIORef (decRefNext d)
  writeIORef (decRefNext d) (rid + 1)
  v <- runDM inner d
  modifyIORef' (decRefPool d) (IM.insert rid v)
  pure (VV.RefVal rid v)
{-# INLINE withFreshRef #-}

lookupStringPool :: Int -> DecodeM (Maybe Text)
lookupStringPool i = DecodeM $ \d -> do
  m <- readIORef (decStrPool d)
  pure (IM.lookup i m)
{-# INLINE lookupStringPool #-}

registerStringPool :: Text -> DecodeM ()
registerStringPool t = DecodeM $ \d -> do
  i <- readIORef (decStrNext d)
  writeIORef (decStrNext d) (i + 1)
  modifyIORef' (decStrPool d) (IM.insert i t)
{-# INLINE registerStringPool #-}

lookupTypeDef :: Int -> DecodeM (Maybe TypeDef)
lookupTypeDef i = DecodeM $ \d -> do
  m <- readIORef (decTdPool d)
  pure (IM.lookup i m)
{-# INLINE lookupTypeDef #-}

registerTypeDef :: Int -> TypeDef -> DecodeM ()
registerTypeDef i td = DecodeM $ \d -> do
  modifyIORef' (decTdPool d) (IM.insert i td)
  writeIORef (decTdNext d) (i + 1)
{-# INLINE registerTypeDef #-}

getOff :: DecodeM Int
getOff = DecodeM $ \d -> readIORef (decPos d)
{-# INLINE getOff #-}

-- ---------------------------------------------------------------------------
-- Slot flag bytes
-- ---------------------------------------------------------------------------

slotNotNullValue, slotNull, slotRefValue, slotRef :: Word8
slotNotNullValue = 0xFF
slotNull         = 0xFD
slotRefValue     = 0x00
slotRef          = 0xFE

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse a fory-encoded byte string back to a 'Value' under
-- the default 'Opt.defaultDecodeOptions' (no reference tracking).
decode :: ByteString -> Either String VV.Value
decode = decodeWith Opt.defaultDecodeOptions

-- | Decode under explicit options.
decodeWith :: Opt.DecodeOptions -> ByteString -> Either String VV.Value
decodeWith opts bs = runDecodeMWith opts go bs
  where
    go = do
      hdr <- readByteD
      if hdr .&. 0x02 == 0
        then failD ("Fory.Decode.decode: missing xlang flag in header byte "
                    ++ show hdr)
        else decodeValueSlot

decodeValueSlot :: DecodeM VV.Value
decodeValueSlot = do
  flag <- readByteD
  case flag of
    f | f == slotNull       -> pure VV.NoneVal
      | f == slotRef        -> decodeRefBack
      | f == slotRefValue   -> withFreshRef decodeTypedPayload
      | f == slotNotNullValue -> decodeTypedPayload
      | otherwise -> failD $
          "Fory.Decode: unexpected slot flag byte " ++ show flag

decodeRefBack :: DecodeM VV.Value
decodeRefBack = do
  wid <- fromIntegral <$> readVaruint32D
  m <- lookupRefValue wid
  case m of
    Nothing -> failD $
      "Fory.Decode: REF flag references unknown ref_id " ++ show wid
    Just v  -> pure (VV.RefVal wid v)

-- | Read a type tag (varuint32 in pyfory's wire format) plus
-- payload.
decodeTypedPayload :: DecodeM VV.Value
decodeTypedPayload = do
  tagW <- readVaruint32D
  decodePayloadFor (T.TypeId (fromIntegral tagW))

decodePayloadFor :: T.TypeId -> DecodeM VV.Value
decodePayloadFor tag = case tag of
  T.NONE     -> pure VV.NoneVal
  T.BOOL     -> VV.BoolVal . (/= 0) <$> readByteD
  T.INT8     -> VV.Int8Val . fromIntegral <$> readByteD
  T.INT16    -> VV.Int16Val <$> readInt16D
  T.INT32    -> VV.Int32Val <$> readInt32D
  T.VARINT32 -> VV.VarInt32Val <$> readVarint32D
  T.INT64    -> VV.Int64Val <$> readInt64D
  T.VARINT64 -> VV.VarInt64Val <$> readVarint64D
  T.UINT8    -> VV.Uint8Val <$> readByteD
  T.UINT16   -> VV.Uint16Val <$> readWord16D
  T.UINT32   -> VV.Uint32Val <$> readWord32D
  T.VAR_UINT32 -> VV.VarUint32Val <$> readVaruint32D
  T.UINT64   -> VV.Uint64Val <$> readWord64D
  T.VAR_UINT64 -> VV.VarUint64Val <$> readVaruint64D
  T.FLOAT32  -> VV.Float32Val <$> readFloat32D
  T.FLOAT64  -> VV.Float64Val <$> readFloat64D
  T.STRING   -> VV.StringVal <$> readForyString
  T.BINARY   -> do
    n   <- fromIntegral <$> readVaruint32D
    raw <- readBytesD n
    pure (VV.BinaryVal raw)
  T.LIST     -> VV.ListVal <$> decodeCollection
  T.SET      -> VV.SetVal  <$> decodeCollection
  T.MAP      -> decodeMapChunks
  T.NAMED_STRUCT -> do
    ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
    typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
    -- Disambiguate the pyfory-compatible wire layout from our
    -- in-package self-describing layout by checking the
    -- struct registry.
    opts <- getOptions
    case HM.lookup (ns, typeNm) (Opt.doStructRegistry opts) of
      Just sch -> decodeRegisteredStruct ns typeNm sch
      Nothing  -> do
        fields <- decodeStructFields
        pure (VV.StructVal ns typeNm fields)
  T.NAMED_COMPATIBLE_STRUCT -> decodeCompatibleStruct
  T.BOOL_ARRAY    -> VV.BoolArrayVal    <$> decodeBoolArray
  T.INT8_ARRAY    -> VV.Int8ArrayVal    <$> decodeInt8Array
  T.INT16_ARRAY   -> VV.Int16ArrayVal   <$> decodeInt16Array
  T.INT32_ARRAY   -> VV.Int32ArrayVal   <$> decodeInt32Array
  T.INT64_ARRAY   -> VV.Int64ArrayVal   <$> decodeInt64Array
  T.UINT8_ARRAY   -> VV.Uint8ArrayVal   <$> decodeUint8Array
  T.UINT16_ARRAY  -> VV.Uint16ArrayVal  <$> decodeUint16Array
  T.UINT32_ARRAY  -> VV.Uint32ArrayVal  <$> decodeUint32Array
  T.UINT64_ARRAY  -> VV.Uint64ArrayVal  <$> decodeUint64Array
  T.FLOAT32_ARRAY -> VV.Float32ArrayVal <$> decodeFloat32Array
  T.FLOAT64_ARRAY -> VV.Float64ArrayVal <$> decodeFloat64Array
  T.TypeId tw -> failD $
    "Fory.Decode.decodePayloadFor: unsupported type tag " ++ show tw

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

readForyString :: DecodeM Text
readForyString = do
  hdr <- readVaruint64D
  let !enc = hdr .&. 0x03
      !len = fromIntegral (hdr `shiftR` 2) :: Int
  raw <- readBytesD len
  case enc of
    0 -> pure (TE.decodeLatin1 raw)
    1 -> case decodeUtf16LE raw of
           Right t -> pure t
           Left e  -> failD ("Fory.Decode: invalid UTF-16: " ++ e)
    2 -> case TE.decodeUtf8' raw of
           Right t -> pure t
           Left e  -> failD ("Fory.Decode: invalid UTF-8: " ++ show e)
    _ -> failD ("Fory.Decode: reserved string encoding " ++ show enc)
{-# INLINE readForyString #-}

decodeUtf16LE :: ByteString -> Either String Text
decodeUtf16LE bs
  | BS.length bs `mod` 2 /= 0 = Left "odd-length UTF-16 byte string"
  | otherwise = Right (TE.decodeUtf16LE bs)

-- ---------------------------------------------------------------------------
-- Collections
-- ---------------------------------------------------------------------------

collFlagTrackingRef, collFlagHasNull, collFlagDeclElementType, collFlagIsSameType :: Word8
collFlagTrackingRef     = 0b0001
collFlagHasNull         = 0b0010
collFlagDeclElementType = 0b0100
collFlagIsSameType      = 0b1000

decodeCollection :: DecodeM (Vector VV.Value)
decodeCollection = do
  count <- fromIntegral <$> readVaruint32D
  if count == 0
    then pure V.empty
    else do
      flag <- readByteD
      let !sameType    = flag .&. collFlagIsSameType /= 0
          !hasNull     = flag .&. collFlagHasNull    /= 0
          !trackingRef = flag .&. collFlagTrackingRef /= 0
          !decl        = flag .&. collFlagDeclElementType /= 0
      if sameType
        then do
          elemTag <-
            if decl
              then pure Nothing
              else do
                tw <- readVaruint32D
                pure (Just (T.TypeId (fromIntegral tw)))
          case elemTag of
            Nothing ->
              failD "Fory.Decode: same-type collection without element type"
            Just tg -> do
              elemReader <- elementReaderForSameType tg
              if trackingRef
                then V.replicateM count (readSameTypeRefSlotE elemReader)
                else if hasNull
                  then V.replicateM count $ do
                    f <- readByteD
                    if f == slotNull
                      then pure VV.NoneVal
                      else if f == slotNotNullValue
                        then elemReader
                        else failD ("Fory.Decode: unexpected element flag "
                                    ++ show f)
                  else case sameTypeInlineBatch tg of
                    Just !rdr -> rdr count
                    Nothing -> case sameTypeFastReader tg of
                      Just rdr -> readSameTypeBatch count rdr
                      Nothing  -> V.replicateM count elemReader
        else
          if hasNull
            then V.replicateM count $ do
              f <- readByteD
              case f of
                _ | f == slotNull -> pure VV.NoneVal
                  | f == slotRef -> decodeRefBack
                  | f == slotRefValue -> withFreshRef decodeTypedPayload
                  | f == slotNotNullValue -> decodeTypedPayload
                  | otherwise -> failD
                      ("Fory.Decode: unexpected element flag " ++ show f)
            else V.replicateM count decodeTypedPayload

-- | Build a per-element payload reader for a same-type
-- collection. For named-struct elements we pre-read the
-- namespace + type-name meta-strings (and the matching
-- registry lookup) right after the element type tag; the
-- returned action then reads only the per-element payload.
elementReaderForSameType :: T.TypeId -> DecodeM (DecodeM VV.Value)
elementReaderForSameType tg = case tg of
  T.NAMED_STRUCT -> do
    ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
    typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
    opts   <- getOptions
    case HM.lookup (ns, typeNm) (Opt.doStructRegistry opts) of
      Just sch ->
        pure (decodeRegisteredStruct ns typeNm sch)
      Nothing -> do
        pure $ do
          fields <- decodeStructFields
          pure (VV.StructVal ns typeNm fields)
  T.NAMED_COMPATIBLE_STRUCT ->
    pure (decodePayloadFor tg)
  _ ->
    pure (decodePayloadFor tg)

-- | Fast-path reader table for the @same-type + no-null +
-- no-ref-tracking@ collection inner loop. Returns a raw
-- 'Ptr Word8'-based reader that gets a value + new offset
-- without touching the decoder's IORefs. The vector decode
-- loop calls it @count@ times against a single cached pointer
-- + position, paying the IORef cycle exactly once at the end.
-- | Fully-specialised same-type batch decoder.
--
-- Like 'sameTypeFastReader' but returns a /complete/
-- batch decoder rather than a per-element @rdr@. The
-- per-element rdr forces a boxed @(Value, Int)@ tuple
-- per element (see Core: @rVarInt64 ... -> (VarInt64Val
-- ww3, ww4) #@); this version writes 'Value' constructors
-- straight into a 'VM.IOVector' with no per-element tuple
-- allocation. Currently only specialised for VARINT64
-- (the encode/list-of-int / decode/list-of-int Value-
-- pipeline shape).
--
-- Returns 'Nothing' for typeIds that don't have an inline
-- specialisation; the caller falls back to
-- 'sameTypeFastReader' / 'V.replicateM'.
sameTypeInlineBatch
  :: T.TypeId -> Maybe (Int -> DecodeM (Vector VV.Value))
sameTypeInlineBatch !tag = case tag of
  T.VARINT64 -> Just readVarInt64ValBatch
  _          -> Nothing
{-# INLINE sameTypeInlineBatch #-}

-- | Read @count@ varint64s as 'VV.VarInt64Val' values
-- straight into a 'VM.IOVector', no per-element tuple
-- allocation. Compare to 'sameTypeFastReader''s
-- 'rVarInt64' which the dump shows materialising a fresh
-- @(Value, Int)@ tuple per call.
readVarInt64ValBatch :: Int -> DecodeM (Vector VV.Value)
readVarInt64ValBatch !count = DecodeM $ \d -> do
  pos0 <- readIORef (decPos d)
  let !p = decBase d
  mvec <- VM.unsafeNew count
  let go !i !pos
        | i >= count = pure pos
        | otherwise = do
            (!w, !pos1) <- peekVaruint64Raw p pos
            -- Inline zigzag decode without going through
            -- 'peekVarint64Raw''s boxed @(Int64, Int)@
            -- result tuple.
            let !signed = fromIntegral (w `shiftR` 1) :: Int64
                !sgn    = fromIntegral (w .&. 1) :: Int64
                !i64    = signed `xor` (-sgn)
            VM.unsafeWrite mvec i (VV.VarInt64Val i64)
            go (i + 1) pos1
  posF <- go 0 pos0
  writeIORef (decPos d) posF
  V.unsafeFreeze mvec
{-# INLINE readVarInt64ValBatch #-}

sameTypeFastReader
  :: T.TypeId
  -> Maybe (Ptr Word8 -> Int -> IO (VV.Value, Int))
sameTypeFastReader !tag = case tag of
  T.BOOL     -> Just rBool
  T.INT8     -> Just rInt8
  T.INT16    -> Just rInt16
  T.INT32    -> Just rInt32
  T.VARINT32 -> Just rVarInt32
  T.INT64    -> Just rInt64
  T.VARINT64 -> Just rVarInt64
  T.UINT8    -> Just rUint8
  T.UINT16   -> Just rUint16
  T.UINT32   -> Just rUint32
  T.VAR_UINT32 -> Just rVarUint32
  T.UINT64   -> Just rUint64
  T.VAR_UINT64 -> Just rVarUint64
  T.FLOAT32  -> Just rFloat32
  T.FLOAT64  -> Just rFloat64
  _          -> Nothing
  where
    rBool, rInt8, rUint8 :: Ptr Word8 -> Int -> IO (VV.Value, Int)
    rInt16, rUint16 :: Ptr Word8 -> Int -> IO (VV.Value, Int)
    rInt32, rUint32, rFloat32 :: Ptr Word8 -> Int -> IO (VV.Value, Int)
    rInt64, rUint64, rFloat64 :: Ptr Word8 -> Int -> IO (VV.Value, Int)
    rVarInt32, rVarInt64 :: Ptr Word8 -> Int -> IO (VV.Value, Int)
    rVarUint32, rVarUint64 :: Ptr Word8 -> Int -> IO (VV.Value, Int)

    rBool       p pos = do (b, p') <- peekByteRaw p pos
                           pure (VV.BoolVal (b /= 0), p')
    rInt8       p pos = do (b, p') <- peekByteRaw p pos
                           pure (VV.Int8Val (fromIntegral b), p')
    rInt16      p pos = do n <- peekByteOff p pos :: IO Int16
                           pure (VV.Int16Val n, pos + 2)
    rInt32      p pos = do (n, p') <- peekInt32LERaw p pos
                           pure (VV.Int32Val n, p')
    rVarInt32   p pos = do
      (v, p') <- peekVaruint64Raw p pos
      let !i = fromIntegral (v `shiftR` 1) :: Int32
          !s = fromIntegral (v .&. 1) :: Int32
      pure (VV.VarInt32Val (i `xor` (-s)), p')
    rInt64      p pos = do (n, p') <- peekInt64LERaw p pos
                           pure (VV.Int64Val n, p')
    rVarInt64   p pos = do (n, p') <- peekVarint64Raw p pos
                           pure (VV.VarInt64Val n, p')
    rUint8      p pos = do (b, p') <- peekByteRaw p pos
                           pure (VV.Uint8Val b, p')
    rUint16     p pos = do (w, p') <- peekWord16LERaw p pos
                           pure (VV.Uint16Val w, p')
    rUint32     p pos = do (w, p') <- peekWord32LERaw p pos
                           pure (VV.Uint32Val w, p')
    rVarUint32  p pos = do
      (v, p') <- peekVaruint64Raw p pos
      pure (VV.VarUint32Val (fromIntegral v), p')
    rUint64     p pos = do (w, p') <- peekWord64LERaw p pos
                           pure (VV.Uint64Val w, p')
    rVarUint64  p pos = do (v, p') <- peekVaruint64Raw p pos
                           pure (VV.VarUint64Val v, p')
    rFloat32    p pos = do (w, p') <- peekWord32LERaw p pos
                           pure (VV.Float32Val (castWord32ToFloat w), p')
    rFloat64    p pos = do (w, p') <- peekWord64LERaw p pos
                           pure (VV.Float64Val (castWord64ToDouble w), p')

-- | Reads @count@ elements from the wire using the supplied
-- raw reader, with one IORef cycle on the cursor at the
-- start and one at the end. The intermediate work is all
-- against a cached 'Ptr Word8' base and a local offset.
readSameTypeBatch
  :: Int
  -> (Ptr Word8 -> Int -> IO (a, Int))
  -> DecodeM (Vector a)
readSameTypeBatch !count !rdr = DecodeM $ \d -> do
  pos0 <- readIORef (decPos d)
  let !p = decBase d
  mvec <- VM.unsafeNew count
  let go !i !pos
        | i >= count = pure pos
        | otherwise = do
            (val, pos') <- rdr p pos
            VM.unsafeWrite mvec i val
            go (i + 1) pos'
  posF <- go 0 pos0
  writeIORef (decPos d) posF
  V.unsafeFreeze mvec
{-# INLINE readSameTypeBatch #-}

-- | Like 'readSameTypeBatch' but returns a plain Haskell list
-- without going through 'Vector' as an intermediate. Saves the
-- 'Vector' → list conversion when the caller wants a list.
readSameTypeBatchList
  :: Int
  -> (Ptr Word8 -> Int -> IO (a, Int))
  -> DecodeM [a]
readSameTypeBatchList !count !rdr = DecodeM $ \d -> do
  pos0 <- readIORef (decPos d)
  let !p = decBase d
      go !i !pos !acc
        | i >= count = pure (reverse acc, pos)
        | otherwise = do
            (val, pos') <- rdr p pos
            go (i + 1) pos' (val : acc)
  (xs, posF) <- go 0 pos0 []
  writeIORef (decPos d) posF
  pure xs
{-# INLINE readSameTypeBatchList #-}

-- | Element of a same-type ref-tracked homogeneous collection.
readSameTypeRefSlotE :: DecodeM VV.Value -> DecodeM VV.Value
readSameTypeRefSlotE inner = do
  f <- readByteD
  case f of
    _ | f == slotNull -> pure VV.NoneVal
      | f == slotRef -> decodeRefBack
      | f == slotRefValue -> withFreshRef inner
      | otherwise -> failD
          ("Fory.Decode: unexpected ref-tracked element flag " ++ show f)

-- ---------------------------------------------------------------------------
-- Maps
-- ---------------------------------------------------------------------------

mapKeyHasNull, mapValueHasNull :: Word8
mapKeyHasNull       = 0b0000_0010
mapValueHasNull     = 0b0001_0000

decodeMapChunks :: DecodeM VV.Value
decodeMapChunks = do
  total <- fromIntegral <$> readVaruint32D
  if total == 0
    then pure (VV.MapVal V.empty)
    else do
      pairs <- collectChunks total []
      pure (VV.MapVal (V.fromList (reverse pairs)))
  where
    collectChunks 0 acc = pure acc
    collectChunks remaining acc = do
      header <- readByteD
      let !keyNull = header .&. mapKeyHasNull /= 0
          !valNull = header .&. mapValueHasNull /= 0
      case (keyNull, valNull) of
        (True, True) ->
          collectChunks (remaining - 1) ((VV.NoneVal, VV.NoneVal) : acc)
        (True, False) -> do
          flag <- readByteD
          if flag /= slotNotNullValue
            then failD ("Fury.Decode: expected NOT_NULL_VALUE for partial-"
                        ++ "null map value, got " ++ show flag)
            else do
              tw <- readVaruint32D
              v  <- decodePayloadFor (T.TypeId (fromIntegral tw))
              collectChunks (remaining - 1) ((VV.NoneVal, v) : acc)
        (False, True) -> do
          flag <- readByteD
          if flag /= slotNotNullValue
            then failD ("Fury.Decode: expected NOT_NULL_VALUE for partial-"
                        ++ "null map key, got " ++ show flag)
            else do
              tw <- readVaruint32D
              k  <- decodePayloadFor (T.TypeId (fromIntegral tw))
              collectChunks (remaining - 1) ((k, VV.NoneVal) : acc)
        (False, False) -> do
          chunkSize <- fromIntegral <$> readByteD
          twk <- readVaruint32D
          twv <- readVaruint32D
          let keyTag = T.TypeId (fromIntegral twk)
              valTag = T.TypeId (fromIntegral twv)
          entries <- replicateMList chunkSize $ do
            k <- decodePayloadFor keyTag
            v <- decodePayloadFor valTag
            pure (k, v)
          collectChunks (remaining - chunkSize)
                        (foldl (flip (:)) acc entries)

replicateMList :: Int -> DecodeM a -> DecodeM [a]
replicateMList n act
  | n <= 0    = pure []
  | otherwise = do
      x  <- act
      xs <- replicateMList (n - 1) act
      pure (x : xs)

-- ---------------------------------------------------------------------------
-- Struct (NAMED_STRUCT)
-- ---------------------------------------------------------------------------

decodeStructFields :: DecodeM VV.StructFields
decodeStructFields = do
  n <- fromIntegral <$> readVaruint32D
  V.replicateM (n :: Int) $ do
    name <- decodeMetaStringWith MSE.namespaceSpecialChars
    val  <- decodeValueSlot
    pure (name, val)

decodeRegisteredStruct
  :: Text -> Text -> ST.StructSchema -> DecodeM VV.Value
decodeRegisteredStruct ns typeNm sch = do
  wireHash <- readInt32D
  let expected = ST.ssHash sch
  if wireHash /= expected
    then failD $
      "Fory.Decode: struct schema hash mismatch for "
        ++ T.unpack ns ++ "." ++ T.unpack typeNm
        ++ ": wire " ++ show wireHash
        ++ " /= local " ++ show expected
    else do
      let !canonical = ST.fieldOrder sch
          !names     = ST.ssFieldOrderNames sch
          !nFields   = V.length canonical
      -- Build the result @Vector (Text, Value)@ directly
      -- via 'VM.unsafeNew' / 'VM.unsafeWrite' / freeze
      -- instead of going through @V.mapM ...@ + @V.zip
      -- names ...@. Saves one intermediate 'Vector Value'
      -- allocation per struct.
      DecodeM $ \d -> do
        mvec <- VM.unsafeNew nFields
        let go !i
              | i >= nFields = pure ()
              | otherwise    = do
                  let !spec = V.unsafeIndex canonical i
                      !nm   = V.unsafeIndex names i
                  v <- runDM (readField spec) d
                  VM.unsafeWrite mvec i (nm, v)
                  go (i + 1)
        go 0
        result <- V.unsafeFreeze mvec
        pure (VV.RegisteredStructVal ns typeNm result)
  where
    readField :: ST.FieldSpec -> DecodeM VV.Value
    readField spec
      | ST.isBasicTypeId (ST.fsTypeId spec) =
          if ST.fsNullable spec
            then do
              flag <- readByteD
              if flag == slotNull
                then pure VV.NoneVal
                else if flag == slotNotNullValue
                  then decodePayloadFor (ST.fsTypeId spec)
                  else failD ("Fory.Decode: bad nullable basic field flag "
                              ++ show flag)
            else decodePayloadFor (ST.fsTypeId spec)
      | otherwise = do
          flag <- readByteD
          if flag == slotNull
            then pure VV.NoneVal
            else if flag == slotNotNullValue
              then decodePayloadFor (ST.fsTypeId spec)
              else failD ("Fory.Decode: bad non-basic field flag "
                          ++ show flag)

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

decodeMetaStringWith :: MSE.SpecialChars -> DecodeM Text
decodeMetaStringWith sc = do
  hdr <- liftEitherD MS.readMetaStringHeader
  case hdr of
    MS.MetaStringRef rid -> do
      m <- lookupStringPool rid
      case m of
        Nothing -> failD $
          "Fory.Decode.decodeMetaString: ref to unknown id " ++ show rid
        Just t  -> pure t
    MS.MetaStringFresh len -> do
      t <- liftEitherD (MS.readFreshMetaStringPayload sc len)
      registerStringPool t
      pure t

-- | Lift an old-style @ByteString -> Int -> Either String (a, Int)@
-- function (still used by 'Fory.MetaString') into the new IO-based
-- 'DecodeM'.
liftEitherD :: (ByteString -> Int -> Either String (a, Int)) -> DecodeM a
liftEitherD f = DecodeM $ \d -> do
  pos <- readIORef (decPos d)
  case f (decBs d) pos of
    Left e -> throwIO (DecodeError e)
    Right (a, pos') -> do
      writeIORef (decPos d) pos'
      pure a
{-# INLINE liftEitherD #-}

-- ---------------------------------------------------------------------------
-- TypeDef + NAMED_COMPATIBLE_STRUCT
-- ---------------------------------------------------------------------------

decodeCompatibleStruct :: DecodeM VV.Value
decodeCompatibleStruct = do
  marker <- readVaruint64D
  td <- if marker .&. 1 /= 0
    then do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      m <- lookupTypeDef idx
      case m of
        Nothing -> failD $
          "Fory.Decode: TypeDef ref to unknown index " ++ show idx
        Just td -> pure td
    else do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      td <- decodeTypeDefBytes
      registerTypeDef idx td
      pure td
  values <- V.mapM (const decodeValueSlot) (tdFieldNames td)
  let fields = V.zip (tdFieldNames td) values
  pure (VV.CompatibleStructVal (tdNamespace td) (tdTypeName td) fields)

decodeTypeDefBytes :: DecodeM TypeDef
decodeTypeDefBytes = do
  hdr <- readWord64D
  let !rawSize = hdr .&. 0xFF
  bodyLen <-
    if rawSize == 0xFF
      then do
        ext <- readVaruint32D
        pure (0xFF + fromIntegral ext :: Int)
      else
        pure (fromIntegral rawSize :: Int)
  off0 <- getOff
  td <- decodeTypeDefBody
  off1 <- getOff
  let !consumed = off1 - off0
  if consumed /= bodyLen
    then failD $ "Fory.Decode: TypeDef body size mismatch (header said "
                  ++ show bodyLen ++ ", consumed " ++ show consumed ++ ")"
    else pure td

decodeTypeDefBody :: DecodeM TypeDef
decodeTypeDefBody = do
  metaHeader <- readByteD
  let !rawNumFields = fromIntegral (metaHeader .&. 0x1F) :: Int
      !registered = (metaHeader .&. 0x20) /= 0
  numFields <- if rawNumFields >= 31
    then do
      ext <- readVaruint32D
      pure (rawNumFields + fromIntegral ext)
    else
      pure rawNumFields
  if not registered
    then failD "Fory.Decode: TypeDef without REGISTER_BY_NAME flag"
    else do
      ns     <- decodeMetaStringWith MSE.namespaceSpecialChars
      typeNm <- decodeMetaStringWith MSE.typenameSpecialChars
      fieldNames <- V.replicateM numFields $ do
        fname <- decodeMetaStringWith MSE.namespaceSpecialChars
        _typeId <- readVaruint32D
        pure fname
      pure (TypeDef ns typeNm fieldNames)

-- ---------------------------------------------------------------------------
-- Primitive 1-D arrays
-- ---------------------------------------------------------------------------

bulkArray
  :: Int                              -- ^ element size in bytes
  -> (ByteString -> VS.Vector a)      -- ^ zero-copy reinterpret
  -> DecodeM (VS.Vector a)
bulkArray elemBytes f = do
  byteLen <- fromIntegral <$> readVaruint32D
  let (_, r) = byteLen `quotRem` elemBytes
  if r /= 0
    then failD $ "Fory.Decode: array byte length " ++ show byteLen
                  ++ " not a multiple of element size " ++ show elemBytes
    else f <$> readBytesD byteLen

decodeBoolArray    = bulkArray 1 B.bytesToBoolArray
decodeInt8Array    = bulkArray 1 B.bytesToInt8Array
decodeInt16Array   = bulkArray 2 B.bytesToInt16Array
decodeInt32Array   = bulkArray 4 B.bytesToInt32Array
decodeInt64Array   = bulkArray 8 B.bytesToInt64Array
decodeUint8Array   = bulkArray 1 B.bytesToUint8Array
decodeUint16Array  = bulkArray 2 B.bytesToUint16Array
decodeUint32Array  = bulkArray 4 B.bytesToUint32Array
decodeUint64Array  = bulkArray 8 B.bytesToUint64Array
decodeFloat32Array = bulkArray 4 B.bytesToFloat32Array
decodeFloat64Array = bulkArray 8 B.bytesToFloat64Array

decodeBoolArray    :: DecodeM (VS.Vector Word8)
decodeInt8Array    :: DecodeM (VS.Vector Int8)
decodeInt16Array   :: DecodeM (VS.Vector Int16)
decodeInt32Array   :: DecodeM (VS.Vector Int32)
decodeInt64Array   :: DecodeM (VS.Vector Int64)
decodeUint8Array   :: DecodeM (VS.Vector Word8)
decodeUint16Array  :: DecodeM (VS.Vector Word16)
decodeUint32Array  :: DecodeM (VS.Vector Word32)
decodeUint64Array  :: DecodeM (VS.Vector Word64)
decodeFloat32Array :: DecodeM (VS.Vector Float)
decodeFloat64Array :: DecodeM (VS.Vector Double)
