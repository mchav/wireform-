{-# LANGUAGE BangPatterns #-}
-- | Apache Fory xlang value decoder.
--
-- Mirrors 'Fury.Encode.encode'. See that module's haddock for the
-- exact subset of the spec we round-trip through and the
-- intentional simplifications.
module Fury.Decode
  ( decode
  , decodeValueSlot
  , DecodeM
  , runDecodeM
  ) where

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Fury.Encoding as E
import qualified Fury.MetaString as MS
import qualified Fury.TypeId as T
import qualified Fury.Value as VV

-- ---------------------------------------------------------------------------
-- Decoder state monad
-- ---------------------------------------------------------------------------

data DecodeState = DecodeState
  { dsStringPool :: !(IntMap Text)
    -- ^ Index → meta string. Indexed by the order in which fresh
    -- meta strings are read, matching the encoder\'s assignment.
  , dsNextStringId :: {-# UNPACK #-} !Int
  , dsRefValues  :: !(IntMap VV.Value)
    -- ^ Index → already-decoded ref-tracked value, indexed by the
    -- wire @ref_id@.
  , dsNextRefId  :: {-# UNPACK #-} !Int
  , dsTypeDefs   :: !(IntMap TypeDef)
  , dsNextTypeDefId :: {-# UNPACK #-} !Int
  }

-- | Cached schema for a 'CompatibleStructVal'. Carries enough to
-- decode subsequent occurrences without re-reading the body.
data TypeDef = TypeDef
  { tdNamespace  :: !Text
  , tdTypeName   :: !Text
  , tdFieldNames :: !(Vector Text)
  } deriving (Show, Eq)

emptyDecodeState :: DecodeState
emptyDecodeState =
  DecodeState IM.empty 0 IM.empty 0 IM.empty 0

newtype DecodeM a = DecodeM
  { runDM
      :: ByteString
      -> Int
      -> DecodeState
      -> Either String (a, Int, DecodeState)
  }

instance Functor DecodeM where
  fmap f (DecodeM g) = DecodeM $ \bs off st ->
    case g bs off st of
      Left e               -> Left e
      Right (a, off', st') -> Right (f a, off', st')
  {-# INLINE fmap #-}

instance Applicative DecodeM where
  pure x = DecodeM $ \_ off st -> Right (x, off, st)
  {-# INLINE pure #-}
  DecodeM f <*> DecodeM x = DecodeM $ \bs off st ->
    case f bs off st of
      Left e -> Left e
      Right (g, off1, st1) -> case x bs off1 st1 of
        Left e -> Left e
        Right (a, off2, st2) -> Right (g a, off2, st2)

instance Monad DecodeM where
  DecodeM m >>= k = DecodeM $ \bs off st ->
    case m bs off st of
      Left e -> Left e
      Right (a, off', st') -> runDM (k a) bs off' st'
  {-# INLINE (>>=) #-}

-- | Run a 'DecodeM' action against a byte string starting at the
-- given offset, returning the decoded value, the new offset, and
-- the final state.
runDecodeM :: DecodeM a -> ByteString -> Either String a
runDecodeM (DecodeM m) bs =
  case m bs 0 emptyDecodeState of
    Left e -> Left e
    Right (a, off, _)
      | off == BS.length bs -> Right a
      | otherwise -> Left $ "Fury.Decode: " ++ show (BS.length bs - off)
                            ++ " trailing bytes"

liftEither :: (ByteString -> Int -> Either String (a, Int)) -> DecodeM a
liftEither f = DecodeM $ \bs off st ->
  case f bs off of
    Left e               -> Left e
    Right (a, off')      -> Right (a, off', st)
{-# INLINE liftEither #-}

getOff :: DecodeM Int
getOff = DecodeM $ \_ off st -> Right (off, off, st)

getState :: DecodeM DecodeState
getState = DecodeM $ \_ off st -> Right (st, off, st)

modifyState :: (DecodeState -> DecodeState) -> DecodeM ()
modifyState f = DecodeM $ \_ off st -> Right ((), off, f st)

failD :: String -> DecodeM a
failD e = DecodeM $ \_ _ _ -> Left e

readByteD :: DecodeM Word8
readByteD = liftEither E.readByte

readBytesD :: Int -> DecodeM ByteString
readBytesD n = liftEither (E.readBytes n)

readWord16D :: DecodeM Word16
readWord16D = liftEither E.readWord16LE

readWord32D :: DecodeM Word32
readWord32D = liftEither E.readWord32LE

readWord64D :: DecodeM Word64
readWord64D = liftEither E.readWord64LE

readInt16D :: DecodeM Int16
readInt16D = liftEither E.readInt16LE

readInt32D :: DecodeM Int32
readInt32D = liftEither E.readInt32LE

readInt64D :: DecodeM Int64
readInt64D = liftEither E.readInt64LE

readFloat32D :: DecodeM Float
readFloat32D = liftEither E.readFloat32LE

readFloat64D :: DecodeM Double
readFloat64D = liftEither E.readFloat64LE

readVaruint32D :: DecodeM Word32
readVaruint32D = liftEither E.readVaruint32

readVaruint64D :: DecodeM Word64
readVaruint64D = liftEither E.readVaruint64

readUtf8StringD :: DecodeM Text
readUtf8StringD = liftEither E.readUtf8String

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse a fory-encoded byte string back to a 'Value'.
decode :: ByteString -> Either String VV.Value
decode bs = runDecodeM go bs
  where
    go = do
      hdr <- readByteD
      if hdr .&. 0x01 /= 0
        then pure VV.NoneVal
        else if hdr .&. 0x02 == 0
          then failD ("Fury.Decode.decode: missing xlang flag in header byte "
                      ++ show hdr)
          else decodeValueSlot

-- ---------------------------------------------------------------------------
-- Value slot
-- ---------------------------------------------------------------------------

-- | Decode one value slot (ref flag + payload).
decodeValueSlot :: DecodeM VV.Value
decodeValueSlot = do
  flag <- readByteD
  case flag of
    f | f == E.refFlagNull       -> pure VV.NoneVal
      | f == E.refFlagRef        -> decodeRefBack
      | f == E.refFlagRefValue   -> decodeRefValue
      | otherwise                -> decodePayload (T.TypeId f)

decodeRefBack :: DecodeM VV.Value
decodeRefBack = do
  wid <- fromIntegral <$> readVaruint32D
  st  <- getState
  case IM.lookup wid (dsRefValues st) of
    Nothing -> failD $
      "Fury.Decode: REF flag references unknown ref_id " ++ show wid
    Just v  -> pure (VV.RefVal wid v)

decodeRefValue :: DecodeM VV.Value
decodeRefValue = do
  st <- getState
  let !wid = dsNextRefId st
  modifyState $ \s -> s { dsNextRefId = wid + 1 }
  inner <- do
    -- Read the inner value\'s payload; the next byte is its type
    -- tag (since the surrounding REF_VALUE_FLAG already played
    -- the role of the slot ref flag).
    tag <- readByteD
    decodePayload (T.TypeId tag)
  modifyState $ \s -> s { dsRefValues = IM.insert wid inner (dsRefValues s) }
  pure (VV.RefVal wid inner)

-- | Decode a payload, given its already-consumed type tag.
decodePayload :: T.TypeId -> DecodeM VV.Value
decodePayload tag = case tag of
  T.NONE     -> pure VV.NoneVal
  T.BOOL     -> VV.BoolVal . (/= 0) <$> readByteD
  T.INT8     -> VV.Int8Val . fromIntegral <$> readByteD
  T.INT16    -> VV.Int16Val <$> readInt16D
  T.INT32    -> VV.Int32Val <$> readInt32D
  T.INT64    -> VV.Int64Val <$> readInt64D
  T.UINT8    -> VV.Uint8Val <$> readByteD
  T.UINT16   -> VV.Uint16Val <$> readWord16D
  T.UINT32   -> VV.Uint32Val <$> readWord32D
  T.UINT64   -> VV.Uint64Val <$> readWord64D
  T.FLOAT32  -> VV.Float32Val <$> readFloat32D
  T.FLOAT64  -> VV.Float64Val <$> readFloat64D
  T.STRING   -> VV.StringVal <$> readUtf8StringD
  T.BINARY   -> do
    n   <- fromIntegral <$> readVaruint32D
    raw <- readBytesD n
    pure (VV.BinaryVal raw)
  T.LIST           -> VV.ListVal <$> decodeCollection
  T.SET            -> VV.SetVal  <$> decodeCollection
  T.MAP            -> decodeMap
  T.NAMED_STRUCT   -> do
    ns     <- decodeMetaString
    typeNm <- decodeMetaString
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
    "Fury.Decode.decodePayload: unsupported type tag " ++ show tw

-- ---------------------------------------------------------------------------
-- Collections
-- ---------------------------------------------------------------------------

decodeCollection :: DecodeM (Vector VV.Value)
decodeCollection = do
  n <- fromIntegral <$> readVaruint32D
  V.replicateM n decodeValueSlot

decodeMap :: DecodeM VV.Value
decodeMap = do
  n <- fromIntegral <$> readVaruint32D
  pairs <- V.replicateM n $ do
    k <- decodeValueSlot
    v <- decodeValueSlot
    pure (k, v)
  pure (VV.MapVal pairs)

decodeStructFields :: DecodeM VV.StructFields
decodeStructFields = do
  n <- fromIntegral <$> readVaruint32D
  V.replicateM n $ do
    name <- decodeMetaString
    val  <- decodeValueSlot
    pure (name, val)

-- ---------------------------------------------------------------------------
-- Meta-string deduplication (decoder side)
-- ---------------------------------------------------------------------------

decodeMetaString :: DecodeM Text
decodeMetaString = do
  hdr <- liftEither MS.readMetaStringHeader
  case hdr of
    MS.MetaStringRef rid -> do
      st <- getState
      case IM.lookup rid (dsStringPool st) of
        Nothing -> failD $
          "Fury.Decode.decodeMetaString: ref to unknown id " ++ show rid
        Just t  -> pure t
    MS.MetaStringFresh len -> do
      t  <- liftEither (MS.readFreshMetaStringPayload len)
      st <- getState
      let !nid = dsNextStringId st
      modifyState $ \s -> s
        { dsStringPool   = IM.insert nid t (dsStringPool s)
        , dsNextStringId = nid + 1
        }
      pure t

-- ---------------------------------------------------------------------------
-- TypeDef + NAMED_COMPATIBLE_STRUCT
-- ---------------------------------------------------------------------------

decodeCompatibleStruct :: DecodeM VV.Value
decodeCompatibleStruct = do
  marker <- readVaruint64D
  td <- if marker .&. 1 /= 0
    then do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      st <- getState
      case IM.lookup idx (dsTypeDefs st) of
        Nothing -> failD $
          "Fury.Decode: TypeDef ref to unknown index " ++ show idx
        Just td -> pure td
    else do
      let !idx = fromIntegral (marker `shiftR` 1) :: Int
      td <- decodeTypeDefBytes
      modifyState $ \s -> s
        { dsTypeDefs      = IM.insert idx td (dsTypeDefs s)
        , dsNextTypeDefId = idx + 1
        }
      pure td
  -- Field values follow, in TypeDef order.
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
  -- We don\'t use bodyLen to bound reads because the inner reads
  -- consume exactly the right number of bytes already; the size
  -- prefix is purely informational for our simplified body layout.
  off0 <- getOff
  td <- decodeTypeDefBody
  off1 <- getOff
  let !consumed = off1 - off0
  if consumed /= bodyLen
    then failD $ "Fury.Decode: TypeDef body size mismatch (header said "
                  ++ show bodyLen ++ ", consumed " ++ show consumed ++ ")"
    else pure td

decodeTypeDefBody :: DecodeM TypeDef
decodeTypeDefBody = do
  metaHeader <- readByteD
  let !rawNumFields = fromIntegral (metaHeader .&. 0x1F) :: Int
      -- Bit 5 is REGISTER_BY_NAME; we always set it on the
      -- encoder side, so decoders need only verify it.
      !registered = (metaHeader .&. 0x20) /= 0
  numFields <- if rawNumFields >= 31
    then do
      ext <- readVaruint32D
      pure (rawNumFields + fromIntegral ext)
    else
      pure rawNumFields
  if not registered
    then failD "Fury.Decode: TypeDef without REGISTER_BY_NAME flag"
    else do
      ns     <- decodeMetaString
      typeNm <- decodeMetaString
      fieldNames <- V.replicateM numFields $ do
        fname <- decodeMetaString
        _typeId <- readVaruint32D  -- discarded; see Fury.Encode
        pure fname
      pure (TypeDef ns typeNm fieldNames)

-- ---------------------------------------------------------------------------
-- Primitive 1-D arrays
-- ---------------------------------------------------------------------------

readArrayCount :: DecodeM Int
readArrayCount = fromIntegral <$> readVaruint32D

decodeBoolArray :: DecodeM (Vector Bool)
decodeBoolArray = do
  n <- readArrayCount
  V.replicateM n ((/= 0) <$> readByteD)

decodeInt8Array :: DecodeM (Vector Int8)
decodeInt8Array = do
  n <- readArrayCount
  V.replicateM n (fromIntegral <$> readByteD)

decodeInt16Array :: DecodeM (Vector Int16)
decodeInt16Array = do
  n <- readArrayCount
  V.replicateM n readInt16D

decodeInt32Array :: DecodeM (Vector Int32)
decodeInt32Array = do
  n <- readArrayCount
  V.replicateM n readInt32D

decodeInt64Array :: DecodeM (Vector Int64)
decodeInt64Array = do
  n <- readArrayCount
  V.replicateM n readInt64D

decodeUint8Array :: DecodeM (Vector Word8)
decodeUint8Array = do
  n <- readArrayCount
  V.replicateM n readByteD

decodeUint16Array :: DecodeM (Vector Word16)
decodeUint16Array = do
  n <- readArrayCount
  V.replicateM n readWord16D

decodeUint32Array :: DecodeM (Vector Word32)
decodeUint32Array = do
  n <- readArrayCount
  V.replicateM n readWord32D

decodeUint64Array :: DecodeM (Vector Word64)
decodeUint64Array = do
  n <- readArrayCount
  V.replicateM n readWord64D

decodeFloat32Array :: DecodeM (Vector Float)
decodeFloat32Array = do
  n <- readArrayCount
  V.replicateM n readFloat32D

decodeFloat64Array :: DecodeM (Vector Double)
decodeFloat64Array = do
  n <- readArrayCount
  V.replicateM n readFloat64D

