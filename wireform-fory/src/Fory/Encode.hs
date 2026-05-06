{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Fory xlang value encoder.
--
-- Architecture: an in-place IO encoder.
--
-- Each top-level @encode@ allocates a 'Fory.IO.Encoder' (a
-- @ForeignPtr Word8@ that grows on demand plus 'IORef's for
-- the dedup pools), recursively walks the 'Fory.Value.Value'
-- tree writing bytes via raw 'pokeByteOff' calls, and finalises
-- the buffer to a 'ByteString'. The encoder runs in 'IO' but is
-- exposed as a pure function: @encode@ wraps the action in
-- 'unsafeDupablePerformIO' because the buffer is local to one
-- call.
--
-- This drops the per-byte cost of the previous writer-state-monad
-- + 'ByteString.Builder' design — every emit now costs roughly
-- one @readIORef@ + one @pokeByteOff@ + one @writeIORef@, plus
-- an amortised capacity check. Bulk paths (primitive arrays,
-- long strings) reuse 'Fory.Bulk' under the hood.
--
-- Wire format compatibility with @pyfory@ 0.17 is preserved
-- exactly; see "Fory.Decode" for the corresponding reader.
module Fory.Encode
  ( -- * Top-level encoders
    encode
  , encodeWith
  ) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word64)
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Fory.Bulk as B
import qualified Fory.IO as IO
import qualified Fory.MetaString.Encoder as MSE
import qualified Fory.MetaString.Hash as MSH
import qualified Fory.Options as Opt
import qualified Fory.Struct as ST
import qualified Fory.TypeId as T
import qualified Fory.Value as VV

-- ---------------------------------------------------------------------------
-- Slot flag bytes (matching pyfory's @resolver.NULL_FLAG@ etc.)
-- ---------------------------------------------------------------------------

slotNotNullValue, slotNull, slotRefValue, slotRef :: Word8
slotNotNullValue = 0xFF
slotNull         = 0xFD
slotRefValue     = 0x00
slotRef          = 0xFE

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Encode a 'Value' to a fory xlang byte sequence under the
-- default 'Opt.defaultEncodeOptions' (no reference tracking).
encode :: VV.Value -> ByteString
encode = encodeWith Opt.defaultEncodeOptions

-- | Encode under explicit 'Opt.EncodeOptions'.
encodeWith :: Opt.EncodeOptions -> VV.Value -> ByteString
encodeWith !opts !v = unsafeDupablePerformIO $
  IO.runEncoder opts (\e -> emitTopLevel e v)
{-# NOINLINE encodeWith #-}

-- ---------------------------------------------------------------------------
-- Top-level
-- ---------------------------------------------------------------------------

emitTopLevel :: IO.Encoder -> VV.Value -> IO ()
emitTopLevel !e VV.NoneVal = do
  -- pyfory: header has the xlang flag (bit 1) /and/ the null
  -- flag (bit 0); the slot byte is then NULL_FLAG too.
  IO.emitByte e 0x03
  IO.emitByte e slotNull
emitTopLevel !e v = do
  IO.emitByte e 0x02
  emitValueSlot e v

-- ---------------------------------------------------------------------------
-- Value slot
-- ---------------------------------------------------------------------------

emitValueSlot :: IO.Encoder -> VV.Value -> IO ()
emitValueSlot !e v = case v of
  VV.NoneVal        -> IO.emitByte e slotNull
  VV.RefVal i inner -> emitRefByUserKey e i inner
  _ -> do
    let !refOn = Opt.eoRefTracking (IO.encOptions e)
    if refOn && needsToWriteRef v
      then emitRefStructural e v False
      else do
        IO.emitByte e slotNotNullValue
        emitTypedPayload e v
{-# INLINE emitValueSlot #-}

needsToWriteRef :: VV.Value -> Bool
needsToWriteRef = \case
  VV.NoneVal           -> False
  VV.BoolVal{}         -> False
  VV.Int8Val{}         -> False
  VV.Int16Val{}        -> False
  VV.Int32Val{}        -> False
  VV.VarInt32Val{}     -> False
  VV.Int64Val{}        -> False
  VV.VarInt64Val{}     -> False
  VV.Uint8Val{}        -> False
  VV.Uint16Val{}       -> False
  VV.Uint32Val{}       -> False
  VV.VarUint32Val{}    -> False
  VV.Uint64Val{}       -> False
  VV.VarUint64Val{}    -> False
  VV.Float32Val{}      -> False
  VV.Float64Val{}      -> False
  VV.StringVal{}       -> False
  VV.BinaryVal{}       -> False
  VV.ListVal{}         -> True
  VV.SetVal{}          -> True
  VV.MapVal{}          -> True
  VV.StructVal{}       -> True
  VV.RegisteredStructVal{} -> True
  VV.CompatibleStructVal{} -> True
  VV.RefVal{}          -> True
  VV.BoolArrayVal{}    -> True
  VV.Int8ArrayVal{}    -> True
  VV.Int16ArrayVal{}   -> True
  VV.Int32ArrayVal{}   -> True
  VV.Int64ArrayVal{}   -> True
  VV.Uint8ArrayVal{}   -> True
  VV.Uint16ArrayVal{}  -> True
  VV.Uint32ArrayVal{}  -> True
  VV.Uint64ArrayVal{}  -> True
  VV.Float32ArrayVal{} -> True
  VV.Float64ArrayVal{} -> True

-- | First time we see a particular structural value, emit
-- @REF_VALUE_FLAG + payload@ and remember the wire id; second
-- and later time, emit @REF_FLAG + varuint32 wire_id@.
--
-- The 'untagged' flag is True when we are inside a
-- @SAME_TYPE@ ref-tracked collection (where the element type
-- tag was already written once).
emitRefStructural :: IO.Encoder -> VV.Value -> Bool -> IO ()
emitRefStructural !e !v !untagged = do
  m <- IO.structRefLookup e v
  case m of
    Just wid -> do
      IO.emitByte e slotRef
      IO.emitVaruint32 e (fromIntegral wid)
    Nothing -> do
      _ <- IO.structRefRegister e v
      IO.emitByte e slotRefValue
      if untagged then emitUntaggedPayload e v else emitTypedPayload e v

emitRefByUserKey :: IO.Encoder -> Int -> VV.Value -> IO ()
emitRefByUserKey !e !key !inner = do
  m <- IO.refLookup e key
  case m of
    Just wid -> do
      IO.emitByte e slotRef
      IO.emitVaruint32 e (fromIntegral wid)
    Nothing -> do
      _ <- IO.refRegister e key
      IO.emitByte e slotRefValue
      emitTypedPayload e inner

-- ---------------------------------------------------------------------------
-- Type-tagged payload: type tag + payload bytes.
-- ---------------------------------------------------------------------------

emitTypedPayload :: IO.Encoder -> VV.Value -> IO ()
emitTypedPayload !e val = case val of
  VV.NoneVal       -> emitTag e T.NONE
  VV.BoolVal b     -> do
    emitTag e T.BOOL
    IO.emitByte e (if b then 1 else 0)
  VV.Int8Val n     -> do
    emitTag e T.INT8
    IO.emitByte e (fromIntegral n)
  VV.Int16Val n    -> emitTag e T.INT16   >> IO.emitInt16LE e n
  VV.Int32Val n    -> emitTag e T.INT32   >> IO.emitInt32LE e n
  VV.VarInt32Val n -> emitTag e T.VARINT32 >> IO.emitVarint32 e n
  VV.Int64Val n    -> emitTag e T.INT64   >> IO.emitInt64LE e n
  VV.VarInt64Val n -> emitTag e T.VARINT64 >> IO.emitVarint64 e n
  VV.Uint8Val n    -> emitTag e T.UINT8   >> IO.emitByte e n
  VV.Uint16Val n   -> emitTag e T.UINT16  >> IO.emitWord16LE e n
  VV.Uint32Val n   -> emitTag e T.UINT32  >> IO.emitWord32LE e n
  VV.VarUint32Val n -> emitTag e T.VAR_UINT32 >> IO.emitVaruint32 e n
  VV.Uint64Val n   -> emitTag e T.UINT64  >> IO.emitWord64LE e n
  VV.VarUint64Val n -> emitTag e T.VAR_UINT64 >> IO.emitVaruint64 e n
  VV.Float32Val f  -> emitTag e T.FLOAT32 >> IO.emitFloat32LE e f
  VV.Float64Val d  -> emitTag e T.FLOAT64 >> IO.emitFloat64LE e d
  VV.StringVal s   -> emitTag e T.STRING  >> emitForyString e s
  VV.BinaryVal bs  -> do
    emitTag e T.BINARY
    IO.emitVaruint32 e (fromIntegral (BS.length bs))
    IO.emitBytes e bs
  VV.ListVal vs    -> emitTag e T.LIST >> emitCollection e vs
  VV.SetVal vs     -> emitTag e T.SET  >> emitCollection e vs
  VV.MapVal kvs    -> emitTag e T.MAP  >> emitMapChunks e kvs
  VV.StructVal ns nm fields -> do
    emitTag e T.NAMED_STRUCT
    emitMetaStringWith e MSE.namespaceSpecialChars ns
    emitMetaStringWith e MSE.typenameSpecialChars  nm
    emitStructFields e fields
  VV.RegisteredStructVal ns nm fields -> do
    emitTag e T.NAMED_STRUCT
    emitMetaStringWith e MSE.namespaceSpecialChars ns
    emitMetaStringWith e MSE.typenameSpecialChars  nm
    emitRegisteredStruct e ns nm fields
  VV.CompatibleStructVal ns nm fields -> do
    emitTag e T.NAMED_COMPATIBLE_STRUCT
    emitTypeDef e ns nm fields
    V.forM_ fields $ \(_, fv) -> emitValueSlot e fv
  VV.RefVal{} -> emitValueSlot e val
  VV.BoolArrayVal vs    -> emitTag e T.BOOL_ARRAY    >> emitArrayBytes e (B.boolArrayBytes vs)
  VV.Int8ArrayVal vs    -> emitTag e T.INT8_ARRAY    >> emitArrayBytes e (B.int8ArrayBytes vs)
  VV.Int16ArrayVal vs   -> emitTag e T.INT16_ARRAY   >> emitArrayBytes e (B.int16ArrayBytes vs)
  VV.Int32ArrayVal vs   -> emitTag e T.INT32_ARRAY   >> emitArrayBytes e (B.int32ArrayBytes vs)
  VV.Int64ArrayVal vs   -> emitTag e T.INT64_ARRAY   >> emitArrayBytes e (B.int64ArrayBytes vs)
  VV.Uint8ArrayVal vs   -> emitTag e T.UINT8_ARRAY   >> emitArrayBytes e (B.uint8ArrayBytes vs)
  VV.Uint16ArrayVal vs  -> emitTag e T.UINT16_ARRAY  >> emitArrayBytes e (B.uint16ArrayBytes vs)
  VV.Uint32ArrayVal vs  -> emitTag e T.UINT32_ARRAY  >> emitArrayBytes e (B.uint32ArrayBytes vs)
  VV.Uint64ArrayVal vs  -> emitTag e T.UINT64_ARRAY  >> emitArrayBytes e (B.uint64ArrayBytes vs)
  VV.Float32ArrayVal vs -> emitTag e T.FLOAT32_ARRAY >> emitArrayBytes e (B.float32ArrayBytes vs)
  VV.Float64ArrayVal vs -> emitTag e T.FLOAT64_ARRAY >> emitArrayBytes e (B.float64ArrayBytes vs)

emitTag :: IO.Encoder -> T.TypeId -> IO ()
emitTag !e (T.TypeId w) = IO.emitVaruint32 e (fromIntegral w)
{-# INLINE emitTag #-}

-- | The wire format for primitive arrays is
-- @varuint32 byteLen + raw bytes@. Since the input
-- 'ByteString' already aliases the array's bytes (zero-copy
-- via 'Fory.Bulk.vecSToBytes'), we can write its length and
-- then 'IO.emitBytes' it directly.
emitArrayBytes :: IO.Encoder -> ByteString -> IO ()
emitArrayBytes !e !bs = do
  IO.emitVaruint32 e (fromIntegral (BS.length bs))
  IO.emitBytes e bs
{-# INLINE emitArrayBytes #-}

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

emitForyString :: IO.Encoder -> Text -> IO ()
emitForyString !e !t = do
  -- Encode to UTF-8 once (O(1) on text-2.x for ASCII strings —
  -- the underlying ByteArray is reused). Then scan the bytes
  -- for any high bit; if none, the bytes are simultaneously
  -- valid LATIN-1 and we emit them with encoding tag 0 to
  -- match pyfory's output. Otherwise we fall back: if every
  -- /character/ has code point < 256 we manually re-encode as
  -- 1-byte-per-char LATIN-1; otherwise emit UTF-8 (encoding
  -- tag 2).
  let !utf8 = TE.encodeUtf8 t
      !len  = BS.length utf8
  if BS.all (< 0x80) utf8
    then do
      let !hdr = (fromIntegral len `shiftL` 2) :: Word64
      IO.emitVaruint36Small e hdr
      IO.emitBytes e utf8
    else if isLatin1 t
      then do
        let !raw = B.latin1Bytes t
            !rawLen = BS.length raw
            !hdr = (fromIntegral rawLen `shiftL` 2) :: Word64
        IO.emitVaruint36Small e hdr
        IO.emitBytes e raw
      else do
        let !hdr = (fromIntegral len `shiftL` 2) .|. 2 :: Word64
        IO.emitVaruint36Small e hdr
        IO.emitBytes e utf8

isLatin1 :: Text -> Bool
isLatin1 = T.all (\c -> ord c < 256)
{-# INLINE isLatin1 #-}

-- ---------------------------------------------------------------------------
-- Untagged payload (used inside SAME_TYPE collections)
-- ---------------------------------------------------------------------------

-- | Emit just the payload, /no/ leading type tag and /no/ slot
-- flag. Matches the per-element layout of a homogeneous
-- collection where the element type was declared once at the
-- collect-flag header.
emitUntaggedPayload :: IO.Encoder -> VV.Value -> IO ()
emitUntaggedPayload !e val = case val of
  VV.NoneVal       -> pure ()
  VV.BoolVal b     -> IO.emitByte e (if b then 1 else 0)
  VV.Int8Val n     -> IO.emitByte e (fromIntegral n)
  VV.Int16Val n    -> IO.emitInt16LE e n
  VV.Int32Val n    -> IO.emitInt32LE e n
  VV.VarInt32Val n -> IO.emitVarint32 e n
  VV.Int64Val n    -> IO.emitInt64LE e n
  VV.VarInt64Val n -> IO.emitVarint64 e n
  VV.Uint8Val n    -> IO.emitByte e n
  VV.Uint16Val n   -> IO.emitWord16LE e n
  VV.Uint32Val n   -> IO.emitWord32LE e n
  VV.VarUint32Val n -> IO.emitVaruint32 e n
  VV.Uint64Val n   -> IO.emitWord64LE e n
  VV.VarUint64Val n -> IO.emitVaruint64 e n
  VV.Float32Val f  -> IO.emitFloat32LE e f
  VV.Float64Val d  -> IO.emitFloat64LE e d
  VV.StringVal s   -> emitForyString e s
  VV.BinaryVal bs  -> do
    IO.emitVaruint32 e (fromIntegral (BS.length bs))
    IO.emitBytes e bs
  VV.ListVal vs    -> emitCollection e vs
  VV.SetVal vs     -> emitCollection e vs
  VV.MapVal kvs    -> emitMapChunks e kvs
  VV.StructVal _ns _nm fields -> emitStructFields e fields
  VV.RegisteredStructVal ns nm fields -> emitRegisteredStruct e ns nm fields
  VV.CompatibleStructVal ns nm fields -> do
    emitTypeDef e ns nm fields
    V.forM_ fields $ \(_, fv) -> emitValueSlot e fv
  VV.RefVal{} -> emitValueSlot e val
  VV.BoolArrayVal vs    -> emitArrayBytes e (B.boolArrayBytes vs)
  VV.Int8ArrayVal vs    -> emitArrayBytes e (B.int8ArrayBytes vs)
  VV.Int16ArrayVal vs   -> emitArrayBytes e (B.int16ArrayBytes vs)
  VV.Int32ArrayVal vs   -> emitArrayBytes e (B.int32ArrayBytes vs)
  VV.Int64ArrayVal vs   -> emitArrayBytes e (B.int64ArrayBytes vs)
  VV.Uint8ArrayVal vs   -> emitArrayBytes e (B.uint8ArrayBytes vs)
  VV.Uint16ArrayVal vs  -> emitArrayBytes e (B.uint16ArrayBytes vs)
  VV.Uint32ArrayVal vs  -> emitArrayBytes e (B.uint32ArrayBytes vs)
  VV.Uint64ArrayVal vs  -> emitArrayBytes e (B.uint64ArrayBytes vs)
  VV.Float32ArrayVal vs -> emitArrayBytes e (B.float32ArrayBytes vs)
  VV.Float64ArrayVal vs -> emitArrayBytes e (B.float64ArrayBytes vs)

-- ---------------------------------------------------------------------------
-- Collections (LIST / SET)
-- ---------------------------------------------------------------------------

collFlagTrackingRef, collFlagHasNull, collFlagIsSameType :: Word8
collFlagTrackingRef     = 0b0001
collFlagHasNull         = 0b0010
collFlagIsSameType      = 0b1000

emitCollection :: IO.Encoder -> Vector VV.Value -> IO ()
emitCollection !e vs = do
  let !len = V.length vs
  IO.emitVaruint32 e (fromIntegral len)
  if len == 0
    then pure ()
    else do
      let !refOn = Opt.eoRefTracking (IO.encOptions e)
          (sameType, hasNull, mElemTag) = analyseCollection vs
          elemTrackingRef = case mElemTag of
            Just t  -> refOn && sameTypeNeedsRef t
            Nothing -> False
          !flag =
                  (if sameType        then collFlagIsSameType else 0)
              .|. (if hasNull         then collFlagHasNull    else 0)
              .|. (if elemTrackingRef then collFlagTrackingRef else 0)
      IO.emitByte e flag
      case (sameType, mElemTag) of
        (True, Just tag) -> do
          emitElementTypeInfo e tag vs
          case (elemTrackingRef, hasNull) of
            (True, _) ->
              V.forM_ vs $ \x ->
                if isNoneV x
                  then IO.emitByte e slotNull
                  else emitRefStructural e x True
            (False, True) ->
              V.forM_ vs $ \x -> case x of
                VV.NoneVal -> IO.emitByte e slotNull
                _ -> do
                  IO.emitByte e slotNotNullValue
                  emitUntaggedPayload e x
            (False, False) ->
              V.forM_ vs $ \x -> emitUntaggedPayload e x
        (True, Nothing) -> do
          -- Every element is None.
          emitTag e T.NONE
          V.forM_ vs $ \_ -> IO.emitByte e slotNull
        (False, _) ->
          if hasNull
            then V.forM_ vs $ \x -> case x of
                   VV.NoneVal  -> IO.emitByte e slotNull
                   VV.RefVal{} -> emitValueSlot e x
                   _ ->
                     if refOn && needsToWriteRef x
                       then emitRefStructural e x False
                       else do
                         IO.emitByte e slotNotNullValue
                         emitTypedPayload e x
            else V.forM_ vs $ \x ->
                   if refOn && needsToWriteRef x
                     then emitRefStructural e x False
                     else emitTypedPayload e x

isNoneV :: VV.Value -> Bool
isNoneV VV.NoneVal = True
isNoneV _          = False
{-# INLINE isNoneV #-}

sameTypeNeedsRef :: T.TypeId -> Bool
sameTypeNeedsRef !t = case t of
  T.LIST                    -> True
  T.SET                     -> True
  T.MAP                     -> True
  T.NAMED_STRUCT            -> True
  T.NAMED_COMPATIBLE_STRUCT -> True
  T.BOOL_ARRAY              -> True
  T.INT8_ARRAY              -> True
  T.INT16_ARRAY             -> True
  T.INT32_ARRAY             -> True
  T.INT64_ARRAY             -> True
  T.UINT8_ARRAY             -> True
  T.UINT16_ARRAY            -> True
  T.UINT32_ARRAY            -> True
  T.UINT64_ARRAY            -> True
  T.FLOAT32_ARRAY           -> True
  T.FLOAT64_ARRAY           -> True
  _                         -> False

analyseCollection :: Vector VV.Value -> (Bool, Bool, Maybe T.TypeId)
analyseCollection !vs =
  let (sameType, hasNull, mTag) =
        V.foldl' step (True, False, Nothing) vs
  in (sameType, hasNull, mTag)
  where
    step (!st, !hn, !mt) x = case x of
      VV.NoneVal  -> (st, True, mt)
      VV.RefVal{} -> (False, True, mt)
      _ ->
        let !tg = VV.typeIdOf x
        in case mt of
             Nothing -> (st, hn, Just tg)
             Just t
               | t == tg   -> (st, hn, mt)
               | otherwise -> (False, hn, mt)

emitElementTypeInfo
  :: IO.Encoder -> T.TypeId -> Vector VV.Value -> IO ()
emitElementTypeInfo !e !tag !vs = do
  emitTag e tag
  case tag of
    T.NAMED_STRUCT -> case V.find (not . isNoneV) vs of
      Just (VV.StructVal ns nm _)           -> do
        emitMetaStringWith e MSE.namespaceSpecialChars ns
        emitMetaStringWith e MSE.typenameSpecialChars  nm
      Just (VV.RegisteredStructVal ns nm _) -> do
        emitMetaStringWith e MSE.namespaceSpecialChars ns
        emitMetaStringWith e MSE.typenameSpecialChars  nm
      _ -> pure ()
    _ -> pure ()

-- ---------------------------------------------------------------------------
-- Maps (chunked key-type / value-type format)
-- ---------------------------------------------------------------------------

mapTrackingKeyRef, mapKeyHasNull, mapTrackingValueRef, mapValueHasNull :: Word8
mapTrackingKeyRef   = 0b0000_0001
mapKeyHasNull       = 0b0000_0010
mapTrackingValueRef = 0b0000_1000
mapValueHasNull     = 0b0001_0000

emitMapChunks :: IO.Encoder -> Vector (VV.Value, VV.Value) -> IO ()
emitMapChunks !e !kvs = do
  let !len = V.length kvs
  IO.emitVaruint32 e (fromIntegral len)
  if len == 0
    then pure ()
    else V.forM_ kvs $ \(k, v) -> emitOneEntryChunk e k v

emitOneEntryChunk :: IO.Encoder -> VV.Value -> VV.Value -> IO ()
emitOneEntryChunk !e !k !v = do
  let keyNull = isNoneV k
      valNull = isNoneV v
  case (keyNull, valNull) of
    (True,  True)  -> IO.emitByte e (mapKeyHasNull .|. mapValueHasNull)
    (False, True)  -> do
      IO.emitByte e (mapValueHasNull .|. mapTrackingKeyRef)
      IO.emitByte e slotNotNullValue
      emitTag e (VV.typeIdOf k)
      emitUntaggedPayload e k
    (True,  False) -> do
      IO.emitByte e (mapKeyHasNull .|. mapTrackingValueRef)
      IO.emitByte e slotNotNullValue
      emitTag e (VV.typeIdOf v)
      emitUntaggedPayload e v
    (False, False) -> do
      IO.emitByte e 0
      IO.emitByte e 1
      emitTag e (VV.typeIdOf k)
      emitTag e (VV.typeIdOf v)
      emitUntaggedPayload e k
      emitUntaggedPayload e v

-- ---------------------------------------------------------------------------
-- Structs (the in-package self-describing 'StructVal' format)
-- ---------------------------------------------------------------------------

emitStructFields :: IO.Encoder -> VV.StructFields -> IO ()
emitStructFields !e fields = do
  IO.emitVaruint32 e (fromIntegral (V.length fields))
  V.forM_ fields $ \(name, value) -> do
    emitMetaStringWith e MSE.namespaceSpecialChars name
    emitValueSlot e value

-- ---------------------------------------------------------------------------
-- Meta-string emission with deduplication
-- ---------------------------------------------------------------------------

emitMetaStringWith :: IO.Encoder -> MSE.SpecialChars -> Text -> IO ()
emitMetaStringWith !e !sc !t = do
  m <- IO.metaStringLookup e t
  case m of
    Just rid ->
      -- Reference: same single-varuint64 header that
      -- 'MS.refMetaString' produces, written via the IO encoder
      -- so we don't allocate a builder.
      IO.emitVaruint64 e
        (((fromIntegral rid + 1) `shiftL` 1) .|. 1 :: Word64)
    Nothing -> do
      _ <- IO.metaStringRegister e t
      emitFreshMetaString e sc t

emitFreshMetaString :: IO.Encoder -> MSE.SpecialChars -> Text -> IO ()
emitFreshMetaString !e !sc !t = do
  -- Compute the metastring layer (encoded data + chosen
  -- encoding), then write the appropriate header (with a
  -- 64-bit hashcode for >16-byte payloads, or a
  -- single-byte encoding tag otherwise) plus the bytes.
  let (enc, bs) = MSE.encodeMetaString sc t
      !len = BS.length bs
      !hdr = (fromIntegral len `shiftL` 1) :: Word64
  IO.emitVaruint64 e hdr
  if len == 0
    then pure ()
    else if len <= 16
      then do
        IO.emitByte e (MSE.encodingId enc)
        IO.emitBytes e bs
      else do
        let !hash = MSH.metaStringHashcode bs
                      (fromIntegral (MSE.encodingId enc))
        IO.emitInt64LE e (fromIntegral hash)
        IO.emitBytes e bs

-- ---------------------------------------------------------------------------
-- Registered structs (NAMED_STRUCT, pyfory-compatible)
-- ---------------------------------------------------------------------------

emitRegisteredStruct
  :: IO.Encoder -> Text -> Text -> VV.StructFields -> IO ()
emitRegisteredStruct !e ns nm fields =
  case lookupSchema e ns nm of
    Nothing -> error $
      "Fory.Encode: no schema registered for "
        ++ T.unpack ns ++ "." ++ T.unpack nm
        ++ "; build EncodeOptions with eoStructRegistry containing this schema"
    Just sch -> do
      IO.emitInt32LE e (ST.ssHash sch)
      let !canonical = ST.fieldOrder sch
      V.forM_ canonical $ \spec ->
        case VV.registeredStructFieldByName (ST.fsName spec) fields of
          Nothing -> error $ "Fory.Encode: field "
                              ++ T.unpack (ST.fsName spec)
                              ++ " missing from RegisteredStructVal"
          Just v  -> emitRegisteredField e spec v

lookupSchema :: IO.Encoder -> Text -> Text -> Maybe ST.StructSchema
lookupSchema !e !ns !nm =
  let !reg = Opt.eoStructRegistry (IO.encOptions e)
  in lookupRegistry ns nm reg
  where
    lookupRegistry x y r = lookupHM (x, y) r

lookupHM :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookupHM = HM.lookup

emitRegisteredField :: IO.Encoder -> ST.FieldSpec -> VV.Value -> IO ()
emitRegisteredField !e spec v
  | ST.isBasicTypeId (ST.fsTypeId spec) =
      if ST.fsNullable spec
        then case v of
          VV.NoneVal -> IO.emitByte e slotNull
          _ -> do
            IO.emitByte e slotNotNullValue
            emitUntaggedPayload e v
        else emitUntaggedPayload e v
  | otherwise = case v of
      VV.NoneVal -> IO.emitByte e slotNull
      _ -> do
        IO.emitByte e slotNotNullValue
        emitUntaggedPayload e v

-- ---------------------------------------------------------------------------
-- TypeDef sidecar (NAMED_COMPATIBLE_STRUCT)
-- ---------------------------------------------------------------------------

emitTypeDef
  :: IO.Encoder -> Text -> Text -> VV.StructFields -> IO ()
emitTypeDef !e ns nm fields = do
  let !key = (ns, nm, V.toList (V.map fst fields))
  m <- IO.typeDefLookup e key
  case m of
    Just idx ->
      IO.emitVaruint64 e ((fromIntegral idx `shiftL` 1) .|. 1)
    Nothing -> do
      idx <- IO.typeDefRegister e key
      IO.emitVaruint64 e (fromIntegral idx `shiftL` 1)
      emitTypeDefBytes e ns nm fields

emitTypeDefBytes
  :: IO.Encoder -> Text -> Text -> VV.StructFields -> IO ()
emitTypeDefBytes !e ns nm fields = do
  -- The TypeDef body is short enough that we can compute it
  -- inline via a sub-encoder with the same option set, then
  -- splice the resulting bytes after the 8-byte global header.
  body <- IO.runEncoder (IO.encOptions e) $ \subE ->
    emitTypeDefBody subE ns nm fields
  let !bodyLen = BS.length body
  emitGlobalHeader e bodyLen
  IO.emitBytes e body

emitGlobalHeader :: IO.Encoder -> Int -> IO ()
emitGlobalHeader !e !bodyLen
  | bodyLen < 0xFF = IO.emitWord64LE e (fromIntegral bodyLen)
  | otherwise = do
      IO.emitWord64LE e 0xFF
      IO.emitVaruint32 e (fromIntegral (bodyLen - 0xFF))

emitTypeDefBody
  :: IO.Encoder -> Text -> Text -> VV.StructFields -> IO ()
emitTypeDefBody !e ns nm fields = do
  let !nfRaw = V.length fields
      !registerByName = 1 `shiftL` 5
  if nfRaw <= 30
    then IO.emitByte e (fromIntegral (nfRaw .|. registerByName))
    else do
      IO.emitByte e (fromIntegral (31 .|. registerByName))
      IO.emitVaruint32 e (fromIntegral (nfRaw - 31))
  emitMetaStringWith e MSE.namespaceSpecialChars ns
  emitMetaStringWith e MSE.typenameSpecialChars  nm
  V.forM_ fields $ \(fname, fvalue) -> do
    emitMetaStringWith e MSE.namespaceSpecialChars fname
    let T.TypeId tw = VV.typeIdOf fvalue
    IO.emitVaruint32 e (fromIntegral tw)

