{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Fory xlang value encoder.
--
-- The wire format produced here is byte-for-byte compatible with
-- the Apache Fory Python implementation (@pyfory@ 0.17) for the
-- following value shapes:
--
-- * top-level header (xlang flag, optional null bit) + slot flag
--   (NOT_NULL_VALUE / NULL / REF_VALUE / REF) + type tag + payload
-- * null
-- * BOOL, INT8, INT16, INT32, VARINT32, INT64, VARINT64, UINT8,
--   UINT16, UINT32, VAR_UINT32, UINT64, VAR_UINT64,
--   FLOAT32, FLOAT64
-- * STRING with the LATIN-1 / UTF-8 encoding selection that
--   pyfory uses (no 64-bit content hash on STRING)
-- * BINARY (varuint32 length + raw bytes)
-- * LIST and SET with the chunked @collect_flag@ format used by
--   pyfory's CollectionSerializer (homogeneous / heterogeneous,
--   has-nulls / no-nulls, decl-element-type, tracking-ref)
-- * MAP with the chunked key-type / value-type @chunk_header@
--   format used by pyfory's MapSerializer
-- * top-level RefVal as REF_VALUE_FLAG / REF_FLAG
--
-- Intentionally /not/ yet wire-compatible (round-trip in this
-- package only):
--
-- * @NAMED_STRUCT@ / @NAMED_COMPATIBLE_STRUCT@ — pyfory uses a
--   bit-packed @TypeDef@ field-info format and a meta-string
--   layer with content-hash dedup that we approximate with a
--   simpler layout. See "Fury.Encode.encodeValueSlot" comments.
-- * One-dimensional primitive arrays — pyfory routes these
--   through NumPy-typed paths; we emit our own dense encoding.
-- * Reference tracking inside collections — we only support
--   top-level RefVal; pyfory tracks references through every
--   nested object slot when @ref=True@.
module Fury.Encode
  ( -- * Top-level encoders
    encode
  , encodeBuilder
  , encodeValueSlot

    -- * Internals (re-exported for tests / advanced callers)
  , EncodeM
  , runEncodeM
  , emit
  , emitMetaString
  , emitMetaStringWith
  ) where

import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word16, Word32, Word64)

import qualified Fury.Encoding as E
import qualified Fury.MetaString as MS
import qualified Fury.MetaString.Encoder as MSE
import qualified Fury.TypeId as T
import qualified Fury.Value as VV

-- ---------------------------------------------------------------------------
-- Encoder state monad
-- ---------------------------------------------------------------------------

type TypeDefKey = (Text, Text, [Text])

data EncodeState = EncodeState
  { esStringPool    :: !(HashMap Text Int)
  , esNextStringId  :: {-# UNPACK #-} !Int
  , esRefMap        :: !(IntMap Int)
  , esNextRefId     :: {-# UNPACK #-} !Int
  , esTypeDefPool   :: !(HashMap TypeDefKey Int)
  , esNextTypeDefId :: {-# UNPACK #-} !Int
  }

emptyState :: EncodeState
emptyState =
  EncodeState HM.empty 0 IM.empty 0 HM.empty 0

newtype EncodeM a =
  EncodeM { runEM :: EncodeState -> (a, EncodeState, E.Builder) }

instance Functor EncodeM where
  fmap f (EncodeM g) = EncodeM $ \s ->
    case g s of
      (a, s', b) -> (f a, s', b)
  {-# INLINE fmap #-}

instance Applicative EncodeM where
  pure x = EncodeM $ \s -> (x, s, mempty)
  {-# INLINE pure #-}
  EncodeM f <*> EncodeM x = EncodeM $ \s ->
    case f s of
      (g, s1, b1) -> case x s1 of
        (a, s2, b2) -> (g a, s2, b1 <> b2)
  {-# INLINE (<*>) #-}

instance Monad EncodeM where
  EncodeM m >>= k = EncodeM $ \s ->
    case m s of
      (a, s1, b1) -> case runEM (k a) s1 of
        (b, s2, b2) -> (b, s2, b1 <> b2)
  {-# INLINE (>>=) #-}

runEncodeM :: EncodeM () -> ByteString
runEncodeM (EncodeM m) =
  case m emptyState of
    (_, _, b) -> E.runBuilder b

emit :: E.Builder -> EncodeM ()
emit !b = EncodeM $ \s -> ((), s, b)
{-# INLINE emit #-}

getState :: EncodeM EncodeState
getState = EncodeM $ \s -> (s, s, mempty)
{-# INLINE getState #-}

modifyState :: (EncodeState -> EncodeState) -> EncodeM ()
modifyState f = EncodeM $ \s -> ((), f s, mempty)
{-# INLINE modifyState #-}

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

-- | Encode a 'Value' to a fory xlang byte sequence.
encode :: VV.Value -> ByteString
encode v = runEncodeM (encodeBuilder v)

encodeBuilder :: VV.Value -> EncodeM ()
encodeBuilder VV.NoneVal = do
  -- pyfory matches: header has the xlang flag (bit 1) /and/ the
  -- null flag (bit 0); the slot byte is then NULL_FLAG too.
  emit (E.byte 0x03)
  emit (E.byte slotNull)
encodeBuilder v = do
  emit (E.byte 0x02)
  encodeValueSlot v

-- ---------------------------------------------------------------------------
-- Value slot
-- ---------------------------------------------------------------------------

-- | Encode a single value slot. Always emits one slot flag byte
-- followed by either nothing (for NULL / REF) or a type tag and a
-- payload (for NOT_NULL_VALUE / REF_VALUE).
encodeValueSlot :: VV.Value -> EncodeM ()
encodeValueSlot v = case v of
  VV.NoneVal       -> emit (E.byte slotNull)
  VV.RefVal i inner -> encodeRef i inner
  _                -> do
    emit (E.byte slotNotNullValue)
    encodeTypedPayload v

encodeRef :: Int -> VV.Value -> EncodeM ()
encodeRef userKey inner = do
  s <- getState
  case IM.lookup userKey (esRefMap s) of
    Just wid -> do
      emit (E.byte slotRef)
      emit (E.varuint32 (fromIntegral wid :: Word32))
    Nothing -> do
      let !wid = esNextRefId s
      modifyState $ \s' -> s'
        { esRefMap    = IM.insert userKey wid (esRefMap s')
        , esNextRefId = wid + 1
        }
      emit (E.byte slotRefValue)
      encodeTypedPayload inner

-- | Type tag + payload, no leading slot flag.
encodeTypedPayload :: VV.Value -> EncodeM ()
encodeTypedPayload val = case val of
  VV.NoneVal       -> emitTag T.NONE
  VV.BoolVal b     -> emitTag T.BOOL    >> emit (E.byte (if b then 1 else 0))
  VV.Int8Val n     -> emitTag T.INT8    >> emit (E.byte (fromIntegral n))
  VV.Int16Val n    -> emitTag T.INT16   >> emit (E.int16LE n)
  VV.Int32Val n    -> emitTag T.INT32   >> emit (E.int32LE n)
  VV.VarInt32Val n -> emitTag T.VARINT32 >> emit (E.varint32 n)
  VV.Int64Val n    -> emitTag T.INT64   >> emit (E.int64LE n)
  VV.VarInt64Val n -> emitTag T.VARINT64 >> emit (E.varint64 n)
  VV.Uint8Val n    -> emitTag T.UINT8   >> emit (E.byte n)
  VV.Uint16Val n   -> emitTag T.UINT16  >> emit (E.word16LE n)
  VV.Uint32Val n   -> emitTag T.UINT32  >> emit (E.word32LE n)
  VV.VarUint32Val n -> emitTag T.VAR_UINT32 >> emit (E.varuint32 n)
  VV.Uint64Val n   -> emitTag T.UINT64  >> emit (E.word64LE n)
  VV.VarUint64Val n -> emitTag T.VAR_UINT64 >> emit (E.varuint64 n)
  VV.Float32Val f  -> emitTag T.FLOAT32 >> emit (E.float32LE f)
  VV.Float64Val d  -> emitTag T.FLOAT64 >> emit (E.float64LE d)
  VV.StringVal s   -> emitTag T.STRING  >> emit (encodeForyString s)
  VV.BinaryVal bs  -> emitTag T.BINARY  >> emitBinaryPayload bs
  VV.ListVal vs    -> emitTag T.LIST    >> emitCollection vs
  VV.SetVal vs     -> emitTag T.SET     >> emitCollection vs
  VV.MapVal kvs    -> emitTag T.MAP     >> emitMapChunks kvs
  VV.StructVal ns nm fields -> do
    emitTag T.NAMED_STRUCT
    emitMetaStringWith MSE.namespaceSpecialChars ns
    emitMetaStringWith MSE.typenameSpecialChars  nm
    emitStructFields fields
  VV.CompatibleStructVal ns nm fields -> do
    emitTag T.NAMED_COMPATIBLE_STRUCT
    emitTypeDef ns nm fields
    V.forM_ fields $ \(_, fv) -> encodeValueSlot fv
  VV.RefVal{} -> encodeValueSlot val
  VV.BoolArrayVal vs    -> emitTag T.BOOL_ARRAY    >> emitBoolArray vs
  VV.Int8ArrayVal vs    -> emitTag T.INT8_ARRAY    >> emitInt8Array vs
  VV.Int16ArrayVal vs   -> emitTag T.INT16_ARRAY   >> emitInt16Array vs
  VV.Int32ArrayVal vs   -> emitTag T.INT32_ARRAY   >> emitInt32Array vs
  VV.Int64ArrayVal vs   -> emitTag T.INT64_ARRAY   >> emitInt64Array vs
  VV.Uint8ArrayVal vs   -> emitTag T.UINT8_ARRAY   >> emitUint8Array vs
  VV.Uint16ArrayVal vs  -> emitTag T.UINT16_ARRAY  >> emitUint16Array vs
  VV.Uint32ArrayVal vs  -> emitTag T.UINT32_ARRAY  >> emitUint32Array vs
  VV.Uint64ArrayVal vs  -> emitTag T.UINT64_ARRAY  >> emitUint64Array vs
  VV.Float32ArrayVal vs -> emitTag T.FLOAT32_ARRAY >> emitFloat32Array vs
  VV.Float64ArrayVal vs -> emitTag T.FLOAT64_ARRAY >> emitFloat64Array vs

emitTag :: T.TypeId -> EncodeM ()
emitTag (T.TypeId w) = emit (E.varuint32 (fromIntegral w :: Word32))
{-# INLINE emitTag #-}

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

-- | Encode a string value (after the type tag) using the
-- LATIN-1 / UTF-8 selection that pyfory uses.
encodeForyString :: Text -> E.Builder
encodeForyString !t
  | isLatin1 t =
      let !raw = encodeLatin1 t
          !len = BS.length raw
          !hdr = (fromIntegral len `shiftL` 2) :: Word64
          -- encoding tag = 0 (LATIN1) goes in the bottom 2 bits.
      in E.varuint36Small hdr <> E.bytes raw
  | otherwise =
      let !raw = TE.encodeUtf8 t
          !len = BS.length raw
          !hdr = (fromIntegral len `shiftL` 2) .|. 2 :: Word64
          -- encoding tag = 2 (UTF-8) in the bottom 2 bits.
      in E.varuint36Small hdr <> E.bytes raw

isLatin1 :: Text -> Bool
isLatin1 = T.all (\c -> ord c < 256)

encodeLatin1 :: Text -> ByteString
encodeLatin1 = BS.pack . map (fromIntegral . ord) . T.unpack

-- ---------------------------------------------------------------------------
-- Binary
-- ---------------------------------------------------------------------------

emitBinaryPayload :: ByteString -> EncodeM ()
emitBinaryPayload !bs = do
  emit (E.varuint32 (fromIntegral (BS.length bs) :: Word32))
  emit (E.bytes bs)

-- ---------------------------------------------------------------------------
-- Collections (LIST / SET) — chunked @collect_flag@ format
-- ---------------------------------------------------------------------------
--
-- Mirrors pyfory's CollectionSerializer:
--
--   write_var_uint32(len(value))
--   if len == 0: return
--   collect_flag := 0
--   for item in value:
--     determine homogeneous + has_null
--   write_int8(collect_flag)
--   if SAME_TYPE && !DECL_ELEMENT_TYPE: write element type info
--   case (SAME_TYPE, HAS_NULL):
--     (T, F): each element = serialize(item)
--     (T, T): each element = NULL_FLAG | NOT_NULL_VALUE_FLAG + serialize(item)
--     (F, F): each element = type_info + serialize(item)
--     (F, T): each element = NULL_FLAG | NOT_NULL_VALUE_FLAG + type_info + serialize(item)
--
-- Tracking_ref is not implemented inside collections (top-level
-- only); we never set the bit.

collFlagHasNull, collFlagIsSameType :: Word8
collFlagHasNull         = 0b0010
collFlagIsSameType      = 0b1000

emitCollection :: Vector VV.Value -> EncodeM ()
emitCollection vs = do
  let !len = V.length vs
  emit (E.varuint32 (fromIntegral len :: Word32))
  if len == 0
    then pure ()
    else do
      let (sameType, hasNull, mElemTag) = analyseCollection vs
          !flag =
                  (if sameType then collFlagIsSameType else 0)
              .|. (if hasNull  then collFlagHasNull    else 0)
      emit (E.byte flag)
      case (sameType, mElemTag) of
        (True, Just tag) -> do
          emitTag tag
          if hasNull
            then V.forM_ vs $ \x -> case x of
                   VV.NoneVal -> emit (E.byte slotNull)
                   _ -> do
                     emit (E.byte slotNotNullValue)
                     encodeUntaggedPayload x
            else V.forM_ vs $ \x -> encodeUntaggedPayload x
        (True, Nothing) ->
          -- All elements are None.
          V.forM_ vs $ \_ -> emit (E.byte slotNull)
        (False, _) ->
          if hasNull
            then V.forM_ vs $ \x -> case x of
                   VV.NoneVal -> emit (E.byte slotNull)
                   VV.RefVal{} -> encodeValueSlot x
                   _ -> do
                     emit (E.byte slotNotNullValue)
                     encodeTypedPayload x
            else V.forM_ vs $ \x -> encodeTypedPayload x

-- | Inspect a collection: do all non-null elements share a type
-- tag, and is there at least one null? 'RefVal' elements force
-- the heterogeneous path because the per-occurrence ref flag they
-- emit replaces the slot's type tag, which the homogeneous
-- single-type optimization can't accommodate.
analyseCollection :: Vector VV.Value -> (Bool, Bool, Maybe T.TypeId)
analyseCollection vs =
  let (sameType, hasNull, mTag) =
        V.foldl' step (True, False, Nothing) vs
  in (sameType, hasNull, mTag)
  where
    step (!st, !hn, !mt) x = case x of
      VV.NoneVal  -> (st, True, mt)
      -- RefVal forces heterogeneous /and/ has-null so the
      -- per-element slot flag carries the ref flag bytes
      -- (0xFD / 0xFE / 0x00) — those are the same byte
      -- positions the spec's reference tracking uses, and
      -- they don't collide with any valid type tag.
      VV.RefVal{} -> (False, True, mt)
      _ ->
        let !tg = VV.typeIdOf x
        in case mt of
             Nothing -> (st, hn, Just tg)
             Just t
               | t == tg   -> (st, hn, mt)
               | otherwise -> (False, hn, mt)

-- | Encode just the payload (no type tag, no slot flag).
encodeUntaggedPayload :: VV.Value -> EncodeM ()
encodeUntaggedPayload val = case val of
  VV.NoneVal       -> pure ()  -- Should be intercepted by caller.
  VV.BoolVal b     -> emit (E.byte (if b then 1 else 0))
  VV.Int8Val n     -> emit (E.byte (fromIntegral n))
  VV.Int16Val n    -> emit (E.int16LE n)
  VV.Int32Val n    -> emit (E.int32LE n)
  VV.VarInt32Val n -> emit (E.varint32 n)
  VV.Int64Val n    -> emit (E.int64LE n)
  VV.VarInt64Val n -> emit (E.varint64 n)
  VV.Uint8Val n    -> emit (E.byte n)
  VV.Uint16Val n   -> emit (E.word16LE n)
  VV.Uint32Val n   -> emit (E.word32LE n)
  VV.VarUint32Val n -> emit (E.varuint32 n)
  VV.Uint64Val n   -> emit (E.word64LE n)
  VV.VarUint64Val n -> emit (E.varuint64 n)
  VV.Float32Val f  -> emit (E.float32LE f)
  VV.Float64Val d  -> emit (E.float64LE d)
  VV.StringVal s   -> emit (encodeForyString s)
  VV.BinaryVal bs  -> emitBinaryPayload bs
  VV.ListVal vs    -> emitCollection vs
  VV.SetVal vs     -> emitCollection vs
  VV.MapVal kvs    -> emitMapChunks kvs
  VV.StructVal ns nm fields -> do
    emitMetaStringWith MSE.namespaceSpecialChars ns
    emitMetaStringWith MSE.typenameSpecialChars  nm
    emitStructFields fields
  VV.CompatibleStructVal ns nm fields -> do
    emitTypeDef ns nm fields
    V.forM_ fields $ \(_, fv) -> encodeValueSlot fv
  VV.RefVal{} -> encodeValueSlot val
  VV.BoolArrayVal vs    -> emitBoolArray vs
  VV.Int8ArrayVal vs    -> emitInt8Array vs
  VV.Int16ArrayVal vs   -> emitInt16Array vs
  VV.Int32ArrayVal vs   -> emitInt32Array vs
  VV.Int64ArrayVal vs   -> emitInt64Array vs
  VV.Uint8ArrayVal vs   -> emitUint8Array vs
  VV.Uint16ArrayVal vs  -> emitUint16Array vs
  VV.Uint32ArrayVal vs  -> emitUint32Array vs
  VV.Uint64ArrayVal vs  -> emitUint64Array vs
  VV.Float32ArrayVal vs -> emitFloat32Array vs
  VV.Float64ArrayVal vs -> emitFloat64Array vs

-- ---------------------------------------------------------------------------
-- Maps — chunked format
-- ---------------------------------------------------------------------------
--
-- pyfory's MapSerializer writes:
--
--   write_var_uint32(total_size)
--   while remaining entries:
--     pick a chunk where the key type and value type are uniform
--     (max 255 entries per chunk)
--     write_int8(chunk_header) -- bits combine KEY/VALUE
--                                 HAS_NULL / DECL_TYPE / TRACKING_REF
--     write_var_uint32(chunk_size)
--     write_type_info(key_type)
--     write_type_info(value_type)
--     for (k, v) in chunk: serialize(k), serialize(v)
--
-- The simplifications we make: tracking_ref is always zero;
-- DECL_TYPE flags are zero (we never use a pre-declared element
-- type); HAS_NULL is set on whichever side has any None inside
-- the chunk. We further simplify by emitting one chunk per
-- entry, which is wasteful but always valid (pyfory's reader
-- happily consumes single-entry chunks). This avoids the
-- type-uniformity grouping pass.

mapKeyHasNull, mapValueHasNull :: Word8
mapKeyHasNull   = 0b0000_0010
mapValueHasNull = 0b0001_0000

emitMapChunks :: Vector (VV.Value, VV.Value) -> EncodeM ()
emitMapChunks kvs = do
  let !len = V.length kvs
  emit (E.varuint32 (fromIntegral len :: Word32))
  if len == 0
    then pure ()
    else V.forM_ kvs $ \(k, v) -> emitOneEntryChunk k v

emitOneEntryChunk :: VV.Value -> VV.Value -> EncodeM ()
emitOneEntryChunk k v = do
  let keyNull = isNoneVal k
      valNull = isNoneVal v
      !flag = (if keyNull then mapKeyHasNull else 0)
          .|. (if valNull then mapValueHasNull else 0)
  emit (E.byte flag)
  emit (E.byte 1)  -- chunk size as uint8, matching pyfory's MapSerializer.
  -- pyfory's wire shape is | chunk_header | chunk_size | key_type | value_type | (key, value) * chunk_size |.
  case (keyNull, valNull) of
    (True,  True)  -> pure ()
    (False, True)  -> do
      emitTag (VV.typeIdOf k)
      encodeUntaggedPayload k
    (True,  False) -> do
      emitTag (VV.typeIdOf v)
      encodeUntaggedPayload v
    (False, False) -> do
      emitTag (VV.typeIdOf k)
      emitTag (VV.typeIdOf v)
      encodeUntaggedPayload k
      encodeUntaggedPayload v
  where
    isNoneVal VV.NoneVal = True
    isNoneVal _          = False

-- ---------------------------------------------------------------------------
-- Struct (NAMED_STRUCT) — non-interop-compliant; round-trips here
-- ---------------------------------------------------------------------------

emitStructFields :: VV.StructFields -> EncodeM ()
emitStructFields fields = do
  emit (E.varuint32 (fromIntegral (V.length fields) :: Word32))
  V.forM_ fields $ \(name, value) -> do
    -- Field names use the namespace special chars (. _) by
    -- convention; pyfory threads the same MetaStringEncoder
    -- context through the TypeDef body for field names.
    emitMetaStringWith MSE.namespaceSpecialChars name
    encodeValueSlot value

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

-- | Emit a meta-string in the namespace context (special chars
-- @('.', '_')@). For typename context, use 'emitMetaStringWith'
-- with 'MSE.typenameSpecialChars'.
emitMetaString :: Text -> EncodeM ()
emitMetaString = emitMetaStringWith MSE.namespaceSpecialChars

-- | Emit a meta-string under an explicit 'SpecialChars' context.
emitMetaStringWith :: MSE.SpecialChars -> Text -> EncodeM ()
emitMetaStringWith sc !t = do
  s <- getState
  case HM.lookup t (esStringPool s) of
    Just rid -> emit (MS.refMetaString rid)
    Nothing -> do
      let !rid = esNextStringId s
      modifyState $ \s' -> s'
        { esStringPool   = HM.insert t rid (esStringPool s')
        , esNextStringId = rid + 1
        }
      emit (MS.freshMetaString sc t)

-- ---------------------------------------------------------------------------
-- TypeDef sidecar (NAMED_COMPATIBLE_STRUCT)
-- ---------------------------------------------------------------------------

emitTypeDef :: Text -> Text -> VV.StructFields -> EncodeM ()
emitTypeDef ns nm fields = do
  let !key = (ns, nm, V.toList (V.map fst fields))
  s <- getState
  case HM.lookup key (esTypeDefPool s) of
    Just idx ->
      emit (E.varuint64 ((fromIntegral idx `shiftL` 1) .|. 1 :: Word64))
    Nothing -> do
      let !idx = esNextTypeDefId s
      modifyState $ \s' -> s'
        { esTypeDefPool   = HM.insert key idx (esTypeDefPool s')
        , esNextTypeDefId = idx + 1
        }
      emit (E.varuint64 (fromIntegral idx `shiftL` 1 :: Word64))
      emitTypeDefBytes ns nm fields

emitTypeDefBytes :: Text -> Text -> VV.StructFields -> EncodeM ()
emitTypeDefBytes ns nm fields = do
  s0 <- getState
  let (s1, bodyBs) = runSubEncoder (typeDefBody ns nm fields) s0
      !bodyLen     = BS.length bodyBs
  modifyState (const s1)
  emitGlobalHeader bodyLen
  emit (E.bytes bodyBs)

runSubEncoder :: EncodeM () -> EncodeState -> (EncodeState, ByteString)
runSubEncoder (EncodeM m) s =
  case m s of
    (_, s', b) -> (s', E.runBuilder b)

emitGlobalHeader :: Int -> EncodeM ()
emitGlobalHeader !bodyLen
  | bodyLen < 0xFF = emit (E.word64LE (fromIntegral bodyLen :: Word64))
  | otherwise = do
      emit (E.word64LE 0xFF)
      emit (E.varuint32 (fromIntegral (bodyLen - 0xFF) :: Word32))

typeDefBody :: Text -> Text -> VV.StructFields -> EncodeM ()
typeDefBody ns nm fields = do
  let !nfRaw = V.length fields
      registerByName = 1 `shiftL` 5
  if nfRaw <= 30
    then emit (E.byte (fromIntegral (nfRaw .|. registerByName)))
    else do
      emit (E.byte (fromIntegral (31 .|. registerByName)))
      emit (E.varuint32 (fromIntegral (nfRaw - 31) :: Word32))
  emitMetaStringWith MSE.namespaceSpecialChars ns
  emitMetaStringWith MSE.typenameSpecialChars  nm
  V.forM_ fields $ \(fname, fvalue) -> do
    emitMetaStringWith MSE.namespaceSpecialChars fname
    let T.TypeId tw = VV.typeIdOf fvalue
    emit (E.varuint32 (fromIntegral tw :: Word32))

-- ---------------------------------------------------------------------------
-- Primitive 1-D arrays
-- ---------------------------------------------------------------------------
--
-- pyfory's NumPy-typed serializer emits | varuint32 byte_length |
-- raw little-endian element bytes |. The element count is implicit:
-- byte_length \/ sizeof(element). Bool is one byte per element.

emitByteLen :: Int -> Int -> EncodeM ()
emitByteLen elemBytes count =
  emit (E.varuint32 (fromIntegral (elemBytes * count) :: Word32))
{-# INLINE emitByteLen #-}

emitBoolArray :: Vector Bool -> EncodeM ()
emitBoolArray vs = do
  emitByteLen 1 (V.length vs)
  V.forM_ vs $ \b -> emit (E.byte (if b then 1 else 0))

emitInt8Array :: Vector Int8 -> EncodeM ()
emitInt8Array vs = do
  emitByteLen 1 (V.length vs)
  V.forM_ vs $ \x -> emit (E.byte (fromIntegral x))

emitInt16Array :: Vector Int16 -> EncodeM ()
emitInt16Array vs = do
  emitByteLen 2 (V.length vs)
  V.forM_ vs $ \x -> emit (E.int16LE x)

emitInt32Array :: Vector Int32 -> EncodeM ()
emitInt32Array vs = do
  emitByteLen 4 (V.length vs)
  V.forM_ vs $ \x -> emit (E.int32LE x)

emitInt64Array :: Vector Int64 -> EncodeM ()
emitInt64Array vs = do
  emitByteLen 8 (V.length vs)
  V.forM_ vs $ \x -> emit (E.int64LE x)

emitUint8Array :: Vector Word8 -> EncodeM ()
emitUint8Array vs = do
  emitByteLen 1 (V.length vs)
  V.forM_ vs $ \x -> emit (E.byte x)

emitUint16Array :: Vector Word16 -> EncodeM ()
emitUint16Array vs = do
  emitByteLen 2 (V.length vs)
  V.forM_ vs $ \x -> emit (E.word16LE x)

emitUint32Array :: Vector Word32 -> EncodeM ()
emitUint32Array vs = do
  emitByteLen 4 (V.length vs)
  V.forM_ vs $ \x -> emit (E.word32LE x)

emitUint64Array :: Vector Word64 -> EncodeM ()
emitUint64Array vs = do
  emitByteLen 8 (V.length vs)
  V.forM_ vs $ \x -> emit (E.word64LE x)

emitFloat32Array :: Vector Float -> EncodeM ()
emitFloat32Array vs = do
  emitByteLen 4 (V.length vs)
  V.forM_ vs $ \x -> emit (E.float32LE x)

emitFloat64Array :: Vector Double -> EncodeM ()
emitFloat64Array vs = do
  emitByteLen 8 (V.length vs)
  V.forM_ vs $ \x -> emit (E.float64LE x)

