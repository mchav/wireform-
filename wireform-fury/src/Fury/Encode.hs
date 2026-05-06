{-# LANGUAGE BangPatterns #-}
-- | Apache Fory xlang value encoder.
--
-- 'encode' lays out a 'Value' tree as
--
-- @
-- | fory header (1 byte) | reference flag | type id | payload |
-- @
--
-- with the @xlang@ flag bit set in the header. Each value slot
-- emits one of:
--
-- * @NULL_FLAG@ (@0xFD@) for 'NoneVal' — no payload.
-- * @REF_FLAG@ (@0xFE@) + @varuint32 id@ for the second and later
--   occurrence of a 'RefVal'.
-- * @REF_VALUE_FLAG@ (@0x00@) + payload for the first occurrence
--   of a 'RefVal'.
-- * a bare type tag + payload for everything else (the spec\'s
--   @NOT_NULL_VALUE_FLAG@ is folded into the type tag byte; valid
--   internal type ids do not collide with the three flag bytes).
--
-- This means primitive values like @BoolVal True@ remain a single
-- byte plus payload — the canonical reference flag is only paid
-- when the spec actually needs it.
--
-- == Spec features implemented
--
-- * Reference tracking (the 'RefVal' constructor).
-- * Meta-string deduplication: namespaces, type names, and
--   compatible-struct field names are written via a
--   first-occurrence-then-back-reference scheme keyed by exact
--   string content.
-- * @NAMED_COMPATIBLE_STRUCT@ with a shared 'TypeDef' sidecar
--   (the 'CompatibleStructVal' constructor).
-- * One-dimensional primitive arrays (BOOL_ARRAY \… FLOAT64_ARRAY).
--
-- == Intentional simplifications
--
-- * The 64-bit content hash that the spec inserts after the
--   meta-string header for strings longer than 16 bytes is not
--   emitted; deduplication is purely by exact byte equality.
-- * The encoding tag in a meta-string header is always UTF-8 (0);
--   the LATIN1 / UTF-16 alternatives in the spec are not produced.
-- * 'TypeDef' bodies follow a simplified layout: namespace +
--   type name as meta strings, then one @meta-string + varuint32
--   type id@ pair per field. The spec\'s field-header bit packing
--   (TAG_ID encoding, ref-tracking flag in field type info) is
--   not reproduced — the dynamic decoder reads value type tags
--   from the value bytes themselves.
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
  ) where

import Data.Bits (shiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
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
-- Encoder state monad
-- ---------------------------------------------------------------------------

-- | Cache key for the 'TypeDef' pool. We hash on the namespace,
-- type name, and the ordered list of field names; field types do
-- not contribute because the dynamic decoder reads value type
-- tags from the value bytes themselves rather than from the
-- 'TypeDef'.
type TypeDefKey = (Text, Text, [Text])

data EncodeState = EncodeState
  { esStringPool    :: !(HashMap Text Int)
  , esNextStringId  :: {-# UNPACK #-} !Int
  , esRefMap        :: !(IntMap Int)
    -- ^ Map from user-supplied 'RefVal' sharing keys to the wire
    -- @ref_id@ assigned on first occurrence.
  , esNextRefId     :: {-# UNPACK #-} !Int
  , esTypeDefPool   :: !(HashMap TypeDefKey Int)
  , esNextTypeDefId :: {-# UNPACK #-} !Int
  }

emptyState :: EncodeState
emptyState =
  EncodeState HM.empty 0 IM.empty 0 HM.empty 0

-- | Writer + state monad over 'E.Builder'. Hand-rolled to keep
-- the package free of an mtl / transformers dependency and to
-- inline the common @<>@ on the writer half.
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

-- | Run an 'EncodeM' action with a fresh empty pool, returning the
-- accumulated bytes.
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
-- Public API
-- ---------------------------------------------------------------------------

-- | Encode a 'Value' to a fory xlang byte sequence.
encode :: VV.Value -> ByteString
encode v = runEncodeM (encodeBuilder v)

-- | Like 'encode' but as an 'EncodeM' action so callers can compose
-- multiple top-level values that share the same dedup pools.
encodeBuilder :: VV.Value -> EncodeM ()
encodeBuilder v = do
  emit (E.byte E.foryXlangHeader)
  encodeValueSlot v

-- ---------------------------------------------------------------------------
-- Value slot
-- ---------------------------------------------------------------------------

-- | Encode a single value slot (ref flag + payload).
encodeValueSlot :: VV.Value -> EncodeM ()
encodeValueSlot v = case v of
  VV.NoneVal       -> emit (E.byte E.refFlagNull)
  VV.RefVal i inner -> encodeRef i inner
  _                -> encodePayload v

encodeRef :: Int -> VV.Value -> EncodeM ()
encodeRef userKey inner = do
  s <- getState
  case IM.lookup userKey (esRefMap s) of
    Just wid -> do
      emit (E.byte E.refFlagRef)
      emit (E.varuint32 (fromIntegral wid :: Word32))
    Nothing -> do
      let !wid = esNextRefId s
      modifyState $ \s' -> s'
        { esRefMap    = IM.insert userKey wid (esRefMap s')
        , esNextRefId = wid + 1
        }
      emit (E.byte E.refFlagRefValue)
      encodePayload inner

-- | Type tag + payload, no leading ref flag. The caller is
-- expected to have already emitted (or decided not to emit) the
-- slot\'s ref flag.
encodePayload :: VV.Value -> EncodeM ()
encodePayload val = case val of
  VV.NoneVal       -> emitTag T.NONE
  VV.BoolVal b     -> emitTag T.BOOL    >> emit (E.byte (if b then 1 else 0))
  VV.Int8Val n     -> emitTag T.INT8    >> emit (E.byte (fromIntegral n))
  VV.Int16Val n    -> emitTag T.INT16   >> emit (E.int16LE n)
  VV.Int32Val n    -> emitTag T.INT32   >> emit (E.int32LE n)
  VV.Int64Val n    -> emitTag T.INT64   >> emit (E.int64LE n)
  VV.Uint8Val n    -> emitTag T.UINT8   >> emit (E.byte n)
  VV.Uint16Val n   -> emitTag T.UINT16  >> emit (E.word16LE n)
  VV.Uint32Val n   -> emitTag T.UINT32  >> emit (E.word32LE n)
  VV.Uint64Val n   -> emitTag T.UINT64  >> emit (E.word64LE n)
  VV.Float32Val f  -> emitTag T.FLOAT32 >> emit (E.float32LE f)
  VV.Float64Val d  -> emitTag T.FLOAT64 >> emit (E.float64LE d)
  VV.StringVal s   -> emitTag T.STRING  >> emit (E.utf8String s)
  VV.BinaryVal bs  -> emitTag T.BINARY  >> emitBinaryPayload bs
  VV.ListVal vs    -> emitTag T.LIST    >> emitCollection vs
  VV.SetVal vs     -> emitTag T.SET     >> emitCollection vs
  VV.MapVal kvs    -> emitTag T.MAP     >> emitMap kvs
  VV.StructVal ns nm fields -> do
    emitTag T.NAMED_STRUCT
    emitMetaString ns
    emitMetaString nm
    emitStructFields fields
  VV.CompatibleStructVal ns nm fields -> do
    emitTag T.NAMED_COMPATIBLE_STRUCT
    emitTypeDef ns nm fields
    -- Field values follow the meta-share marker, in TypeDef order.
    V.forM_ fields $ \(_, fv) -> encodeValueSlot fv
  VV.RefVal{} ->
    -- Should be intercepted by 'encodeValueSlot'; reaching here
    -- means a 'RefVal' is nested under a ref flag we already
    -- emitted (e.g. inside another RefVal first occurrence). Just
    -- recurse into the slot encoder so the inner ref handling
    -- runs.
    encodeValueSlot val
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
emitTag (T.TypeId w) = emit (E.byte w)
{-# INLINE emitTag #-}

-- ---------------------------------------------------------------------------
-- Containers
-- ---------------------------------------------------------------------------

emitBinaryPayload :: ByteString -> EncodeM ()
emitBinaryPayload !bs = do
  emit (E.varuint32 (fromIntegral (BS.length bs) :: Word32))
  emit (E.bytes bs)

emitCollection :: Vector VV.Value -> EncodeM ()
emitCollection vs = do
  emit (E.varuint32 (fromIntegral (V.length vs) :: Word32))
  V.mapM_ encodeValueSlot vs

emitMap :: Vector (VV.Value, VV.Value) -> EncodeM ()
emitMap kvs = do
  emit (E.varuint32 (fromIntegral (V.length kvs) :: Word32))
  V.forM_ kvs $ \(k, v) -> do
    encodeValueSlot k
    encodeValueSlot v

emitStructFields :: VV.StructFields -> EncodeM ()
emitStructFields fields = do
  emit (E.varuint32 (fromIntegral (V.length fields) :: Word32))
  V.forM_ fields $ \(name, value) -> do
    emitMetaString name
    encodeValueSlot value

-- ---------------------------------------------------------------------------
-- Meta-string deduplication
-- ---------------------------------------------------------------------------

-- | Emit a meta string. The first time a particular 'Text' is
-- written we emit a fresh meta-string and register it in the
-- pool. Subsequent occurrences emit a varuint64 back-reference.
emitMetaString :: Text -> EncodeM ()
emitMetaString !t = do
  s <- getState
  case HM.lookup t (esStringPool s) of
    Just rid -> emit (MS.refMetaString rid)
    Nothing -> do
      let !rid = esNextStringId s
      modifyState $ \s' -> s'
        { esStringPool   = HM.insert t rid (esStringPool s')
        , esNextStringId = rid + 1
        }
      emit (MS.freshMetaString t)

-- ---------------------------------------------------------------------------
-- TypeDef sidecar (NAMED_COMPATIBLE_STRUCT)
-- ---------------------------------------------------------------------------

-- | Emit the meta-share marker + (on first occurrence) the
-- 'TypeDef' bytes for a 'CompatibleStructVal'.
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

-- | The TypeDef itself: an 8-byte global header carrying the body
-- size in its low 8 bits, followed by the body.
emitTypeDefBytes :: Text -> Text -> VV.StructFields -> EncodeM ()
emitTypeDefBytes ns nm fields = do
  -- We need the body size up front to fill the global header.
  -- Encode the body into its own state-threaded sub-builder; the
  -- string pool / typedef pool MUST share with the outer encoder
  -- (so dedup works across the TypeDef and the value layer).
  s0 <- getState
  let (s1, bodyBs) = runSubEncoder (typeDefBody ns nm fields) s0
      !bodyLen     = BS.length bodyBs
  modifyState (const s1)
  emitGlobalHeader bodyLen
  emit (E.bytes bodyBs)

-- | Run an 'EncodeM' action against the supplied state and bake
-- its writer half into a 'ByteString', returning the final state.
-- Used to compute a TypeDef body size before emitting the size
-- prefix.
runSubEncoder :: EncodeM () -> EncodeState -> (EncodeState, ByteString)
runSubEncoder (EncodeM m) s =
  case m s of
    (_, s', b) -> (s', E.runBuilder b)

emitGlobalHeader :: Int -> EncodeM ()
emitGlobalHeader !bodyLen
  | bodyLen < 0xFF = do
      let !w = fromIntegral bodyLen :: Word64
      emit (E.word64LE w)
  | otherwise = do
      emit (E.word64LE 0xFF)
      emit (E.varuint32 (fromIntegral (bodyLen - 0xFF) :: Word32))

typeDefBody :: Text -> Text -> VV.StructFields -> EncodeM ()
typeDefBody ns nm fields = do
  let !nfRaw = V.length fields
      -- meta header: bits 0-4 num_fields (capped at 30; 31 means
      -- "extended"); bit 5 = REGISTER_BY_NAME (always set here).
      registerByName = 1 `shiftL` 5
  if nfRaw <= 30
    then do
      let !mh = fromIntegral nfRaw .|. registerByName :: Word32
      emit (E.byte (fromIntegral (mh .&. 0xFF)))
    else do
      let !mh = 31 .|. registerByName :: Word32
      emit (E.byte (fromIntegral (mh .&. 0xFF)))
      emit (E.varuint32 (fromIntegral (nfRaw - 31) :: Word32))
  emitMetaString ns
  emitMetaString nm
  V.forM_ fields $ \(fname, fvalue) -> do
    emitMetaString fname
    -- Field type id placeholder. The decoder ignores this (it
    -- reads value type tags from the value bytes themselves), but
    -- writing the value\'s actual type id makes the TypeDef bytes
    -- somewhat self-describing for inspection / debugging.
    let T.TypeId tw = VV.typeIdOf fvalue
    emit (E.varuint32 (fromIntegral tw :: Word32))

-- ---------------------------------------------------------------------------
-- Primitive 1-D arrays
-- ---------------------------------------------------------------------------
--
-- Each array is | varuint32 element_count | element bytes |, with
-- elements packed without any per-element flag, per the spec\'s
-- "fixed-width little-endian" requirement for primitive arrays.

emitArrayCount :: Vector a -> EncodeM ()
emitArrayCount vs =
  emit (E.varuint32 (fromIntegral (V.length vs) :: Word32))
{-# INLINE emitArrayCount #-}

emitBoolArray :: Vector Bool -> EncodeM ()
emitBoolArray vs = do
  emitArrayCount vs
  V.forM_ vs $ \b -> emit (E.byte (if b then 1 else 0))

emitInt8Array :: Vector Int8 -> EncodeM ()
emitInt8Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.byte (fromIntegral x))

emitInt16Array :: Vector Int16 -> EncodeM ()
emitInt16Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.int16LE x)

emitInt32Array :: Vector Int32 -> EncodeM ()
emitInt32Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.int32LE x)

emitInt64Array :: Vector Int64 -> EncodeM ()
emitInt64Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.int64LE x)

emitUint8Array :: Vector Word8 -> EncodeM ()
emitUint8Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.byte x)

emitUint16Array :: Vector Word16 -> EncodeM ()
emitUint16Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.word16LE x)

emitUint32Array :: Vector Word32 -> EncodeM ()
emitUint32Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.word32LE x)

emitUint64Array :: Vector Word64 -> EncodeM ()
emitUint64Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.word64LE x)

emitFloat32Array :: Vector Float -> EncodeM ()
emitFloat32Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.float32LE x)

emitFloat64Array :: Vector Double -> EncodeM ()
emitFloat64Array vs = do
  emitArrayCount vs
  V.forM_ vs $ \x -> emit (E.float64LE x)
