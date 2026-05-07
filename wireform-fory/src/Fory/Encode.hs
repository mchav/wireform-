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
import Data.Int (Int32)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Word (Word8, Word64)
import qualified Data.ByteString.Internal as BSI
import qualified Foreign.ForeignPtr
import qualified Data.Text.Internal as TI
import Foreign.Ptr (Ptr, plusPtr)
import qualified Foreign.Ptr
import qualified Foreign.Marshal.Utils
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Fory.Bulk as B
import qualified Fory.IO as IO
import qualified Fory.MetaString.Encoder as MSE
import qualified Fory.MetaString.Hash as MSH
import qualified Fory.Options as Opt
import qualified Fory.Struct as ST
import qualified Fory.TextHelpers as TH
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
    emitArrayBytes e bs
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
    emitRegisteredHeader e ns nm
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
-- | Emit a 'ByteString' payload prefixed with a varuint
-- length header. Reserves @9 + len@ bytes up front and
-- inlines the header poke + 'memcpy' in a single
-- 'IO.withReservedRaw' batch — saves one
-- @ensure / readIORef / writeIORef@ trio compared to
-- @emitVaruint32 + emitBytes@.
emitArrayBytes :: IO.Encoder -> ByteString -> IO ()
emitArrayBytes !e !bs = do
  let !blen = BS.length bs
      (BSI.BS fpSrc _) = bs
  IO.withReservedRaw e (9 + blen) $ \p start -> do
    !off1 <- IO.pokeVaruint64Raw p start (fromIntegral blen)
    Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
      Foreign.Marshal.Utils.copyBytes
        (p `Foreign.Ptr.plusPtr` off1) pSrc blen
    pure (off1 + blen)
{-# INLINE emitArrayBytes #-}

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

-- | Emit a 'Text' as a Fory string-payload.
--
-- Reaches into the 'Text''s underlying 'TI.Text arr off len'
-- and scans the 'ByteArray' bytes for the high bit (Word64-
-- stride OR-fold via 'TH.byteArrayIsAscii'). For ASCII
-- strings the underlying UTF-8 bytes are byte-identical to
-- the LATIN-1 wire format (tag 0); we 'memcpy' them straight
-- into the encoder buffer via 'TH.copyTextArrayToPtr',
-- skipping the 'TE.encodeUtf8' allocation and the 'BS.all'
-- scan.
--
-- For Latin-1-only strings (chars 128–255 only, no chars
-- ≥ 256), wire bytes differ from UTF-8 (1 byte/char vs
-- 2 bytes/char), so we still re-encode via 'B.latin1Bytes'.
-- For true UTF-8 (any char ≥ 256), the underlying bytes
-- are already UTF-8 and we 'memcpy' them with tag 2.
emitForyString :: IO.Encoder -> Text -> IO ()
emitForyString !e t@(TI.Text arr srcOff len)
  | TH.byteArrayIsAscii arr srcOff (srcOff + len) =
      -- Reserve enough for the 9-byte worst-case varuint
      -- header + the payload up front, then inline-poke
      -- the header and 'memcpy' the bytes in a single
      -- 'IO.withReservedRaw' batch — saves the second
      -- 'ensure / readIORef / writeIORef' trio that the
      -- old @emitVaruint36Small + withReservedRaw@ pair
      -- used to do.
      IO.withReservedRaw e (9 + len) $ \p start -> do
        !off1 <- IO.pokeVaruint64Raw p start
                   (fromIntegral len `shiftL` 2 :: Word64)
        TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
        pure (off1 + len)
  | isLatin1 t = do
      let !raw    = B.latin1Bytes t
          !rawLen = BS.length raw
      IO.withReservedRaw e (9 + rawLen) $ \p start -> do
        !off1 <- IO.pokeVaruint64Raw p start
                   (fromIntegral rawLen `shiftL` 2 :: Word64)
        let (BSI.BS fpSrc _) = raw
        Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
          Foreign.Marshal.Utils.copyBytes
            (p `Foreign.Ptr.plusPtr` off1) pSrc rawLen
        pure (off1 + rawLen)
  | otherwise =
      IO.withReservedRaw e (9 + len) $ \p start -> do
        !off1 <- IO.pokeVaruint64Raw p start
                   ((fromIntegral len `shiftL` 2) .|. 2 :: Word64)
        TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
        pure (off1 + len)
{-# INLINE emitForyString #-}

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
  VV.BinaryVal bs  -> emitArrayBytes e bs
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
            (False, False) -> case sameTypeFastTag tag of
              Just !perElemMax -> do
                -- Hand-rolled inner loop with 'Int#'
                -- offset threading. The previous
                -- 'V.foldM' (\\off x -> writer p off x) start vs'
                -- pattern paid one box/unbox of the boxed
                -- @Int@ accumulator on every iteration —
                -- the writer's type is fixed at
                -- @Ptr Word8 -> Int -> Value -> IO Int@ so
                -- GHC couldn't specialize. Calling
                -- 'pokeUntaggedRaw' directly lets the
                -- inner loop thread the offset as @Int#@
                -- and pay the 'I#' wrap exactly once at
                -- the end.
                let !nVs = V.length vs
                IO.withReservedRaw e (perElemMax * nVs) $ \p start -> do
                  let goFP !i !off
                        | i >= nVs = pure off
                        | otherwise = do
                            !off' <- pokeUntaggedRaw p off
                                       (V.unsafeIndex vs i)
                            goFP (i + 1) off'
                  goFP 0 start
              Nothing
                | tag == T.STRING -> emitStringListFast e vs
                | tag == T.NAMED_STRUCT ->
                    emitNamedStructListFast e vs
                | otherwise ->
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

-- | Fast-path writer table for the @sameType + no-null +
-- no-ref-tracking@ collection inner loop. Returns a per-element
-- byte upper bound + a raw 'Ptr Word8'-based writer that
-- updates the cursor without touching the encoder's IORefs.
-- Only the fixed-size primitive tags are batched; variable-size
-- types (strings, binary, structs, nested collections) fall
-- through to the standard 'emitUntaggedPayload' path.
-- | Per-element worst-case byte size for the same-type +
-- no-null + no-ref-tracking inner loop, indexed by the
-- collection's element 'TypeId'. Used by 'emitCollection'
-- to decide whether to take the inline raw-poke fast
-- path; 'pokeUntaggedRaw' does the actual write.
sameTypeFastTag :: T.TypeId -> Maybe Int
sameTypeFastTag !tag = case tag of
  T.BOOL       -> Just 1
  T.INT8       -> Just 1
  T.INT16      -> Just 2
  T.INT32      -> Just 4
  T.VARINT32   -> Just 5
  T.INT64      -> Just 8
  T.VARINT64   -> Just 9
  T.UINT8      -> Just 1
  T.UINT16     -> Just 2
  T.UINT32     -> Just 4
  T.VAR_UINT32 -> Just 5
  T.UINT64     -> Just 8
  T.VAR_UINT64 -> Just 9
  T.FLOAT32    -> Just 4
  T.FLOAT64    -> Just 8
  _            -> Nothing
{-# INLINE sameTypeFastTag #-}

-- | Inline raw-pointer untagged-payload write for the
-- primitive-element same-type fast path. Has the same
-- shape as 'emitUntaggedPayload' but pokes via raw
-- pointer + 'Int#' offset (so the caller can thread the
-- offset as @Int#@ instead of paying a box/unbox per
-- element). Variable-size payloads (strings, binary,
-- structs, nested collections) aren't covered here —
-- those go through the dedicated fast paths
-- ('emitStringListFast', 'emitNamedStructListFast') or
-- the slow per-element 'emitUntaggedPayload' path.
pokeUntaggedRaw :: Ptr Word8 -> Int -> VV.Value -> IO Int
pokeUntaggedRaw !p !off val = case val of
  VV.BoolVal b      -> IO.pokeByteRaw p off (if b then 1 else 0)
  VV.Int8Val n      -> IO.pokeByteRaw p off (fromIntegral n)
  VV.Int16Val n     -> IO.pokeInt16LERaw p off n
  VV.Int32Val n     -> IO.pokeInt32LERaw p off n
  VV.VarInt32Val n  -> IO.pokeVarint32Raw p off n
  VV.Int64Val n     -> IO.pokeInt64LERaw p off n
  VV.VarInt64Val n  -> IO.pokeVarint64Raw p off n
  VV.Uint8Val n     -> IO.pokeByteRaw p off n
  VV.Uint16Val n    -> IO.pokeWord16LERaw p off n
  VV.Uint32Val n    -> IO.pokeWord32LERaw p off n
  VV.VarUint32Val n -> IO.pokeVaruint32Raw p off n
  VV.Uint64Val n    -> IO.pokeWord64LERaw p off n
  VV.VarUint64Val n -> IO.pokeVaruint64Raw p off n
  VV.Float32Val f   -> IO.pokeFloat32LERaw p off f
  VV.Float64Val d   -> IO.pokeFloat64LERaw p off d
  v -> error ("pokeUntaggedRaw: non-primitive value " ++ show v)
{-# INLINE pokeUntaggedRaw #-}

sameTypeFastPath
  :: T.TypeId
  -> Maybe (Int, Ptr Word8 -> Int -> VV.Value -> IO Int)
sameTypeFastPath !tag = case tag of
  T.BOOL     -> Just (1, fpBool)
  T.INT8     -> Just (1, fpInt8)
  T.INT16    -> Just (2, fpInt16)
  T.INT32    -> Just (4, fpInt32)
  T.VARINT32 -> Just (5, fpVarInt32)
  T.INT64    -> Just (8, fpInt64)
  T.VARINT64 -> Just (9, fpVarInt64)
  T.UINT8    -> Just (1, fpUint8)
  T.UINT16   -> Just (2, fpUint16)
  T.UINT32   -> Just (4, fpUint32)
  T.VAR_UINT32 -> Just (5, fpVarUint32)
  T.UINT64   -> Just (8, fpUint64)
  T.VAR_UINT64 -> Just (9, fpVarUint64)
  T.FLOAT32  -> Just (4, fpFloat32)
  T.FLOAT64  -> Just (8, fpFloat64)
  _          -> Nothing
  where
    fpBool, fpInt8, fpUint8 :: Ptr Word8 -> Int -> VV.Value -> IO Int
    fpInt16, fpUint16 :: Ptr Word8 -> Int -> VV.Value -> IO Int
    fpInt32, fpUint32, fpFloat32 :: Ptr Word8 -> Int -> VV.Value -> IO Int
    fpInt64, fpUint64, fpFloat64 :: Ptr Word8 -> Int -> VV.Value -> IO Int
    fpVarInt32, fpVarInt64 :: Ptr Word8 -> Int -> VV.Value -> IO Int
    fpVarUint32, fpVarUint64 :: Ptr Word8 -> Int -> VV.Value -> IO Int

    fpBool       p off (VV.BoolVal b)         = IO.pokeByteRaw p off (if b then 1 else 0)
    fpBool       _ _   _                      = error "sameTypeFastPath fpBool"
    fpInt8       p off (VV.Int8Val n)         = IO.pokeByteRaw p off (fromIntegral n)
    fpInt8       _ _   _                      = error "sameTypeFastPath fpInt8"
    fpInt16      p off (VV.Int16Val n)        = IO.pokeInt16LERaw p off n
    fpInt16      _ _   _                      = error "sameTypeFastPath fpInt16"
    fpInt32      p off (VV.Int32Val n)        = IO.pokeInt32LERaw p off n
    fpInt32      _ _   _                      = error "sameTypeFastPath fpInt32"
    fpVarInt32   p off (VV.VarInt32Val n)     = IO.pokeVarint32Raw p off n
    fpVarInt32   _ _   _                      = error "sameTypeFastPath fpVarInt32"
    fpInt64      p off (VV.Int64Val n)        = IO.pokeInt64LERaw p off n
    fpInt64      _ _   _                      = error "sameTypeFastPath fpInt64"
    fpVarInt64   p off (VV.VarInt64Val n)     = IO.pokeVarint64Raw p off n
    fpVarInt64   _ _   _                      = error "sameTypeFastPath fpVarInt64"
    fpUint8      p off (VV.Uint8Val n)        = IO.pokeByteRaw p off n
    fpUint8      _ _   _                      = error "sameTypeFastPath fpUint8"
    fpUint16     p off (VV.Uint16Val n)       = IO.pokeWord16LERaw p off n
    fpUint16     _ _   _                      = error "sameTypeFastPath fpUint16"
    fpUint32     p off (VV.Uint32Val n)       = IO.pokeWord32LERaw p off n
    fpUint32     _ _   _                      = error "sameTypeFastPath fpUint32"
    fpVarUint32  p off (VV.VarUint32Val n)    = IO.pokeVaruint32Raw p off n
    fpVarUint32  _ _   _                      = error "sameTypeFastPath fpVarUint32"
    fpUint64     p off (VV.Uint64Val n)       = IO.pokeWord64LERaw p off n
    fpUint64     _ _   _                      = error "sameTypeFastPath fpUint64"
    fpVarUint64  p off (VV.VarUint64Val n)    = IO.pokeVaruint64Raw p off n
    fpVarUint64  _ _   _                      = error "sameTypeFastPath fpVarUint64"
    fpFloat32    p off (VV.Float32Val f)      = IO.pokeFloat32LERaw p off f
    fpFloat32    _ _   _                      = error "sameTypeFastPath fpFloat32"
    fpFloat64    p off (VV.Float64Val d)      = IO.pokeFloat64LERaw p off d
    fpFloat64    _ _   _                      = error "sameTypeFastPath fpFloat64"

-- | Fast path for a same-type list of 'StringVal'. Pre-encodes
-- each element to UTF-8 (essentially free for ASCII Text 2.x —
-- the underlying ByteArray is shared), classifies each as
-- pure-ASCII (LATIN-1 wire encoding tag) or not (UTF-8 tag),
-- sums the upper bound on total bytes, then emits the whole
-- batch with a single 'IO.withReservedRaw' call.
emitStringListFast :: IO.Encoder -> Vector VV.Value -> IO ()
emitStringListFast !e !vs = do
  -- Hand-rolled monomorphic 'Int# -> Int# -> Int#' size
  -- walk; avoids the 'V.foldl' closure allocation for the
  -- accumulator that GHC otherwise has to box.
  let !nVs = V.length vs
      goSize !i !sz
        | i >= nVs = sz
        | otherwise = case V.unsafeIndex vs i of
            VV.StringVal (TI.Text _ _ l) ->
              goSize (i + 1) (sz + 9 + l)
            v -> error $ "emitStringListFast: non-StringVal " ++ show v
      !totalSize = goSize 0 0
  IO.withReservedRaw e totalSize $ \p start -> do
    let goWr !i !off
          | i >= nVs = pure off
          | otherwise = do
              !off' <- writeOne p off (V.unsafeIndex vs i)
              goWr (i + 1) off'
    goWr 0 start
  where

    writeOne :: Ptr Word8 -> Int -> VV.Value -> IO Int
    writeOne !p !off (VV.StringVal t@(TI.Text arr srcOff len))
      | TH.byteArrayIsAscii arr srcOff (srcOff + len) = do
          off1 <- IO.pokeVaruint64Raw p off
                    (fromIntegral len `shiftL` 2 :: Word64)
          TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
          pure (off1 + len)
      | isLatin1 t = do
          let !raw  = B.latin1Bytes t
              !rlen = BS.length raw
          off1 <- IO.pokeVaruint64Raw p off
                    (fromIntegral rlen `shiftL` 2 :: Word64)
          pokeBytesRaw p off1 raw
      | otherwise = do
          off1 <- IO.pokeVaruint64Raw p off
                    ((fromIntegral len `shiftL` 2) .|. 2 :: Word64)
          TH.copyTextArrayToPtr arr srcOff (p `plusPtr` off1) len
          pure (off1 + len)
    writeOne _ _ v = error $ "emitStringListFast: non-StringVal " ++ show v

pokeBytesRaw :: Ptr Word8 -> Int -> ByteString -> IO Int
pokeBytesRaw !p !pos !bs = do
  let (BSI.BS fpSrc lenSrc) = bs
      !destPtr = p `Foreign.Ptr.plusPtr` pos
  Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
    Foreign.Marshal.Utils.copyBytes destPtr pSrc lenSrc
  pure (pos + lenSrc)
{-# INLINE pokeBytesRaw #-}

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
  -- Hand-rolled loop using only unboxed 'Int#' accumulators
  -- (after worker-wrapper).
  --
  -- Earlier this was 'V.foldl' step (True, False, Nothing) vs',
  -- which allocated a fresh @(Bool, Bool, Maybe TypeId)@
  -- tuple per iteration — visible in the Core as
  -- @$wfoldlM'_loop :: ... -> Bool -> Bool -> Maybe TypeId
  -- -> Int# -> ...@.
  --
  -- The first attempt at unboxing (@goA !i !st !hn !hasT
  -- !tag@ without explicit types) regressed: GHC defaulted
  -- the @0@ / @1@ accumulator literals to 'Integer' (boxed
  -- arbitrary-precision), giving a
  -- @$wgoA :: Int# -> Integer -> Integer -> Integer ->
  -- Int#@ loop with a 3-case @IS x | IP x | IN x@
  -- pattern-match per accumulator per iteration. The
  -- explicit 'Int' type signature here forces strict-
  -- worker-wrapper to unbox each flag to a single @Int#@.
  let !n = V.length vs
      goA :: Int -> Int -> Int -> Int -> Int
          -> (Bool, Bool, Maybe T.TypeId)
      goA !i !st !hn !hasT !tag
        | i >= n =
            ( st /= 0
            , hn /= 0
            , if hasT /= 0
                then Just (T.TypeId (fromIntegral tag))
                else Nothing
            )
        | otherwise = case V.unsafeIndex vs i of
            VV.NoneVal  -> goA (i + 1) st     (1 :: Int) hasT tag
            VV.RefVal{} -> goA (i + 1) (0 :: Int) (1 :: Int) hasT tag
            x ->
              let !(T.TypeId tg) = VV.typeIdOf x
                  !tgI = fromIntegral tg :: Int
              in if hasT == (0 :: Int)
                   then goA (i + 1) st hn (1 :: Int) tgI
                   else if tag == tgI
                     then goA (i + 1) st hn (1 :: Int) tag
                     else goA (i + 1) (0 :: Int) hn (1 :: Int) tag
  in goA 0 (1 :: Int) (0 :: Int) (0 :: Int) (0 :: Int)
{-# INLINABLE analyseCollection #-}

emitElementTypeInfo
  :: IO.Encoder -> T.TypeId -> Vector VV.Value -> IO ()
emitElementTypeInfo !e !tag !vs = do
  emitTag e tag
  case tag of
    T.NAMED_STRUCT -> case V.find (not . isNoneV) vs of
      Just (VV.StructVal ns nm _)           -> do
        emitMetaStringWith e MSE.namespaceSpecialChars ns
        emitMetaStringWith e MSE.typenameSpecialChars  nm
      Just (VV.RegisteredStructVal ns nm _) ->
        emitRegisteredHeader e ns nm
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
    else case homogeneousMap kvs of
      Just (keyTag, valTag) -> emitMapChunkedHomogeneous e kvs keyTag valTag
      Nothing -> V.forM_ kvs $ \(k, v) -> emitOneEntryChunk e k v

-- | Returns @Just (keyTag, valTag)@ if every entry has the
-- same non-null key type and the same non-null value type.
-- We use this to emit the map as one (or a few) maximally-large
-- chunk(s) instead of N single-entry chunks, saving the
-- per-entry chunk header overhead and enabling the
-- 'withReservedRaw' fast path below.
homogeneousMap
  :: Vector (VV.Value, VV.Value) -> Maybe (T.TypeId, T.TypeId)
homogeneousMap !kvs =
  let (kHomo, vHomo, ok) = V.foldl' step (Nothing, Nothing, True) kvs
  in if ok then (,) <$> kHomo <*> vHomo else Nothing
  where
    step (mKt, mVt, ok) (k, v)
      | not ok = (mKt, mVt, False)
      | isNoneV k || isNoneV v = (mKt, mVt, False)
      | otherwise =
          let !kt = VV.typeIdOf k
              !vt = VV.typeIdOf v
          in case (mKt, mVt) of
               (Nothing, Nothing) -> (Just kt, Just vt, True)
               (Just kt0, Just vt0)
                 | kt == kt0 && vt == vt0 -> (mKt, mVt, True)
                 | otherwise              -> (mKt, mVt, False)
               _ -> (mKt, mVt, False)

-- | Emit a homogeneous map as a sequence of large chunks
-- (max 255 entries per chunk, since chunk_size is a single
-- byte). Within each chunk header is @0x00 + chunkSize +
-- keyTag + valTag@; the per-entry payload is written by
-- 'emitMapHomogeneousPayload' which dispatches to a tight
-- batched path for the common (STRING, VARINT64) case.
emitMapChunkedHomogeneous
  :: IO.Encoder -> Vector (VV.Value, VV.Value) -> T.TypeId -> T.TypeId -> IO ()
emitMapChunkedHomogeneous !e !kvs !keyTag !valTag = go kvs
  where
    go !rest
      | V.null rest = pure ()
      | otherwise = do
          let !len = V.length rest
              !cs  = min 255 len
              !chunk = V.take cs rest
          IO.emitByte e 0
          IO.emitByte e (fromIntegral cs)
          emitTag e keyTag
          emitTag e valTag
          emitMapHomogeneousPayload e chunk keyTag valTag
          go (V.drop cs rest)

emitMapHomogeneousPayload
  :: IO.Encoder
  -> Vector (VV.Value, VV.Value)
  -> T.TypeId
  -> T.TypeId
  -> IO ()
emitMapHomogeneousPayload !e !kvs !keyTag !valTag
  | keyTag == T.STRING && valTag == T.VARINT64 =
      emitMapStringVarInt64 e kvs
  | otherwise =
      case (sameTypeFastPath keyTag, sameTypeFastPath valTag) of
        (Just (kMax, kw), Just (vMax, vw)) ->
          IO.withReservedRaw e ((kMax + vMax) * V.length kvs) $ \p start ->
            V.foldM' (\off (k, v) -> do
              off1 <- kw p off k
              vw p off1 v) start kvs
        _ -> V.forM_ kvs $ \(k, v) -> do
          emitUntaggedPayload e k
          emitUntaggedPayload e v

-- | Same-type list of @NAMED_STRUCT@ fast path. The
-- 'emitElementTypeInfo' caller has already written the
-- shared @ns@ + @typeName@ meta-strings once at the
-- collection-element-type position, so each element only
-- needs its 4-byte fingerprint hash + canonical-order
-- field payload. We hoist the registry lookup outside the
-- inner loop so the per-element cost drops to one cached
-- 'ST.StructSchema' read + the field emits.
emitNamedStructListFast :: IO.Encoder -> Vector VV.Value -> IO ()
emitNamedStructListFast !e !vs =
  case V.find (not . isNoneV) vs of
    Just (VV.RegisteredStructVal ns nm sample) ->
      case lookupSchema e ns nm of
        Just sch ->
          let !canonical = ST.fieldOrder sch
              !nFields   = V.length canonical
              !hashCode  = ST.ssHash sch
              -- Resolve the canonical -> user-field-index
              -- permutation once from the first non-null
              -- element. Subsequent elements are assumed to
              -- have the same field-name ordering (the
              -- standard pattern for typed users — they
              -- construct via a single helper like
              -- @mkPerson@). On mismatch we crash via the
              -- 'V.unsafeIndex' bounds-check rather than
              -- silently emit garbage.
              !perm        = computeFieldPerm canonical sample
              -- Detect the common case where the user's
              -- field ordering matches the canonical order
              -- (i.e. @perm == [0, 1, .., nFields-1]@). In
              -- that case we can skip the per-field
              -- 'V.unsafeIndex perm i' and index directly
              -- into the user's @StructFields@ — saves one
              -- array load per field on the per-struct hot
              -- loop.
              !permIsId    = isIdentityPerm perm nFields
              -- See 'ST.ssAllBasicNonNull'. When set
              -- (typed-client default), we can replace
              -- the per-field 'FieldSpec' destructure +
              -- 'isBasicTypeId' / 'fsNullable' checks
              -- with a single 'emitUntaggedPayload' call.
              !allBNN      = ST.ssAllBasicNonNull sch
              !nVs         = V.length vs
              -- Hoist the @permIsId@ + @allBNN@ branch
              -- selection /outside/ the per-element loop
              -- so each iteration jumps straight into the
              -- chosen specialised emitter without
              -- re-checking the schema-level flags.
              goFastest !i
                | i >= nVs = pure ()
                | otherwise = do
                    case V.unsafeIndex vs i of
                      VV.RegisteredStructVal _ _ fields ->
                        emitRegisteredStructFastest
                          e nFields hashCode fields
                      x -> emitUntaggedPayload e x
                    goFastest (i + 1)
              goIdentity !i
                | i >= nVs = pure ()
                | otherwise = do
                    case V.unsafeIndex vs i of
                      VV.RegisteredStructVal _ _ fields ->
                        emitRegisteredStructIdentity
                          e canonical nFields
                          hashCode fields
                      x -> emitUntaggedPayload e x
                    goIdentity (i + 1)
              goPermuted !i
                | i >= nVs = pure ()
                | otherwise = do
                    case V.unsafeIndex vs i of
                      VV.RegisteredStructVal _ _ fields ->
                        emitRegisteredStructPermuted
                          e canonical perm nFields
                          hashCode fields
                      x -> emitUntaggedPayload e x
                    goPermuted (i + 1)
          in if permIsId && allBNN
               then goFastest 0
               else if permIsId
                 then goIdentity 0
                 else goPermuted 0
        Nothing -> V.forM_ vs $ \x -> emitUntaggedPayload e x
    -- Heterogeneous (StructVal vs RegisteredStructVal) or
    -- all-None: fall back to the generic per-element path.
    _ -> V.forM_ vs $ \x -> emitUntaggedPayload e x

-- | Compute a 'Vector Int' that maps canonical field
-- positions to user-field positions. @perm[i]@ is the
-- index into @userFields@ that holds the @i@-th
-- canonical field's value. Used by
-- 'emitRegisteredStructPermuted' so the per-struct emit
-- doesn't pay an O(fields) name-lookup at every element.
computeFieldPerm
  :: Vector ST.FieldSpec
  -> VV.StructFields
  -> Vector Int
computeFieldPerm !canonical !fields =
  V.map findIdx canonical
  where
    findIdx spec = case lookupIdx (ST.fsName spec) fields 0 of
      Just i  -> i
      Nothing -> error $ "Fory.Encode: field "
                          ++ T.unpack (ST.fsName spec)
                          ++ " missing from RegisteredStructVal"

    lookupIdx :: Text -> VV.StructFields -> Int -> Maybe Int
    lookupIdx !name !fs !i
      | i >= V.length fs = Nothing
      | fst (V.unsafeIndex fs i) == name = Just i
      | otherwise = lookupIdx name fs (i + 1)

-- | Inner emitter for 'emitNamedStructListFast'. Same
-- contract as 'emitRegisteredStructWithSchema' but uses a
-- precomputed canonical -> user-index permutation
-- (@perm@) so each per-field access is an O(1)
-- 'V.unsafeIndex' instead of an O(fields) name-keyed scan
-- through 'VV.registeredStructFieldByName'.
emitRegisteredStructPermuted
  :: IO.Encoder
  -> Vector ST.FieldSpec
  -> Vector Int
  -> Int
  -- ^ field count, hoisted from the caller so the per-
  -- struct emit doesn't pay 'V.length' on every iteration.
  -> Int32
  -> VV.StructFields
  -> IO ()
emitRegisteredStructPermuted !e !canonical !perm !nFields !hashCode !fields = do
  IO.emitInt32LE e hashCode
  let go !i
        | i >= nFields = pure ()
        | otherwise = do
            let !spec    = V.unsafeIndex canonical i
                !userIdx = V.unsafeIndex perm i
                (_, !v)  = V.unsafeIndex fields userIdx
            emitRegisteredField e spec v
            go (i + 1)
  go 0

-- | Identity-permutation specialisation of
-- 'emitRegisteredStructPermuted' — saves one
-- 'V.unsafeIndex' per field. Used for the
-- 'permIsId == True' branch in 'emitNamedStructListFast'.
emitRegisteredStructIdentity
  :: IO.Encoder
  -> Vector ST.FieldSpec
  -> Int
  -> Int32
  -> VV.StructFields
  -> IO ()
emitRegisteredStructIdentity !e !canonical !nFields !hashCode !fields = do
  IO.emitInt32LE e hashCode
  let go !i
        | i >= nFields = pure ()
        | otherwise = do
            let !spec   = V.unsafeIndex canonical i
                (_, !v) = V.unsafeIndex fields i
            emitRegisteredField e spec v
            go (i + 1)
  go 0

-- | Returns 'True' iff @perm[i] == i@ for every @i@ in
-- @[0, n)@.
isIdentityPerm :: Vector Int -> Int -> Bool
isIdentityPerm !perm !n = go 0
  where
    go !i
      | i >= n = True
      | V.unsafeIndex perm i /= i = False
      | otherwise = go (i + 1)
{-# INLINE isIdentityPerm #-}

-- | Fastest specialisation of 'emitRegisteredStructPermuted'
-- — used when both the user's field ordering matches the
-- canonical order AND every field is basic-typeId
-- non-nullable (the common typed-client default; see
-- 'ST.ssAllBasicNonNull').
--
-- Skips the per-field 'FieldSpec' destructure and the
-- 'isBasicTypeId' / 'fsNullable' checks entirely — just
-- pulls each field's 'Value' out of the user's
-- 'StructFields' and 'emitUntaggedPayload's it. Saves
-- ~5 ns / field on the per-struct hot loop relative to
-- 'emitRegisteredStructIdentity'.
emitRegisteredStructFastest
  :: IO.Encoder
  -> Int
  -> Int32
  -> VV.StructFields
  -> IO ()
emitRegisteredStructFastest !e !nFields !hashCode !fields = do
  IO.emitInt32LE e hashCode
  let go !i
        | i >= nFields = pure ()
        | otherwise = do
            let (_, !v) = V.unsafeIndex fields i
            emitUntaggedPayload e v
            go (i + 1)
  go 0

-- | Inner emitter shared by 'emitTypedPayload' on a single
-- struct (where the permutation isn't pre-resolved).
-- Writes the 4-byte fingerprint hash and the canonical-order
-- field payloads for a single struct, given the cached
-- schema fields.
emitRegisteredStructWithSchema
  :: IO.Encoder
  -> Vector ST.FieldSpec
  -> Int32
  -> VV.StructFields
  -> IO ()
emitRegisteredStructWithSchema !e !canonical !hashCode !fields = do
  IO.emitInt32LE e hashCode
  V.forM_ canonical $ \spec ->
    case VV.registeredStructFieldByName (ST.fsName spec) fields of
      Nothing -> error $ "Fory.Encode: field "
                          ++ T.unpack (ST.fsName spec)
                          ++ " missing from RegisteredStructVal"
      Just v  -> emitRegisteredField e spec v

-- | Tight batched path for @Map String VarInt64@. Pre-encodes
-- each string key once (TE.encodeUtf8 + BS.all classification),
-- sums the upper-bound total bytes, then writes the whole chunk
-- via 'IO.withReservedRaw'.
emitMapStringVarInt64
  :: IO.Encoder -> Vector (VV.Value, VV.Value) -> IO ()
emitMapStringVarInt64 !e !kvs = do
  -- Single-pass size accumulator over the underlying Text
  -- byte length; no per-element 'TE.encodeUtf8' allocation.
  let !total = V.foldl' sumOne 0 kvs
  IO.withReservedRaw e total $ \p start ->
    V.foldM' (writeOne p) start kvs
  where
    sumOne :: Int -> (VV.Value, VV.Value) -> Int
    sumOne !acc (VV.StringVal (TI.Text _ _ len), _) =
      acc + 9 + len + 9
    sumOne _ kv = error $
      "emitMapStringVarInt64: expected (StringVal, VarInt64Val), got "
        ++ show kv

    -- Per-entry write: scan the Text's underlying ByteArray
    -- for ASCII-ness via the shared 'TH.byteArrayIsAscii'
    -- (Word64-stride OR-fold), 'memcpy' the source bytes
    -- straight into the encoder buffer via
    -- 'TH.copyTextArrayToPtr', then append the varint64
    -- value. No 'TE.encodeUtf8' allocation per key.
    writeOne :: Ptr Word8 -> Int -> (VV.Value, VV.Value) -> IO Int
    writeOne !p !off (VV.StringVal t@(TI.Text arr srcOff len),
                      VV.VarInt64Val n) = do
      !off1 <-
        if TH.byteArrayIsAscii arr srcOff (srcOff + len)
          then do
            -- ASCII: tag 0, raw memcpy of UTF-8 bytes (which
            -- equal LATIN-1 bytes for ASCII).
            !o <- IO.pokeVaruint64Raw p off
                    (fromIntegral len `shiftL` 2 :: Word64)
            TH.copyTextArrayToPtr arr srcOff (p `plusPtr` o) len
            pure (o + len)
          else if T.all (\c -> ord c < 256) t
            then do
              let !raw = B.latin1Bytes t
                  !rlen = BS.length raw
              !o <- IO.pokeVaruint64Raw p off
                      (fromIntegral rlen `shiftL` 2 :: Word64)
              pokeBytesRaw p o raw
            else do
              !o <- IO.pokeVaruint64Raw p off
                      ((fromIntegral len `shiftL` 2) .|. 2 :: Word64)
              TH.copyTextArrayToPtr arr srcOff (p `plusPtr` o) len
              pure (o + len)
      IO.pokeVarint64Raw p off1 n
    writeOne _ _ kv = error $
      "emitMapStringVarInt64.writeOne: unexpected " ++ show kv

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
  emitFreshMetaStringPrecomputed e enc bs

-- | Variant of 'emitMetaStringWith' that uses a pre-encoded
-- meta-string body (typically cached on a 'StructSchema' so
-- that repeated encodes of the same schema's namespace +
-- type name skip the per-call 'MSE.encodeMetaString'
-- char-classification + bit-pack pipeline).
emitMetaStringPrecomputed
  :: IO.Encoder -> Text -> MSE.Encoding -> ByteString -> IO ()
emitMetaStringPrecomputed !e !t !enc !bs = do
  m <- IO.metaStringLookup e t
  case m of
    Just rid ->
      IO.emitVaruint64 e
        (((fromIntegral rid + 1) `shiftL` 1) .|. 1 :: Word64)
    Nothing -> do
      _ <- IO.metaStringRegister e t
      emitFreshMetaStringPrecomputed e enc bs

emitFreshMetaStringPrecomputed
  :: IO.Encoder -> MSE.Encoding -> ByteString -> IO ()
emitFreshMetaStringPrecomputed !e !enc !bs = do
  let !len = BS.length bs
      !hdr = (fromIntegral len `shiftL` 1) :: Word64
      (BSI.BS fpSrc _) = bs
  if len == 0
    then IO.emitVaruint64 e hdr
    else if len <= 16
      then
        -- Combine: 9-byte varuint hdr + 1-byte encoding tag
        -- + payload, single 'IO.withReservedRaw'.
        IO.withReservedRaw e (10 + len) $ \p start -> do
          !off1 <- IO.pokeVaruint64Raw p start hdr
          !off2 <- IO.pokeByteRaw p off1 (MSE.encodingId enc)
          Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
            Foreign.Marshal.Utils.copyBytes
              (p `Foreign.Ptr.plusPtr` off2) pSrc len
          pure (off2 + len)
      else do
        let !hash = MSH.metaStringHashcode bs
                      (fromIntegral (MSE.encodingId enc))
        -- Combine: 9-byte varuint hdr + 8-byte hash +
        -- payload, single 'IO.withReservedRaw'.
        IO.withReservedRaw e (17 + len) $ \p start -> do
          !off1 <- IO.pokeVaruint64Raw p start hdr
          !off2 <- IO.pokeInt64LERaw p off1 (fromIntegral hash)
          Foreign.ForeignPtr.withForeignPtr fpSrc $ \pSrc ->
            Foreign.Marshal.Utils.copyBytes
              (p `Foreign.Ptr.plusPtr` off2) pSrc len
          pure (off2 + len)

-- | Emit the @namespace + type-name@ meta-string pair for
-- a 'NAMED_STRUCT' value. If the schema is registered with
-- the encoder we reuse its pre-encoded meta-string bodies
-- (avoiding the per-encode 'MSE.encodeMetaString'
-- bit-packing); otherwise we fall back to encoding from
-- scratch. This is the hot path for both
-- 'emitTypedPayload' on a single struct and
-- 'emitElementTypeInfo' at the head of a same-type list.
emitRegisteredHeader :: IO.Encoder -> Text -> Text -> IO ()
emitRegisteredHeader !e !ns !nm = case lookupSchema e ns nm of
  Just sch -> do
    emitMetaStringPrecomputed e ns
      (ST.ssNsEncoding sch) (ST.ssNsBody sch)
    emitMetaStringPrecomputed e nm
      (ST.ssTnEncoding sch) (ST.ssTnBody sch)
  Nothing -> do
    emitMetaStringWith e MSE.namespaceSpecialChars ns
    emitMetaStringWith e MSE.typenameSpecialChars  nm

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
{-# INLINE emitRegisteredField #-}

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

