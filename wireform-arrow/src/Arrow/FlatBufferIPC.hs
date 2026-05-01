{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
-- | Real Apache Arrow IPC framing — binary-compatible with the
-- reference implementations (arrow-cpp, arrow-rs, pyarrow).
--
-- "Arrow.IPC" uses a simplified flatbuffer-shaped encoding that
-- only self-round-trips; this module constructs Arrow's metadata
-- tables as actual FlatBuffers (per @format/Schema.fbs@ and
-- @format/Message.fbs@) and emits the encapsulated-message framing
-- so pyarrow / arrow-rs / arrow-cpp can consume the output.
--
-- The encoder is standards-compliant:
--
--   * Buffer is built back-to-front.
--   * Tables carry a signed int32 soffset to their vtable at offset 0.
--   * Vtables share when structurally identical (via a deduplication map).
--   * Scalars are aligned to their width; vectors/strings/tables
--     are 4-aligned.
--   * The root offset at byte 0 is an unsigned uoffset_t pointing to
--     the root table.
--
-- The encoder is hand-tailored for the exact tables Arrow needs
-- (@Schema@, @Field@, @RecordBatch@, @Message@, plus every @Type@
-- union sub-table). It does /not/ try to be a general-purpose
-- FlatBuffers library; that would be a separate project. Keeping it
-- local avoids coupling Arrow IPC to the churn of a future general
-- FlatBuffers rewrite.
module Arrow.FlatBufferIPC
  ( -- * Top-level builders
    buildSchemaMessage
  , buildRecordBatchMessage
    -- * Encapsulated-message framing
  , encapsulateMessage
    -- * Stream / file writers
  , writeArrowStreamFB
  , writeArrowFileFB
    -- * Column-based convenience writer
  , buildRecordBatchBytes
  , writeArrowStreamFBFromColumns
    -- * Reader (parses pyarrow / arrow-cpp output)
  , readArrowStreamFB
  , readArrowFileFB
  , decodeSchemaMessage
  , decodeRecordBatchMessage
  ) where

import Data.Bits ((.&.), (.|.), complement, shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')

import Arrow.Column (ColumnArray, columnLength)
import Arrow.Types
import qualified Arrow.Write as W

-- ============================================================
-- Low-level builder
-- ============================================================

-- | A builder accumulates bytes in reverse (i.e. the /tail/ of the
-- output is written first). The running 'Int' is the number of
-- bytes emitted so far — equal to the size of the buffer at the
-- moment an object is being placed. We call this its /UOffset/
-- (distance from the /end/ of the buffer to the object's start).
--
-- We also track @minalign@, the largest alignment observed across
-- all objects in this buffer. At finalisation the whole buffer is
-- padded to a multiple of this value, which is what guarantees
-- every object's FORWARD position in the final bytes is a multiple
-- of its alignment (because the forward position of an object =
-- final_size - object_uoff, and we pre-pad so that (object_uoff %
-- alignment) == ((final_size) % alignment) == 0 ⇒ forward-pos %
-- alignment == 0).
data Builder = Builder
  { bufPayload  :: !(IORef [BS.ByteString])
    -- ^ chunks, most recently prepended first — concatenating gives
    -- the final output when reversed.
  , bufSize     :: !(IORef Int)
    -- ^ total number of bytes written so far.
  , bufVTables  :: !(IORef (Map.Map VTableKey Int))
    -- ^ vtable deduplication: canonicalised vtable bytes → UOffset of
    -- the vtable within the buffer (distance from end of buffer).
  , bufMinAlign :: !(IORef Int)
  }

-- | A vtable's dedup key: the (vtable_size, table_size, [field_offsets]).
newtype VTableKey = VTableKey (Int, Int, [Int])
  deriving stock (Eq, Ord)

newBuilder :: IO Builder
newBuilder = Builder <$> newIORef [] <*> newIORef 0 <*> newIORef Map.empty <*> newIORef 1

-- | Note an alignment requirement on the running @minalign@. We'll
-- pad the final buffer to a multiple of the largest @minalign@
-- encountered when finalising, which propagates the alignment
-- guarantee to every sub-object (see 'Builder' haddock).
noteMinAlign :: Builder -> Int -> IO ()
noteMinAlign b a = modifyIORef' (bufMinAlign b) (max a)

-- | Pad /before/ writing an object of @objSize@ bytes with
-- alignment @objAlign@, so that after emission the object's
-- @UOffset@ satisfies @uoff % objAlign == 0@ (which combined with
-- the finalisation padding forces the forward position to be
-- aligned).
prepForObject :: Builder -> Int -> Int -> IO ()
prepForObject b objSize objAlign = do
  noteMinAlign b objAlign
  !cur <- readIORef (bufSize b)
  let !after = cur + objSize
      !pad   = (negate after) .&. (objAlign - 1)
  prependBS b (BS.replicate pad 0)

-- | Prepend raw bytes to the builder (i.e. they land before any
-- previously-written content).
prependBS :: Builder -> BS.ByteString -> IO ()
prependBS b bs = do
  modifyIORef' (bufPayload b) (bs :)
  modifyIORef' (bufSize b) (+ BS.length bs)

-- | Prepend one little-endian primitive.
prependU8  :: Builder -> Word8  -> IO ()
prependU8  b w = prependBS b (BS.singleton w)
prependU16 :: Builder -> Word16 -> IO ()
prependU16 b w = prependBS b $ BS.pack [fromIntegral w, fromIntegral (w `div` 0x100)]
prependU32 :: Builder -> Word32 -> IO ()
prependU32 b w = prependBS b $ BS.pack
  [ fromIntegral  w
  , fromIntegral (w `div` 0x100)
  , fromIntegral (w `div` 0x10000)
  , fromIntegral (w `div` 0x1000000)
  ]
prependU64 :: Builder -> Word64 -> IO ()
prependU64 b w = do
  prependU32 b (fromIntegral (w `div` 0x100000000))
  prependU32 b (fromIntegral w)
prependI16 :: Builder -> Int16  -> IO ()
prependI16 b i = prependU16 b (fromIntegral i)
prependI32 :: Builder -> Int32  -> IO ()
prependI32 b i = prependU32 b (fromIntegral i)
prependI64 :: Builder -> Int64  -> IO ()
prependI64 b i = prependU64 b (fromIntegral i)

-- | Finalise the builder into a single ByteString.
--
-- We pad so that the final buffer size is a multiple of
-- @max(minAlign, 4)@. Because every object's UOffset is already a
-- multiple of its own alignment (see 'Builder'), and
-- @forward_pos = final_size - uoff@, both terms are divisible by
-- the required alignment ⇒ forward_pos too.
finish :: Builder -> Int -> IO ByteString
finish b rootUOff = do
  !minA <- readIORef (bufMinAlign b)
  prepForObject b 4 (max minA 4)
  !curBefore <- readIORef (bufSize b)
  -- After we prepend 4 more bytes the buffer size = curBefore + 4
  -- and the root table sits at forward offset (curBefore + 4 -
  -- rootUOff) from the start.
  let !rootFromStart = (curBefore + 4) - rootUOff
  prependU32 b (fromIntegral rootFromStart)
  chunks <- readIORef (bufPayload b)
  pure $! BS.concat chunks

-- | Current UOffset of the bytes we're about to write = current
-- size. After the caller emits their object, the object's UOffset
-- is @bufSize-at-completion@; the /absolute file position/ of the
-- object = @totalSize - UOffset@.
currentUOff :: Builder -> IO Int
currentUOff b = readIORef (bufSize b)

-- ============================================================
-- Tables / vtables
-- ============================================================

-- | A single field slot within a table. @fsAlign@ is the on-disk
-- size of the inline scalar (2, 4, 8, …); for VOffset fields
-- (tables / strings / vectors) it's always 4.
data Field' = Field'
  { fsAlign :: !Int
  , fsWrite :: !(Builder -> Int -> IO ())
    -- ^ @fsWrite builder tableStartUOff@: prepend this field's inline
    -- data. For VOffset fields, that means writing @target_uoff -
    -- (bufSize-right-after-we-return)@ as a u32.
  }

-- | A scalar field: writes a fixed-size primitive.
scalar :: Int -> (Builder -> IO ()) -> Field'
scalar !align writer = Field' align $ \b _ -> writer b

-- | A VOffset field: writes a u32 relative offset to an already-laid-
-- out sub-object at @targetUOff@.
voff :: Int -> Field'
voff !targetUOff = Field' 4 $ \b _ -> do
  cur <- currentUOff b
  -- After we write 4 bytes, total = cur+4. The field is located at
  -- offset (total - cur-4) from end? No: the field starts at
  -- UOffset = cur+4 (before write), then occupies 4 bytes. The
  -- relative offset stored in the field should be:
  -- target_abs - field_abs. In UOffset terms: field_uoff - target_uoff.
  -- Here field_uoff (after write) = cur + 4. So write = cur+4 -
  -- targetUOff.
  prependU32 b (fromIntegral (cur + 4 - targetUOff))

-- | A table's forward-order on-disk layout is:
--
-- @
-- [vtable] [soffset_t i32] [slot 0] [slot 1] ... [slot N-1] [tail padding]
-- @
--
-- Slots are laid out in schema-declaration order, each aligned to
-- its own alignment. The vtable records the /inline offset/
-- (relative to the soffset_t, i.e. measured from the start of the
-- table body) of every present slot and 0 for absent slots.
--
-- @soffset_t = table_start_addr - vtable_start_addr > 0@, because
-- the vtable sits physically /before/ the table in forward order.
--
-- Build order (back-to-front emission):
--
--   1. tail padding            (lands at end of buffer)
--   2. slots in reverse order  (slot N-1 first, slot 0 last)
--   3. soffset_t
--   4. vtable
--
-- Returns the UOffset of the table start (= position of the
-- soffset_t in the final buffer).
writeTable :: Builder -> [Maybe Field'] -> IO Int
writeTable b slots = do
  let !present = [(i, fs) | (i, Just fs) <- zip [0..] slots]
      !nSlots  = length slots
      -- Forward-direction layout: start at offset 4 (after
      -- soffset_t), lay out present slots in declaration order.
      layout !_pos [] = ([], 0)
      layout !pos ((idx, fs) : rest) =
        let !padPos  = alignUp pos (fsAlign fs)
            !pos'    = padPos + fsAlign fs
            (rs, end) = layout pos' rest
        in ((idx, padPos) : rs, max pos' end)
      (inlineOffs, rawEnd) = layout 4 present
      !maxAlign  = foldr (\(_, fs) m -> max m (fsAlign fs)) 4 present
      !tableSize = alignUp rawEnd maxAlign

  -- Pre-align: the table's soffset_t must land at a forward
  -- position that is a multiple of maxAlign (so every field inside
  -- at its own aligned offset is globally aligned). The soffset_t
  -- itself will occupy the first 4 bytes of the table; everything
  -- past that (the inline area, size tableSize - 4) follows.
  --
  -- Since prepForObject ensures the NEXT @tableSize@ bytes will be
  -- aligned to maxAlign at emission time, and we're about to emit
  -- exactly tableSize bytes (tailPad + slots + soffset) here, this
  -- makes the soffset land aligned.
  prepForObject b tableSize maxAlign

  -- 1. Tail padding (from rawEnd to tableSize).
  prependBS b (BS.replicate (tableSize - rawEnd) 0)

  -- 2. Slots in reverse declaration order, with alignment padding
  --    between successive fields. The "expected end" tracks the
  --    forward byte position that the NEXT-emitted (= next in
  --    reverse-declaration-order, i.e. earlier-declared) field
  --    should END at. Starts at rawEnd (tail pad already emitted).
  -- 
  -- When all slots have been emitted the cursor is at the /inline
  -- offset of the first-declared present slot/ (or rawEnd if no
  -- slots present). The gap between there and the end of the
  -- soffset_t (4) must also be zero-padded.
  let emit !nextExpect [] = prependBS b (BS.replicate (nextExpect - 4) 0)
      emit !expectedEnd ((idx, fs) : rest) = do
        let !off = case lookup idx inlineOffs of
                     Just o  -> o
                     Nothing -> error "Arrow.FlatBufferIPC: internal error (missing inlineOff for present slot)"
            !fieldEnd   = off + fsAlign fs
            !padAfter   = expectedEnd - fieldEnd
        prependBS b (BS.replicate padAfter 0)
        fsWrite fs b 0
        emit off rest
  emit rawEnd (reverse present)

  -- 3. soffset_t. We haven't emitted the vtable yet; we need its
  --    UOffset to compute the value. But writeVTable will advance
  --    bufSize further, and soffset = vtable_uoff - table_uoff (>0).
  --    So we /peek/ the table UOffset now, emit the soffset
  --    placeholder with a dummy, then emit the vtable, then
  --    back-patch... except we can't back-patch chunks easily.
  --
  --    Trick: emit the soffset AFTER the vtable, using a tiny
  --    post-hoc reorder via a scratch write. Simpler: compute
  --    directly — the vtable has a known, deterministic size, so we
  --    know exactly where it will land. We emit the vtable first
  --    (it has higher UOffset = earlier in forward order), /then/
  --    the soffset, /then/ there's a separate rule: but that puts
  --    [vtable][padding?][soffset][fields]. 
  --
  --    Actually that IS the right layout; the ordering I wrote in
  --    the docstring above (vtable before soffset in forward order,
  --    but after in build order) is fine. Build order:
  --       step 1 (tail pad)
  --       step 2 (slots, reverse)
  --       step 3 (soffset)         ← now
  --       step 4 (vtable)          ← after
  --    produces forward layout [vtable][soffset][slots][pad]. ✓

  tableUOff <- do
    curBeforeSoff <- currentUOff b
    let !tableUOff = curBeforeSoff + 4
    -- We need vtable_uoff = tableUOff + soff where soff > 0.
    -- But vtable_uoff is determined AFTER we prepend the soffset
    -- and then emit the vtable. Let the vtable emission tell us
    -- its UOffset afterward; for now we buffer the soff slot and
    -- fill it in via a mutable byte-string. Simpler: emit vtable
    -- FIRST, then compute soff = vtable_uoff - tableUOff, and emit
    -- soffset. This swaps the build order but still yields
    -- [vtable][soffset][slots][pad] in forward order because the
    -- vtable is written /later in back-to-front build/ only if it
    -- ends up at a LOWER UOffset... no, that would place vtable
    -- AFTER soffset in forward order (vtable has smaller UOffset).
    --
    -- Conclusion: we must emit the vtable AFTER the soffset in
    -- build order. That means we emit a soffset placeholder now,
    -- remember its location, emit the vtable, then patch. Since
    -- our builder holds chunks by prepend, we can't random-write
    -- a chunk easily. Workaround: hold the table-body + soffset
    -- state in-memory, determine the vtable's UOffset by simulated
    -- size-only layout, then emit for real.
    --
    -- Actually simplest and absolutely correct: the vtable size is
    -- deterministic from slot layout. Compute vtable size first,
    -- emit vtable immediately, and THEN emit soffset that
    -- references it. Build order:
    --     step 2 (slots, reverse)
    --     step 3 (soffset)         ← now writing
    --     step 4 (vtable emission)
    -- But I realised step 4 happens AFTER step 3, so step 4 ends
    -- up at a LOWER UOffset than step 3 (since prepends stack to
    -- the front). Lower UOffset = EARLIER in forward layout.
    -- → Forward layout: [vtable][soffset][slots][pad]. ✓
    --
    -- soffset value = (vtable_uoff_after_step_4) - tableUOff.
    -- Since vtable_uoff > tableUOff (vtable emitted later in back-
    -- to-front = higher UOffset), soff > 0. ✓
    --
    -- We need vtable's UOffset BEFORE emitting the soffset, because
    -- the soffset encodes it. But we haven't emitted vtable yet.
    -- Resolution: pre-compute the vtable's UOffset analytically.
    --
    --   After step 3, bufSize = curBeforeSoff + 4 = tableUOff.
    --   Then step 4 emits vtable bytes (size `vtBytes`).
    --   After step 4, bufSize = tableUOff + vtBytes, so vtable_uoff
    --   = tableUOff + vtBytes. Hence soff = vtBytes.
    --
    -- !! But vtable dedup may mean we DON'T emit a fresh vtable,
    -- we reuse an existing one at vtUOff. Then soff = vtUOff -
    -- tableUOff, which may be much larger than vtBytes. Handle
    -- that branch specially.
    let (vtKey, vtBytesCount, vtBytes) = makeVTableBytes inlineOffs tableSize nSlots
    dedup <- readIORef (bufVTables b)
    case Map.lookup vtKey dedup of
      Just existingUOff -> do
        prependI32 b (fromIntegral (existingUOff - tableUOff))
        pure tableUOff
      Nothing -> do
        -- soff = vtBytesCount (because step 4 will add that many bytes).
        prependI32 b (fromIntegral vtBytesCount)
        prependBS b vtBytes
        -- Record the vtable's new UOffset for dedup.
        newU <- currentUOff b
        modifyIORef' (bufVTables b) (Map.insert vtKey newU)
        pure tableUOff
  pure tableUOff

-- | Serialise a vtable as raw bytes + its dedup key. The vtable
-- layout is:
--
-- @
-- [vtable_size : u16] [table_size : u16] [slot 0 inline offset : u16] ...
-- @
--
-- Trailing zero slots may be dropped for compactness (readers
-- interpret missing slots as absent, same as explicit zero).
makeVTableBytes
  :: [(Int, Int)]   -- (slotIdx, inlineOffset) present
  -> Int            -- tableSize
  -> Int            -- nSlots
  -> (VTableKey, Int, BS.ByteString)
makeVTableBytes present tableSize nSlots =
  let slotMap = Map.fromList present
      slots   = [Map.findWithDefault 0 i slotMap | i <- [0 .. nSlots - 1]]
      trimmed = reverse (dropWhile (== 0) (reverse slots))
      !nT     = length trimmed
      !vtSize = 2 + 2 + 2 * nT
      !bytes  = BSI.unsafeCreate vtSize $ \p -> do
        -- vtable_size
        pokeByteOff p 0 (fromIntegral vtSize       :: Word8)
        pokeByteOff p 1 (fromIntegral (vtSize `div` 0x100) :: Word8)
        -- table_size
        pokeByteOff p 2 (fromIntegral tableSize    :: Word8)
        pokeByteOff p 3 (fromIntegral (tableSize `div` 0x100) :: Word8)
        let writeSlot !i (s:ss) = do
              pokeByteOff p (4 + 2*i)     (fromIntegral s :: Word8)
              pokeByteOff p (4 + 2*i + 1) (fromIntegral (s `div` 0x100) :: Word8)
              writeSlot (i+1) ss
            writeSlot !_ [] = pure ()
        writeSlot 0 trimmed
      !key = VTableKey (vtSize, tableSize, trimmed)
  in (key, vtSize, bytes)

alignUp :: Int -> Int -> Int
alignUp n a = (n + a - 1) .&. complement (a - 1)

-- ============================================================
-- Strings, vectors, structs
-- ============================================================

-- | Emit a UTF-8 string: length (u32) + bytes + NUL terminator.
-- Returns the UOffset where the /length/ field begins. The string
-- object is 4-aligned.
writeString :: Builder -> T.Text -> IO Int
writeString b txt = do
  let !bytes = TE.encodeUtf8 txt
      !n     = BS.length bytes
  -- Align to 4 before the length prefix, i.e. prepare 4 bytes with
  -- alignment 4. Then prepend the null-terminated payload, then the
  -- u32 length. Because we're writing back-to-front we emit in
  -- reverse order: payload+nul first (pad so it lands at uoff=n+1
  -- post-write), then length (alignment 4).
  prepForObject b (4 + n + 1) 4
  prependBS b (BS.snoc bytes 0)
  prependU32 b (fromIntegral n)
  currentUOff b

-- | Emit a vector of UOffsets to previously-laid-out objects.
writeVectorOfOffsets :: Builder -> [Int] -> IO Int
writeVectorOfOffsets b targetUOffs = do
  let !n = length targetUOffs
  prepForObject b (4 + 4 * n) 4
  mapM_ (\t -> do
             cur <- currentUOff b
             prependU32 b (fromIntegral (cur + 4 - t))
        ) (reverse targetUOffs)
  prependU32 b (fromIntegral n)
  currentUOff b

-- | Emit a vector of fixed-size inline structs.
writeVectorOfStructs
  :: Builder
  -> Int                         -- ^ per-struct size in bytes
  -> Int                         -- ^ struct alignment
  -> [Builder -> IO ()]
  -> IO Int
writeVectorOfStructs b elemSize elemAlign writers = do
  let !n = length writers
      !totalBytes = 4 + elemSize * n
      !align = max 4 elemAlign
  prepForObject b totalBytes align
  mapM_ (\w -> w b) (reverse writers)
  prependU32 b (fromIntegral n)
  currentUOff b

-- | Emit a vector of signed int32 scalars.
writeVectorInt32 :: Builder -> [Int32] -> IO Int
writeVectorInt32 b xs = do
  let !n = length xs
  prepForObject b (4 + 4 * n) 4
  mapM_ (prependI32 b) (reverse xs)
  prependU32 b (fromIntegral n)
  currentUOff b

-- ============================================================
-- Arrow-specific: Type tables
-- ============================================================

-- | Returns (union_tag, UOffset of the type table).
writeType :: Builder -> ArrowType -> IO (Word8, Int)
writeType b ty = case ty of
  ANull             -> emptyT 1
  AInt bits signed  -> do
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral bits)))
           , if signed then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
           ]
    pure (2, u)
  AFloatingPoint p  -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (precisionTag p)))) ]
    pure (3, u)
  ABinary           -> emptyT 4
  AUtf8             -> emptyT 5
  ABool             -> emptyT 6
  ADecimal p s      -> do
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral p)))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral s)))
           , Nothing   -- bitWidth default 128
           ]
    pure (7, u)
  ADecimal256 p s   -> do
    u <- writeTable b
           [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral p)))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral s)))
           , Just (scalar 4 (\bb -> prependI32 bb 256))
           ]
    pure (7, u)
  ADate u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (dateUnitTag u')))) ]
    pure (8, u)
  ATime u' bits -> do
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u'))))
           , Just (scalar 4 (\bb -> prependI32 bb (fromIntegral bits)))
           ]
    pure (9, u)
  ATimestamp u' tz -> do
    tzOff <- case tz of
      Nothing -> pure Nothing
      Just t  -> Just <$> writeString b t
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u'))))
           , case tzOff of
               Nothing  -> Nothing
               Just uo  -> Just (voff uo)
           ]
    pure (10, u)
  AInterval u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (intervalUnitTag u')))) ]
    pure (11, u)
  AList  -> emptyT 12
  AStruct -> emptyT 13
  AUnion mode typeIds -> do
    idsOff <- if V.null typeIds
                then pure Nothing
                else Just <$> writeVectorInt32 b (V.toList typeIds)
    u <- writeTable b
           [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (unionModeTag mode))))
           , case idsOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
           ]
    pure (14, u)
  AFixedSizeBinary n -> do
    u <- writeTable b [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral n))) ]
    pure (15, u)
  AFixedSizeList n -> do
    u <- writeTable b [ Just (scalar 4 (\bb -> prependI32 bb (fromIntegral n))) ]
    pure (16, u)
  AMap sorted -> do
    u <- writeTable b
           [ if sorted then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing ]
    pure (17, u)
  ADuration u' -> do
    u <- writeTable b [ Just (scalar 2 (\bb -> prependI16 bb (fromIntegral (timeUnitTag u')))) ]
    pure (18, u)
  ALargeBinary -> emptyT 19
  ALargeUtf8   -> emptyT 20
  ALargeList   -> emptyT 21
  where
    emptyT !tag = do
      u <- writeTable b []
      pure (tag, u)

precisionTag :: Precision -> Int
precisionTag Half            = 0
precisionTag Single          = 1
precisionTag DoublePrecision = 2

dateUnitTag :: DateUnit -> Int
dateUnitTag DateDay         = 0
dateUnitTag DateMillisecond = 1

timeUnitTag :: TimeUnit -> Int
timeUnitTag Second      = 0
timeUnitTag Millisecond = 1
timeUnitTag Microsecond = 2
timeUnitTag Nanosecond  = 3

intervalUnitTag :: IntervalUnit -> Int
intervalUnitTag YearMonth    = 0
intervalUnitTag DayTime      = 1
intervalUnitTag MonthDayNano = 2

unionModeTag :: UnionMode -> Int
unionModeTag Sparse = 0
unionModeTag Dense  = 1

-- ============================================================
-- Field + Schema tables
-- ============================================================

-- | @
-- table Field {
--   name            : string;           // 0
--   nullable        : bool;             // 1
--   type_type       : ubyte;            // 2
--   type            : Type;             // 3
--   dictionary      : DictionaryEncoding; // 4
--   children        : [Field];          // 5
--   custom_metadata : [KeyValue];       // 6
-- }
-- @
writeField :: Builder -> Field -> IO Int
writeField b fld = do
  -- Write children first (we build back-to-front; sub-objects must
  -- be laid out before references to them).
  childrenVec <- if V.null (fieldChildren fld)
                   then pure Nothing
                   else do
                     childUOffs <- mapM (writeField b) (V.toList (fieldChildren fld))
                     Just <$> writeVectorOfOffsets b childUOffs
  (tyTag, tyUOff) <- writeType b (fieldType fld)
  nameOff <- if T.null (fieldName fld)
               then pure Nothing
               else Just <$> writeString b (fieldName fld)
  writeTable b
    [ case nameOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , if fieldNullable fld then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    , Just (scalar 1 (\bb -> prependU8 bb tyTag))
    , Just (voff tyUOff)
    , Nothing   -- dictionary
    , case childrenVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , Nothing   -- custom_metadata
    ]

-- | @
-- table Schema {
--   endianness     : Endianness = Little;
--   fields         : [Field];
--   custom_metadata: [KeyValue];
--   features       : [long];
-- }
-- @
writeSchema :: Builder -> Schema -> IO Int
writeSchema b sch = do
  fieldUOffs <- mapM (writeField b) (V.toList (arrowFields sch))
  fieldsVec  <- writeVectorOfOffsets b fieldUOffs
  writeTable b
    [ case arrowEndianness sch of
        Little -> Nothing
        Big    -> Just (scalar 2 (\bb -> prependI16 bb 1))
    , Just (voff fieldsVec)
    , Nothing
    , Nothing
    ]

-- ============================================================
-- RecordBatch
-- ============================================================

writeFieldNodeStruct :: FieldNode -> Builder -> IO ()
writeFieldNodeStruct fn b = do
  -- Struct layout: length (i64), null_count (i64). We're writing
  -- back-to-front so write null_count first, then length.
  prependI64 b (fnNullCount fn)
  prependI64 b (fnLength fn)

writeBufferStruct :: Buffer -> Builder -> IO ()
writeBufferStruct bf b = do
  prependI64 b (bufLength bf)
  prependI64 b (bufOffset bf)

writeRecordBatch :: Builder -> RecordBatchDef -> IO Int
writeRecordBatch b rb = do
  buffersVec <- writeVectorOfStructs b 16 8
                  [ writeBufferStruct buf | buf <- V.toList (rbBuffers rb) ]
  nodesVec   <- writeVectorOfStructs b 16 8
                  [ writeFieldNodeStruct fn | fn <- V.toList (rbNodes rb) ]
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb (rbLength rb)))
    , Just (voff nodesVec)
    , Just (voff buffersVec)
    , Nothing
    , Nothing
    ]

-- ============================================================
-- Message envelope
-- ============================================================

-- | @
-- table Message {
--   version        : MetadataVersion;  // short, V5 = 4
--   header_type    : MessageHeader;    // ubyte (1=Schema, 3=RecordBatch)
--   header         : MessageHeader;    // union payload
--   bodyLength     : long;
--   custom_metadata: [KeyValue];
-- }
-- @
--
-- File-identifier is /not/ emitted for Arrow Messages; the
-- encapsulating stream framing distinguishes message boundaries.
buildSchemaMessage :: Schema -> ByteString
buildSchemaMessage sch = unsafePerformIO $ do
  b <- newBuilder
  schUOff <- writeSchema b sch
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 1))   -- header type: Schema
    , Just (voff schUOff)
    , Just (scalar 8 (\bb -> prependI64 bb 0))  -- bodyLength
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildSchemaMessage #-}

buildRecordBatchMessage :: RecordBatchDef -> Int64 -> ByteString
buildRecordBatchMessage rb bodyLen = unsafePerformIO $ do
  b <- newBuilder
  rbUOff <- writeRecordBatch b rb
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 3))   -- header type: RecordBatch
    , Just (voff rbUOff)
    , Just (scalar 8 (\bb -> prependI64 bb bodyLen))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildRecordBatchMessage #-}

metadataVersionV5 :: Int16
metadataVersionV5 = 4

-- ============================================================
-- Encapsulated framing + stream / file writers
-- ============================================================

-- | Wrap a raw flatbuffer @Message@ in the encapsulated IPC frame:
--
-- @
-- <continuation 0xFFFFFFFF : u32-LE>
-- <metadata_length : i32-LE, padded so body starts aligned to 8>
-- <flatbuffer bytes, padded>
-- <body bytes, padded to 8-byte alignment>
-- @
encapsulateMessage :: ByteString -> ByteString -> ByteString
encapsulateMessage meta body =
  let !metaLen  = BS.length meta
      !padded   = alignUp8 (8 + metaLen) - 8 -- meta+8-byte-prefix must end on 8B
      !metaPad  = padded - metaLen
      !bodyLen  = BS.length body
      !bodyPad  = alignUp8 bodyLen - bodyLen
  in BL.toStrict $ B.toLazyByteString $
        B.word32LE 0xFFFFFFFF
        <> B.int32LE (fromIntegral padded)
        <> B.byteString meta
        <> B.byteString (BS.replicate metaPad 0)
        <> B.byteString body
        <> B.byteString (BS.replicate bodyPad 0)
  where
    alignUp8 n = (n + 7) .&. complement 7

-- | Emit a complete Arrow IPC stream (schema + batches + EOS).
writeArrowStreamFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowStreamFB sch batches =
  let !schemaMsg = encapsulateMessage (buildSchemaMessage sch) BS.empty
      !batchBytes = BS.concat
        [ encapsulateMessage
            (buildRecordBatchMessage rb (fromIntegral (BS.length body)))
            body
        | (rb, body) <- batches
        ]
      !eos = BL.toStrict $ B.toLazyByteString $
               B.word32LE 0xFFFFFFFF <> B.int32LE 0
  in schemaMsg <> batchBytes <> eos

-- | Build @(RecordBatchDef, body bytes)@ from a 'Schema' + columns.
-- Delegates to 'Arrow.Write.encodeColumns' for the physical body
-- layout (validity bitmaps, typed buffers, 8-byte aligned), then
-- normalises the 'Buffer' list so every field has the Arrow-spec
-- buffer count — which for non-nullable primitives means
-- prepending a zero-length validity buffer (the simplified internal
-- encoder in "Arrow.Write" omits the slot for non-nullable fields
-- because its own reader does too; the spec requires the slot to
-- exist even when empty).
buildRecordBatchBytes
  :: Schema
  -> V.Vector ColumnArray
  -> (RecordBatchDef, ByteString)
buildRecordBatchBytes sch cols =
  let !acc = W.encodeColumns (arrowFields sch) cols W.emptyBuildAcc
      !rawNodes = V.fromList (reverse (W.baNodes acc))
      !rawBufs  = V.fromList (reverse (W.baBufs acc))
      !(!nodes, !bufs) = normaliseBuffers (arrowFields sch) rawNodes rawBufs
      !numRows = if V.null cols then 0 else columnLength (V.head cols)
      !rb = RecordBatchDef
              { rbLength  = fromIntegral numRows
              , rbNodes   = nodes
              , rbBuffers = bufs
              }
      !body = BL.toStrict (B.toLazyByteString (W.baBody acc))
  in (rb, body)

-- | Walk the schema and insert an empty 'Buffer' (offset=0, len=0)
-- in the validity slot of every non-nullable primitive field, so the
-- resulting 'RecordBatchDef' conforms to the Arrow spec's
-- @n_buffers_per_layout@ rule. Leaves nullable and variable-length
-- columns alone — those already emit their validity slot.
normaliseBuffers
  :: V.Vector Field
  -> V.Vector FieldNode
  -> V.Vector Buffer
  -> (V.Vector FieldNode, V.Vector Buffer)
normaliseBuffers fields nodes bufs =
  let !(_nIdx', _bIdx', outNodes, outBufs) =
        V.foldl' step (0 :: Int, 0 :: Int, [], []) fields
      step (!nIdx, !bIdx, !nAcc, !bAcc) f =
        let (!nConsumed, !bConsumed, !emittedBufs) =
              normaliseField f nodes bufs nIdx bIdx
        in  ( nIdx + nConsumed
            , bIdx + bConsumed
            , [ nodes V.! i | i <- [nIdx .. nIdx + nConsumed - 1] ] ++ nAcc
            , reverse emittedBufs ++ bAcc
            )
  in  (V.fromList (reverse outNodes), V.fromList (reverse outBufs))

-- | For a single top-level field, return @(#fieldNodes consumed,
-- #source buffers consumed, output buffers in emission order)@.
-- The output buffer list is what the RecordBatch should advertise
-- after normalisation.
normaliseField
  :: Field
  -> V.Vector FieldNode
  -> V.Vector Buffer
  -> Int
  -> Int
  -> (Int, Int, [Buffer])
normaliseField f _nodes bufs _nIdx bIdx =
  -- For now we handle only flat fields (no children). The
  -- upstream encoder rejects nested children anyway so this stays
  -- consistent. The per-type buffer counts match the
  -- 'buffersPerField' table in "Arrow.Column" but /always/
  -- prepend a validity slot for non-nullable fields.
  let nulls = V.null (fieldChildren f)
      nInputBufs = case fieldType f of
        AInt {}            -> 1
        ABool              -> 1
        AFloatingPoint _   -> 1
        AUtf8              -> 2
        ABinary            -> 2
        ALargeUtf8         -> 2
        ALargeBinary       -> 2
        AFixedSizeBinary _ -> 1
        ADate _            -> 1
        ATime _ _          -> 1
        ATimestamp _ _     -> 1
        ADuration _        -> 1
        ADecimal _ _       -> 1
        ADecimal256 _ _    -> 1
        AInterval _        -> 1
        _                  -> 1  -- conservative fallback
      inputCount  = (if fieldNullable f then 1 else 0) + nInputBufs
      srcBufs     = [ bufs V.! i | i <- [bIdx .. bIdx + inputCount - 1] ]
      outBufs     = if fieldNullable f || not nulls
                      then srcBufs
                      else emptyValidityBuffer : srcBufs
  in  (1, inputCount, outBufs)

-- | Placeholder validity buffer: offset 0, length 0. Readers treat
-- a zero-length validity buffer as "all values valid".
emptyValidityBuffer :: Buffer
emptyValidityBuffer = Buffer 0 0

-- | Convenience: pyarrow-compatible Arrow IPC stream from columnar
-- data. Calls 'buildRecordBatchBytes' per batch and wraps each in
-- the encapsulated framing.
writeArrowStreamFBFromColumns
  :: Schema
  -> V.Vector (V.Vector ColumnArray)
  -> ByteString
writeArrowStreamFBFromColumns sch batches =
  writeArrowStreamFB sch
    (V.toList (V.map (buildRecordBatchBytes sch) batches))

-- | Arrow IPC /file/ format (per @format/File.fbs@):
--
-- @
-- 'ARROW1'\\0\\0
-- <encapsulated schema message>
-- <encapsulated record batch 1>
-- ...
-- <encapsulated record batch N>
-- <Footer flatbuffer>             // table Footer { schema, recordBatches: [Block], ... }
-- <i32 footer length>
-- 'ARROW1'
-- @
--
-- Each 'Block' references one record batch by
-- @(offset, metaDataLength, bodyLength)@ — @offset@ pointing at the
-- continuation marker that begins the encapsulated message. We emit
-- the EOS marker too so the file simultaneously parses as a stream.
writeArrowFileFB :: Schema -> [(RecordBatchDef, ByteString)] -> ByteString
writeArrowFileFB sch batches =
  let !magic       = "ARROW1"
      !magicPad    = BS.pack [0, 0]
      !headerLen   = BS.length magic + BS.length magicPad   -- 8 bytes
      !schemaMsg   = encapsulateMessage (buildSchemaMessage sch) BS.empty
      -- Per-batch encapsulated bytes plus the Block we'll record
      -- in the footer. metaDataLength includes the 8-byte
      -- continuation+length prefix and all metadata padding (per
      -- arrow-cpp's @WriteRecordBatchMessage@ which advertises
      -- the prefix-inclusive length so the reader can seek past
      -- the metadata in one read).
      step (revBlocks, !off, accBytes) (rb, body) =
        let !msgBytes = encapsulateMessage
              (buildRecordBatchMessage rb (fromIntegral (BS.length body)))
              body
            !msgLen   = BS.length msgBytes
            !bodyLen  = BS.length body
            !paddedBody = alignUp8FB bodyLen
            !metaLen  = msgLen - paddedBody  -- 8-byte prefix + padded metadata
            !blk = ArrowBlock
                     { abOffset    = fromIntegral off
                     , abMetaLen   = fromIntegral metaLen
                     , abBodyLen   = fromIntegral paddedBody
                     }
        in  (blk : revBlocks, off + msgLen, accBytes ++ [msgBytes])
      (revBlocks, _eosOff, msgBs) =
        foldl step ([], headerLen + BS.length schemaMsg, []) batches
      !blocks = reverse revBlocks
      !eos = BL.toStrict $ B.toLazyByteString $
               B.word32LE 0xFFFFFFFF <> B.int32LE 0
      !streamBytes = BS.concat (schemaMsg : msgBs ++ [eos])
      !footer = buildFileFooter sch blocks
      -- The footer body must be 8-aligned before the trailing
      -- length+magic, per arrow-cpp.
      !footerPad =
        let raw = BS.length footer
        in  BS.replicate (alignUp8FB raw - raw) 0
      !footerLenLE = BL.toStrict $ B.toLazyByteString $
                       B.int32LE (fromIntegral (BS.length footer))
  in BS.concat
       [ magic, magicPad
       , streamBytes
       , footer, footerPad
       , footerLenLE
       , magic
       ]
  where
    alignUp8FB n = (n + 7) .&. complement (7 :: Int)

-- | A 'Block' struct as emitted in the @Footer.recordBatches@
-- vector. Inline fixed-size struct (24 bytes total): offset i64,
-- metaDataLength i32, bodyLength i64.
data ArrowBlock = ArrowBlock
  { abOffset  :: !Int64
  , abMetaLen :: !Int32
  , abBodyLen :: !Int64
  }

-- | Build the @Footer@ flatbuffer:
--
-- @
-- table Footer {
--   version       : MetadataVersion;   // i16
--   schema        : Schema;            // table uoffset
--   dictionaries  : [Block];           // vector of structs (struct size = 24 with padding)
--   recordBatches : [Block];
--   custom_metadata : [KeyValue];
-- }
-- @
--
-- @Block@ is a struct with layout
-- @offset: i64; metaDataLength: i32; bodyLength: i64;@. The
-- @metaDataLength@ field is 4 bytes wide but the struct is
-- 8-aligned, so each Block occupies 24 bytes (8 + 4 + 4 padding +
-- 8). FlatBuffers structs are compiler-generated, but we hand-roll
-- the same layout here.
buildFileFooter :: Schema -> [ArrowBlock] -> ByteString
buildFileFooter sch blocks = unsafePerformIO $ do
  b <- newBuilder
  schUOff <- writeSchema b sch
  blocksVec <- writeVectorOfStructs b 24 8
                 (map writeBlockStruct blocks)
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (voff schUOff)
    , Nothing                        -- dictionaries (always empty for now)
    , Just (voff blocksVec)
    , Nothing                        -- custom_metadata
    ]
  finish b msgUOff
{-# NOINLINE buildFileFooter #-}

writeBlockStruct :: ArrowBlock -> Builder -> IO ()
writeBlockStruct (ArrowBlock o ml bl) bb = do
  -- Reverse order: bodyLength (i64), 4-byte pad, metaDataLength
  -- (i32), offset (i64).
  prependI64 bb bl
  prependBS  bb (BS.replicate 4 0)
  prependI32 bb ml
  prependI64 bb o

-- ============================================================
-- Reader: pyarrow / arrow-cpp / arrow-rs → wireform
-- ============================================================
--
-- The reader is the symmetric inverse of the writer. It walks the
-- FlatBuffer by chasing 'soffset_t' / 'uoffset_t' fields and never
-- materialises a generic 'Value' representation in between (the
-- writer's vtable layout is shared, so the reader stays small).
--
-- The reader accepts the encapsulated stream framing emitted by the
-- standard Arrow implementations:
--
-- @
--   <continuation 0xFFFFFFFF : u32>
--   <metadata_length : i32>
--   <metadata flatbuffer + padding>
--   <body bytes>          -- bodyLength of them, then padding to 8
--   ... repeats per message ...
--   <continuation 0xFFFFFFFF : u32>
--   <metadata_length 0>   -- EOS marker
-- @
--
-- Older producers (arrow-cpp < 0.15) omit the continuation marker
-- and write a positive @metadata_length@ as the first field; both
-- shapes are recognised. (See @ConsumeInitial@ in arrow-cpp's
-- @message.cc@ for the canonical decoder logic.)

-- | A position in the metadata flatbuffer.
type Pos = Int

-- | Read a u16 (LE) at byte position @off@ in @bs@.
peekU16 :: ByteString -> Pos -> Either String Word16
peekU16 bs off
  | off + 2 > BS.length bs = Left "Arrow.FlatBufferIPC: peekU16 out of range"
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)       :: Word16
          !b1 = fromIntegral (BS.index bs (off + 1)) :: Word16
      in  Right (b0 .|. (b1 `shiftL` 8))

peekU32 :: ByteString -> Pos -> Either String Word32
peekU32 bs off
  | off + 4 > BS.length bs = Left "Arrow.FlatBufferIPC: peekU32 out of range"
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)       :: Word32
          !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
          !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
          !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
      in  Right (b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24))

peekI32 :: ByteString -> Pos -> Either String Int32
peekI32 bs off = fromIntegral <$> peekU32 bs off

peekI16 :: ByteString -> Pos -> Either String Int16
peekI16 bs off = fromIntegral <$> peekU16 bs off

peekI64 :: ByteString -> Pos -> Either String Int64
peekI64 bs off = do
  lo <- peekU32 bs off
  hi <- peekU32 bs (off + 4)
  Right $! fromIntegral $!
    (fromIntegral hi `shiftL` 32 :: Int64) + fromIntegral lo

peekU8 :: ByteString -> Pos -> Either String Word8
peekU8 bs off
  | off >= BS.length bs = Left "Arrow.FlatBufferIPC: peekU8 out of range"
  | otherwise = Right (BS.index bs off)

-- | Resolve a table at @tablePos@ and return @(vtablePos, tableSize)@
-- plus a function that maps slot index to its absolute byte offset
-- (or 'Nothing' if the slot is absent / beyond vtable).
resolveTable :: ByteString -> Pos -> Either String (Int -> Maybe Pos)
resolveTable bs tablePos = do
  soff <- peekI32 bs tablePos
  let vtablePos = tablePos - fromIntegral soff
  when' (vtablePos < 0 || vtablePos >= BS.length bs) $
    Left "Arrow.FlatBufferIPC: vtable out of bounds"
  vtSize <- peekU16 bs vtablePos
  let nSlots = (fromIntegral vtSize - 4) `div` 2 :: Int
  Right $ \i ->
    if i >= nSlots
      then Nothing
      else case peekU16 bs (vtablePos + 4 + 2 * i) of
        Left _    -> Nothing
        Right 0   -> Nothing
        Right off -> Just (tablePos + fromIntegral off)

when' :: Bool -> Either String () -> Either String ()
when' True  e = e
when' False _ = Right ()

-- | Follow a u32 uoffset at @fieldPos@ to the absolute byte
-- position of the referenced object.
followUOffset :: ByteString -> Pos -> Either String Pos
followUOffset bs fieldPos = do
  rel <- peekU32 bs fieldPos
  Right (fieldPos + fromIntegral rel)

-- | Decode a UTF-8 string at the given uoffset target position.
readString :: ByteString -> Pos -> Either String T.Text
readString bs strPos = do
  len <- peekU32 bs strPos
  let n  = fromIntegral len :: Int
      !b = BS.take n (BS.drop (strPos + 4) bs)
  case TE.decodeUtf8' b of
    Left _  -> Left "Arrow.FlatBufferIPC: invalid UTF-8 in string"
    Right t -> Right t

-- | Decode a vector of @uoffset_t@ table references.
readVectorOfOffsets :: ByteString -> Pos -> Either String (V.Vector Pos)
readVectorOfOffsets bs vecPos = do
  n <- peekU32 bs vecPos
  let elemPositions = V.generate (fromIntegral n) $ \i ->
        let !ePos = vecPos + 4 + 4 * i
        in  case peekU32 bs ePos of
              Left _    -> ePos
              Right rel -> ePos + fromIntegral rel
  Right elemPositions

-- | Decode a vector of fixed-size inline structs. Returns the
-- byte position of each element start.
readVectorOfStructs
  :: ByteString
  -> Pos
  -> Int   -- ^ stride (per-struct size)
  -> Either String (Int, V.Vector Pos)
readVectorOfStructs bs vecPos stride = do
  n <- peekU32 bs vecPos
  let elems = V.generate (fromIntegral n) (\i -> vecPos + 4 + i * stride)
  Right (fromIntegral n, elems)

-- | Decode a complete @Schema@ table at the given position.
readSchemaTable :: ByteString -> Pos -> Either String Schema
readSchemaTable bs schPos = do
  slot <- resolveTable bs schPos
  endian <- case slot 0 of
    Nothing -> Right Little
    Just p  -> do
      v <- peekI16 bs p
      case v of
        0 -> Right Little
        1 -> Right Big
        _ -> Left ("Arrow.FlatBufferIPC: unknown endianness " ++ show v)
  fieldsVec <- case slot 1 of
    Nothing -> Right V.empty
    Just p  -> do
      vecPos <- followUOffset bs p
      readVectorOfOffsets bs vecPos
  fields <- V.mapM (readField bs) fieldsVec
  Right Schema { arrowFields = fields, arrowEndianness = endian }

-- | Decode one @Field@ table.
readField :: ByteString -> Pos -> Either String Field
readField bs fldPos = do
  slot <- resolveTable bs fldPos
  name <- case slot 0 of
    Nothing -> Right ""
    Just p  -> do
      strPos <- followUOffset bs p
      readString bs strPos
  nullable <- case slot 1 of
    Nothing -> Right False
    Just p  -> do
      v <- peekU8 bs p
      Right (v /= 0)
  tyTag <- case slot 2 of
    Nothing -> Right 0
    Just p  -> peekU8 bs p
  ty <- case slot 3 of
    Nothing  -> readType bs (fromIntegral tyTag) Nothing
    Just p   -> do
      tyPos <- followUOffset bs p
      readType bs (fromIntegral tyTag) (Just tyPos)
  children <- case slot 5 of
    Nothing -> Right V.empty
    Just p  -> do
      vecPos <- followUOffset bs p
      childPositions <- readVectorOfOffsets bs vecPos
      V.mapM (readField bs) childPositions
  Right Field
    { fieldName     = name
    , fieldNullable = nullable
    , fieldType     = ty
    , fieldChildren = children
    }

-- | Decode a @Type@ union variant. The discriminator (@type_type@)
-- selects which sub-table layout to read at @typePos@.
readType :: ByteString -> Int -> Maybe Pos -> Either String ArrowType
readType _  0 _ = Right ANull   -- "None" / Null
readType _  1 _ = Right ANull
readType bs 2 (Just p) = do
  -- Int { bitWidth: i32, is_signed: bool }
  s <- resolveTable bs p
  bits <- case s 0 of
    Nothing -> Right 32
    Just b  -> peekI32 bs b
  signed <- case s 1 of
    Nothing -> Right True
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (AInt (fromIntegral bits) signed)
readType bs 3 (Just p) = do
  -- FloatingPoint { precision: i16 }
  s <- resolveTable bs p
  prec <- case s 0 of
    Nothing -> Right 1
    Just b  -> peekI16 bs b
  case prec of
    0 -> Right (AFloatingPoint Half)
    1 -> Right (AFloatingPoint Single)
    2 -> Right (AFloatingPoint DoublePrecision)
    n -> Left $ "Arrow.FlatBufferIPC: unknown precision " ++ show n
readType _  4 _ = Right ABinary
readType _  5 _ = Right AUtf8
readType _  6 _ = Right ABool
readType bs 7 (Just p) = do
  s <- resolveTable bs p
  prec  <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  scale <- case s 1 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  bw    <- case s 2 of { Nothing -> Right 128; Just b -> peekI32 bs b }
  case bw of
    128 -> Right (ADecimal (fromIntegral prec) (fromIntegral scale))
    256 -> Right (ADecimal256 (fromIntegral prec) (fromIntegral scale))
    n   -> Left $ "Arrow.FlatBufferIPC: unsupported decimal bitWidth " ++ show n
readType bs 8 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 1; Just b -> peekI16 bs b }
  case u of
    0 -> Right (ADate DateDay)
    1 -> Right (ADate DateMillisecond)
    n -> Left $ "Arrow.FlatBufferIPC: unknown date unit " ++ show n
readType bs 9 (Just p) = do
  s <- resolveTable bs p
  u  <- case s 0 of { Nothing -> Right 1;  Just b -> peekI16 bs b }
  bw <- case s 1 of { Nothing -> Right 32; Just b -> peekI32 bs b }
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ATime unit (fromIntegral bw))
readType bs 10 (Just p) = do
  s <- resolveTable bs p
  u  <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  tz <- case s 1 of
    Nothing -> Right Nothing
    Just b  -> do
      strPos <- followUOffset bs b
      Just <$> readString bs strPos
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ATimestamp unit tz)
readType bs 11 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  iu <- case u of
    0 -> Right YearMonth
    1 -> Right DayTime
    2 -> Right MonthDayNano
    n -> Left $ "Arrow.FlatBufferIPC: unknown interval unit " ++ show n
  Right (AInterval iu)
readType _  12 _ = Right AList
readType _  13 _ = Right AStruct
readType bs 14 (Just p) = do
  s <- resolveTable bs p
  m <- case s 0 of { Nothing -> Right 0; Just b -> peekI16 bs b }
  mode <- case m of
    0 -> Right Sparse
    1 -> Right Dense
    n -> Left $ "Arrow.FlatBufferIPC: unknown union mode " ++ show n
  ids <- case s 1 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      n <- peekU32 bs vecPos
      V.generateM (fromIntegral n) (\i ->
        peekI32 bs (vecPos + 4 + 4 * i))
  Right (AUnion mode ids)
readType bs 15 (Just p) = do
  s <- resolveTable bs p
  bw <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  Right (AFixedSizeBinary (fromIntegral bw))
readType bs 16 (Just p) = do
  s <- resolveTable bs p
  ls <- case s 0 of { Nothing -> Right 0; Just b -> peekI32 bs b }
  Right (AFixedSizeList (fromIntegral ls))
readType bs 17 (Just p) = do
  s <- resolveTable bs p
  sorted <- case s 0 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (AMap sorted)
readType bs 18 (Just p) = do
  s <- resolveTable bs p
  u <- case s 0 of { Nothing -> Right 1; Just b -> peekI16 bs b }
  unit <- timeUnitFromTag (fromIntegral u)
  Right (ADuration unit)
readType _  19 _ = Right ALargeBinary
readType _  20 _ = Right ALargeUtf8
readType _  21 _ = Right ALargeList
readType _  n  _ = Left $ "Arrow.FlatBufferIPC: unsupported Type discriminator " ++ show n

timeUnitFromTag :: Int -> Either String TimeUnit
timeUnitFromTag 0 = Right Second
timeUnitFromTag 1 = Right Millisecond
timeUnitFromTag 2 = Right Microsecond
timeUnitFromTag 3 = Right Nanosecond
timeUnitFromTag n = Left $ "Arrow.FlatBufferIPC: unknown time unit " ++ show n

-- | Decode a @RecordBatch@ table.
readRecordBatchTable :: ByteString -> Pos -> Either String RecordBatchDef
readRecordBatchTable bs rbPos = do
  s <- resolveTable bs rbPos
  len <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI64 bs b
  nodes <- case s 1 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      (_, elems) <- readVectorOfStructs bs vecPos 16
      V.mapM (\ep -> do
                l <- peekI64 bs ep
                nc <- peekI64 bs (ep + 8)
                Right (FieldNode l nc)) elems
  bufs <- case s 2 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      (_, elems) <- readVectorOfStructs bs vecPos 16
      V.mapM (\ep -> do
                o <- peekI64 bs ep
                l <- peekI64 bs (ep + 8)
                Right (Buffer o l)) elems
  Right RecordBatchDef
    { rbLength  = len
    , rbNodes   = nodes
    , rbBuffers = bufs
    }

-- | Decode a Schema-typed @Message@ flatbuffer (just the metadata
-- bytes; the caller has already stripped the encapsulated framing).
decodeSchemaMessage :: ByteString -> Either String Schema
decodeSchemaMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 1) $
    Left ("Arrow.FlatBufferIPC: expected Schema header (1), got " ++ show ht)
  case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (Schema) missing"
    Just b  -> do
      schPos <- followUOffset meta b
      readSchemaTable meta schPos

-- | Decode a RecordBatch-typed @Message@ flatbuffer to
-- @(RecordBatchDef, bodyLength)@.
decodeRecordBatchMessage :: ByteString -> Either String (RecordBatchDef, Int64)
decodeRecordBatchMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 3) $
    Left ("Arrow.FlatBufferIPC: expected RecordBatch header (3), got " ++ show ht)
  rb <- case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (RecordBatch) missing"
    Just b  -> do
      rbPos <- followUOffset meta b
      readRecordBatchTable meta rbPos
  bodyLen <- case s 3 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  Right (rb, bodyLen)

-- | Parse an Arrow IPC stream produced by any spec-compliant
-- writer (pyarrow / arrow-cpp / arrow-rs) into wireform's
-- 'Schema' + a list of @(RecordBatchDef, body bytes)@ pairs.
--
-- Recognises both the post-0.15.0 framing (continuation marker +
-- length) and the legacy framing (positive length first, no
-- continuation), per the @ConsumeInitial@ logic in arrow-cpp's
-- @message.cc@.
readArrowStreamFB
  :: ByteString
  -> Either String (Schema, [(RecordBatchDef, ByteString)])
readArrowStreamFB bs0 = do
  (schema, after) <- consumeOne bs0 readSchemaFrame
  consumeBatches schema after []
  where
    -- Strip one frame and apply @decodeFrame@ to the metadata bytes.
    consumeOne bs decodeFrame = do
      (mlen, meta, rest) <- readFrameHeader bs
      when' (mlen <= 0) $
        Left "Arrow.FlatBufferIPC: unexpected EOS while reading schema"
      decoded <- decodeFrame meta
      Right (decoded, rest)

    readSchemaFrame = decodeSchemaMessage

    consumeBatches sch bs acc
      | BS.length bs < 4 = Right (sch, reverse acc)
      | otherwise = do
          (mlen, meta, rest1) <- readFrameHeader bs
          if mlen == 0
            then Right (sch, reverse acc)
            else do
              (rb, bodyLen) <- decodeRecordBatchMessage meta
              let !nBody    = fromIntegral bodyLen :: Int
                  !nBodyPad = alignUp8FB nBody
                  body      = BS.take nBody rest1
                  rest2     = BS.drop nBodyPad rest1
              consumeBatches sch rest2 ((rb, body) : acc)

    alignUp8FB n = (n + 7) .&. complement (7 :: Int)

-- | Parse an Arrow IPC /file/ (per @format/File.fbs@), accepting
-- either the legacy stream-shaped output of 'writeArrowFileFB' or
-- the canonical pyarrow / arrow-cpp file with a trailing 'Footer'.
-- The strategy: skip the 8-byte @ARROW1\\0\\0@ header and parse the
-- contents as a stream. The trailing @Footer + length + ARROW1@
-- comes after the EOS marker so 'readArrowStreamFB' stops there.
readArrowFileFB
  :: ByteString
  -> Either String (Schema, [(RecordBatchDef, ByteString)])
readArrowFileFB bs = do
  when' (BS.length bs < 14) $   -- minimum: header + EOS + trailer
    Left "Arrow.FlatBufferIPC: input too small to be an Arrow file"
  when' (BS.take 6 bs /= "ARROW1") $
    Left "Arrow.FlatBufferIPC: missing leading ARROW1 magic"
  when' (BS.takeEnd 6 bs /= "ARROW1") $
    Left "Arrow.FlatBufferIPC: missing trailing ARROW1 magic"
  readArrowStreamFB (BS.drop 8 bs)

-- | Strip one encapsulated-message frame:
--
--   * 4 bytes continuation (0xFFFFFFFF) — optional in legacy mode
--   * 4 bytes metadata_length (i32 LE)
--   * @metadata_length@ metadata bytes (already padded)
--
-- Returns @(mlen, metadata bytes, rest of stream after metadata)@.
-- @mlen == 0@ signals the EOS marker.
readFrameHeader
  :: ByteString
  -> Either String (Int, ByteString, ByteString)
readFrameHeader bs = do
  when' (BS.length bs < 4) $
    Left "Arrow.FlatBufferIPC: truncated frame header"
  first4 <- peekU32 bs 0
  if first4 == 0xFFFFFFFF
    then do
      when' (BS.length bs < 8) $
        Left "Arrow.FlatBufferIPC: truncated frame after continuation"
      mlen <- peekI32 bs 4
      let !mlenI = fromIntegral mlen :: Int
      when' (mlenI < 0) $
        Left "Arrow.FlatBufferIPC: negative metadata length"
      Right ( mlenI
            , BS.take mlenI (BS.drop 8 bs)
            , BS.drop (8 + mlenI) bs
            )
    else
      -- Legacy: first 4 bytes are the metadata length itself.
      if first4 == 0
        then Right (0, BS.empty, BS.drop 4 bs)
        else do
          let !mlenI = fromIntegral first4 :: Int
          Right ( mlenI
                , BS.take mlenI (BS.drop 4 bs)
                , BS.drop (4 + mlenI) bs
                )
