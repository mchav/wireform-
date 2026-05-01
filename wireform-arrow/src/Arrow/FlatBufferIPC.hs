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
  , decodeDictionaryBatchMessage
  , denormaliseBuffers
  , materializeRecordBatchFB
    -- * Dictionary support
  , DictBatch (..)
  , readArrowStreamFBWithDicts
  , buildDictionaryBatchMessage
  , writeArrowStreamFBWithDicts
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

import Arrow.Column (ColumnArray (..), columnLength, materializeRecordBatch)
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
  ALargeBinary    -> emptyT 19
  ALargeUtf8      -> emptyT 20
  ALargeList      -> emptyT 21
  ARunEndEncoded  -> emptyT 22
  ABinaryView     -> emptyT 23
  AUtf8View       -> emptyT 24
  AListView       -> emptyT 25
  ALargeListView  -> emptyT 26
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
  childrenVec <- if V.null (fieldChildren fld)
                   then pure Nothing
                   else do
                     childUOffs <- mapM (writeField b) (V.toList (fieldChildren fld))
                     Just <$> writeVectorOfOffsets b childUOffs
  (tyTag, tyUOff) <- writeType b (fieldType fld)
  dictOff <- case fieldDictionary fld of
    Nothing -> pure Nothing
    Just de -> Just <$> writeDictionaryEncoding b de
  nameOff <- if T.null (fieldName fld)
               then pure Nothing
               else Just <$> writeString b (fieldName fld)
  writeTable b
    [ case nameOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , if fieldNullable fld then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    , Just (scalar 1 (\bb -> prependU8 bb tyTag))
    , Just (voff tyUOff)
    , case dictOff of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , case childrenVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    , Nothing   -- custom_metadata
    ]

-- | Build a 'DictionaryEncoding' table:
--
-- @
-- table DictionaryEncoding {
--   id: long;
--   indexType: Int;
--   isOrdered: bool;
--   dictionaryKind: DictionaryKind;
-- }
-- @
writeDictionaryEncoding :: Builder -> DictionaryEncoding -> IO Int
writeDictionaryEncoding b (DictionaryEncoding did indexTy ordered) = do
  -- The indexType is always an Int table; build via 'writeType' to
  -- reuse the layout, but we must always emit the table even if
  -- indexTy is the default Int32-signed.
  (_, intUOff) <- writeType b indexTy
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb did))
    , Just (voff intUOff)
    , if ordered then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    -- dictionaryKind defaults to DenseArray (0); omit.
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
  variadicVec <- if V.null (rbVariadicBufferCounts rb)
    then pure Nothing
    else do
      uo <- writeVectorInt64 b (V.toList (rbVariadicBufferCounts rb))
      pure (Just uo)
  buffersVec <- writeVectorOfStructs b 16 8
                  [ writeBufferStruct buf | buf <- V.toList (rbBuffers rb) ]
  nodesVec   <- writeVectorOfStructs b 16 8
                  [ writeFieldNodeStruct fn | fn <- V.toList (rbNodes rb) ]
  writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb (rbLength rb)))
    , Just (voff nodesVec)
    , Just (voff buffersVec)
    , Nothing
    , case variadicVec of { Nothing -> Nothing; Just uo -> Just (voff uo) }
    ]

-- | Vector of @int64@ scalars (used by @variadicBufferCounts@).
writeVectorInt64 :: Builder -> [Int64] -> IO Int
writeVectorInt64 b xs = do
  let !n = length xs
  prepForObject b (4 + 8 * n) 8
  mapM_ (prependI64 b) (reverse xs)
  prependU32 b (fromIntegral n)
  currentUOff b

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

-- | Build a @Message@ flatbuffer wrapping a @DictionaryBatch@:
--
-- @
-- table DictionaryBatch {
--   id      : long;          // 0
--   data    : RecordBatch;   // 1
--   isDelta : bool = false;  // 2
-- }
-- @
buildDictionaryBatchMessage :: DictBatch -> ByteString
buildDictionaryBatchMessage (DictBatch did isDelta rb body) = unsafePerformIO $ do
  b <- newBuilder
  rbUOff  <- writeRecordBatch b rb
  dbUOff  <- writeTable b
    [ Just (scalar 8 (\bb -> prependI64 bb did))
    , Just (voff rbUOff)
    , if isDelta then Just (scalar 1 (\bb -> prependU8 bb 1)) else Nothing
    ]
  msgUOff <- writeTable b
    [ Just (scalar 2 (\bb -> prependI16 bb metadataVersionV5))
    , Just (scalar 1 (\bb -> prependU8 bb 2))   -- header type: DictionaryBatch
    , Just (voff dbUOff)
    , Just (scalar 8 (\bb -> prependI64 bb (fromIntegral (BS.length body))))
    , Nothing
    ]
  finish b msgUOff
{-# NOINLINE buildDictionaryBatchMessage #-}

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
writeArrowStreamFB sch batches = writeArrowStreamFBWithDicts sch [] batches

-- | Emit a stream with dictionary batches preceding the record
-- batches. Dictionary batches are placed in the order given; each
-- carries an @id@ that must match a @DictionaryEncoding.id@
-- referenced by the schema. Most consumers expect dictionary
-- batches before any record batch that references them — that's
-- the order we emit.
writeArrowStreamFBWithDicts
  :: Schema
  -> [DictBatch]
  -> [(RecordBatchDef, ByteString)]
  -> ByteString
writeArrowStreamFBWithDicts sch dicts batches =
  let !schemaMsg = encapsulateMessage (buildSchemaMessage sch) BS.empty
      !dictBytes = BS.concat
        [ encapsulateMessage
            (buildDictionaryBatchMessage db)
            (dbBody db)
        | db <- dicts
        ]
      !batchBytes = BS.concat
        [ encapsulateMessage
            (buildRecordBatchMessage rb (fromIntegral (BS.length body)))
            body
        | (rb, body) <- batches
        ]
      !eos = BL.toStrict $ B.toLazyByteString $
               B.word32LE 0xFFFFFFFF <> B.int32LE 0
  in schemaMsg <> dictBytes <> batchBytes <> eos

-- | Build @(RecordBatchDef, body bytes)@ from a 'Schema' + columns.
-- Delegates to 'Arrow.Write.encodeColumns' for the physical body
-- layout (validity bitmaps, typed buffers, 8-byte aligned), then
-- normalises the 'Buffer' list so every top-level non-nullable
-- /primitive/ field has the Arrow-spec buffer count — which means
-- prepending a zero-length validity buffer (the simplified internal
-- encoder in "Arrow.Write" omits the slot for non-nullable
-- primitives because its own reader does too; the spec requires
-- the slot to exist even when empty). Nested / view / REE columns
-- are emitted with their canonical buffer layout by 'encodeCol'
-- already, and pass through unchanged.
buildRecordBatchBytes
  :: Schema
  -> V.Vector ColumnArray
  -> (RecordBatchDef, ByteString)
buildRecordBatchBytes sch cols =
  let !acc = W.encodeColumns (arrowFields sch) cols W.emptyBuildAcc
      !rawNodes = V.fromList (reverse (W.baNodes acc))
      !rawBufs  = V.fromList (reverse (W.baBufs acc))
      !(!nodes, !bufs) = normaliseBuffers (arrowFields sch) cols rawNodes rawBufs
      !numRows = if V.null cols then 0 else columnLength (V.head cols)
      !rb = RecordBatchDef
              { rbLength  = fromIntegral numRows
              , rbNodes   = nodes
              , rbBuffers = bufs
              , rbVariadicBufferCounts =
                  V.fromList (reverse (W.baVariadic acc))
              }
      !body = BL.toStrict (B.toLazyByteString (W.baBody acc))
  in (rb, body)

-- | Walk every TOP-LEVEL column in the batch and inject an empty
-- 'Buffer' (offset=0, length=0) for the validity slot of any
-- non-nullable column whose layout has one. The Arrow spec
-- requires the slot at every level; pyarrow, arrow-cpp, and
-- arrow-rs all happily accept a "missing" empty slot for nested
-- non-nullable children, so we only fix this at the top level
-- where the readers are stricter.
--
-- For columns whose layout has /no/ validity slot (Union, REE,
-- FixedSizeBinary's data buffer, struct's children, ...) we pass
-- through unchanged.
--
-- 'FieldNode' counts pass through unchanged — the spec says one
-- @FieldNode@ per field in pre-order, which 'encodeCol' already
-- produces.
normaliseBuffers
  :: V.Vector Field
  -> V.Vector ColumnArray
  -> V.Vector FieldNode
  -> V.Vector Buffer
  -> (V.Vector FieldNode, V.Vector Buffer)
normaliseBuffers _fields cols nodes rawBufs =
  let (_, !revBufs) = V.foldl' step (0 :: Int, []) cols
      step (!bIdx, acc) col =
        let (!consumed, !emitted) = injectColumn col rawBufs bIdx
        in  (bIdx + consumed, reverse emitted ++ acc)
  in  (nodes, V.fromList (reverse revBufs))

-- | Recursively inject empty validity buffers at every layout
-- position that has a validity slot but where the writer omitted
-- it (non-nullable column).  Returns @(#source buffers consumed,
-- spec-compliant output buffers in emission order)@.
injectColumn
  :: ColumnArray -> V.Vector Buffer -> Int -> (Int, [Buffer])
injectColumn col bufs bIdx0 = case col of
  -- Flat primitives: layout = [validity, data]. Nullable supplies
  -- 2 buffers; non-nullable supplies 1 (data only) and we prepend
  -- an empty validity.
  _ | isFlatPrim col ->
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          dataBuf       = bufs V.! bIdx1
      in  (bIdx1 + 1 - bIdx0, [vBuf, dataBuf])
  _ | isVarLen col ->
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          offsetsBuf    = bufs V.! bIdx1
          dataBuf       = bufs V.! (bIdx1 + 1)
      in  (bIdx1 + 2 - bIdx0, [vBuf, offsetsBuf, dataBuf])

  ColStruct children          -> goStruct False (V.toList (V.map snd children)) bIdx0
  ColStructMaybe _ children   -> goStruct True  (V.toList (V.map snd children)) bIdx0

  ColList _ child             -> goList False child bIdx0
  ColListMaybe _ _ child      -> goList True  child bIdx0
  ColLargeList _ child        -> goList False child bIdx0
  ColLargeListMaybe _ _ child -> goList True  child bIdx0

  ColFixedSizeList _ child       -> goFixedSizeList False child bIdx0
  ColFixedSizeListMaybe _ _ child -> goFixedSizeList True  child bIdx0

  ColMap _ k v                -> goMap False k v bIdx0
  ColMapMaybe _ _ k v         -> goMap True  k v bIdx0

  ColDenseUnion _ _ children ->
      let typeIds = bufs V.! bIdx0
          offsets = bufs V.! (bIdx0 + 1)
          (cc, cb) = goSiblings (V.toList children) (bIdx0 + 2)
      in  (cc + 2, typeIds : offsets : cb)
  ColSparseUnion _ children ->
      let typeIds = bufs V.! bIdx0
          (cc, cb) = goSiblings (V.toList children) (bIdx0 + 1)
      in  (cc + 1, typeIds : cb)

  ColRunEndEncoded re vals ->
      -- REE parent has zero buffers, then run_ends + values.
      let (cre, bre) = injectColumn re bufs bIdx0
          (cv,  bv)  = injectColumn vals bufs (bIdx0 + cre)
      in  (cre + cv, bre ++ bv)

  ColListView _ _ child            -> goListView False child bIdx0
  ColListViewMaybe _ _ _ child     -> goListView True  child bIdx0
  ColLargeListView _ _ child       -> goListView False child bIdx0
  ColLargeListViewMaybe _ _ _ child -> goListView True  child bIdx0

  ColUtf8View {}        -> goView col bIdx0
  ColUtf8ViewMaybe {}   -> goView col bIdx0
  ColBinaryView {}      -> goView col bIdx0
  ColBinaryViewMaybe {} -> goView col bIdx0

  ColDictionary _ _ _ ->
      let (vBuf, bIdx1) = takeValidity (isNullable col) bufs bIdx0
          indices       = bufs V.! bIdx1
      in  (bIdx1 + 1 - bIdx0, [vBuf, indices])

  _ -> (0, [])
  where
    goStruct nullable children bIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          (cc, cb) = goSiblings children bIdx1
      in  (bIdx1 + cc - bIdx, vBuf : cb)
    goList nullable child bIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          (cc, cb)      = injectColumn child bufs (bIdx1 + 1)
      in  (bIdx1 + 1 + cc - bIdx, vBuf : offsetsBuf : cb)
    goFixedSizeList nullable child bIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          (cc, cb)      = injectColumn child bufs bIdx1
      in  (bIdx1 + cc - bIdx, vBuf : cb)
    goMap nullable k v bIdx =
      -- Map's child is a (key, value) struct; the writer skips
      -- emitting a struct buffer (no validity) — its FieldNode
      -- exists but contributes 0 buffers. So our spec output has
      -- [validity, offsets, struct-validity (empty), key bufs..., value bufs...].
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          (ck, kb) = injectColumn k bufs (bIdx1 + 1)
          (cv, kv) = injectColumn v bufs (bIdx1 + 1 + ck)
      in  ( bIdx1 + 1 + ck + cv - bIdx
          , vBuf : offsetsBuf : emptyValidityBuffer : kb ++ kv
          )
    goListView nullable child bIdx =
      let (vBuf, bIdx1) = takeValidity nullable bufs bIdx
          offsetsBuf    = bufs V.! bIdx1
          sizesBuf      = bufs V.! (bIdx1 + 1)
          (cc, cb)      = injectColumn child bufs (bIdx1 + 2)
      in  (bIdx1 + 2 + cc - bIdx, vBuf : offsetsBuf : sizesBuf : cb)
    goView c bIdx =
      let (vBuf, bIdx1) = takeValidity (isNullable c) bufs bIdx
          viewBuf       = bufs V.! bIdx1
          !varCount = case c of
            ColUtf8View vs        -> viewVariadic (V.map (Just . TE.encodeUtf8) vs)
            ColUtf8ViewMaybe vs   -> viewVariadic (V.map (fmap TE.encodeUtf8) vs)
            ColBinaryView vs      -> viewVariadic (V.map Just vs)
            ColBinaryViewMaybe vs -> viewVariadic vs
            _                     -> 0
          variadics = [ bufs V.! (bIdx1 + 1 + i) | i <- [0 .. varCount - 1] ]
      in  (bIdx1 + 1 + varCount - bIdx, vBuf : viewBuf : variadics)
    goSiblings :: [ColumnArray] -> Int -> (Int, [Buffer])
    goSiblings [] _ = (0, [])
    goSiblings (c:cs) bIdx =
      let (cc, cb) = injectColumn c bufs bIdx
          (rc, rb) = goSiblings cs (bIdx + cc)
      in  (cc + rc, cb ++ rb)

-- | Take a validity buffer (or substitute an empty one) and step
-- the source-buffer cursor accordingly.
takeValidity :: Bool -> V.Vector Buffer -> Int -> (Buffer, Int)
takeValidity True  bufs bIdx = (V.unsafeIndex bufs bIdx, bIdx + 1)
takeValidity False _    bIdx = (emptyValidityBuffer, bIdx)

isFlatPrim :: ColumnArray -> Bool
isFlatPrim = \case
  ColInt8 {} -> True; ColInt16 {} -> True; ColInt32 {} -> True; ColInt64 {} -> True
  ColUInt8 {} -> True; ColUInt16 {} -> True; ColUInt32 {} -> True; ColUInt64 {} -> True
  ColFloat16 {} -> True; ColFloat {} -> True; ColDouble {} -> True
  ColBool {} -> True
  ColDate32 {} -> True; ColDate64 {} -> True
  ColTime32 {} -> True; ColTime64 {} -> True
  ColTimestamp {} -> True; ColDuration {} -> True
  ColDecimal128 {} -> True; ColDecimal256 {} -> True
  ColFixedSizeBinary {} -> True
  ColIntervalYearMonth {} -> True
  ColIntervalDayTime {} -> True
  ColIntervalMonthDayNano {} -> True
  ColInt8Maybe {} -> True; ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True; ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True; ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True; ColUInt64Maybe {} -> True
  ColFloat16Maybe {} -> True
  ColFloatMaybe {} -> True; ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColDate32Maybe {} -> True; ColDate64Maybe {} -> True
  ColTime32Maybe {} -> True; ColTime64Maybe {} -> True
  ColTimestampMaybe {} -> True; ColDurationMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  _ -> False

isVarLen :: ColumnArray -> Bool
isVarLen = \case
  ColUtf8 {} -> True; ColBinary {} -> True
  ColLargeUtf8 {} -> True; ColLargeBinary {} -> True
  ColUtf8Maybe {} -> True; ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True; ColLargeBinaryMaybe {} -> True
  _ -> False

isNullable :: ColumnArray -> Bool
isNullable = \case
  ColInt8Maybe {} -> True; ColInt16Maybe {} -> True
  ColInt32Maybe {} -> True; ColInt64Maybe {} -> True
  ColUInt8Maybe {} -> True; ColUInt16Maybe {} -> True
  ColUInt32Maybe {} -> True; ColUInt64Maybe {} -> True
  ColFloat16Maybe {} -> True
  ColFloatMaybe {} -> True; ColDoubleMaybe {} -> True
  ColBoolMaybe {} -> True
  ColUtf8Maybe {} -> True; ColBinaryMaybe {} -> True
  ColLargeUtf8Maybe {} -> True; ColLargeBinaryMaybe {} -> True
  ColFixedSizeBinaryMaybe {} -> True
  ColDate32Maybe {} -> True; ColDate64Maybe {} -> True
  ColTime32Maybe {} -> True; ColTime64Maybe {} -> True
  ColTimestampMaybe {} -> True; ColDurationMaybe {} -> True
  ColStructMaybe {} -> True
  ColListMaybe {} -> True; ColLargeListMaybe {} -> True
  ColFixedSizeListMaybe {} -> True
  ColMapMaybe {} -> True
  ColListViewMaybe {} -> True; ColLargeListViewMaybe {} -> True
  ColUtf8ViewMaybe {} -> True; ColBinaryViewMaybe {} -> True
  _ -> False

-- | 1 if any non-null payload in the view column exceeds 12 bytes,
-- 0 otherwise.
viewVariadic :: V.Vector (Maybe BS.ByteString) -> Int
viewVariadic vs
  | V.any tooLong vs = 1
  | otherwise        = 0
  where
    tooLong (Just b) = BS.length b > 12
    tooLong Nothing  = False

-- ============================================================
-- Inverse: spec-format → simplified-format
-- ============================================================

-- | Strip the empty validity slots that the spec mandates at every
-- layout position from a 'RecordBatchDef' produced by an
-- arrow-cpp / arrow-rs / pyarrow writer (or our own
-- 'normaliseBuffers'). Returns a @(rb', body')@ pair whose buffer
-- list matches what 'Arrow.Write.encodeColumns' would have
-- emitted for the same schema, so the existing
-- 'Arrow.Column.materializeRecordBatch' can consume it directly.
--
-- The body bytes are unchanged — we only rewrite the buffer
-- /index/ list.  A spec-format empty-validity slot has
-- @offset == 0 && length == 0@ and points nowhere, so dropping it
-- doesn't disturb the body offsets the surviving buffers carry.
denormaliseBuffers
  :: Schema -> RecordBatchDef -> RecordBatchDef
denormaliseBuffers sch rb =
  let !inputBufs = rbBuffers rb
      !varCounts = rbVariadicBufferCounts rb
      (_, _, !revOut) = V.foldl' step (0 :: Int, 0 :: Int, []) (arrowFields sch)
      step (!bIdx, !vIdx, acc) f =
        let (!consumed, !varConsumed, !emitted) =
              stripField f inputBufs bIdx varCounts vIdx
        in  (bIdx + consumed, vIdx + varConsumed, reverse emitted ++ acc)
  in  rb { rbBuffers = V.fromList (reverse revOut) }

-- | Walk one schema field and decide which spec-format buffers to
-- keep. Returns @(#source buffers consumed, #variadic-count
-- entries consumed, simplified-format output buffers in
-- encoder-emission order)@. Empty validity slots (zero length) on
-- non-nullable fields are dropped.
stripField
  :: Field -> V.Vector Buffer -> Int -> V.Vector Int64 -> Int
  -> (Int, Int, [Buffer])
stripField f bufs bIdx0 varCounts vIdx0
  -- Dictionary-encoded fields carry the index column on the wire,
  -- not the value column. Treat them like a flat int field of the
  -- index width (validity + data layout = 2 buffers).
  | Just _ <- fieldDictionary f =
      let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
          dataBuf       = bufs V.! bIdx1
          out = if fieldNullable f then [vBuf, dataBuf] else [dataBuf]
      in  (2, 0, out)
  | otherwise = case fieldType f of
  AInt _ _           -> flatPrim
  ABool              -> flatPrim
  AFloatingPoint _   -> flatPrim
  AFixedSizeBinary _ -> flatPrim
  ADate _            -> flatPrim
  ATime _ _          -> flatPrim
  ATimestamp _ _     -> flatPrim
  ADuration _        -> flatPrim
  ADecimal _ _       -> flatPrim
  ADecimal256 _ _    -> flatPrim
  AInterval _        -> flatPrim

  AUtf8       -> varLen
  ABinary     -> varLen
  ALargeUtf8  -> varLen
  ALargeBinary -> varLen

  AStruct ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        outV = if fieldNullable f then [vBuf] else []
        (cc, cv, cb) = stripChildren (V.toList (fieldChildren f)) bufs bIdx1 varCounts vIdx0
    in  (1 + cc, cv, outV ++ cb)

  AList ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        offsetsBuf = bufs V.! bIdx1
        outV = if fieldNullable f then [vBuf, offsetsBuf] else [offsetsBuf]
        (cc, cv, cb) = stripField (V.head (fieldChildren f)) bufs (bIdx1 + 1) varCounts vIdx0
    in  (2 + cc, cv, outV ++ cb)

  ALargeList ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        offsetsBuf = bufs V.! bIdx1
        outV = if fieldNullable f then [vBuf, offsetsBuf] else [offsetsBuf]
        (cc, cv, cb) = stripField (V.head (fieldChildren f)) bufs (bIdx1 + 1) varCounts vIdx0
    in  (2 + cc, cv, outV ++ cb)

  AFixedSizeList _ ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        outV = if fieldNullable f then [vBuf] else []
        (cc, cv, cb) = stripField (V.head (fieldChildren f)) bufs bIdx1 varCounts vIdx0
    in  (1 + cc, cv, outV ++ cb)

  AMap _ ->
    -- Spec layout: [validity, offsets, struct-validity (empty),
    -- key bufs..., value bufs...]. Simplified writer emits
    -- [validity?, offsets, key bufs..., value bufs...] (no struct
    -- buffer; the simplified reader doesn't expect one).
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        offsetsBuf = bufs V.! bIdx1
        bIdx2 = bIdx1 + 1 + 1
        outV = if fieldNullable f then [vBuf, offsetsBuf] else [offsetsBuf]
    in  case V.toList (fieldChildren f) of
          [structField] ->
            case V.toList (fieldChildren structField) of
              [keyField, valField] ->
                let (ck, ckv, kb) = stripField keyField bufs bIdx2 varCounts vIdx0
                    (cv, cvv, vb) = stripField valField bufs (bIdx2 + ck) varCounts (vIdx0 + ckv)
                in  ( bIdx2 - bIdx0 + ck + cv
                    , ckv + cvv
                    , outV ++ kb ++ vb
                    )
              _ -> bail
          _ -> bail

  AUnion mode _ ->
    case mode of
      Dense ->
        let typeIds = bufs V.! bIdx0
            offsets = bufs V.! (bIdx0 + 1)
            (cc, cv, cb) = stripChildren (V.toList (fieldChildren f)) bufs (bIdx0 + 2) varCounts vIdx0
        in  (2 + cc, cv, typeIds : offsets : cb)
      Sparse ->
        let typeIds = bufs V.! bIdx0
            (cc, cv, cb) = stripChildren (V.toList (fieldChildren f)) bufs (bIdx0 + 1) varCounts vIdx0
        in  (1 + cc, cv, typeIds : cb)

  ARunEndEncoded ->
    case V.toList (fieldChildren f) of
      [reField, valField] ->
        let (cre, crev, bre) = stripField reField  bufs bIdx0 varCounts vIdx0
            (cv,  cvv,  bv)  = stripField valField bufs (bIdx0 + cre) varCounts (vIdx0 + crev)
        in  (cre + cv, crev + cvv, bre ++ bv)
      _ -> bail

  AListView ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        offsetsBuf = bufs V.! bIdx1
        sizesBuf   = bufs V.! (bIdx1 + 1)
        outV = if fieldNullable f
                 then [vBuf, offsetsBuf, sizesBuf]
                 else [offsetsBuf, sizesBuf]
        (cc, cv, cb) = stripField (V.head (fieldChildren f)) bufs (bIdx1 + 2) varCounts vIdx0
    in  (3 + cc, cv, outV ++ cb)
  ALargeListView ->
    let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
        offsetsBuf = bufs V.! bIdx1
        sizesBuf   = bufs V.! (bIdx1 + 1)
        outV = if fieldNullable f
                 then [vBuf, offsetsBuf, sizesBuf]
                 else [offsetsBuf, sizesBuf]
        (cc, cv, cb) = stripField (V.head (fieldChildren f)) bufs (bIdx1 + 2) varCounts vIdx0
    in  (3 + cc, cv, outV ++ cb)

  AUtf8View       -> viewLayout
  ABinaryView     -> viewLayout

  ANull           -> (0, 0, [])
  _               -> bail
  where
    bail = (V.length bufs - bIdx0, V.length varCounts - vIdx0, V.toList (V.drop bIdx0 bufs))
    flatPrim =
      let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
          dataBuf       = bufs V.! bIdx1
          out = if fieldNullable f then [vBuf, dataBuf] else [dataBuf]
      in  (2, 0, out)
    varLen =
      let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
          offsetsBuf    = bufs V.! bIdx1
          dataBuf       = bufs V.! (bIdx1 + 1)
          out = if fieldNullable f
                  then [vBuf, offsetsBuf, dataBuf]
                  else [offsetsBuf, dataBuf]
      in  (3, 0, out)
    viewLayout =
      -- Spec: [validity, view, ...variadic]. Variadic count comes
      -- from rbVariadicBufferCounts at the per-view-column slot.
      let (vBuf, bIdx1) = (bufs V.! bIdx0, bIdx0 + 1)
          viewBuf       = bufs V.! bIdx1
          !varCount = case varCounts V.!? vIdx0 of
            Just c  -> fromIntegral c :: Int
            Nothing -> 0
          variadics =
            [ bufs V.! (bIdx1 + 1 + i) | i <- [0 .. varCount - 1] ]
          outV = if fieldNullable f
                   then [vBuf, viewBuf] ++ variadics
                   else [viewBuf] ++ variadics
      in  (2 + varCount, 1, outV)

stripChildren
  :: [Field] -> V.Vector Buffer -> Int -> V.Vector Int64 -> Int
  -> (Int, Int, [Buffer])
stripChildren []     _    _    _         _    = (0, 0, [])
stripChildren (c:cs) bufs bIdx varCounts vIdx =
  let (cc, cv, cb) = stripField c bufs bIdx varCounts vIdx
      (rc, rv, rb) = stripChildren cs bufs (bIdx + cc) varCounts (vIdx + cv)
  in  (cc + rc, cv + rv, cb ++ rb)

-- | Convenience: parse + materialise.  Pyarrow / arrow-cpp output
-- → a 'V.Vector ColumnArray' per batch in one call.
materializeRecordBatchFB
  :: Schema -> RecordBatchDef -> ByteString
  -> Either String (V.Vector ColumnArray)
materializeRecordBatchFB sch rb body =
  materializeRecordBatch sch (denormaliseBuffers sch rb) body

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
  dictionary <- case slot 4 of
    Nothing -> Right Nothing
    Just p  -> do
      dePos <- followUOffset bs p
      Just <$> readDictionaryEncodingTable bs dePos
  Right Field
    { fieldName     = name
    , fieldNullable = nullable
    , fieldType     = ty
    , fieldChildren = children
    , fieldDictionary = dictionary
    }

-- | Decode a 'DictionaryEncoding' table:
--
-- @
-- table DictionaryEncoding {
--   id: long;
--   indexType: Int;
--   isOrdered: bool;
--   dictionaryKind: DictionaryKind;
-- }
-- @
readDictionaryEncodingTable :: ByteString -> Pos -> Either String DictionaryEncoding
readDictionaryEncodingTable bs dePos = do
  s <- resolveTable bs dePos
  did <- case s 0 of
    Nothing -> Right 0
    Just b  -> peekI64 bs b
  idxTy <- case s 1 of
    Nothing -> Right (AInt 32 True)   -- spec default
    Just b  -> do
      tyPos <- followUOffset bs b
      readType bs 2 (Just tyPos)
  ordered <- case s 2 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 bs b
      Right (v /= 0)
  Right (DictionaryEncoding did idxTy ordered)

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
readType _  22 _ = Right ARunEndEncoded
readType _  23 _ = Right ABinaryView
readType _  24 _ = Right AUtf8View
readType _  25 _ = Right AListView
readType _  26 _ = Right ALargeListView
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
  variadic <- case s 4 of
    Nothing -> Right V.empty
    Just b  -> do
      vecPos <- followUOffset bs b
      n <- peekU32 bs vecPos
      V.generateM (fromIntegral n) $ \i -> peekI64 bs (vecPos + 4 + 8 * i)
  Right RecordBatchDef
    { rbLength  = len
    , rbNodes   = nodes
    , rbBuffers = bufs
    , rbVariadicBufferCounts = variadic
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

-- | One decoded dictionary batch — the raw payload that defines a
-- dictionary's index → value mapping. The @data@ field is the
-- inner @RecordBatch@ (a single column whose values are the
-- dictionary values, in index order).
data DictBatch = DictBatch
  { dbId      :: !Int64
    -- ^ Dictionary id; matches @DictionaryEncoding.id@ in the
    -- schema.
  , dbIsDelta :: !Bool
    -- ^ When @True@, the values append to the existing dictionary
    -- with this id; otherwise they replace it.
  , dbData    :: !RecordBatchDef
  , dbBody    :: !ByteString
  } deriving stock (Show, Eq)

-- | Decode a DictionaryBatch-typed @Message@ flatbuffer to
-- @(id, isDelta, RecordBatchDef, bodyLength)@.
decodeDictionaryBatchMessage
  :: ByteString -> Either String (Int64, Bool, RecordBatchDef, Int64)
decodeDictionaryBatchMessage meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  ht <- case s 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header_type missing"
    Just b  -> peekU8 meta b
  when' (ht /= 2) $
    Left ("Arrow.FlatBufferIPC: expected DictionaryBatch header (2), got " ++ show ht)
  dbTblPos <- case s 2 of
    Nothing -> Left "Arrow.FlatBufferIPC: Message header (DictionaryBatch) missing"
    Just b  -> followUOffset meta b
  ds <- resolveTable meta dbTblPos
  did <- case ds 0 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  rb <- case ds 1 of
    Nothing -> Left "Arrow.FlatBufferIPC: DictionaryBatch.data missing"
    Just b  -> do
      rbPos <- followUOffset meta b
      readRecordBatchTable meta rbPos
  isDelta <- case ds 2 of
    Nothing -> Right False
    Just b  -> do
      v <- peekU8 meta b
      Right (v /= 0)
  bodyLen <- case s 3 of
    Nothing -> Right 0
    Just b  -> peekI64 meta b
  Right (did, isDelta, rb, bodyLen)

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
  (sch, _, batches) <- readArrowStreamFBWithDicts bs0
  Right (sch, batches)

-- | Like 'readArrowStreamFB' but also returns any 'DictBatch'
-- frames encountered (in stream order). Most pyarrow / arrow-cpp
-- streams emit dictionary batches before the first record batch
-- whose schema references their @id@.
readArrowStreamFBWithDicts
  :: ByteString
  -> Either String (Schema, [DictBatch], [(RecordBatchDef, ByteString)])
readArrowStreamFBWithDicts bs0 = do
  (schema, after) <- consumeOne bs0 decodeSchemaMessage
  go schema after [] []
  where
    consumeOne bs decodeFrame = do
      (mlen, meta, rest) <- readFrameHeader bs
      when' (mlen <= 0) $
        Left "Arrow.FlatBufferIPC: unexpected EOS while reading schema"
      decoded <- decodeFrame meta
      Right (decoded, rest)

    go sch bs dicts batches
      | BS.length bs < 4 = Right (sch, reverse dicts, reverse batches)
      | otherwise = do
          (mlen, meta, rest1) <- readFrameHeader bs
          if mlen == 0
            then Right (sch, reverse dicts, reverse batches)
            else do
              -- Peek the message header_type without forcing a
              -- specific decoder.
              ht <- peekHeaderType meta
              case ht of
                3 -> do
                  (rb, bodyLen) <- decodeRecordBatchMessage meta
                  let !nBody    = fromIntegral bodyLen :: Int
                      !nBodyPad = alignUp8FB nBody
                      body      = BS.take nBody rest1
                      rest2     = BS.drop nBodyPad rest1
                  go sch rest2 dicts ((rb, body) : batches)
                2 -> do
                  (did, isDelta, rb, bodyLen) <-
                    decodeDictionaryBatchMessage meta
                  let !nBody    = fromIntegral bodyLen :: Int
                      !nBodyPad = alignUp8FB nBody
                      body      = BS.take nBody rest1
                      rest2     = BS.drop nBodyPad rest1
                      !db = DictBatch { dbId = did, dbIsDelta = isDelta
                                      , dbData = rb, dbBody = body }
                  go sch rest2 (db : dicts) batches
                _ ->
                  Left ("Arrow.FlatBufferIPC: unsupported message header_type "
                        ++ show ht)

    alignUp8FB n = (n + 7) .&. complement (7 :: Int)

-- | Look up the @header_type@ ubyte from a Message flatbuffer.
peekHeaderType :: ByteString -> Either String Int
peekHeaderType meta = do
  msgPos <- fromIntegral <$> peekU32 meta 0
  s <- resolveTable meta msgPos
  case s 1 of
    Nothing -> Right 0
    Just b  -> fromIntegral <$> peekU8 meta b

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
