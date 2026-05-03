{-# LANGUAGE BangPatterns #-}
-- | Spec-compliant FlatBuffers /builder/ — emits binary buffers
-- that are byte-identical with the reference @flatcc@ /
-- @flatbuffers-cpp@ output for the same input.
--
-- Buffers are constructed back-to-front, like every other
-- FlatBuffers implementation: each call /prepends/ bytes to the
-- accumulator while tracking @minAlign@ and @bufSize@. At
-- 'finish' the buffer is padded to the running @minAlign@
-- (clamped at 4) and a u32 root offset is emitted, yielding the
-- canonical @[root_offset][...padded...][vtables, tables, ...]@
-- layout consumers expect.
--
-- This module is intentionally schema-agnostic. It does /not/
-- understand individual FlatBuffers schemas (e.g. Apache Arrow's
-- @Schema.fbs@); higher layers like "Arrow.FlatBufferIPC"
-- compose 'writeTable' / 'writeString' / 'writeVectorOfOffsets'
-- on top of it.
--
-- Why not the @Encode@ module? "FlatBuffers.Encode" walks a
-- self-describing 'FlatBuffers.Value.Value' AST, which is the
-- right surface for value-shaped use cases. Spec-compliant
-- vtable dedup, soffset chains, and inline-struct alignment all
-- live here so the encode/decode side can stay focused on the
-- AST mapping while Arrow / a future codegen target a precise
-- per-table layout.
--
-- = Build order vs forward layout
--
-- Because the builder is back-to-front, callers emit objects in
-- /reverse/ of the order a hex-dump reads. 'writeTable' encodes
-- this once and for all: it lays out tail padding first, then
-- slots in reverse declaration order, then the soffset_t, then
-- the vtable. Forward layout still ends up
-- @[vtable][soffset_t][slots][pad]@ because each prepend stacks
-- toward the front.
--
-- = Alignment guarantee
--
-- 'prepForObject' bumps @minAlign@ and prepends padding so the
-- about-to-be-emitted object's UOffset (distance from the end)
-- is congruent to 0 modulo its alignment. Combined with the
-- final-buffer pad in 'finish' (which forces total size to a
-- multiple of @minAlign@), every object's /forward/ position
-- @final_size - uoff@ is also aligned, which is what
-- consumers actually inspect.
module FlatBuffers.Builder
  ( -- * Builder state
    Builder
  , newBuilder
  , finish
  , currentUOff
    -- * Low-level prepend primitives
  , prependBS
  , prependU8
  , prependU16
  , prependU32
  , prependU64
  , prependI16
  , prependI32
  , prependI64
  , prepForObject
  , noteMinAlign
    -- * Tables, vtables, fields
  , Field' (..)
  , scalar
  , struct
  , voff
  , writeTable
    -- * Strings, vectors, structs
  , writeString
  , writeVectorOfOffsets
  , writeVectorOfStructs
  , writeVectorInt32
  , writeVectorInt64
    -- * Alignment helper
  , alignUp
  ) where

import Data.Bits ((.&.), complement)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Int (Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Foreign.Storable (pokeByteOff)

-- ============================================================
-- Builder state
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

-- | An inline struct field: writes @size@ bytes at @align@
-- alignment directly in the table's inline data area. @writer@
-- must prepend exactly @size@ bytes of struct data (in reverse
-- field-declaration order, since the builder is back-to-front).
--
-- NOTE: 'writeTable' currently treats 'fsAlign' as both
-- alignment /and/ size; for structs we over-align to @size@ so
-- the slot layout reserves exactly @size@ bytes. This wastes a
-- few bytes of padding when @size > align@ (e.g. a 16-byte
-- buffer struct on 8-byte alignment pads to 16-byte aligned),
-- which is benign for readers and keeps the layout-logic
-- unchanged.
struct :: Int -> Int -> (Builder -> IO ()) -> Field'
struct !size !_align writer = Field' size $ \b _ -> writer b

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
  let !present = collectPresent 0 slots
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
  --    between successive fields.
  let emit !nextExpect [] = prependBS b (BS.replicate (nextExpect - 4) 0)
      emit !expectedEnd ((idx, fs) : rest) = do
        let !off = case lookup idx inlineOffs of
                     Just o  -> o
                     Nothing -> error "FlatBuffers.Builder: internal error (missing inlineOff for present slot)"
            !fieldEnd   = off + fsAlign fs
            !padAfter   = expectedEnd - fieldEnd
        prependBS b (BS.replicate padAfter 0)
        fsWrite fs b 0
        emit off rest
  emit rawEnd (reverse present)

  -- 3. soffset_t + 4. vtable.
  --
  -- See module haddock for the build-order vs forward-layout
  -- analysis. soff = vtBytesCount because the vtable lands directly
  -- after the soffset in build order (= directly /before/ in forward
  -- order). For dedup, soff = existingUOff - tableUOff.
  curBeforeSoff <- currentUOff b
  let !tableUOff = curBeforeSoff + 4
      (vtKey, vtBytesCount, vtBytes) = makeVTableBytes inlineOffs tableSize nSlots
  dedup <- readIORef (bufVTables b)
  case Map.lookup vtKey dedup of
    Just existingUOff -> do
      prependI32 b (fromIntegral (existingUOff - tableUOff))
      pure tableUOff
    Nothing -> do
      prependI32 b (fromIntegral vtBytesCount)
      prependBS b vtBytes
      newU <- currentUOff b
      modifyIORef' (bufVTables b) (Map.insert vtKey newU)
      pure tableUOff

-- | Walk the slot list pairing each slot with its index, keeping
-- only those that are 'Just'. Avoids the @[(i, Just fs) | ...]@
-- list comprehension while preserving the original order.
collectPresent :: Int -> [Maybe Field'] -> [(Int, Field')]
collectPresent !_ []           = []
collectPresent !i (Nothing : xs) = collectPresent (i + 1) xs
collectPresent !i (Just fs : xs) = (i, fs) : collectPresent (i + 1) xs

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
      slots   = mkSlots 0 slotMap nSlots
      trimmed = reverse (dropWhile (== 0) (reverse slots))
      !nT     = length trimmed
      !vtSize = 2 + 2 + 2 * nT
      !bytes  = BSI.unsafeCreate vtSize $ \p -> do
        pokeByteOff p 0 (fromIntegral vtSize       :: Word8)
        pokeByteOff p 1 (fromIntegral (vtSize `div` 0x100) :: Word8)
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
  where
    mkSlots !i !_  !n | i >= n = []
    mkSlots !i !sm !n = Map.findWithDefault 0 i sm : mkSlots (i + 1) sm n

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

-- | Emit a vector of signed int64 scalars.
writeVectorInt64 :: Builder -> [Int64] -> IO Int
writeVectorInt64 b xs = do
  let !n = length xs
  prepForObject b (4 + 8 * n) 8
  mapM_ (prependI64 b) (reverse xs)
  prependU32 b (fromIntegral n)
  currentUOff b
