{-# LANGUAGE BangPatterns #-}
-- | Spec-compliant FlatBuffers /reader/ primitives.
--
-- Mirror image of "FlatBuffers.Builder": chases @soffset_t@ /
-- @uoffset_t@ chains through a flat byte buffer and exposes the
-- table-slot resolution + scalar peeks that higher layers (Apache
-- Arrow IPC, hand-targeted decoders) need.
--
-- The functions here never materialise an intermediate
-- 'FlatBuffers.Value.Value' — they're for callers who know the
-- exact schema of the table they're decoding (e.g. Arrow's
-- @Schema.fbs@) and want to traverse it without paying for a
-- generic representation. "FlatBuffers.Decode" stays the
-- value-shaped surface.
--
-- All offsets are measured from the start of the input
-- 'ByteString' (= forward layout). 'resolveTable' returns a
-- closure that maps a slot index to the absolute byte offset of
-- that slot's inline data, or 'Nothing' for an absent slot.
module FlatBuffers.Reader
  ( -- * Position type
    Pos
    -- * Scalar peeks
  , peekU8
  , peekU16
  , peekU32
  , peekI16
  , peekI32
  , peekI64
    -- * Table / vector navigation
  , resolveTable
  , followUOffset
  , readString
  , readVectorOfOffsets
  , readVectorInt64
  , readVectorOfStructs
  ) where

import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32)

-- | A position in a flatbuffer 'ByteString'. The reader uses
-- absolute byte offsets throughout, never the back-to-front
-- @UOffset@s that the builder works with.
type Pos = Int

-- | Read a u16 (LE) at byte position @off@ in @bs@.
peekU16 :: ByteString -> Pos -> Either String Word16
peekU16 bs off
  | off + 2 > BS.length bs = Left "FlatBuffers.Reader: peekU16 out of range"
  | otherwise =
      let !b0 = fromIntegral (BS.index bs off)       :: Word16
          !b1 = fromIntegral (BS.index bs (off + 1)) :: Word16
      in  Right (b0 .|. (b1 `shiftL` 8))

peekU32 :: ByteString -> Pos -> Either String Word32
peekU32 bs off
  | off + 4 > BS.length bs = Left "FlatBuffers.Reader: peekU32 out of range"
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
  | off >= BS.length bs = Left "FlatBuffers.Reader: peekU8 out of range"
  | otherwise = Right (BS.index bs off)

-- | Resolve a table at @tablePos@. Returns a closure that maps
-- slot indices to absolute byte offsets, or 'Nothing' for absent
-- (zero-offset / out-of-vtable) slots.
--
-- The closure intentionally does /not/ thread an error monad: an
-- out-of-range slot index is treated as absent, matching how
-- generated FlatBuffers readers behave when a schema gains new
-- fields after a buffer was written.
resolveTable :: ByteString -> Pos -> Either String (Int -> Maybe Pos)
resolveTable bs tablePos = do
  soff <- peekI32 bs tablePos
  let vtablePos = tablePos - fromIntegral soff
  if vtablePos < 0 || vtablePos >= BS.length bs
    then Left "FlatBuffers.Reader: vtable out of bounds"
    else do
      vtSize <- peekU16 bs vtablePos
      let !nSlots = (fromIntegral vtSize - 4) `div` 2 :: Int
      Right $ \i ->
        if i >= nSlots || i < 0
          then Nothing
          else case peekU16 bs (vtablePos + 4 + 2 * i) of
            Left _    -> Nothing
            Right 0   -> Nothing
            Right off -> Just (tablePos + fromIntegral off)

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
    Left _  -> Left "FlatBuffers.Reader: invalid UTF-8 in string"
    Right t -> Right t

-- | Decode a vector of @uoffset_t@ table references. Returns each
-- referenced object's absolute byte position.
readVectorOfOffsets :: ByteString -> Pos -> Either String (V.Vector Pos)
readVectorOfOffsets bs vecPos = do
  n <- peekU32 bs vecPos
  let elemPositions = V.generate (fromIntegral n) $ \i ->
        let !ePos = vecPos + 4 + 4 * i
        in  case peekU32 bs ePos of
              Left _    -> ePos
              Right rel -> ePos + fromIntegral rel
  Right elemPositions

-- | Decode a vector of little-endian Int64 values.
readVectorInt64 :: ByteString -> Pos -> Either String [Int64]
readVectorInt64 bs vecPos = do
  n <- peekU32 bs vecPos
  goVec (fromIntegral n) (\i -> peekI64 bs (vecPos + 4 + 8 * i))

-- | Walk @0..n-1@ and accumulate results, short-circuiting on
-- @Left@. Equivalent to @mapM f [0..n-1]@ but spelled out so we
-- don't lean on a list comprehension or laziness in a hot decode
-- path.
goVec :: Int -> (Int -> Either String a) -> Either String [a]
goVec n f = go 0
  where
    go !i
      | i >= n = Right []
      | otherwise = do
          x  <- f i
          xs <- go (i + 1)
          Right (x : xs)

-- | Decode a vector of fixed-size inline structs. Returns
-- @(elemCount, byte position of each element start)@.
readVectorOfStructs
  :: ByteString
  -> Pos
  -> Int   -- ^ stride (per-struct size)
  -> Either String (Int, V.Vector Pos)
readVectorOfStructs bs vecPos stride = do
  n <- peekU32 bs vecPos
  let elems = V.generate (fromIntegral n) (\i -> vecPos + 4 + i * stride)
  Right (fromIntegral n, elems)
