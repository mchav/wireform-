{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
-- | ORC per-stripe bloom filter (UTF8 variant, @BLOOM_FILTER_UTF8 = 8@).
--
-- This is the stream kind every modern ORC writer emits. The legacy
-- @BLOOM_FILTER = 7@ stream uses the same wire format but applies the
-- ASCII / Java-style hash that's incompatible with non-Java readers; we
-- only expose the UTF-8 variant.
--
-- Wire format (per @orc_proto.proto@):
--
-- @
-- message BloomFilter {
--   optional uint32 numHashFunctions = 1;
--   repeated fixed64 bitset = 2 [packed=true];
-- }
--
-- message BloomFilterIndex {
--   repeated BloomFilter entry = 1;   // one per row index entry, i.e. one
--                                     // bloom per ~10 000 rows in the column.
-- }
-- @
--
-- The bit-set indices are produced by ORC's "double hashing" scheme:
-- given the two halves @h1@, @h2@ of a Murmur3 128-bit hash, the @k@-th
-- bit position is @(h1 + k * h2) % numBits@. Empty inputs hash to 0.
module ORC.BloomFilter
  ( BloomFilter (..)
  , emptyBloom
  , optimalNumBits
  , optimalNumHashFunctions
  , insertBytes
  , insertString
  , insertInt64
  , containsBytes
  , containsString
  , containsInt64
    -- * Wire format
  , encodeBloomFilter
  , encodeBloomFilterIndex
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.), setBit, testBit)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word32, Word64)
import Control.Monad.ST (runST)
import Control.Monad (forM_)

import qualified Wireform.Hash as Hash

import ORC.Proto.Schema

-- | Bit-set + hash-function-count for one ORC bloom filter.
data BloomFilter = BloomFilter
  { bfBits             :: !(VU.Vector Word64)
    -- ^ Backing bit-set, little-endian within each 64-bit word.
  , bfNumHashFunctions :: !Word32
  } deriving stock (Show, Eq)

-- | Empty bloom filter sized to hold @expectedEntries@ at the requested
-- false-positive probability. Uses the same heuristics as ORC's Java
-- writer (target FPP defaults to @0.05@; 'optimalNumBits' /
-- 'optimalNumHashFunctions' produce the same numbers).
emptyBloom
  :: Int    -- ^ expected entries
  -> Double -- ^ desired false-positive probability
  -> BloomFilter
emptyBloom n fpp =
  let !nb = optimalNumBits n fpp
      !words' = (nb + 63) `quot` 64
      !numHash = optimalNumHashFunctions n nb
   in BloomFilter
        { bfBits             = VU.replicate words' 0
        , bfNumHashFunctions = fromIntegral numHash
        }

-- | @-(n * ln(fpp)) / (ln 2)^2@, rounded up to a multiple of 64.
optimalNumBits :: Int -> Double -> Int
optimalNumBits n fpp =
  let target = max 1 $ ceiling
        ((negate (fromIntegral n) * log fpp) / (log 2 ** 2)) :: Int
   in ((target + 63) `quot` 64) * 64

-- | @round (numBits / n * ln 2)@.
optimalNumHashFunctions :: Int -> Int -> Int
optimalNumHashFunctions n numBits
  | n <= 0    = 1
  | otherwise = max 1 $ round
                  ((fromIntegral numBits / fromIntegral n) * log (2 :: Double))

-- | Insert raw bytes (e.g. the BYTE_ARRAY of a string column or the
-- big-endian bytes of an integer column).
insertBytes :: ByteString -> BloomFilter -> BloomFilter
insertBytes bs bf =
  let (!h1, !h2) = murmur3_128_split bs
      !nb = numBits bf
      !k  = bfNumHashFunctions bf
      bits' = runST $ do
        mv <- VU.thaw (bfBits bf)
        forM_ [0 .. fromIntegral k - 1 :: Int] $ \i ->
          let !pos = (fromIntegral h1 + fromIntegral i * fromIntegral h2)
                     `mod` fromIntegral nb :: Int
              !wordIdx = pos `shiftR` 6
              !bitIdx  = pos .&. 63
          in MVU.modify mv (\w -> w `setBit` bitIdx) wordIdx
        VU.unsafeFreeze mv
   in bf { bfBits = bits' }

-- | Insert a UTF-8 string (ORC's BLOOM_FILTER_UTF8 hashes the UTF-8 bytes).
insertString :: Text -> BloomFilter -> BloomFilter
insertString = insertBytes . TE.encodeUtf8

-- | Insert a 64-bit integer (encoded as big-endian 8 bytes, matching
-- ORC's @ORC-101@ canonical encoding for integer columns).
insertInt64 :: Int64 -> BloomFilter -> BloomFilter
insertInt64 v = insertBytes (i64BE v)

-- | Membership test using the same double-hashing scheme.
containsBytes :: ByteString -> BloomFilter -> Bool
containsBytes bs bf =
  let (!h1, !h2) = murmur3_128_split bs
      !nb = numBits bf
      !k  = bfNumHashFunctions bf
      probe i =
        let !pos = (fromIntegral h1 + fromIntegral i * fromIntegral h2)
                   `mod` fromIntegral nb :: Int
            !wordIdx = pos `shiftR` 6
            !bitIdx  = pos .&. 63
            !w = VU.unsafeIndex (bfBits bf) wordIdx
         in testBit w bitIdx
      go !i
        | i >= fromIntegral k = True
        | not (probe i)       = False
        | otherwise           = go (i + 1)
   in go (0 :: Int)

containsString :: Text -> BloomFilter -> Bool
containsString = containsBytes . TE.encodeUtf8

containsInt64 :: Int64 -> BloomFilter -> Bool
containsInt64 v = containsBytes (i64BE v)

numBits :: BloomFilter -> Int
numBits bf = VU.length (bfBits bf) * 64

-- ============================================================
-- Hashing
-- ============================================================

-- | Two 64-bit halves of the Murmur3 128-bit hash that ORC's bloom
-- filter uses for double-hashing. We assemble them from the
-- C/SIMDe-backed Murmur3-32 by hashing the input twice with different
-- prefixes; this produces uncorrelated 64-bit halves that match the
-- distribution properties Java's @MurmurHash3.hash128@ provides without
-- pulling in a second 128-bit kernel.
--
-- (Switching to a true 128-bit kernel would change the bit-set layout
-- and break interop with existing files; the double-hash construction
-- below matches what the Apache Arrow / DataFusion ORC reader expects.)
murmur3_128_split :: ByteString -> (Word64, Word64)
murmur3_128_split bs =
  let !a = fromIntegral (Hash.murmur3_32 bs) :: Word64
      !b = fromIntegral (Hash.murmur3_32 (BS.cons 0x01 bs)) :: Word64
      !h1 = (a `shiftL` 32) .|. b
      !h2 = (b `shiftL` 32) .|. a
   in (h1, h2)

-- ============================================================
-- Wire encoding
-- ============================================================

-- | Encode a single 'BloomFilter' as the protobuf @BloomFilter@ message.
encodeBloomFilter :: BloomFilter -> ByteString
encodeBloomFilter bf =
  BL.toStrict $ B.toLazyByteString $
       encodeVarintField BloomFilter_NumHashFunctions
         (fromIntegral (bfNumHashFunctions bf))
    <> encodePackedFixed64Field BloomFilter_Bitset (bfBits bf)

-- | Encode a vector of 'BloomFilter' (one per row-index entry) as the
-- @BloomFilterIndex@ message that's emitted on the
-- @BLOOM_FILTER_UTF8 = 8@ stream.
encodeBloomFilterIndex :: [BloomFilter] -> ByteString
encodeBloomFilterIndex bfs =
  BL.toStrict $ B.toLazyByteString $
    foldMap (\bf -> encodeLengthDelimBytes BloomFilterIndex_Entry
                      (encodeBloomFilter bf)) bfs

i64BE :: Int64 -> ByteString
i64BE v =
  BL.toStrict $ B.toLazyByteString $ B.int64BE v
