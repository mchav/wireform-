-- | Zero-copy buffer slices for HPACK decoding.
-- Instead of creating ByteString slices during decode, we track
-- (offset, length) pairs into the source buffer. ByteString materialization
-- is deferred until the data needs to escape (stored in dynamic table or
-- returned to the user).
module Network.HTTP2.HPACK.Slice
  ( Slice (..)
  , sliceToByteString
  , sliceCompare
  , sliceCompareBS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)

-- | A region within a source ByteString. No allocation to create.
data Slice = Slice
  { sliceSrc :: !ByteString  -- source buffer (shared, not copied)
  , sliceOff :: {-# UNPACK #-} !Int
  , sliceLen :: {-# UNPACK #-} !Int
  }

-- | Materialize a slice into an independent ByteString.
-- This copies the data so the source buffer can be reused.
{-# INLINE sliceToByteString #-}
sliceToByteString :: Slice -> ByteString
sliceToByteString (Slice src off len) = BS.take len (BS.drop off src)

-- | Compare two slices for equality without materializing either.
-- Uses memcmp on the underlying pointers.
{-# INLINE sliceCompare #-}
sliceCompare :: Slice -> Slice -> Bool
sliceCompare (Slice src1 off1 len1) (Slice src2 off2 len2)
  | len1 /= len2 = False
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen src1 $ \(p1, _) ->
        BSU.unsafeUseAsCStringLen src2 $ \(p2, _) -> do
          result <- BSI.memcmp (castPtr p1 `plusPtr` off1) (castPtr p2 `plusPtr` off2) len1
          pure (result == 0)

-- | Compare a slice against a ByteString without materializing the slice.
-- Direct memcmp between the slice's region and the ByteString's content.
{-# INLINE sliceCompareBS #-}
sliceCompareBS :: Slice -> ByteString -> Bool
sliceCompareBS (Slice src off len) bs
  | len /= BS.length bs = False
  | len == 0 = True
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen src $ \(p1, _) ->
        BSU.unsafeUseAsCStringLen bs $ \(p2, bsLen) -> do
          result <- BSI.memcmp (castPtr p1 `plusPtr` off) (castPtr p2) len
          pure (result == 0)
