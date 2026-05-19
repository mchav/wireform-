module Network.HTTP2.HPACK.Huffman
  ( huffmanEncode
  , huffmanDecode
  , huffmanEncodeLength
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

import Network.HTTP2.HPACK.Types (DecodeError(..))

foreign import ccall unsafe "wireform_hpack_huffman_encode"
  c_huffman_encode :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize

foreign import ccall unsafe "wireform_hpack_huffman_encode_len"
  c_huffman_encode_len :: Ptr Word8 -> CSize -> IO CSize

foreign import ccall unsafe "wireform_hpack_huffman_decode"
  c_huffman_decode :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> Ptr CSize -> IO CInt

{-# INLINE huffmanEncodeLength #-}
huffmanEncodeLength :: ByteString -> Int
huffmanEncodeLength bs = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    result <- c_huffman_encode_len (castPtr ptr) (fromIntegral len)
    pure (fromIntegral result)

huffmanEncode :: ByteString -> ByteString
huffmanEncode bs
  | BS.null bs = BS.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(srcPtr, srcLen) -> do
        let maxOut = srcLen * 4 + 1
        BSI.createAndTrim maxOut $ \dstPtr -> do
          written <- c_huffman_encode
            (castPtr srcPtr) (fromIntegral srcLen)
            dstPtr
          pure (fromIntegral written)

huffmanDecode :: ByteString -> Either DecodeError ByteString
huffmanDecode bs
  | BS.null bs = Right BS.empty
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(srcPtr, srcLen) -> do
        let maxOut = srcLen * 2 + 256
        dstFp <- BSI.mallocByteString maxOut
        withForeignPtr dstFp $ \dstPtr -> do
          alloca $ \outLenPtr -> do
            rc <- c_huffman_decode
              (castPtr srcPtr) (fromIntegral srcLen)
              dstPtr (fromIntegral maxOut)
              outLenPtr
            if rc /= 0
              then pure (Left InvalidHuffmanEncoding)
              else do
                outLen <- peek outLenPtr
                pure (Right (BSI.fromForeignPtr dstFp 0 (fromIntegral outLen)))
