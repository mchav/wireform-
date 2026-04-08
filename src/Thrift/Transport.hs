{-# LANGUAGE BangPatterns #-}
-- | Thrift framed transport: 4-byte big-endian length prefix framing.
--
-- Many Thrift servers (e.g. TNonblockingServer, TThreadedSelectorServer)
-- require framed transport where each message is preceded by a 4-byte
-- big-endian length prefix.
module Thrift.Transport
  ( frameMessage
  , unframeMessage
  , unframeMessages
  ) where

import Data.Bits (shiftL, shiftR, (.|.), (.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word8, Word32)

-- | Frame a message: 4-byte big-endian length prefix + payload.
frameMessage :: ByteString -> ByteString
frameMessage !payload =
  let !len = BS.length payload
      !b0 = fromIntegral ((len `shiftR` 24) .&. 0xFF) :: Word8
      !b1 = fromIntegral ((len `shiftR` 16) .&. 0xFF) :: Word8
      !b2 = fromIntegral ((len `shiftR` 8) .&. 0xFF) :: Word8
      !b3 = fromIntegral (len .&. 0xFF) :: Word8
  in BS.pack [b0, b1, b2, b3] <> payload

-- | Unframe: read 4-byte length, extract payload.
unframeMessage :: ByteString -> Either String ByteString
unframeMessage !bs
  | BS.length bs < 4 = Left "Thrift.Transport: input too short for frame header"
  | otherwise =
      let !len = readBE32 bs 0
      in if fromIntegral len + 4 > BS.length bs
         then Left $ "Thrift.Transport: frame claims " ++ show len
                   ++ " bytes but only " ++ show (BS.length bs - 4) ++ " available"
         else Right (BSU.unsafeTake (fromIntegral len) (BSU.unsafeDrop 4 bs))

-- | Unframe multiple messages from a stream.
unframeMessages :: ByteString -> Either String [ByteString]
unframeMessages !bs = go 0 []
  where
    !bsLen = BS.length bs
    go !off !acc
      | off >= bsLen = Right (reverse acc)
      | off + 4 > bsLen = Left "Thrift.Transport: truncated frame header"
      | otherwise =
          let !len = fromIntegral (readBE32 bs off) :: Int
              !payOff = off + 4
          in if payOff + len > bsLen
             then Left $ "Thrift.Transport: truncated frame payload"
             else let !payload = BSU.unsafeTake len (BSU.unsafeDrop payOff bs)
                  in go (payOff + len) (payload : acc)

readBE32 :: ByteString -> Int -> Word32
readBE32 !bs !off =
  let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
      !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
      !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
      !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
{-# INLINE readBE32 #-}
