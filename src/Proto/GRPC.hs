{-# LANGUAGE BangPatterns #-}
-- | gRPC message framing for HTTP\/2.
--
-- The gRPC wire format wraps each serialized protobuf message in a 5-byte
-- header: 1 byte compression flag + 4 bytes big-endian message length.
-- This module provides framing and unframing for both single and streaming
-- messages.
module Proto.GRPC
  ( grpcFrame
  , grpcUnframe
  , grpcFrameMany
  , grpcUnframeMany
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word32)

-- | Wrap a serialized message in a gRPC frame (compression=0).
grpcFrame :: ByteString -> ByteString
grpcFrame !msg = BL.toStrict $ B.toLazyByteString $
  B.word8 0x00 <> putBE32 (fromIntegral (BS.length msg)) <> B.byteString msg
{-# INLINE grpcFrame #-}

-- | Extract the payload from a single gRPC frame.
grpcUnframe :: ByteString -> Either String ByteString
grpcUnframe !bs
  | BS.length bs < 5 =
      Left "grpcUnframe: insufficient data for frame header"
  | otherwise =
      let !compFlag = BS.index bs 0
          !len = decodeBE32 bs 1
          !totalLen = 5 + fromIntegral len
      in if compFlag > 1
         then Left $ "grpcUnframe: invalid compression flag: " ++ show compFlag
         else if compFlag == 1
         then Left "grpcUnframe: compressed frames not supported"
         else if BS.length bs < totalLen
         then Left "grpcUnframe: payload shorter than declared length"
         else if BS.length bs > totalLen
         then Left "grpcUnframe: trailing data after frame"
         else Right (BS.take (fromIntegral len) (BS.drop 5 bs))

-- | Frame multiple messages (for streaming).
grpcFrameMany :: [ByteString] -> ByteString
grpcFrameMany !msgs = BL.toStrict $ B.toLazyByteString $ mconcat
  [ B.word8 0x00 <> putBE32 (fromIntegral (BS.length m)) <> B.byteString m
  | m <- msgs
  ]

-- | Extract multiple messages from a concatenated gRPC frame stream.
grpcUnframeMany :: ByteString -> Either String [ByteString]
grpcUnframeMany !bs = go bs 0 []
  where
    !bsLen = BS.length bs
    go _ !off !acc
      | off >= bsLen = Right (reverse acc)
    go _ !off !acc
      | off + 5 > bsLen =
          Left "grpcUnframeMany: truncated frame header"
      | otherwise =
          let !compFlag = BS.index bs off
              !len = decodeBE32 bs (off + 1)
              !payloadStart = off + 5
              !payloadEnd = payloadStart + fromIntegral len
          in if compFlag > 1
             then Left $ "grpcUnframeMany: invalid compression flag: " ++ show compFlag
             else if compFlag == 1
             then Left "grpcUnframeMany: compressed frames not supported"
             else if payloadEnd > bsLen
             then Left "grpcUnframeMany: payload shorter than declared length"
             else go bs payloadEnd
                    (BS.take (fromIntegral len) (BS.drop payloadStart bs) : acc)

--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------

putBE32 :: Word32 -> B.Builder
putBE32 !w =
  B.word8 (fromIntegral (w `shiftR` 24)) <>
  B.word8 (fromIntegral ((w `shiftR` 16) .&. 0xFF)) <>
  B.word8 (fromIntegral ((w `shiftR` 8) .&. 0xFF)) <>
  B.word8 (fromIntegral (w .&. 0xFF))
{-# INLINE putBE32 #-}

decodeBE32 :: ByteString -> Int -> Word32
decodeBE32 !bs !off =
  let !b0 = fromIntegral (BS.index bs off) :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
{-# INLINE decodeBE32 #-}
