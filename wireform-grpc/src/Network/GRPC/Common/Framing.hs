{-# LANGUAGE BangPatterns #-}

-- | gRPC message framing for HTTP\/2.
--
-- The gRPC wire format wraps each serialised payload in a 5-byte header:
-- 1 byte compression flag + 4 bytes big-endian message length. This module
-- provides framing and unframing for both single and streaming messages.
--
-- Framing is built on 'Wireform.Builder', the same builder the rest of the
-- monorepo uses, so framing fragments compose with any other wireform
-- encoder via @('<>')@ without going through 'Data.ByteString.Builder'.
module Network.GRPC.Common.Framing
  ( grpcFrame
  , grpcUnframe
  , grpcFrameMany
  , grpcUnframeMany
  , grpcFrameB
  , grpcFrameManyB
  ) where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word32)
import Wireform.Builder (Builder)
import Wireform.Builder qualified as B


-- | Wrap a serialised message in a gRPC frame (compression flag = 0).
grpcFrame :: ByteString -> ByteString
grpcFrame !msg = B.toStrictByteString (grpcFrameB msg)
{-# INLINE grpcFrame #-}


-- | Builder-form 'grpcFrame'. Compose with other 'Builder's via @('<>')@ to
-- emit multiple frames without an intermediate strict 'ByteString' copy.
grpcFrameB :: ByteString -> Builder
grpcFrameB !msg =
  B.word8 0x00
    <> B.word32BE (fromIntegral (BS.length msg))
    <> B.byteString msg
{-# INLINE grpcFrameB #-}


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


-- | Frame multiple messages for streaming. Equivalent to
-- @mconcat (map grpcFrameB msgs)@ materialised to a strict bytestring.
grpcFrameMany :: [ByteString] -> ByteString
grpcFrameMany !msgs = B.toStrictByteString (grpcFrameManyB msgs)
{-# INLINE grpcFrameMany #-}


-- | Builder-form 'grpcFrameMany'.
grpcFrameManyB :: [ByteString] -> Builder
grpcFrameManyB = foldr (\m acc -> grpcFrameB m <> acc) mempty
{-# INLINE grpcFrameManyB #-}


-- | Extract multiple messages from a concatenated gRPC frame stream.
grpcUnframeMany :: ByteString -> Either String [ByteString]
grpcUnframeMany !bs = go 0 []
  where
    !bsLen = BS.length bs

    go !off !acc
      | off == bsLen = Right (reverse acc)
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
             else go payloadEnd
                    (BS.take (fromIntegral len) (BS.drop payloadStart bs) : acc)


--------------------------------------------------------------------------------
-- Internal helpers
--------------------------------------------------------------------------------


decodeBE32 :: ByteString -> Int -> Word32
decodeBE32 !bs !off =
  let !b0 = fromIntegral (BS.index bs off)       :: Word32
      !b1 = fromIntegral (BS.index bs (off + 1)) :: Word32
      !b2 = fromIntegral (BS.index bs (off + 2)) :: Word32
      !b3 = fromIntegral (BS.index bs (off + 3)) :: Word32
  in (b0 `shiftL` 24) .|. (b1 `shiftL` 16) .|. (b2 `shiftL` 8) .|. b3
{-# INLINE decodeBE32 #-}
