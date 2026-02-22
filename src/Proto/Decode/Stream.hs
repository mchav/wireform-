-- | Streaming decoder for protobuf messages from lazy 'ByteString'.
--
-- Protobuf streams use length-delimited framing: each message is preceded
-- by a varint-encoded byte length. This module decodes such streams lazily,
-- yielding results on demand as input chunks become available.
--
-- The existing strict 'Proto.Decode.decodeMessage' is unchanged; this module
-- adds lazy\/streaming counterparts.
module Proto.Decode.Stream
  ( -- * Single-message lazy decode
    decodeMessageLazy

    -- * Stream decoding (length-delimited framing)
  , decodeMessageStream
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int64)
import Data.Word (Word64)

import Proto.Decode (MessageDecode, decodeMessage)
import Proto.Wire.Decode (DecodeError (..))

-- | Decode a single message from a lazy 'ByteString'.
--
-- Strictly materialises the input before decoding. Use this when the
-- full message is available but arrives as lazy chunks (e.g. from a file
-- read or network recv).
decodeMessageLazy :: MessageDecode a => BL.ByteString -> Either DecodeError a
decodeMessageLazy = decodeMessage . BL.toStrict
{-# INLINE decodeMessageLazy #-}

-- | Decode a stream of length-delimited protobuf messages.
--
-- Each message in the input must be preceded by a varint length prefix
-- (the standard protobuf streaming framing used by gRPC and other systems).
--
-- Results are produced lazily: only as much input is consumed as needed
-- to yield the next decoded message. This works with infinite or
-- incrementally-produced lazy 'ByteString' inputs.
--
-- Decoding stops when the input is exhausted. A per-message 'DecodeError'
-- is returned inline; subsequent messages are still attempted.
decodeMessageStream :: MessageDecode a => BL.ByteString -> [Either DecodeError a]
decodeMessageStream lbs
  | BL.null lbs = []
  | otherwise = case getVarintLazy lbs of
      Left e -> [Left e]
      Right (len, rest) ->
        let msgLen = fromIntegral len :: Int64
            (msgBytes, remaining) = BL.splitAt msgLen rest
        in if BL.length msgBytes < msgLen
           then [Left UnexpectedEnd]
           else decodeMessage (BL.toStrict msgBytes) : decodeMessageStream remaining

getVarintLazy :: BL.ByteString -> Either DecodeError (Word64, BL.ByteString)
getVarintLazy = go 0 0
  where
    go :: Word64 -> Int -> BL.ByteString -> Either DecodeError (Word64, BL.ByteString)
    go !acc !shift !bs
      | shift > 63 = Left InvalidVarint
      | otherwise = case BL.uncons bs of
          Nothing -> Left UnexpectedEnd
          Just (b, rest) ->
            let val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
            in if b < 0x80
               then Right (val, rest)
               else go val (shift + 7) rest
