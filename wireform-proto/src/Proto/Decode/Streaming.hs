{-# LANGUAGE BangPatterns #-}
-- | Incremental\/streaming decode for length-delimited protobuf messages.
--
-- Provides a 'DecodeStep'-based API that reads as much as possible from
-- the input, returning 'Partial' when more bytes are needed.
--
-- This complements the existing 'Proto.Decode.Stream' module with a
-- simpler, format-agnostic step type shared across formats.
module Proto.Decode.Streaming
  ( DecodeStep(..)
  , streamDecode
  , feedMore
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word64)

import Proto.Decode (MessageDecode, decodeMessage)

-- | Result of an incremental decode step.
data DecodeStep a
  = Done !a !ByteString
  | Partial (ByteString -> DecodeStep a)
  | Fail !String

instance Show a => Show (DecodeStep a) where
  show (Done a bs) = "Done " ++ show a ++ " (" ++ show (BS.length bs) ++ " leftover)"
  show (Partial _) = "Partial _"
  show (Fail e)    = "Fail " ++ show e

-- | Begin streaming decode of a length-delimited protobuf message.
streamDecode :: MessageDecode a => ByteString -> DecodeStep a
streamDecode = goVarint 0 0

-- | Feed more bytes into a 'Partial' continuation.
feedMore :: DecodeStep a -> ByteString -> DecodeStep a
feedMore (Partial k) bs = k bs
feedMore step _         = step

goVarint :: MessageDecode a => Word64 -> Int -> ByteString -> DecodeStep a
goVarint !acc !shift !buf
  | shift > 63 = Fail "Proto.Decode.Streaming: varint overflow"
  | BS.null buf = Partial $ \more ->
      if BS.null more
      then Fail "Proto.Decode.Streaming: unexpected end of input in varint"
      else goVarint acc shift more
  | otherwise =
      let !b    = BS.index buf 0
          !rest = BS.drop 1 buf
          !val  = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
      in if b < 0x80
         then let !msgLen = fromIntegral val :: Int
              in if msgLen < 0
                 then Fail "Proto.Decode.Streaming: negative message length"
                 else goBody msgLen 0 [] rest
         else goVarint val (shift + 7) rest

goBody :: MessageDecode a => Int -> Int -> [ByteString] -> ByteString -> DecodeStep a
goBody !needed !have !acc !buf
  | BS.null buf =
      if have >= needed
      then finishDecode needed acc buf
      else Partial $ \more ->
        if BS.null more
        then Fail "Proto.Decode.Streaming: unexpected end of input in body"
        else goBody needed have acc more
  | otherwise =
      let !available = BS.length buf
          !remaining = needed - have
      in if available >= remaining
         then let (!msgPart, !leftover) = BS.splitAt remaining buf
              in finishDecode needed (msgPart : acc) leftover
         else goBody needed (have + available) (buf : acc) BS.empty

finishDecode :: MessageDecode a => Int -> [ByteString] -> ByteString -> DecodeStep a
finishDecode needed acc leftover =
  let !msgBytes = case acc of
        [single] | BS.length single == needed -> single
        _        -> BS.concat (reverse acc)
  in case decodeMessage msgBytes of
       Left e  -> Fail (show e)
       Right a -> Done a leftover
