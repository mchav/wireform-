-- | Streaming and incremental decoders for protobuf messages.
--
-- Protobuf streams use length-delimited framing: each message is preceded
-- by a varint-encoded byte length. This module provides three decoding
-- strategies for non-strict input:
--
-- * 'decodeMessageLazy' — single message from a lazy 'ByteString'
-- * 'decodeMessageStream' — list of messages from a lazy 'ByteString'
-- * 'decodeMessageIncremental' — continuation-based decoder that requests
--   more input when incomplete, suitable for integration with any streaming
--   library (conduit, pipes, streaming, etc.)
--
-- The existing strict 'Proto.Decode.decodeMessage' is unchanged.
module Proto.Decode.Stream
  ( -- * Single-message lazy decode
    decodeMessageLazy

    -- * Stream decoding (length-delimited framing)
  , decodeMessageStream

    -- * Incremental (resumable) decoding
  , IDecode (..)
  , decodeMessageIncremental
  , feedChunk
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

-- ---------------------------------------------------------------------------
-- Incremental (resumable) decoder
-- ---------------------------------------------------------------------------

-- | Result of an incremental decode step.
--
-- This is the standard incremental parser type (à la @binary@, @cereal@,
-- @attoparsec@) that integrates with any streaming library:
--
-- @
-- loop dec = case dec of
--   'IDone' val leftover -> handleMessage val >> loop ('decodeMessageIncremental' \`feedChunk\` leftover)
--   'IFail' err leftover -> handleError err
--   'IPartial' k         -> readChunk >>= \\mbs -> loop (k mbs)
-- @
--
-- Feed @Just chunk@ for more input, @Nothing@ to signal end-of-input
-- (which produces 'IFail' if the message is incomplete).
data IDecode a
  = IDone !a !ByteString
    -- ^ Successfully decoded a message. The 'ByteString' contains
    -- any unconsumed input that follows the message.
  | IFail !DecodeError !ByteString
    -- ^ Decode failed. The 'ByteString' contains unconsumed input.
  | IPartial (Maybe ByteString -> IDecode a)
    -- ^ More input needed. Supply @Just chunk@ for more bytes, or
    -- @Nothing@ to signal that no more input will arrive.

instance Show a => Show (IDecode a) where
  show (IDone a bs) = "IDone " <> show a <> " (" <> show (BS.length bs) <> " bytes leftover)"
  show (IFail e bs) = "IFail " <> show e <> " (" <> show (BS.length bs) <> " bytes leftover)"
  show (IPartial _) = "IPartial _"

-- | Begin incremental decoding of a single length-delimited message.
--
-- The message must use the standard protobuf streaming framing: a varint
-- length prefix followed by that many bytes of message payload.
--
-- Feed input chunks via the 'IPartial' continuation until 'IDone' or
-- 'IFail' is returned. The 'ByteString' in 'IDone' contains any
-- leftover bytes after the decoded message — pass these to the next
-- call to 'decodeMessageIncremental' to decode further messages from
-- the same stream.
--
-- Typical streaming-library integration:
--
-- @
-- go dec = case dec of
--   IDone msg leftover -> yield msg >> go (decodeMessageIncremental \`feedChunk\` leftover)
--   IPartial k         -> do
--     mchunk <- await
--     go (k mchunk)
--   IFail e _ -> throwError e
-- @
decodeMessageIncremental :: MessageDecode a => IDecode a
decodeMessageIncremental = goVarint 0 0 BS.empty

-- | Feed a chunk of input to an incremental decoder.
feedChunk :: IDecode a -> ByteString -> IDecode a
feedChunk (IPartial k) bs = k (Just bs)
feedChunk done _           = done

-- ---------------------------------------------------------------------------
-- Internal: varint phase
-- ---------------------------------------------------------------------------

goVarint :: MessageDecode a => Word64 -> Int -> ByteString -> IDecode a
goVarint !acc !shift !buf
  | shift > 63 = IFail InvalidVarint buf
  | BS.null buf = IPartial $ \case
      Nothing -> IFail UnexpectedEnd BS.empty
      Just bs
        | BS.null bs -> goVarint acc shift buf
        | otherwise  -> goVarint acc shift bs
  | otherwise =
      let !b   = BS.index buf 0
          !rest = BS.drop 1 buf
          !val  = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
      in if b < 0x80
         then let !msgLen = fromIntegral val :: Int
              in if msgLen < 0
                 then IFail NegativeLength rest
                 else goBody msgLen 0 [] rest
         else goVarint val (shift + 7) rest

-- ---------------------------------------------------------------------------
-- Internal: body accumulation phase
-- ---------------------------------------------------------------------------

goBody :: MessageDecode a => Int -> Int -> [ByteString] -> ByteString -> IDecode a
goBody !needed !have !acc !buf
  | BS.null buf =
      if have >= needed
      then finishDecode needed acc buf
      else IPartial $ \case
        Nothing -> IFail UnexpectedEnd (reassemble acc)
        Just bs
          | BS.null bs -> goBody needed have acc buf
          | otherwise  -> goBody needed have acc bs
  | otherwise =
      let !available = BS.length buf
          !remaining = needed - have
      in if available >= remaining
         then let (!msgPart, !leftover) = BS.splitAt remaining buf
              in finishDecode needed (msgPart : acc) leftover
         else goBody needed (have + available) (buf : acc) BS.empty

finishDecode :: MessageDecode a => Int -> [ByteString] -> ByteString -> IDecode a
finishDecode needed acc leftover =
  let !msgBytes = case acc of
        [single] | BS.length single == needed -> single
        _        -> BS.concat (reverse acc)
  in case decodeMessage msgBytes of
       Left e  -> IFail e leftover
       Right a -> IDone a leftover

reassemble :: [ByteString] -> ByteString
reassemble = BS.concat . reverse

-- ---------------------------------------------------------------------------
-- Internal: lazy varint reader (for decodeMessageStream)
-- ---------------------------------------------------------------------------

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
