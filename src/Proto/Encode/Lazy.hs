-- | Lazy 'ByteString' encoding for protobuf messages.
--
-- Complements the strict encoding in "Proto.Encode" with lazy output
-- suitable for streaming to sockets, files, or other incremental sinks.
--
-- The stream-framing functions ('encodeMessageStream', 'encodeMessageStreamSized')
-- use the standard protobuf length-delimited framing: each message is preceded
-- by a varint length prefix.  This is the same framing used by gRPC and
-- compatible with 'Proto.Decode.Stream.decodeMessageStream'.
module Proto.Encode.Lazy
  ( -- * Single-message lazy encoding
    encodeMessageLazy
  , encodeMessageLazySized

    -- * Stream encoding (length-delimited framing)
  , encodeMessageStream
  , encodeMessageStreamSized
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as BL

import Proto.Encode (MessageEncode (..), MessageSize (..))
import Proto.Wire.Encode (putVarint)

-- | Encode a message to a lazy 'ByteString'.
--
-- Output is produced in reasonably-sized chunks by the 'B.Builder'
-- machinery.  No size pre-computation is performed.
encodeMessageLazy :: MessageEncode a => a -> BL.ByteString
encodeMessageLazy = B.toLazyByteString . buildMessage
{-# INLINE encodeMessageLazy #-}

-- | Encode a message to a lazy 'ByteString' with an allocation hint.
--
-- The pre-computed 'MessageSize' is used to allocate a single initial
-- buffer of exactly the right size, typically producing a one-chunk
-- lazy 'ByteString' with no recopying.
encodeMessageLazySized :: (MessageEncode a, MessageSize a) => a -> BL.ByteString
encodeMessageLazySized msg =
  let sz = messageSize msg
  in BE.toLazyByteStringWith (BE.untrimmedStrategy sz sz) BL.empty (buildMessage msg)
{-# INLINE encodeMessageLazySized #-}

-- | Encode a list of messages with length-delimited framing.
--
-- Each message is preceded by a varint-encoded byte length.  The output
-- is produced lazily: messages are encoded on demand as the result is
-- consumed, so this works with large (or even infinite) input lists
-- without buffering everything in memory.
--
-- Without 'MessageSize', each message is materialised to a strict
-- 'ByteString' to determine its length prefix.
encodeMessageStream :: MessageEncode a => [a] -> BL.ByteString
encodeMessageStream = B.toLazyByteString . go
  where
    go [] = mempty
    go (msg : rest) =
      let payload = BL.toStrict (B.toLazyByteString (buildMessage msg))
      in putVarint (fromIntegral (BS.length payload))
         <> B.byteString payload
         <> go rest

-- | Encode a list of messages with length-delimited framing using
-- pre-computed sizes.
--
-- Like 'encodeMessageStream' but avoids materialising each message to
-- determine its length.  The two-pass approach computes the size first,
-- writes the varint length prefix, then writes the message payload
-- directly into the output buffer.
encodeMessageStreamSized :: (MessageEncode a, MessageSize a) => [a] -> BL.ByteString
encodeMessageStreamSized = B.toLazyByteString . go
  where
    go [] = mempty
    go (msg : rest) =
      let sz = messageSize msg
      in putVarint (fromIntegral sz)
         <> buildMessage msg
         <> go rest
