{- | Lazy and streaming encoding for protobuf messages.

Complements the strict encoding in "Proto.Encode" with:

* Lazy single-message encoding ('encodeMessageLazy')
* Lazy stream encoding ('encodeMessageStream', 'encodeMessageStreamSized')
* Direct Handle output ('hPutMessageStream')
* Builder output for custom pipelines ('buildMessageFramed')

The existing strict 'Proto.Encode.encodeMessage' is unchanged.
-}
module Proto.Encode.Lazy (
  -- * Single-message lazy encoding
  encodeMessageLazy,

  -- * Stream encoding (length-delimited framing)
  encodeMessageStream,
  encodeMessageStreamSized,

  -- * Direct Handle output (no intermediate ByteString)
  hPutMessageStream,

  -- * Builder-level framing (for custom pipelines)
  buildMessageFramed,
) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Proto.Encode (MessageEncode (..), MessageSize (..))
import Proto.Wire.Encode (putVarint)
import System.IO (Handle)
import Wireform.Builder qualified as B


-- | Encode a message to a lazy 'ByteString'.
encodeMessageLazy :: MessageEncode a => a -> BL.ByteString
encodeMessageLazy = B.toLazyByteString . buildMessage
{-# INLINE encodeMessageLazy #-}


-- | Encode a list of messages with length-delimited framing.
--
-- Each message is preceded by a varint length prefix. Output is
-- produced lazily.
encodeMessageStream :: MessageEncode a => [a] -> BL.ByteString
encodeMessageStream = B.toLazyByteString . foldMap buildMessageFramedMaterialized


-- | Like 'encodeMessageStream' but uses 'MessageSize' to avoid
-- materialising each message for its length prefix.
encodeMessageStreamSized :: (MessageEncode a, MessageSize a) => [a] -> BL.ByteString
encodeMessageStreamSized = B.toLazyByteString . foldMap buildMessageFramed


-- | Write a stream of messages directly to a 'Handle' with
-- length-delimited framing. No intermediate 'ByteString' is
-- allocated for the stream; each message is framed and flushed
-- via the Builder.
hPutMessageStream :: (MessageEncode a, MessageSize a) => Handle -> [a] -> IO ()
hPutMessageStream h = B.hPutBuilder h . foldMap buildMessageFramed


-- | Build a single length-delimited frame: varint size prefix
-- followed by the message payload. Requires 'MessageSize' to
-- avoid materialising the payload for its length.
--
-- Compose these with '<>' or 'foldMap' for multi-message streams.
buildMessageFramed :: (MessageEncode a, MessageSize a) => a -> B.Builder
buildMessageFramed msg =
  let !sz = messageSize msg
  in putVarint (fromIntegral sz) <> buildMessage msg
{-# INLINE buildMessageFramed #-}


-- Internal: framed encode without MessageSize (materializes to get length).
buildMessageFramedMaterialized :: MessageEncode a => a -> B.Builder
buildMessageFramedMaterialized msg =
  let payload = B.toStrictByteString (buildMessage msg)
  in putVarint (fromIntegral (BS.length payload))
      <> B.byteStringCopy payload
