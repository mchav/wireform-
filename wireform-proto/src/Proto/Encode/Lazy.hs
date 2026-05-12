{- | Lazy 'ByteString' and incremental encoding for protobuf messages.

Complements the strict encoding in "Proto.Encode" with:

* Lazy single-message encoding ('encodeMessageLazy', 'encodeMessageLazySized')
* Lazy stream encoding ('encodeMessageStream', 'encodeMessageStreamSized')
* Incremental (push-based) stream encoding ('IEncode', 'newStreamEncoder',
  'newStreamEncoderSized') for integration with streaming libraries

The existing strict 'Proto.Encode.encodeMessage' is unchanged.
-}
module Proto.Encode.Lazy (
  -- * Single-message lazy encoding
  encodeMessageLazy,
  encodeMessageLazySized,

  -- * Stream encoding (length-delimited framing)
  encodeMessageStream,
  encodeMessageStreamSized,

  -- * Incremental (push-based) stream encoding
  IEncode (..),
  newStreamEncoder,
  newStreamEncoderSized,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Proto.Encode (MessageEncode (..), MessageSize (..))
import Proto.Wire.Encode (putVarint, varintSize)
import Wireform.Builder qualified as B


-- ---------------------------------------------------------------------------
-- Single-message lazy encoding
-- ---------------------------------------------------------------------------

{- | Encode a message to a lazy 'ByteString'.

Output is produced in reasonably-sized chunks by the 'B.Builder'
machinery.  No size pre-computation is performed.
-}
encodeMessageLazy :: MessageEncode a => a -> BL.ByteString
encodeMessageLazy = B.toLazyByteString . buildMessage
{-# INLINE encodeMessageLazy #-}


{- | Encode a message to a lazy 'ByteString' with an allocation hint.

With fast-builder, the allocation strategy is managed internally.
The size hint is unused but the type signature is kept for
compatibility.
-}
encodeMessageLazySized :: (MessageEncode a, MessageSize a) => a -> BL.ByteString
encodeMessageLazySized msg = B.toLazyByteString (buildMessage msg)
{-# INLINE encodeMessageLazySized #-}


-- ---------------------------------------------------------------------------
-- Lazy stream encoding (length-delimited framing)
-- ---------------------------------------------------------------------------

{- | Encode a list of messages with length-delimited framing.

Each message is preceded by a varint-encoded byte length.  The output
is produced lazily: messages are encoded on demand as the result is
consumed, so this works with large (or even infinite) input lists
without buffering everything in memory.

Without 'MessageSize', each message is materialised to a strict
'ByteString' to determine its length prefix.
-}
encodeMessageStream :: MessageEncode a => [a] -> BL.ByteString
encodeMessageStream = B.toLazyByteString . go
  where
    go [] = mempty
    go (msg : rest) =
      let payload = B.toStrictByteString (buildMessage msg)
      in putVarint (fromIntegral (BS.length payload))
          <> B.byteString payload
          <> go rest


{- | Encode a list of messages with length-delimited framing using
pre-computed sizes.

Like 'encodeMessageStream' but avoids materialising each message to
determine its length.  The two-pass approach computes the size first,
writes the varint length prefix, then writes the message payload
directly into the output buffer.
-}
encodeMessageStreamSized :: (MessageEncode a, MessageSize a) => [a] -> BL.ByteString
encodeMessageStreamSized = B.toLazyByteString . go
  where
    go [] = mempty
    go (msg : rest) =
      let sz = messageSize msg
      in putVarint (fromIntegral sz)
          <> buildMessage msg
          <> go rest


-- ---------------------------------------------------------------------------
-- Incremental (push-based) stream encoder
-- ---------------------------------------------------------------------------

{- | Step type for an incremental stream encoder.

This models a push-based producer compatible with any streaming
library (conduit, pipes, streaming, etc.):

@
go enc = case enc of
  'IEncReady' f    -> do
    mmsg <- awaitMessage
    go (f mmsg)       -- feed Just msg or Nothing for end-of-stream
  'IEncChunk' bs k -> sendBytes bs >> go k
  'IEncDone'       -> pure ()
@

Feed @Just msg@ to encode a message with length-delimited framing;
feed @Nothing@ to signal end-of-stream (producing 'IEncDone').
-}
data IEncode a
  = -- | An output chunk is available. Process the 'ByteString', then
    -- continue with the next step to get more output or the ready state.
    IEncChunk !ByteString (IEncode a)
  | -- | Ready for input. Supply @Just msg@ to encode it with
    -- length-delimited framing, or @Nothing@ to signal end-of-stream.
    IEncReady (Maybe a -> IEncode a)
  | -- | Encoding complete (after end-of-stream signal).
    IEncDone


instance Show (IEncode a) where
  show (IEncChunk bs _) = "IEncChunk (" <> show (BS.length bs) <> " bytes) _"
  show (IEncReady _) = "IEncReady _"
  show IEncDone = "IEncDone"


{- | Create an incremental stream encoder.

Each message fed via 'IEncReady' is encoded with length-delimited
framing (varint length prefix + message bytes). Without 'MessageSize',
each message is materialised to a strict 'ByteString' to compute its
length prefix.

The encoder produces exactly one 'IEncChunk' per message, containing
the full frame (length prefix + payload). After processing the chunk,
the next step is always 'IEncReady' again, ready for the next message.
-}
newStreamEncoder :: MessageEncode a => IEncode a
newStreamEncoder = IEncReady $ \case
  Nothing -> IEncDone
  Just msg ->
    let payload = B.toStrictByteString (buildMessage msg)
        sz = BS.length payload
        frame =
          B.toStrictByteString
            (putVarint (fromIntegral sz) <> B.byteStringCopy payload)
    in IEncChunk frame newStreamEncoder


{- | Like 'newStreamEncoder' but uses 'MessageSize' to avoid
materialising each message to compute the length prefix.

The two-pass approach computes the exact byte size first, writes the
varint length prefix, then serialises the message payload directly
into a single output buffer.
-}
newStreamEncoderSized :: (MessageEncode a, MessageSize a) => IEncode a
newStreamEncoderSized = IEncReady $ \case
  Nothing -> IEncDone
  Just msg ->
    let sz = messageSize msg
        frame =
          B.toStrictByteString
            (putVarint (fromIntegral sz) <> buildMessage msg)
    in IEncChunk frame newStreamEncoderSized
