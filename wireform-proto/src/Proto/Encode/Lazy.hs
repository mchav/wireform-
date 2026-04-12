-- | Lazy 'ByteString' and incremental encoding for protobuf messages.
--
-- Complements the strict encoding in "Proto.Encode" with:
--
-- * Lazy single-message encoding ('encodeMessageLazy', 'encodeMessageLazySized')
-- * Lazy stream encoding ('encodeMessageStream', 'encodeMessageStreamSized')
-- * Incremental (push-based) stream encoding ('IEncode', 'newStreamEncoder',
--   'newStreamEncoderSized') for integration with streaming libraries
--
-- The existing strict 'Proto.Encode.encodeMessage' is unchanged.
module Proto.Encode.Lazy
  ( -- * Single-message lazy encoding
    encodeMessageLazy
  , encodeMessageLazySized

    -- * Stream encoding (length-delimited framing)
  , encodeMessageStream
  , encodeMessageStreamSized

    -- * Incremental (push-based) stream encoding
  , IEncode (..)
  , newStreamEncoder
  , newStreamEncoderSized
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as BL

import Proto.Encode (MessageEncode (..), MessageSize (..))
import Proto.Wire.Encode (putVarint, varintSize)

-- ---------------------------------------------------------------------------
-- Single-message lazy encoding
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Lazy stream encoding (length-delimited framing)
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Incremental (push-based) stream encoder
-- ---------------------------------------------------------------------------

-- | Step type for an incremental stream encoder.
--
-- This models a push-based producer compatible with any streaming
-- library (conduit, pipes, streaming, etc.):
--
-- @
-- go enc = case enc of
--   'IEncReady' f    -> do
--     mmsg <- awaitMessage
--     go (f mmsg)       -- feed Just msg or Nothing for end-of-stream
--   'IEncChunk' bs k -> sendBytes bs >> go k
--   'IEncDone'       -> pure ()
-- @
--
-- Feed @Just msg@ to encode a message with length-delimited framing;
-- feed @Nothing@ to signal end-of-stream (producing 'IEncDone').
data IEncode a
  = IEncChunk !ByteString (IEncode a)
    -- ^ An output chunk is available. Process the 'ByteString', then
    -- continue with the next step to get more output or the ready state.
  | IEncReady (Maybe a -> IEncode a)
    -- ^ Ready for input. Supply @Just msg@ to encode it with
    -- length-delimited framing, or @Nothing@ to signal end-of-stream.
  | IEncDone
    -- ^ Encoding complete (after end-of-stream signal).

instance Show (IEncode a) where
  show (IEncChunk bs _) = "IEncChunk (" <> show (BS.length bs) <> " bytes) _"
  show (IEncReady _)    = "IEncReady _"
  show IEncDone         = "IEncDone"

-- | Create an incremental stream encoder.
--
-- Each message fed via 'IEncReady' is encoded with length-delimited
-- framing (varint length prefix + message bytes). Without 'MessageSize',
-- each message is materialised to a strict 'ByteString' to compute its
-- length prefix.
--
-- The encoder produces exactly one 'IEncChunk' per message, containing
-- the full frame (length prefix + payload). After processing the chunk,
-- the next step is always 'IEncReady' again, ready for the next message.
newStreamEncoder :: MessageEncode a => IEncode a
newStreamEncoder = IEncReady $ \case
  Nothing -> IEncDone
  Just msg ->
    let payload = BL.toStrict (B.toLazyByteString (buildMessage msg))
        sz = BS.length payload
        frame = BL.toStrict (B.toLazyByteString
          (putVarint (fromIntegral sz) <> B.byteString payload))
    in IEncChunk frame newStreamEncoder

-- | Like 'newStreamEncoder' but uses 'MessageSize' to avoid
-- materialising each message to compute the length prefix.
--
-- The two-pass approach computes the exact byte size first, writes the
-- varint length prefix, then serialises the message payload directly
-- into a single output buffer.
newStreamEncoderSized :: (MessageEncode a, MessageSize a) => IEncode a
newStreamEncoderSized = IEncReady $ \case
  Nothing -> IEncDone
  Just msg ->
    let sz = messageSize msg
        frameSz = varintSize (fromIntegral sz) + sz
        frame = toStrictFromBuilder frameSz
          (putVarint (fromIntegral sz) <> buildMessage msg)
    in IEncChunk frame newStreamEncoderSized

toStrictFromBuilder :: Int -> B.Builder -> ByteString
toStrictFromBuilder sz bld =
  let lbs = BE.toLazyByteStringWith (BE.untrimmedStrategy sz sz) BL.empty bld
  in case BL.toChunks lbs of
    [chunk] -> chunk
    chunks  -> BL.toStrict (BL.fromChunks chunks)
{-# INLINE toStrictFromBuilder #-}
