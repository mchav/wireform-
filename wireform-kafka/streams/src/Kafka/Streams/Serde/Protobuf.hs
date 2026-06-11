{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      : Kafka.Streams.Serde.Protobuf
Description : Protobuf payload serde for Confluent Schema Registry

Confluent's Protobuf-on-SR support adds one extra wire byte
between the envelope and the protobuf payload: a varint that
selects /which/ message in the registered .proto file the
payload was encoded with (a .proto can declare multiple message
types). The "main" message is index 0; nested or sibling
messages have their own indices.

This module wraps that semantic on top of the existing
'SR.encodeEnvelope' / 'SR.decodeEnvelope': we prepend / consume
a single zigzag-varint message-index between the envelope and
the user-supplied protobuf encoder / decoder.

The actual protobuf codec is pluggable: callers wire their
favourite library in via 'ProtobufEncoder' / 'ProtobufDecoder'.
-}
module Kafka.Streams.Serde.Protobuf (
  ProtobufEncoder (..),
  ProtobufDecoder (..),
  ProtobufSerdeConfig (..),
  protobufSerde,

  -- * Helpers
  encodeMessageIndex,
  decodeMessageIndex,
) where

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Word (Word64, Word8)
import Kafka.Streams.Serde (Serde (..))
import Kafka.Streams.Serde.SchemaRegistry qualified as SR


newtype ProtobufEncoder a = ProtobufEncoder
  { runProtobufEncoder :: a -> ByteString
  }


newtype ProtobufDecoder a = ProtobufDecoder
  { runProtobufDecoder :: ByteString -> Either String a
  }


data ProtobufSerdeConfig a = ProtobufSerdeConfig
  { pscClient :: !SR.SchemaRegistryClient
  , pscSubject :: !SR.SchemaSubject
  , pscSchema :: !SR.SchemaPayload
  {- ^ The protobuf .proto descriptor, registered with the
  registry on first use.
  -}
  , pscMessageIndex :: !Int
  {- ^ Which message in the .proto this serde encodes/decodes.
  0 for the first.
  -}
  , pscEncoder :: !(ProtobufEncoder a)
  , pscDecoder :: !(ProtobufDecoder a)
  }


protobufSerde :: ProtobufSerdeConfig a -> IO (Serde a)
protobufSerde ProtobufSerdeConfig {..} =
  SR.registrySerde
    SR.SchemaRegistrySerdeConfig
      { SR.srscClient = pscClient
      , SR.srscSubject = pscSubject
      , SR.srscSchema = pscSchema
      , SR.srscPayload =
          Serde
            { serialize = \v ->
                -- Protobuf envelope adds the message-index prefix
                -- \*inside* the SR envelope, before the payload.
                encodeMessageIndex pscMessageIndex
                  <> runProtobufEncoder pscEncoder v
            , deserialize = \bs -> case decodeMessageIndex bs of
                Left err -> Left (T.pack err)
                Right (_idx, payload) ->
                  case runProtobufDecoder pscDecoder payload of
                    Left e -> Left (T.pack e)
                    Right a -> Right a
            , serializeHeaders = const mempty
            }
      }


----------------------------------------------------------------------
-- Message-index varint
----------------------------------------------------------------------

{- | Encode a message index as the Confluent-protobuf flavour:
a /zigzag/ varint; index 0 is the special-cased single byte
@0x00@ (matches what the JVM client / @kafka-protobuf@ emits).
-}
encodeMessageIndex :: Int -> ByteString
encodeMessageIndex 0 = BS.singleton 0
encodeMessageIndex n =
  -- Confluent's encoding is "[count][indices...]" — we always
  -- carry exactly one index here so count = 1.
  let !count = encodeVarintZigzag 1
      !idx = encodeVarintZigzag (fromIntegral n)
  in BS.append count idx


-- | Inverse of 'encodeMessageIndex'. Returns @(index, rest)@.
decodeMessageIndex :: ByteString -> Either String (Int, ByteString)
decodeMessageIndex bs
  | BS.null bs = Left "protobuf envelope: empty payload"
  | BS.head bs == 0 = Right (0, BS.tail bs)
  | otherwise = case decodeVarintZigzag bs of
      Left err -> Left err
      Right (count, rest1)
        | count /= 1 ->
            -- We don't currently support multi-message
            -- arrays. The spec /does/ allow them; production
            -- callers needing nested submessages can wrap their
            -- own decoder around 'decodeMessageIndex'.
            Left
              ( "protobuf envelope: unsupported index count "
                  <> show count
              )
        | otherwise -> case decodeVarintZigzag rest1 of
            Left err -> Left err
            Right (idx, rest2) -> Right (fromIntegral idx, rest2)


----------------------------------------------------------------------
-- Varint helpers
----------------------------------------------------------------------

encodeVarintZigzag :: Int64Like -> ByteString
encodeVarintZigzag n =
  let !z = zigzagEncode n
  in BS.pack (varintBytes z)


decodeVarintZigzag :: ByteString -> Either String (Int64Like, ByteString)
decodeVarintZigzag bs0 = go 0 0 bs0
  where
    go !shift !acc bs
      | BS.null bs = Left "varint: truncated"
      | shift > 63 = Left "varint: too long"
      | otherwise =
          let !b = fromIntegral (BS.head bs) :: Word64
              !rest = BS.tail bs
              !acc' = acc .|. ((b .&. 0x7F) `shiftL` shift)
              !done = (b .&. 0x80) == 0
          in if done
               then Right (zigzagDecode acc', rest)
               else go (shift + 7) acc' rest


type Int64Like = Word64


zigzagEncode :: Int64Like -> Int64Like
zigzagEncode n = (n `shiftL` 1) `xor` (n `shiftR` 63)


zigzagDecode :: Int64Like -> Int64Like
zigzagDecode n = (n `shiftR` 1) `xor` (negate (n .&. 1))


varintBytes :: Word64 -> [Word8]
varintBytes 0 = [0]
varintBytes n0 = go n0
  where
    go 0 = []
    go n
      | n < 0x80 = [fromIntegral n]
      | otherwise =
          fromIntegral ((n .&. 0x7F) .|. 0x80) : go (n `shiftR` 7)
