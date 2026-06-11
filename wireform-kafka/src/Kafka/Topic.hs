{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Topic
Description : Typed topic reference carrying its key / value serdes.
Copyright   : (c) 2025
License     : BSD-3-Clause

A 'Topic' bundles the three things every typed Kafka call needs:
the topic name on the wire, the 'Serde' that encodes the key, and
the 'Serde' that encodes the value. Producers and consumers that
work in terms of 'Topic' don't have to round-trip user-supplied
'ByteString's through 'Data.Text.Encoding.encodeUtf8' / 'decode'
by hand.

@
import qualified Kafka
import qualified Kafka.Serde as Serde

data Event = Event { ... } deriving Generic
instance ToJSON Event
instance FromJSON Event

events :: Kafka.Topic Text Event
events = Kafka.topic \"events\" Serde.textSerde Serde.jsonSerde

main =
  Kafka.withProducer [\"localhost:9092\"] Kafka.defaultProducerConfig $ \\p ->
    'Kafka.Client.Producer.publish' p events (Just \"order-42\") (Event ...)
@
-}
module Kafka.Topic (
  -- * Type
  Topic (..),

  -- * Construction
  topic,
  topicAny,
  bytesTopic,
  textTopic,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Kafka.Serde (Serde, byteStringSerde, textSerde, voidSerde)


{- | A topic reference plus the serdes that turn its records into
typed values.
-}
data Topic k v = Topic
  { topicName :: !Text
  , topicKeySerde :: !(Serde k)
  , topicValueSerde :: !(Serde v)
  }


-- | The everyday smart constructor.
topic :: Text -> Serde k -> Serde v -> Topic k v
topic = Topic


{- | A topic that doesn't expect typed keys (Kafka allows null keys
for compaction tombstones and partition-by-record-count work).
Note: producers can still send a 'Maybe k' for the key; this
function just defaults the key serde to 'voidSerde'.
-}
topicAny :: Text -> Serde v -> Topic () v
topicAny n vs = Topic n voidSerde vs


{- | Identity serdes on both sides — useful for handing raw bytes
through without typed wrapping.
-}
bytesTopic :: Text -> Topic ByteString ByteString
bytesTopic n = Topic n byteStringSerde byteStringSerde


-- | UTF-8 'Text' on both key and value.
textTopic :: Text -> Topic Text Text
textTopic n = Topic n textSerde textSerde
