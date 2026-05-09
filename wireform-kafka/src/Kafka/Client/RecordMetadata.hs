{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Client.RecordMetadata
Description : Enriched record / consumer / producer metadata (KIP-359 / 597 / 843 / 1054 / 1166)

Bundles together the bits of metadata the JVM client surfaces
on @ConsumerRecord@ + @RecordMetadata@ that the wireform-kafka
public types didn't yet expose.

Coverage:

  * KIP-359 — header serializer / deserializer types so headers
    can carry typed values (not just opaque bytes).
  * KIP-364 — propagate record-level errors as typed values
    instead of generic strings.
  * KIP-597 — record-level metadata (leaderEpoch, headers).
  * KIP-843 — committed record metadata on consumer.
  * KIP-1054 — human-readable error messages.
  * KIP-1166 — consistent error-callback shape.
  * KIP-1218 — surface CorruptRecordException distinctly.
-}
module Kafka.Client.RecordMetadata
  ( -- * Header serdes (KIP-359)
    HeaderSerde (..)
  , utf8HeaderSerde
  , bytesHeaderSerde
  , textHeaderSerde
  , doubleHeaderSerde
  , readHeader
  , writeHeader
    -- * Enriched record metadata (KIP-597)
  , EnrichedRecord (..)
  , withLeaderEpoch
    -- * Committed record metadata (KIP-843)
  , CommittedRecordMetadata (..)
  , defaultCommittedRecordMetadata
    -- * Typed error reporting (KIP-1054 / KIP-1166 / KIP-1218)
  , ProducerError (..)
  , producerErrorMessage
  , isCorruptRecordError
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- Header serdes (KIP-359)
----------------------------------------------------------------------

-- | Mirrors Java's @org.apache.kafka.common.header.Header@ +
-- @Headers@ + the @Serializer@ / @Deserializer@ surface for
-- header values.
data HeaderSerde a = HeaderSerde
  { hsEncode :: !(a -> ByteString)
  , hsDecode :: !(ByteString -> Either String a)
  }

bytesHeaderSerde :: HeaderSerde ByteString
bytesHeaderSerde = HeaderSerde
  { hsEncode = id
  , hsDecode = Right
  }

utf8HeaderSerde :: HeaderSerde Text
utf8HeaderSerde = HeaderSerde
  { hsEncode = TE.encodeUtf8
  , hsDecode = \bs -> case TE.decodeUtf8' bs of
      Left  err -> Left (show err)
      Right t   -> Right t
  }

-- | Synonym for 'utf8HeaderSerde'.
textHeaderSerde :: HeaderSerde Text
textHeaderSerde = utf8HeaderSerde

-- | Header value carrying a 'Double' as the canonical decimal
-- string (so it round-trips with Java's
-- @DoubleSerializer.serialize(value)@-style conversions).
doubleHeaderSerde :: HeaderSerde Double
doubleHeaderSerde = HeaderSerde
  { hsEncode = TE.encodeUtf8 . T.pack . show
  , hsDecode = \bs -> case reads (T.unpack (TE.decodeUtf8 bs)) of
      [(d, "")] -> Right d
      _         -> Left ("invalid double: " <> show bs)
  }

-- | Look up a header by name and decode the value through the
-- supplied serde. Returns 'Nothing' when the header is missing,
-- 'Left err' when present but un-decodable.
readHeader
  :: Text
  -> HeaderSerde a
  -> [(Text, ByteString)]
  -> Maybe (Either String a)
readHeader name serde hdrs = do
  v <- lookup name hdrs
  pure (hsDecode serde v)

writeHeader :: HeaderSerde a -> a -> ByteString
writeHeader = hsEncode

----------------------------------------------------------------------
-- Enriched record metadata (KIP-597)
----------------------------------------------------------------------

data EnrichedRecord = EnrichedRecord
  { erTopic       :: !Text
  , erPartition   :: !Int32
  , erOffset      :: !Int64
  , erTimestamp   :: !Int64
  , erKey         :: !(Maybe ByteString)
  , erValue       :: !ByteString
  , erHeaders     :: ![(Text, ByteString)]
  , erLeaderEpoch :: !(Maybe Int32)
    -- ^ KIP-597 / KIP-320: the leader epoch the broker last
    --   committed under for the record's partition. Used by the
    --   consumer for fence-aware seek / commit.
  , erTimestampType :: !(Maybe Int8TimestampType)
    -- ^ KIP-32 timestamp type: @CreateTime@ vs
    --   @LogAppendTime@. 'Nothing' when the record came in
    --   before timestamps were universally encoded.
  }
  deriving stock (Eq, Show, Generic)

-- | Mirror of 'Kafka.Protocol.RecordBatch.TimestampType' kept
-- separate so this module stays free of any wire imports.
data Int8TimestampType
  = TStampCreateTime
  | TStampLogAppendTime
  deriving stock (Eq, Show, Generic)

withLeaderEpoch :: EnrichedRecord -> Int32 -> EnrichedRecord
withLeaderEpoch r ep = r { erLeaderEpoch = Just ep }

----------------------------------------------------------------------
-- Committed record metadata (KIP-843)
----------------------------------------------------------------------

-- | The metadata bundle the broker stores alongside each
-- consumer commit (Java's @OffsetAndMetadata@ +
-- @CommittedRecordMetadata@). We surface this so a consumer can
-- distinguish "committed by my group with this leaderEpoch"
-- from "no commit recorded yet".
data CommittedRecordMetadata = CommittedRecordMetadata
  { crmOffset            :: !Int64
  , crmLeaderEpoch       :: !(Maybe Int32)
  , crmMetadata          :: !(Maybe ByteString)
  , crmCommitTimestampMs :: !(Maybe Int64)
  }
  deriving stock (Eq, Show, Generic)

defaultCommittedRecordMetadata :: Int64 -> CommittedRecordMetadata
defaultCommittedRecordMetadata o = CommittedRecordMetadata
  { crmOffset            = o
  , crmLeaderEpoch       = Nothing
  , crmMetadata          = Nothing
  , crmCommitTimestampMs = Nothing
  }

----------------------------------------------------------------------
-- Typed errors (KIP-1054 / KIP-1166 / KIP-1218)
----------------------------------------------------------------------

data ProducerError
  = PEDeliveryTimeout    !Int       -- ^ ms exceeded
  | PEAccumulatorClosed
  | PEAccumulatorFull
  | PEBrokerError        !Int16 !Text  -- ^ raw error code + broker message
  | PERequestFailed      !Text
  | PEFenced             !Text
  | PEAuthorizationFailed !Text
  | PERecordTooLarge     !Int      -- ^ size in bytes that exceeded the cap
  | PECorruptRecord      !Text     -- ^ KIP-1218
  | PEUnknown            !Text
  deriving stock (Eq, Show, Generic)

producerErrorMessage :: ProducerError -> Text
producerErrorMessage = \case
  PEDeliveryTimeout n  -> "Delivery timeout exceeded ("
                            <> T.pack (show n) <> " ms)"
  PEAccumulatorClosed  -> "Producer accumulator is closed"
  PEAccumulatorFull    -> "Producer accumulator is full"
  PEBrokerError c m    -> "Broker error " <> T.pack (show c) <> ": " <> m
  PERequestFailed m    -> "Producer request failed: " <> m
  PEFenced m           -> "Producer fenced: " <> m
  PEAuthorizationFailed m -> "Authorization failed: " <> m
  PERecordTooLarge n   -> "Record too large (" <> T.pack (show n) <> " bytes)"
  PECorruptRecord m    -> "Corrupt record: " <> m
  PEUnknown m          -> "Producer error: " <> m

isCorruptRecordError :: ProducerError -> Bool
isCorruptRecordError (PECorruptRecord _) = True
isCorruptRecordError _                   = False
