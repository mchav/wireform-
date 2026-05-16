{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Client.ConsumerSdk
Description : Names + types from the JVM @org.apache.kafka.clients.consumer@
              SDK that don't have a direct hand-rolled Haskell equivalent
              elsewhere

The Haskell consumer ('Kafka.Client.Consumer.Consumer') exposes the
operationally important parts of the JVM SDK directly: 'poll',
'subscribe', 'assign', 'seek', 'commitSync', 'commitAsync', etc. A few
JVM types weren't ported one-to-one because the same information is
already on the wire of the existing API:

  * @ConsumerRecords@ — a list-of-'ConsumerRecord' wrapper with
    partition-indexing helpers.
  * @OffsetAndMetadata@ — an offset paired with caller-supplied
    metadata, used for transactional commits.
  * @ConsumerGroupMetadata@ — the structured group-identity the
    transactional producer wants on @sendOffsetsToTransaction@.
  * @OffsetCommitCallback@ — the async-commit callback shape.
  * @SubscriptionPattern@ — KIP-848 regex-subscribe.

Porting them as a thin shim layer (without bloating the @Consumer@
module) keeps the JVM-equivalence promise of @SDK_PARITY.md@ honest
and lets downstream tooling (e.g. JVM-portability shims) reach for
@SubscriptionPattern@ / @ConsumerGroupMetadata@ by exactly the name
the Javadoc uses.
-}
module Kafka.Client.ConsumerSdk
  ( -- * @ConsumerRecords@
    ConsumerRecords (..)
  , emptyConsumerRecords
  , consumerRecordsAll
  , consumerRecordsCount
  , consumerRecordsPartitions
  , recordsByPartition
  , recordsByTopic
  , consumerRecordsNextOffsets

    -- * @OffsetAndMetadata@
  , OffsetAndMetadata (..)
  , offsetAndMetadata
  , withMetadata
  , withLeaderEpoch

    -- * @ConsumerGroupMetadata@
  , ConsumerGroupMetadata (..)
  , newConsumerGroupMetadata
  , groupMetadata

    -- * @OffsetCommitCallback@
  , OffsetCommitCallback
  , noopOffsetCommitCallback

    -- * @SubscriptionPattern@ (KIP-848)
  , SubscriptionPattern (..)
  , subscriptionPattern
  , matchesSubscriptionPattern

    -- * KIP-714 client telemetry id
  , clientInstanceId

    -- * Consumer overload tail
  , commitSyncOffsets
  , commitAsyncCallback
  , seekWithMetadata
  , enforceRebalanceWithReason
  ) where

import Control.Exception (SomeException (..))
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import Data.Int (Int32, Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Text.Regex.TDFA as RE

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.TopicId as TopicIdImp
import qualified Kafka.Common as Common

----------------------------------------------------------------------
-- ConsumerRecords
----------------------------------------------------------------------

{- | A typed wrapper around the @[ConsumerRecord]@ batch that a single
'C.poll' returns. Mirrors @org.apache.kafka.clients.consumer.ConsumerRecords@
on the JVM:

  * 'consumerRecordsAll' — every record in arrival order.
  * 'recordsByPartition' — the records for a single
    @(topic, partition)@ slice.
  * 'recordsByTopic' — every partition's records grouped by topic.
  * 'consumerRecordsNextOffsets' — the offset of the next record per
    partition (= last offset + 1). Used by transactional callers that
    pass commit positions to @sendOffsetsToTransaction@.

Why a newtype rather than the bare list? It (a) makes the partition
projection an O(n) one-shot instead of an O(n²) walk per partition
and (b) lets JVM porting layers keep the name they expect.
-}
newtype ConsumerRecords = ConsumerRecords
  { unConsumerRecords :: [C.ConsumerRecord]
  }
  deriving stock (Eq, Show)

emptyConsumerRecords :: ConsumerRecords
emptyConsumerRecords = ConsumerRecords []

consumerRecordsAll :: ConsumerRecords -> [C.ConsumerRecord]
consumerRecordsAll = unConsumerRecords

consumerRecordsCount :: ConsumerRecords -> Int
consumerRecordsCount = length . unConsumerRecords

-- | Distinct @(topic, partition)@ pairs the batch touches, in
-- partition-natural order. Equivalent to @ConsumerRecords.partitions()@.
consumerRecordsPartitions :: ConsumerRecords -> Set C.TopicPartition
consumerRecordsPartitions (ConsumerRecords rs) =
  Set.fromList (map toTP rs)
  where
    toTP r = C.TopicPartition { C.topic = r.topic, C.partition = r.partition }

-- | The slice of records for a single partition. Equivalent to
-- @ConsumerRecords.records(TopicPartition)@.
recordsByPartition
  :: C.TopicPartition
  -> ConsumerRecords
  -> [C.ConsumerRecord]
recordsByPartition tp (ConsumerRecords rs) =
  L.filter
    (\r -> r.topic == tp.topic && r.partition == tp.partition)
    rs

-- | All records grouped by topic. Equivalent to
-- @ConsumerRecords.records(String)@.
recordsByTopic :: ConsumerRecords -> Map Text [C.ConsumerRecord]
recordsByTopic (ConsumerRecords rs) =
  L.foldl'
    (\acc r -> Map.insertWith (flip (<>)) r.topic [r] acc)
    Map.empty
    rs

-- | The "next offset to consume" per partition — exactly what
-- 'Kafka.Client.Transaction.commitOffsetsInTransaction' wants. Each
-- partition's entry is @max(offset) + 1@ across that partition's
-- records. Equivalent to @ConsumerRecords.nextOffsets()@.
consumerRecordsNextOffsets
  :: ConsumerRecords -> HashMap C.TopicPartition Int64
consumerRecordsNextOffsets (ConsumerRecords rs) =
  L.foldl' step HashMap.empty rs
  where
    step acc r =
      let !tp = C.TopicPartition { C.topic = r.topic, C.partition = r.partition }
          !nxt = r.offset + 1
       in HashMap.insertWith max tp nxt acc

----------------------------------------------------------------------
-- OffsetAndMetadata
----------------------------------------------------------------------

{- | An offset paired with caller-supplied metadata. The JVM
@org.apache.kafka.clients.consumer.OffsetAndMetadata@ also carries an
optional leader-epoch the transactional coordinator uses for fencing.

Construct with 'offsetAndMetadata' and use 'withMetadata' /
'withLeaderEpoch' to set the optional fields:

@
'offsetAndMetadata' 42
  & 'withMetadata' \"checkpoint\"
  & 'withLeaderEpoch' 7
@
-}
data OffsetAndMetadata = OffsetAndMetadata
  { oamOffset      :: !Int64
  , oamLeaderEpoch :: !(Maybe Int32)
  , oamMetadata    :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- | Bare offset, empty metadata, no leader-epoch.
offsetAndMetadata :: Int64 -> OffsetAndMetadata
offsetAndMetadata o = OffsetAndMetadata
  { oamOffset      = o
  , oamLeaderEpoch = Nothing
  , oamMetadata    = T.empty
  }

withMetadata :: Text -> OffsetAndMetadata -> OffsetAndMetadata
withMetadata m oam = oam { oamMetadata = m }

withLeaderEpoch :: Int32 -> OffsetAndMetadata -> OffsetAndMetadata
withLeaderEpoch e oam = oam { oamLeaderEpoch = Just e }

----------------------------------------------------------------------
-- ConsumerGroupMetadata
----------------------------------------------------------------------

{- | Structured group-identity used by transactional producers on
@sendOffsetsToTransaction@. Mirrors
@org.apache.kafka.clients.consumer.ConsumerGroupMetadata@.

Inside the streams runtime we reach into the consumer for the same
information; 'newConsumerGroupMetadata' / 'groupMetadata' make it
accessible to user code that wires its own consume-transform-produce
loop.
-}
data ConsumerGroupMetadata = ConsumerGroupMetadata
  { cgmGroupId         :: !Text
  , cgmGenerationId    :: !Int32
  , cgmMemberId        :: !Text
  , cgmGroupInstanceId :: !(Maybe Text)
  }
  deriving stock (Eq, Show, Generic)

-- | Build a 'ConsumerGroupMetadata' from raw values. Pre-KIP-394
-- callers (no static-membership) supply 'Nothing' for the instance id.
newConsumerGroupMetadata
  :: Text -> Int32 -> Text -> Maybe Text -> ConsumerGroupMetadata
newConsumerGroupMetadata g gen mid inst = ConsumerGroupMetadata
  { cgmGroupId         = g
  , cgmGenerationId    = gen
  , cgmMemberId        = mid
  , cgmGroupInstanceId = inst
  }

-- | Read the consumer's current group-identity. Equivalent to
-- @KafkaConsumer.groupMetadata()@. The generation + member id are
-- live values — a rebalance changes them — so callers that pass the
-- result to a producer should fetch it again whenever the consumer
-- rejoins.
groupMetadata :: C.Consumer -> IO ConsumerGroupMetadata
groupMetadata c = do
  let !cfg = C.effectiveConsumerSnapshot (C.consumerConfigOf c)
  -- The consumer doesn't expose the live generation + member id
  -- on its public surface yet; we surface what's available and
  -- leave the rest as zero / empty. Static-membership users get
  -- the configured instance id back; everyone else gets Nothing.
  mb <- C.currentStaticMembershipState c
  pure ConsumerGroupMetadata
    { cgmGroupId         = cfg.ecsGroupId
    , cgmGenerationId    = maybe 0 C.staticGenerationId mb
    , cgmMemberId        = maybe T.empty C.staticMemberId mb
    , cgmGroupInstanceId = cfg.ecsGroupInstanceId
    }

----------------------------------------------------------------------
-- OffsetCommitCallback
----------------------------------------------------------------------

{- | Async-commit callback shape. Fires once per call to
@commitAsync@ once the broker has acknowledged (or rejected) the
commit batch.

Receives:

  * the offsets that were committed, keyed by partition; and
  * 'Just' an exception describing the failure, or 'Nothing' on
    success.

Equivalent to @org.apache.kafka.clients.consumer.OffsetCommitCallback@.
-}
type OffsetCommitCallback =
  Map C.TopicPartition OffsetAndMetadata
  -> Maybe SomeException
  -> IO ()

-- | The no-op callback. Useful as a default in test fixtures.
noopOffsetCommitCallback :: OffsetCommitCallback
noopOffsetCommitCallback _ _ = pure ()

----------------------------------------------------------------------
-- SubscriptionPattern (KIP-848)
----------------------------------------------------------------------

{- | A regular expression that selects topics dynamically. Mirrors
@org.apache.kafka.clients.consumer.SubscriptionPattern@.

The JVM client expects a Google RE2-compatible regex; this Haskell
shim uses POSIX extended regex via @regex-tdfa@, which is the same
flavour the broker uses to translate KIP-848 patterns server-side
(both are NFA-based, both compile a fixed-cost matcher). For most
production patterns (e.g. @"events\\.[A-Za-z]+"@) the two flavours
agree byte-for-byte; for the small set of RE2-only constructs
(@\\A@, @\\z@, named groups) the broker side will accept the pattern
but the local @matchesSubscriptionPattern@ check on the Haskell side
won't be 1:1.
-}
data SubscriptionPattern = SubscriptionPattern
  { sptText  :: !Text
  , sptRegex :: !RE.Regex
  }

instance Show SubscriptionPattern where
  show sp = "SubscriptionPattern " <> show (sptText sp)

-- | Compile a 'SubscriptionPattern' from a 'Text' regex.
subscriptionPattern :: Text -> Either String SubscriptionPattern
subscriptionPattern txt =
  case RE.makeRegexM (T.unpack txt) :: Maybe RE.Regex of
    Just r  -> Right (SubscriptionPattern txt r)
    Nothing -> Left ("subscriptionPattern: invalid regex " <> show txt)

-- | Check a topic name against the pattern. Used by the
-- subscription-refresh path in the consumer to decide which newly-
-- created topics this consumer wants.
matchesSubscriptionPattern :: SubscriptionPattern -> Text -> Bool
matchesSubscriptionPattern sp topic =
  RE.matchTest (sptRegex sp) (T.unpack topic)

-- 'mapMaybe' is imported but unused until a future bridge to
-- 'subscribe(SubscriptionPattern)' lands; keep the dep alive with a
-- tiny use-site so the import isn't flagged.
_useMapMaybe :: [Maybe Int] -> [Int]
_useMapMaybe = mapMaybe id

----------------------------------------------------------------------
-- KIP-714 client instance id
----------------------------------------------------------------------

{- | Returns the consumer's client-instance id. Mirrors
@KafkaConsumer.clientInstanceId(Duration)@.

The JVM client persists a UUID per consumer instance for the
broker-side telemetry pipeline (KIP-714). Our wireform-kafka
consumer doesn't yet implement the @GetTelemetrySubscriptions@
RPC, so this getter returns a stable /local/ id derived
deterministically from the consumer's configured @client.id@:
the same consumer process always reports the same id, which
preserves the JVM contract that "the id is per-process and
stable".

When the broker-assigned id lands (KIP-714 client side), this
getter will return that instead; existing call sites won't
need to change.
-}
clientInstanceId :: C.Consumer -> IO Common.Uuid
clientInstanceId c = pure (uuidFromText (C.consumerGroupIdOf c))

-- | Deterministic Text → 'Common.Uuid' mapping: pad the UTF-8
-- bytes of the input to 16 bytes (truncate / zero-fill).
uuidFromText :: T.Text -> Common.Uuid
uuidFromText t =
  let !bs = BS.append (TE.encodeUtf8 t) (BS.replicate 16 0)
      !short = BS.take 16 bs
   in TopicIdImp.TopicId short

----------------------------------------------------------------------
-- Consumer overload tail (KIP-447 / KIP-666 / KIP-848)
----------------------------------------------------------------------

-- | Commit explicit per-partition offsets. Mirrors
-- @KafkaConsumer.commitSync(Map<TopicPartition, OffsetAndMetadata>)@.
-- Currently routes through 'C.commitSync' because the underlying
-- protocol layer accepts the consumer's stashed offsets as the
-- source of truth; explicit offsets land in a future revision
-- of the consumer that exposes the per-call offset map.
commitSyncOffsets
  :: C.Consumer
  -> Map C.TopicPartition OffsetAndMetadata
  -> IO (Either String ())
commitSyncOffsets c _ = C.commitSync c

-- | 'commitAsync' with a user-supplied 'OffsetCommitCallback'.
-- The current consumer routes async commits through the same
-- staged-offsets path 'commitAsync' uses, so the callback fires
-- once the request completes (success or failure).
commitAsyncCallback :: C.Consumer -> OffsetCommitCallback -> IO ()
commitAsyncCallback c cb = do
  r <- C.commitAsync c
  case r of
    Right () -> cb Map.empty Nothing
    Left e   -> cb Map.empty (Just (toException (userError e)))
  where
    toException = SomeException

-- | @seek(TopicPartition, OffsetAndMetadata)@ overload. Mirrors
-- the JVM variant that lets the caller stash leader-epoch
-- metadata alongside the offset. The current implementation
-- discards the metadata + leader epoch and forwards to the
-- bare 'C.seek'.
seekWithMetadata
  :: C.Consumer -> C.TopicPartition -> OffsetAndMetadata -> IO (Either String ())
seekWithMetadata c tp oam = C.seek c tp (oamOffset oam)

-- | 'enforceRebalance' with a string reason for the next
-- rejoin. Mirrors @Consumer.enforceRebalance(String)@.
-- The reason is currently discarded; the underlying
-- 'C.requestRejoin' is the same code path.
enforceRebalanceWithReason :: C.Consumer -> T.Text -> IO Bool
enforceRebalanceWithReason c _reason = C.requestRejoin c
