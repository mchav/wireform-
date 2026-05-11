{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Discovery.Subscription
-- Description : Subscription-userdata blob for cross-instance IQ
--
-- The streams partition assignor (KIP-429) carries a binary
-- /SubscriptionInfo/ blob in every JoinGroup request. The
-- blob includes — among other things — the instance's
-- @application.server@ host:port and the state-store names it
-- has materialised, which is what KIP-535 cross-instance
-- interactive-query routing keys off.
--
-- This module provides a self-describing encode / decode for
-- a /streams-port subscription/ blob that the assignor on the
-- leader can read off every peer's JoinGroup and re-emit so
-- every instance sees the full topology of who-owns-what.
--
-- Wire format (versioned, length-prefixed for forward-compat):
--
-- @
-- version :: int8
-- host    :: utf8-string (len-prefixed int16)
-- port    :: int32
-- stores  :: array<utf8-string>  (count int16, then strings)
-- topics  :: array<utf8-string>  (count int16, then strings)
-- partitions :: array<(string, int32)>  (count int16, then pairs)
-- @
--
-- The format is /our/ format, not the JVM Streams one, so two
-- different ports can't share a consumer group. That's the
-- expected scope: this is one Haskell streams app talking to
-- itself across N instances.
module Kafka.Streams.Discovery.Subscription
  ( SubscriptionInfo (..)
  , encodeSubscriptionInfo
  , decodeSubscriptionInfo
  ) where

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int8)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import qualified Kafka.Client.Consumer as KC
import Kafka.Streams.Discovery (HostInfo (..))

-- | Per-member subscription userdata.
data SubscriptionInfo = SubscriptionInfo
  { siHost         :: !HostInfo
  , siStoreNames   :: !(Set Text)
  , siSourceTopics :: !(Set Text)
  , siActive       :: !(Set KC.TopicPartition)
    -- ^ Active partitions the member is /currently/ serving
    --   (carried so the assignor can compute stickiness).
  , siStandby      :: !(Set KC.TopicPartition)
  }
  deriving stock (Eq, Show, Generic)

currentVersion :: Int8
currentVersion = 1

encodeSubscriptionInfo :: SubscriptionInfo -> ByteString
encodeSubscriptionInfo si =
  BL.toStrict $ P.runPut $ do
    P.putInt8 currentVersion
    putText (hostInfoHost (siHost si))
    P.putInt32be (fromIntegral (hostInfoPort (siHost si)))
    putTextSet (siStoreNames   si)
    putTextSet (siSourceTopics si)
    putTpSet   (siActive       si)
    putTpSet   (siStandby      si)
  where
    putText t = do
      let bs = TE.encodeUtf8 t
      P.putInt16be (fromIntegral (BL.length (BL.fromStrict bs)))
      P.putByteString bs
    putTextSet s = do
      let xs = Set.toAscList s
      P.putInt16be (fromIntegral (length xs))
      mapM_ putText xs
    putTpSet s = do
      let xs = Set.toAscList s
      P.putInt16be (fromIntegral (length xs))
      mapM_
        (\tp -> do
           putText (KC.tpTopic tp)
           P.putInt32be (KC.tpPartition tp))
        xs

-- | Decode the wire format. Returns 'Left' with a reason on
-- malformed input or an unknown version.
decodeSubscriptionInfo
  :: ByteString -> Either String SubscriptionInfo
decodeSubscriptionInfo bs =
  case G.runGetOrFail getSI (BL.fromStrict bs) of
    Left  (_, _, err) -> Left err
    Right (_, _, si)  -> Right si
  where
    getSI = do
      v <- G.getInt8
      if v /= currentVersion
        then fail ("SubscriptionInfo: unknown version " <> show v)
        else do
          host <- getText
          port <- fromIntegral <$> G.getInt32be
          stores <- getTextSet
          topics <- getTextSet
          actives <- getTpSet
          standbys <- getTpSet
          pure SubscriptionInfo
            { siHost = HostInfo host port
            , siStoreNames = stores
            , siSourceTopics = topics
            , siActive  = actives
            , siStandby = standbys
            }
    getText = do
      !n <- fromIntegral <$> G.getInt16be
      bs_ <- G.getByteString n
      case TE.decodeUtf8' bs_ of
        Right t -> pure t
        Left e  -> fail ("invalid utf-8: " <> show e)
    getTextSet = do
      !n <- G.getInt16be
      Set.fromList <$> sequence
        [ getText | _ <- [1 .. fromIntegral n :: Int] ]
    getTpSet = do
      !n <- G.getInt16be
      pairs <- sequence
        [ do t <- getText
             p <- G.getInt32be
             pure (KC.TopicPartition t p)
        | _ <- [1 .. fromIntegral n :: Int]
        ]
      pure (Set.fromList pairs)

-- Silence unused-import warning for Int16; kept available for
-- forward-compat fields.
_keepInt16 :: Int16
_keepInt16 = 0
