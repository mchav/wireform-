{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.Mock.Producer
-- Description : Streams-side wrappers for 'Kafka.Client.Mock.Producer'
module Kafka.Streams.Mock.Producer
  ( P.MockProducer
  , P.newMockProducer
  , P.MockProduceResult (..)
  , sendMock
  , sendMockH
  , P.flushMock
    -- * Transactions
  , P.beginTxnMP
  , P.commitTxnMP
  , P.abortTxnMP
  , P.isInTxnMP
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Text

import qualified Kafka.Client.Mock.Producer as P
import Kafka.Streams.Time (Timestamp (..))
import Kafka.Streams.Types (TopicName, unTopicName)

sendMock
  :: P.MockProducer
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> IO P.MockProduceResult
sendMock p t part k v (Timestamp ts) =
  P.sendMock p (unTopicName t) part k v ts

sendMockH
  :: P.MockProducer
  -> TopicName
  -> Int32
  -> Maybe ByteString
  -> ByteString
  -> Timestamp
  -> [(Data.Text.Text, ByteString)]
  -> IO P.MockProduceResult
sendMockH p t part k v (Timestamp ts) hdrs =
  P.sendMockH p (unTopicName t) part k v ts hdrs
