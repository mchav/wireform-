{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the producer / consumer interceptor APIs. The
-- functional cases run against the in-process accumulator +
-- record-batch encoder; the network
-- side is exercised by 'producerOnAcknowledgement' through the
-- broker-rejection path of @sendMessage@ (already covered by
-- the lifecycle tests).
module Client.InterceptorSpec (tests) where

import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Consumer as Consumer
import qualified Kafka.Client.Producer as Producer

tests :: TestTree
tests = testGroup "Interceptor APIs"
  [ testCase "ProducerConfig: default interceptor is identity"
      defaultProducerInterceptorIsIdentity
  , testCase "ProducerConfig: default onAcknowledgement is no-op"
      defaultProducerOnAckIsNoOp
  , testCase "ProducerConfig: interceptor can rewrite topic / key / value"
      interceptorCanRewriteRecord
  , testCase "ConsumerConfig: default interceptor is identity"
      defaultConsumerInterceptorIsIdentity
  , testCase "ConsumerConfig: default onCommit is no-op"
      defaultConsumerOnCommitIsNoOp
  , testCase "ConsumerConfig: interceptor can drop / rewrite records"
      consumerInterceptorCanDropRecords
  , testCase "ConsumerConfig: onCommit receives the offsets passed in"
      consumerOnCommitReceivesOffsets
  ]

defaultProducerInterceptorIsIdentity :: IO ()
defaultProducerInterceptorIsIdentity = do
  let cfg = Producer.defaultProducerConfig
      rec_ = Producer.ProducerRecord
        { topic     = "t"
        , key       = Just "k"
        , value     = "v"
        , headers   = []
        , partition = Nothing
        , timestamp = Nothing
        }
  out <- Producer.producerInterceptor cfg rec_
  out @?= rec_

defaultProducerOnAckIsNoOp :: IO ()
defaultProducerOnAckIsNoOp = do
  let cfg = Producer.defaultProducerConfig
      rec_ = Producer.ProducerRecord "t" Nothing "v" [] Nothing Nothing
  -- Has to not throw and to return ()
  Producer.producerOnAcknowledgement cfg rec_ (Left "broker timeout")

interceptorCanRewriteRecord :: IO ()
interceptorCanRewriteRecord = do
  let cfg = Producer.defaultProducerConfig
        { Producer.producerInterceptor = \r ->
            pure r
              { topic   = r.topic <> "-suffix"
              , headers = r.headers ++ [("trace-id", "abc")]
              }
        }
      input = Producer.ProducerRecord
        { topic     = "events"
        , key       = Just "k"
        , value     = "v"
        , headers   = []
        , partition = Nothing
        , timestamp = Nothing
        }
  out <- Producer.producerInterceptor cfg input
  out.topic   @?= "events-suffix"
  out.headers @?= [("trace-id", "abc")]

defaultConsumerInterceptorIsIdentity :: IO ()
defaultConsumerInterceptorIsIdentity = do
  let cfg = Consumer.defaultConsumerConfig
  out <- Consumer.consumerInterceptor cfg sampleRecords
  out @?= sampleRecords

defaultConsumerOnCommitIsNoOp :: IO ()
defaultConsumerOnCommitIsNoOp = do
  let cfg = Consumer.defaultConsumerConfig
  -- Just verifies the call doesn't throw.
  Consumer.consumerOnCommit cfg
    [(Consumer.TopicPartition "t" 0, 7)]

consumerInterceptorCanDropRecords :: IO ()
consumerInterceptorCanDropRecords = do
  let cfg = Consumer.defaultConsumerConfig
        { Consumer.consumerInterceptor = \rs ->
            pure (filter (\r -> r.key /= Just "drop") rs)
        }
      input =
        [ sampleRec "k1" "v1"
        , sampleRec "drop" "x"
        , sampleRec "k2" "v2"
        ]
  out <- Consumer.consumerInterceptor cfg input
  map (.key) out @?= [Just "k1", Just "k2"]

consumerOnCommitReceivesOffsets :: IO ()
consumerOnCommitReceivesOffsets = do
  ref <- newIORef []
  let cfg = Consumer.defaultConsumerConfig
        { Consumer.consumerOnCommit = \os ->
            atomicModifyIORef' ref (\acc -> (acc ++ os, ()))
        }
      offsets = [ (Consumer.TopicPartition "t" 0, 7)
                , (Consumer.TopicPartition "t" 1, 9)
                ]
  Consumer.consumerOnCommit cfg offsets
  got <- readIORef ref
  got @?= offsets

sampleRecords :: [Consumer.ConsumerRecord]
sampleRecords = [sampleRec "k" "v"]

sampleRec :: String -> String -> Consumer.ConsumerRecord
sampleRec k v = Consumer.ConsumerRecord
  { topic     = "t"
  , partition = 0
  , offset    = 0
  , timestamp = 0
  , key       = Just (toBs k)
  , value     = toBs v
  , headers   = []
  }
  where
    toBs = TE.encodeUtf8 . T.pack
