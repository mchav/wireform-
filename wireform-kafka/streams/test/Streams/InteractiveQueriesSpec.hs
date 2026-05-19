{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.InteractiveQueriesSpec (tests) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Kafka.Streams.Imperative
import Kafka.Streams.State.Store
  ( KeyValueIterator (..)
  , kvIteratorToList
  )

tests :: TestTree
tests = testGroup "InteractiveQueries"
  [ iq_get_and_count
  , iq_concurrent_read_during_writes
  , iq_range_iterator
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

t :: Integer -> Timestamp
t = Timestamp . fromIntegral

iq_get_and_count :: TestTree
iq_get_and_count =
  testCase "queryEngineStore returns the same data as the in-task accessor" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "iq-store"))
    topo <- buildTopology b
    driver <- newDriver topo "iq-app"

    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k2")) (bytes "v2") (t 0) 0
    pipeInput driver (topicName "in") (Just (bytes "k1")) (bytes "v1updated") (t 1) 0

    mRO <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore kt)
    case mRO of
      Nothing -> error "iq store missing"
      Just ro -> do
        ro.roKvGet "k1" >>= (@?= Just "v1updated")
        ro.roKvGet "k2" >>= (@?= Just "v2")
        ro.roKvGet "k3" >>= (@?= Nothing)
        ro.roKvCount >>= (@?= 2)
    closeDriver driver

iq_concurrent_read_during_writes :: TestTree
iq_concurrent_read_during_writes =
  testCase "queryEngineStore is safe under concurrent reads + writes" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "iq-conc-store"))
    topo <- buildTopology b
    driver <- newDriver topo "iq-c-app"

    -- Pre-seed the table.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v0") (t 0) 0

    Just ro <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore kt)
    -- Coordinate via MVar: a reader thread reads in a tight loop;
    -- after every read, signals on the MVar. The main thread feeds
    -- updates and asserts that no read panics.
    finished <- newEmptyMVar
    started  <- newEmptyMVar
    _ <- forkIO $ do
      putMVar started ()
      loop 1000 (readNonNothing ro)
      putMVar finished ()
    takeMVar started
    -- Drive 1000 updates while the reader is hot.
    mapM_
      (\i ->
        pipeInput driver (topicName "in") (Just (bytes "k"))
          (bytes (T.pack ("v" <> show i)))
          (t (fromIntegral i))
          0)
      [1 .. 1000 :: Int]
    takeMVar finished
    -- Final state should be the last value we wrote.
    ro.roKvGet "k" >>= (@?= Just "v1000")
    closeDriver driver
  where
    loop :: Int -> IO () -> IO ()
    loop 0 _ = pure ()
    loop n act = act >> loop (n - 1) act

    readNonNothing ro = do
      _ <- ro.roKvGet "k"
      pure ()

iq_range_iterator :: TestTree
iq_range_iterator =
  testCase "queryEngineStore: range iterator returns expected keys" $ do
    b <- newStreamsBuilder
    kt <- tableFromTopic b (topicName "in")
            (consumed textSerde textSerde)
            (materializedAs (storeName "iq-range-store"))
    topo <- buildTopology b
    driver <- newDriver topo "iq-r-app"

    mapM_
      (\k ->
        pipeInput driver (topicName "in") (Just (bytes (T.pack k)))
          (bytes (T.pack ("v-" <> k))) (t 0) 0)
      ["a", "b", "c", "d", "e"]

    Just ro <- queryEngineStore @Text @Text (driverEngine driver) (ktableStore kt)
    it <- ro.roKvRange "b" "d"
    xs <- kvIteratorToList it
    map fst xs @?= ["b", "c", "d"]
    closeDriver driver
