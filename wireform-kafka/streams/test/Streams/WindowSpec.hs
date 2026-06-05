{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.WindowSpec (tests) where

import qualified Data.ByteString.Char8 as BSC
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Hedgehog
import Hedgehog ((===), forAll, property, assert)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Syd
import Test.Syd.Hedgehog ()

import Kafka.Streams.Imperative
import Kafka.Streams.State.Store
  ( WindowStore (..)
  , kvIteratorToList
  , storeName
  )
import Kafka.Streams.State.Window.InMemory (inMemoryWindowStore)
import Kafka.Streams.Time
  ( Timestamp (..)
  , millis
  )
import Kafka.Streams.Window
  ( Window (..)
  , Windows (..)
  , hoppingWindows
  , slidingWindows
  , tumblingWindows
  , windowContains
  , windowOverlaps
  )

tests :: Spec
tests = describe "Window" $ sequence_
  [ describe "windowAssign" $ sequence_
      [ it "tumbling 100ms at t=0 -> [0,100)" $
          windowsAssign (tumblingWindows (millis 100)) (Timestamp 0)
            `shouldBe` [Window (Timestamp 0) (Timestamp 100)]
      , it "tumbling 100ms at t=99 -> [0,100)" $
          windowsAssign (tumblingWindows (millis 100)) (Timestamp 99)
            `shouldBe` [Window (Timestamp 0) (Timestamp 100)]
      , it "tumbling 100ms at t=100 -> [100,200)" $
          windowsAssign (tumblingWindows (millis 100)) (Timestamp 100)
            `shouldBe` [Window (Timestamp 100) (Timestamp 200)]
      , it "tumbling 100ms at t=350 -> [300,400)" $
          windowsAssign (tumblingWindows (millis 100)) (Timestamp 350)
            `shouldBe` [Window (Timestamp 300) (Timestamp 400)]
      , it "hopping size=100 advance=50 at t=125 covers two windows" $
          let ws = hoppingWindows (millis 100) (millis 50)
              out = windowsAssign ws (Timestamp 125)
           in length out `shouldBe` 2
      , it "sliding size=100 at t=200" $
          windowsAssign (slidingWindows (millis 100)) (Timestamp 200)
            `shouldBe` [Window (Timestamp 101) (Timestamp 201)]
      ]
  , describe "Window predicates" $ sequence_
      [ it "windowContains" $ do
          windowContains (Window (Timestamp 0) (Timestamp 100)) (Timestamp 0) `shouldBe` True
          windowContains (Window (Timestamp 0) (Timestamp 100)) (Timestamp 99) `shouldBe` True
          windowContains (Window (Timestamp 0) (Timestamp 100)) (Timestamp 100) `shouldBe` False
      , it "windowOverlaps" $ do
          windowOverlaps
            (Window (Timestamp 0) (Timestamp 100))
            (Window (Timestamp 50) (Timestamp 150))
            `shouldBe` True
          windowOverlaps
            (Window (Timestamp 0) (Timestamp 100))
            (Window (Timestamp 100) (Timestamp 200))
            `shouldBe` False
      , it "tumbling never overlaps consecutive windows" $ property $ do
          sz <- forAll (Gen.int64 (Range.linear 1 1_000_000))
          ts <- forAll (Gen.int64 (Range.linear 0 1_000_000_000))
          let ws = tumblingWindows (millis sz)
          case (windowsAssign ws (Timestamp ts), windowsAssign ws (Timestamp (ts + sz))) of
            ([w1], [w2]) -> assert (not (windowOverlaps w1 w2))
            _            -> Hedgehog.failure
      , it "tumbling window size is the duration" $ property $ do
          sz <- forAll (Gen.int64 (Range.linear 1 1_000_000))
          ts <- forAll (Gen.int64 (Range.linear 0 1_000_000_000))
          case windowsAssign (tumblingWindows (millis sz)) (Timestamp ts) of
            [Window (Timestamp s) (Timestamp e)] -> e - s === sz
            _                                    -> Hedgehog.failure
      ]
  , describe "WindowStore" $ sequence_
      [ it "put + fetch single" $ do
          ws <- inMemoryWindowStore @Int @Int (storeName "ws") 100 1000
          wsPut ws 1 10 (Timestamp 0)
          v <- wsFetch ws 1 (Timestamp 0)
          v `shouldBe` Just 10
      , it "fetchRange returns sorted" $ do
          ws <- inMemoryWindowStore @Int @Int (storeName "ws") 100 1000
          wsPut ws 1 10 (Timestamp 0)
          wsPut ws 1 20 (Timestamp 100)
          wsPut ws 1 30 (Timestamp 200)
          it <- wsFetchRange ws 1 (Timestamp 0) (Timestamp 200)
          xs <- kvIteratorToList it
          xs `shouldBe` [(Timestamp 0, 10), (Timestamp 100, 20), (Timestamp 200, 30)]
      , it "fetchAllRange spans keys" $ do
          ws <- inMemoryWindowStore @Int @Int (storeName "ws") 100 1000
          wsPut ws 1 10 (Timestamp 0)
          wsPut ws 2 20 (Timestamp 0)
          wsPut ws 3 30 (Timestamp 100)
          it <- wsFetchAllRange ws (Timestamp 0) (Timestamp 100)
          xs <- kvIteratorToList it
          length xs `shouldBe` 3
      , it "retention sweeps old entries" $ do
          ws <- inMemoryWindowStore @Int @Int (storeName "ws") 100 200
          wsPut ws 1 10 (Timestamp 0)
          -- This put is at t=500 with retention=200, so cutoff is t=300; the
          -- prior entry at t=0 should be swept.
          wsPut ws 1 20 (Timestamp 500)
          v0 <- wsFetch ws 1 (Timestamp 0)
          v1 <- wsFetch ws 1 (Timestamp 500)
          v0 `shouldBe` Nothing
          v1 `shouldBe` Just 20
      ]
  , describe "Session windows (driver)" $ sequence_
      [ session_aggregation_merges
      , session_aggregation_separate_sessions
      ]
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

session_aggregation_merges :: Spec
session_aggregation_merges =
  it "session aggregation merges adjacent records" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kgs = groupByKey g src
        sw = sessionWindows (millis 100)
        swks = windowedBySession sw kgs
    handle <- countSessionWindowed materialized swks
    topo <- buildTopology b
    driver <- newDriver topo "session-app"

    -- 3 records within 100ms inactivity gap merge into one session.
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v1") (Timestamp 0)   0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v2") (Timestamp 50)  0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "v3") (Timestamp 90)  0

    mStore <- getSessionStore @Text @Int64 driver (swthStore handle)
    case mStore of
      Just ss -> do
        it <- ssFindAllSessions ss (Timestamp 0) (Timestamp 200)
        rows <- kvIteratorToList it
        length rows `shouldBe` 1
        let (_, count) = head rows
        count `shouldBe` 3
      Nothing -> error "session store missing"
    closeDriver driver

session_aggregation_separate_sessions :: Spec
session_aggregation_separate_sessions =
  it "gap > inactivity opens a new session" $ do
    b <- newStreamsBuilder
    src <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
    let g = grouped textSerde textSerde
        kgs = groupByKey g src
        sw = sessionWindows (millis 100)
        swks = windowedBySession sw kgs
    handle <- countSessionWindowed materialized swks
    topo <- buildTopology b
    driver <- newDriver topo "session-app"

    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "a") (Timestamp 0)   0
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "b") (Timestamp 50)  0
    -- 200ms gap > 100ms inactivity, opens a new session
    pipeInput driver (topicName "in") (Just (bytes "k")) (bytes "c") (Timestamp 250) 0

    mStore <- getSessionStore @Text @Int64 driver (swthStore handle)
    case mStore of
      Just ss -> do
        it <- ssFindAllSessions ss (Timestamp 0) (Timestamp 1000)
        rows <- kvIteratorToList it
        length rows `shouldBe` 2
      Nothing -> error "session store missing"
    closeDriver driver

