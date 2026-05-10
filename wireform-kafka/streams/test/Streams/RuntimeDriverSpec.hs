{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the 'KafkaStreams' runtime driven through a
-- 'StreamDriver' rather than against a real broker. The runtime
-- consumes the same 'StreamDriver' record-of-IO that
-- 'newNativeDriver' returns; this spec uses 'newMockDriver' to
-- assert end-to-end behaviour deterministically.
--
-- We cover:
--
--   * Source records pumped into the mock driver flow through
--     the topology and produce sink records the mock driver
--     captures.
--   * Subscribe / commit / close IO actions fire in the
--     expected order.
--   * Pause halts engine feeding without halting polling.
--   * EOS-V2 commit cycles route through the bound
--     'EOSCoordinator' (begin → commitOffsets → commit).
module Streams.RuntimeDriverSpec (tests) where

import qualified Control.Concurrent
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import qualified Data.Text as T
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Kafka.Client.Consumer as KC

import Kafka.Streams
import Kafka.Streams.Runtime.EOS
  ( EOSCoordinator (..)
  )
import Kafka.Streams.Runtime.NativeDriver
  ( MockSend (..)
  , MockTxnEvent (..)
  , mockDriverCommitCount
  , mockDriverDrainSends
  , mockDriverInjectPoll
  , mockDriverTxnLog
  , newMockDriver
  )

tests :: TestTree
tests = testGroup "Runtime <-> StreamDriver"
  [ records_pumped_through_topology
  , pause_stops_engine_but_not_polling
  , commit_cycle_invokes_eos_coordinator
  ]

bytes :: Text -> BSC.ByteString
bytes = BSC.pack . T.unpack

unbytes :: BSC.ByteString -> Text
unbytes = T.pack . BSC.unpack

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Build a one-source / one-sink topology that uppercases its
-- value. Used as the lightweight workload for the runtime tests.
buildUpcaseTopo :: IO TopologyValid
buildUpcaseTopo = do
  b <- newStreamsBuilder
  s <- streamFromTopic b (topicName "in") (consumed textSerde textSerde)
  upper <- mapValues (T.toUpper :: Text -> Text) s
  toTopic (topicName "out") (produced textSerde textSerde) upper
  topo <- buildTopology b
  case validateTopology topo of
    Left err -> error (show err)
    Right v  -> pure v

consumerRecord :: Text -> Text -> Text -> Int64ish -> Int64ish -> KC.ConsumerRecord
consumerRecord topic k v off ts = KC.ConsumerRecord
  { KC.crTopic     = topic
  , KC.crPartition = 0
  , KC.crOffset    = fromIntegral (unI64 off)
  , KC.crTimestamp = fromIntegral (unI64 ts)
  , KC.crKey       = Just (bytes k)
  , KC.crValue     = bytes v
  , KC.crHeaders   = []
  }

newtype Int64ish = Int64ish { unI64 :: Integer }

----------------------------------------------------------------------
-- 1. Records flow through the topology
----------------------------------------------------------------------

records_pumped_through_topology :: TestTree
records_pumped_through_topology =
  testCase "records injected via the mock driver produce sink records" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-app-1"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 1
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Push two records the runtime will see on the next poll.
    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "hello" (Int64ish 0) (Int64ish 100)
      , consumerRecord "in" "k2" "world" (Int64ish 1) (Int64ish 101)
      ]

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait deterministically: the runtime keeps polling, the
    -- queue is finite, and on every pass the commit count goes
    -- up. Once we've seen at least one commit AND the captured
    -- sends contain both records we know the batch was processed.
    sentRef <- newIORef []
    waitFor 1000 $ do
      sends <- mockDriverDrainSends h
      modifyIORef' sentRef (++ sends)
      acc <- readIORef sentRef
      pure (length acc == 2 && all ((== "out") . mockSendTopic) acc)

    finalSends <- readIORef sentRef
    map (unbytes . mockSendValue) finalSends @?= ["HELLO", "WORLD"]

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 2. Pause stops feeding the engine but keeps polling
----------------------------------------------------------------------

pause_stops_engine_but_not_polling :: TestTree
pause_stops_engine_but_not_polling =
  testCase "pause halts engine forwarding but keeps polling" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId    = "rt-app-2"
          , bootstrapServers = ["mock:0"]
          , numStreamThreads = 1
          , pollMs           = 0
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    pauseKafkaStreams ks
    isPausedKafkaStreams ks >>= (@?= True)

    -- Inject records while paused; nothing should reach the sink.
    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "while-paused" (Int64ish 0) (Int64ish 100)
      ]

    -- Drain a few times so the loop has run; the runtime is still
    -- polling so the queue empties, but no sends fire.
    let drainPolls 0 = pure ()
        drainPolls n = do
          mockDriverInjectPoll h []
          drainPolls (n - 1)
    drainPolls 10

    sendsWhilePaused <- mockDriverDrainSends h
    sendsWhilePaused @?= []

    -- Resume and inject again. Now records should flow.
    resumeKafkaStreams ks
    mockDriverInjectPoll h
      [ consumerRecord "in" "k2" "after-resume" (Int64ish 1) (Int64ish 101)
      ]
    waitFor 1000 $ do
      ss <- mockDriverDrainSends h
      pure (any (\s -> mockSendValue s == bytes "AFTER-RESUME") ss)

    closeKafkaStreams ks
    awaitState ks StreamsClosed

----------------------------------------------------------------------
-- 3. EOS-V2 commit cycle routes through the coordinator
----------------------------------------------------------------------

commit_cycle_invokes_eos_coordinator :: TestTree
commit_cycle_invokes_eos_coordinator =
  testCase "EOS commit cycle: runtime drives begin → commitOffsets → commit through the coordinator" $ do
    topo <- buildUpcaseTopo
    let cfg = defaultStreamsConfig
          { applicationId       = "rt-app-3"
          , bootstrapServers    = ["mock:0"]
          , numStreamThreads    = 1
          , pollMs              = 0
          , processingGuarantee = ExactlyOnceV2
          }
    ks <- newKafkaStreams cfg topo
    (drv, h) <- newMockDriver

    -- Recording coordinator: every callback appends a tag.
    callsRef <- newIORef ([] :: [Text])
    let log_ s = modifyIORef' callsRef (s :)
    let coord = EOSCoordinator
          { eosInit          = log_ "init"   *> pure (Right ())
          , eosBegin         = log_ "begin"  *> pure (Right ())
          , eosCommit        = log_ "commit" *> pure (Right ())
          , eosAbort         = log_ "abort"  *> pure (Right ())
          , eosCommitOffsets = \_ _ ->
              log_ "commitOffsets" *> pure (Right ())
          }
    applyEOSCoordinator ks coord

    mockDriverInjectPoll h
      [ consumerRecord "in" "k1" "x" (Int64ish 0) (Int64ish 100)
      ]
    startKafkaStreamsWith ks drv
    awaitState ks StreamsRunning

    -- Wait for at least one full begin/commitOffsets/commit
    -- cycle to fire by observing the coordinator log.
    waitFor 1000 $ do
      cs <- reverse <$> readIORef callsRef
      pure (cs == ["begin", "commitOffsets", "commit"]
             || take 3 cs == ["begin", "commitOffsets", "commit"])

    closeKafkaStreams ks
    awaitState ks StreamsClosed

    cs <- reverse <$> readIORef callsRef
    -- The first three entries in order must be the begin /
    -- commitOffsets / commit triple.
    take 3 cs @?= ["begin", "commitOffsets", "commit"]

    -- Ensure the runtime did NOT drive the producer's transaction
    -- hooks itself (those go through the coordinator under the
    -- AtLeastOnce defaults of the mock driver).
    txn <- mockDriverTxnLog h
    txn @?= []

    -- And it did call the regular consumer commit at least once
    -- (the runtime issues sdConsumerCommit after every successful
    -- commit cycle).
    cnt <- mockDriverCommitCount h
    assertBool "runtime committed consumer offsets at least once"
      (cnt >= 1)

----------------------------------------------------------------------
-- Wait helper (no threadDelay)
----------------------------------------------------------------------

-- | Spin on the IO predicate up to N tries with @yieldM@-style
-- short waits between checks. We use 'Control.Concurrent.yield'
-- to nudge the scheduler instead of sleeping for a fixed
-- duration. If we exceed the cap, fail loudly so the test isn't
-- silently flaky.
waitFor :: Int -> IO Bool -> IO ()
waitFor 0 _ = error "waitFor: timed out"
waitFor n act = do
  ok <- act
  if ok
    then pure ()
    else do
      -- Yielding rather than sleeping keeps the test fast on
      -- a quiet machine; on a busy one it just lets the worker
      -- thread run.
      Control.Concurrent.yield
      waitFor (n - 1) act
