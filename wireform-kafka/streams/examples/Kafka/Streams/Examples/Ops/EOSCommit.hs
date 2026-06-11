{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Streams.Examples.Ops.EOSCommit
Description : Exactly-once-v2 commit cycle visibility

Spins up the same passthrough topology twice against the
shared 'Kafka.Streams.Mock.Cluster.MockCluster', once in
at-least-once mode and once in exactly-once-v2 mode. Both
drivers emit identical output to @out@; the only difference is
/when/ a downstream 'ReadCommitted' consumer can see it.

The demo then injects a commit-time fault on the EOS driver
and shows that the corresponding records are aborted: a
read-committed peek sees nothing for the aborted batch.

Mirror of 'Streams.MockDriverModesSpec' but printed as a
runnable operations narrative rather than a tasty test.
-}
module Kafka.Streams.Examples.Ops.EOSCommit (
  runDemo,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Kafka.Streams.Examples.Ops.Helpers
import Kafka.Streams.Imperative
import Kafka.Streams.Mock.Cluster (
  GroupId (..),
  TxnId (..),
  dumpPartition,
  newMockCluster,
  srValue,
 )
import Kafka.Streams.Mock.Consumer
import Kafka.Streams.Mock.Fault
import Kafka.Streams.Mock.StreamsDriver


runDemo :: IO ()
runDemo = do
  section "EOSCommitDemo"

  bullet "Mode 1: at-least-once"
  cluster1 <- newMockCluster 1
  fp1 <- noFaults
  topo1 <- passthroughTopo
  alo <- newMockStreamsDriver cluster1 fp1 topo1 "ops-alo" 1
  mapM_
    (\v -> externalSend alo (topicName "in") 0 Nothing (bytes v) ts0)
    (["alpha", "bravo"] :: [Text])
  runUntilQuiet alo
  rc1 <- newMockConsumer cluster1 fp1 (GroupId "alo-peek") ReadCommitted 100
  subscribeMC rc1 [topicName "out"]
  PollResult rs1 _ <- pollMC rc1
  bullet
    ( "    read-committed observer sees: "
        <> show (map (\(_, _, sr) -> unbytes (srValue sr)) rs1)
    )
  closeMockDriver alo

  bullet "Mode 2: exactly-once-v2"
  cluster2 <- newMockCluster 1
  fp2 <- noFaults
  topo2 <- passthroughTopo
  let txn = TxnId "ops-eos-tx"
  eos <- newMockStreamsDriverEOS cluster2 fp2 topo2 "ops-eos" txn 1
  mapM_
    (\v -> externalSend eos (topicName "in") 0 Nothing (bytes v) ts0)
    (["gamma", "delta"] :: [Text])
  runUntilQuiet eos
  rc2 <- newMockConsumer cluster2 fp2 (GroupId "eos-peek") ReadCommitted 100
  subscribeMC rc2 [topicName "out"]
  PollResult rs2 _ <- pollMC rc2
  bullet
    ( "    read-committed observer sees: "
        <> show (map (\(_, _, sr) -> unbytes (srValue sr)) rs2)
    )

  bullet "Mode 2 + commit fault: aborts the tick"
  -- Queue a single commitTxn failure so the next commit aborts.
  addTxnCommitFault fp2 txn ErrCoordinatorNotAvailable
  _ <- externalSend eos (topicName "in") 0 Nothing (bytes ("lost" :: Text)) ts0
  runUntilQuiet eos

  -- A read-uncommitted peek does see the aborted records on the
  -- log (they're physically written before commit).
  ru <- newMockConsumer cluster2 fp2 (GroupId "eos-peek-ru") ReadUncommitted 100
  subscribeMC ru [topicName "out"]
  PollResult rsRU _ <- pollMC ru
  let ruVals = map (\(_, _, sr) -> unbytes (srValue sr)) rsRU
  bullet
    ( "    read-uncommitted peek sees "
        <> show (length ruVals)
        <> " records: "
        <> show ruVals
    )

  rc3 <- newMockConsumer cluster2 fp2 (GroupId "eos-peek-2") ReadCommitted 100
  subscribeMC rc3 [topicName "out"]
  PollResult rsRC _ <- pollMC rc3
  let rcVals = map (\(_, _, sr) -> unbytes (srValue sr)) rsRC
  bullet
    ( "    read-committed observer still sees: "
        <> show rcVals
        <> "  (the aborted batch is filtered out)"
    )

  -- Show the raw partition log too for full visibility.
  raw <- dumpPartition cluster2 (topicName "out") 0
  bullet ("    on-disk log entries: " <> show (length raw))

  closeMockDriver eos
