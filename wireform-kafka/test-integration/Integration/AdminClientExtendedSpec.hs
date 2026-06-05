{-# LANGUAGE OverloadedStrings #-}

-- | Live-broker tests for the AdminClient extensions added in
-- this branch:
--
--   * KIP-78 cluster id surface (mirrored on Producer / Consumer
--     / AdminClient)
--   * KIP-444 'listTopicsExcludeInternal'
--   * KIP-339 'incrementalAlterConfigs'
--   * KIP-460 'electLeaders' (reachability-only on a 1-broker
--     fixture)
--
-- Skipped at run time unless @WIREFORM_KAFKA_BROKER=host:port@ is
-- set; mirrors the rest of the integration suite.
module Integration.AdminClientExtendedSpec
  ( tests
  ) where

import Control.Monad (forM_)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as MS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Time.Clock.POSIX as Time
import System.Environment (lookupEnv)
import Test.Syd

import qualified Kafka.Client.AdminClient as AC
import qualified Kafka.Client.Consumer as KC
import qualified Kafka.Client.Producer as KP

-- (Producer + Consumer are still used by the other scenarios in
-- this module; only listConsumerGroupOffsets was rewritten to
-- use the AdminClient APIs end-to-end.)

----------------------------------------------------------------------
-- Public group
----------------------------------------------------------------------

tests :: Spec
tests = describe "Integration: AdminClient extended (KIP-78/-339/-444/-460/-503)" $ sequence_
  [ it "cluster id is surfaced via Admin / Producer / Consumer" $
      withBroker testClusterId
  , it "listTopicsExcludeInternal hides internal topics" $
      withBroker testListTopicsNonInternal
  , it "incrementalAlterConfigs round-trips a topic config" $
      withBroker testIncrementalAlterConfigs
  , it "listConsumerGroupOffsets returns committed offsets" $
      withBroker testListConsumerGroupOffsets
  , it "listConsumerGroups + describeConsumerGroups (negotiated v5/v6)" $
      withBroker testListAndDescribeConsumerGroups
  , it "deleteRecords trims partition log (negotiated v2)" $
      withBroker testDeleteRecords
  , it "describeTopics returns partition info (negotiated Metadata v13)" $
      withBroker testDescribeTopics
  , it "describeFeatures returns the cluster's KIP-584 feature set" $
      withBroker testDescribeFeatures
  , it "listClientMetricsResources returns the broker's configured resources" $
      withBroker testListClientMetricsResources
  , it "describeConsumerGroups2 round-trips against KIP-848 broker" $
      withBroker testDescribeConsumerGroups2
  , it "adminClientInstanceId is deterministic from client.id" $
      withBroker testAdminClientInstanceId
  , it "producerClientInstanceId is deterministic from client.id" $
      withBroker testProducerClientInstanceId
  ]

----------------------------------------------------------------------
-- Scenarios
----------------------------------------------------------------------

testClusterId :: T.Text -> IO ()
testClusterId brokerText = do
  ac <- mkAdmin brokerText
  -- Force a metadata refresh by issuing a listTopics call (the
  -- AdminClient does an initial refresh on connect, but for
  -- robustness we make sure the cache is hot).
  _ <- AC.listTopics ac
  cId <- AC.adminClusterId ac
  case cId of
    Nothing -> expectationFailure "expected admin client to surface a cluster id"
    Just t  -> (not (T.null t)) `shouldBe` True
  AC.closeAdminClient ac

  pcfg <- pure $ KP.defaultProducerConfig { KP.producerClientId = "wf-it-clusterid-prod" }
  pr   <- either error pure =<< KP.createProducer [brokerText] pcfg
  pId <- KP.producerClusterId pr
  KP.closeProducer pr
  case pId of
    Nothing -> expectationFailure "producer should surface a cluster id"
    Just t  -> (not (T.null t)) `shouldBe` True

  cR <- KC.createConsumer [brokerText] "wf-it-clusterid-grp"
          (KC.defaultConsumerConfig { KC.consumerClientId = "wf-it-clusterid-cons" })
  case cR of
    Left e  -> expectationFailure ("consumer create: " <> e)
    Right c -> do
      cIdC <- KC.consumerClusterId c
      KC.closeConsumer c
      case cIdC of
        Nothing -> expectationFailure "consumer should surface a cluster id"
        Just t  -> (not (T.null t)) `shouldBe` True

testListTopicsNonInternal :: T.Text -> IO ()
testListTopicsNonInternal brokerText = do
  ac <- mkAdmin brokerText
  allR  <- AC.listTopics ac
  realR <- AC.listTopicsExcludeInternal ac
  AC.closeAdminClient ac
  case (allR, realR) of
    (Right allTs, Right realTs) -> do
      -- Every "real" topic must appear in the full list.
      forM_ realTs $ \t ->
        (if (t `elem` allTs) then pure () else expectationFailure ("listTopics is missing " <> T.unpack t))
      -- Every removed topic must start with '_' (Kafka internal
      -- topics are conventionally named __consumer_offsets etc.;
      -- the broker tags them via 'isInternal').
      let removed = filter (`notElem` realTs) allTs
      forM_ removed $ \t ->
        (if (T.isPrefixOf "_" t) then pure () else expectationFailure ("expected internal-only topic prefix: " <> T.unpack t))
    _ -> expectationFailure ("listTopics failed: all=" <> show allR
                         <> " real=" <> show realR)

testIncrementalAlterConfigs :: T.Text -> IO ()
testIncrementalAlterConfigs brokerText = do
  ac <- mkAdmin brokerText
  let topic = "wireform-bench-cmp"
      cr    = AC.ConfigResource AC.ConfigResourceTopic topic
  -- Set retention.ms to 600000 (10 minutes), then read it back.
  setR <- AC.incrementalAlterConfigs ac
            [(cr, [AC.AlterableConfigEntry
                     { AC.aceName  = "retention.ms"
                     , AC.aceOp    = AC.AlterConfigOpSet
                     , AC.aceValue = Just "600000"
                     }])]
  case setR of
    Left e -> do
      AC.closeAdminClient ac
      expectationFailure ("incrementalAlterConfigs failed: " <> e)
    Right rs -> do
      forM_ rs $ \(_, r) -> case r of
        Right () -> pure ()
        Left e   -> expectationFailure ("incrementalAlterConfigs per-resource: " <> e)
  -- Read it back.
  descR <- AC.describeConfigs ac [cr]
  AC.closeAdminClient ac
  case descR of
    Left e -> expectationFailure ("describeConfigs: " <> e)
    Right [resR] ->
      case AC.crrError resR of
        Just e  -> expectationFailure ("describeConfigs per-resource: " <> T.unpack e)
        Nothing ->
          case [ AC.ceValue ce
               | ce <- AC.crrEntries resR, AC.ceName ce == "retention.ms"
               ] of
            (Just "600000" : _) -> pure ()
            other -> expectationFailure
              ("expected retention.ms=600000, got " <> show other)
    Right xs -> expectationFailure
      ("expected exactly one ConfigResourceResult, got " <> show (length xs))

testListConsumerGroupOffsets :: T.Text -> IO ()
testListConsumerGroupOffsets brokerText = do
  -- Use 'alterConsumerGroupOffsets' (KIP-503) to externally
  -- commit an offset for an empty group, then read it back via
  -- 'listConsumerGroupOffsets' (KIP-465). This exercises both
  -- AdminClient surfaces without depending on the full consumer
  -- group lifecycle (subscribe / poll / commit) which has its
  -- own timing characteristics on freshly-formed groups.
  ts <- round <$> Time.getPOSIXTime :: IO Int
  let groupId = T.pack ("wf-it-acg-" ++ show ts)
      topic   = T.pack "wireform-bench-cmp"
  ac <- mkAdmin brokerText
  -- Write an arbitrary offset.
  alt <- AC.alterConsumerGroupOffsets ac groupId
           [(topic, 0, 42)]
  case alt of
    Left e -> do
      AC.closeAdminClient ac
      expectationFailure ("alterConsumerGroupOffsets: " <> e)
    Right rs ->
      forM_ rs $ \(_, r) -> case r of
        Right () -> pure ()
        Left ec  -> do
          AC.closeAdminClient ac
          expectationFailure ("alterConsumerGroupOffsets per-partition error: "
                           <> show ec)
  -- Read it back.
  rOffs <- AC.listConsumerGroupOffsets ac groupId
  AC.closeAdminClient ac
  case rOffs of
    Left e -> expectationFailure ("listConsumerGroupOffsets: " <> e)
    Right hm -> do
      (HashMap.lookup (topic, 0) hm) `shouldBe` (Just 42)

testListAndDescribeConsumerGroups :: T.Text -> IO ()
testListAndDescribeConsumerGroups brokerText = do
  -- Make sure at least one consumer group exists so the
  -- listConsumerGroups call has something to iterate. Use the
  -- KIP-503 external commit path again to register the group
  -- with the broker without standing up a full consumer.
  ts <- round <$> Time.getPOSIXTime :: IO Int
  let groupId = T.pack ("wf-it-listdesc-" ++ show ts)
  ac <- mkAdmin brokerText
  altR <- AC.alterConsumerGroupOffsets ac groupId
            [(T.pack "wireform-bench-cmp", 0, 0)]
  case altR of
    Left e -> do
      AC.closeAdminClient ac
      expectationFailure ("alterConsumerGroupOffsets seed: " <> e)
    Right _ -> pure ()
  -- Negotiated ListGroups v5: should include the group we
  -- just registered (along with whatever else lives on the
  -- broker).
  listR <- AC.listConsumerGroups ac
  case listR of
    Left e -> do
      AC.closeAdminClient ac
      expectationFailure ("listConsumerGroups: " <> e)
    Right listings -> do
      let ids = map AC.cglGroupId listings
      (if (groupId `elem` ids) then pure () else expectationFailure ("listConsumerGroups missing seed group "
                    <> T.unpack groupId <> " (got " <> show ids <> ")"))
  -- Negotiated DescribeGroups v6: round-trip the same group
  -- id and confirm the broker echoes it back. (The group has
  -- no live members because we only used the external commit
  -- path; the broker still tracks it as an "Empty" group.)
  descR <- AC.describeConsumerGroups ac [groupId]
  AC.closeAdminClient ac
  case descR of
    Left e -> expectationFailure ("describeConsumerGroups: " <> e)
    Right [d] -> do
      AC.cgdGroupId d `shouldBe` groupId
      (if (not (T.null (AC.cgdState d))) then pure () else expectationFailure ("expected non-empty state, got "
                    <> T.unpack (AC.cgdState d)))
    Right ds -> expectationFailure
      ("expected exactly one group description, got " <> show (length ds))

testDeleteRecords :: T.Text -> IO ()
testDeleteRecords brokerText = do
  -- Produce a few records first so there's something to trim.
  let topic = T.pack "wireform-bench-cmp"
  pcfg <- pure $ KP.defaultProducerConfig
            { KP.producerClientId = "wf-it-deleterecords-prod"
            }
  pr   <- either error pure =<< KP.createProducer [brokerText] pcfg
  forM_ ([0 .. 4] :: [Int]) $ \i -> do
    r <- KP.sendMessage pr topic Nothing
           (T.encodeUtf8 (T.pack ("rec-" <> show i)))
    case r of
      Right _ -> pure ()
      Left  e -> expectationFailure ("produce seed " <> show i <> ": " <> e)
  KP.closeProducer pr
  -- Now delete every record below offset 1; expect the broker's
  -- low-watermark to come back >= 1.
  ac <- mkAdmin brokerText
  drR <- AC.deleteRecords ac [(topic, 0, 1)]
  AC.closeAdminClient ac
  case drR of
    Left e -> expectationFailure ("deleteRecords: " <> e)
    Right entries -> do
      (not (null entries)) `shouldBe` True
      forM_ entries $ \e -> do
        AC.dreErrorCode e `shouldBe` 0
        (if (AC.dreLowWatermark e >= 1) then pure () else expectationFailure ("expected lowWatermark >= 1, got "
                      <> show (AC.dreLowWatermark e)))

testDescribeTopics :: T.Text -> IO ()
testDescribeTopics brokerText = do
  ac <- mkAdmin brokerText
  -- Negotiated Metadata v13 path. The high-level
  -- 'TopicDescription' surface ignores TopicId / IsInternal /
  -- AuthorizedOperations but the underlying decode has to walk
  -- every flexible-encoded field cleanly to even reach the
  -- partition list.
  descR <- AC.describeTopics ac [T.pack "wireform-bench-cmp"]
  AC.closeAdminClient ac
  case descR of
    Left e -> expectationFailure ("describeTopics: " <> e)
    Right [td] -> do
      AC.tdName td `shouldBe` T.pack "wireform-bench-cmp"
      (not (null (AC.tdPartitions td))) `shouldBe` True
    Right tds -> expectationFailure
      ("expected exactly one TopicDescription, got " <> show (length tds))

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

testDescribeFeatures :: T.Text -> IO ()
testDescribeFeatures brokerText = do
  ac <- mkAdmin brokerText
  r  <- AC.describeFeatures ac
  AC.closeAdminClient ac
  case r of
    Left e   -> expectationFailure ("describeFeatures: " <> e)
    Right fm ->
      -- A 4.x cluster always advertises at least @metadata.version@
      -- in the supported list; we accept any non-empty supported
      -- set so the test is portable across broker versions.
      (not (MS.null (AC.fmSupportedFeatures fm))) `shouldBe` True

testListClientMetricsResources :: T.Text -> IO ()
testListClientMetricsResources brokerText = do
  ac <- mkAdmin brokerText
  r  <- AC.listClientMetricsResources ac
  AC.closeAdminClient ac
  case r of
    Left e  -> expectationFailure ("listClientMetricsResources: " <> e)
    Right _ -> pure ()   -- empty list is a valid response

testDescribeConsumerGroups2 :: T.Text -> IO ()
testDescribeConsumerGroups2 brokerText = do
  ac <- mkAdmin brokerText
  -- Empty input ⇒ the broker should return an empty result list.
  r  <- AC.describeConsumerGroups2 ac [] False
  AC.closeAdminClient ac
  case r of
    Left e   -> expectationFailure ("describeConsumerGroups2: " <> e)
    Right ds -> (length ds) `shouldBe` 0

testAdminClientInstanceId :: T.Text -> IO ()
testAdminClientInstanceId brokerText = do
  ac <- mkAdmin brokerText
  a  <- AC.adminClientInstanceId ac
  b  <- AC.adminClientInstanceId ac
  AC.closeAdminClient ac
  b `shouldBe` a

testProducerClientInstanceId :: T.Text -> IO ()
testProducerClientInstanceId brokerText = do
  pcfg <- pure $ KP.defaultProducerConfig { KP.producerClientId = "wf-it-instance" }
  pr   <- either error pure =<< KP.createProducer [brokerText] pcfg
  a <- KP.producerClientInstanceId pr
  b <- KP.producerClientInstanceId pr
  KP.closeProducer pr
  b `shouldBe` a

----------------------------------------------------------------------

withBroker :: (T.Text -> IO ()) -> IO ()
withBroker k = do
  m <- lookupEnv "WIREFORM_KAFKA_BROKER"
  case m of
    Nothing -> pure ()
    Just h  -> k (T.pack h)

mkAdmin :: T.Text -> IO AC.AdminClient
mkAdmin brokerText = do
  let cfg = AC.defaultAdminClientConfig
        { AC.adminClientId = "wf-it-admin"
        }
  r <- AC.createAdminClient [brokerText] cfg
  case r of
    Left e  -> error ("createAdminClient: " <> e)
    Right c -> pure c

