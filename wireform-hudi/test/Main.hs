{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Hudi.Timeline'.
module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Hudi.Timeline qualified as H
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-hudi" $
      sequence_
        [ it "instant filename: implicit completed" instantImplicitTest
        , it "instant filename: explicit state" instantExplicitTest
        , it "instant filename: malformed → Nothing" instantMalformedTest
        , it "sortInstants orders by timestamp + state" sortInstantsTest
        , it "completedInstants filters" completedFilterTest
        , it "parseCommitJson decodes WriteStats" parseCommitTest
        , it "applyCommit builds file slices per partition" applyCommitTest
        , it "applyCommit accumulates log files newest-first" logFileAccumTest
        ]


instantImplicitTest :: IO ()
instantImplicitTest =
  H.parseInstantFileName "20240106120000000.commit"
    `shouldBe` Just (H.Instant "20240106120000000" H.Commit H.Completed)


instantExplicitTest :: IO ()
instantExplicitTest =
  H.parseInstantFileName "20240106120000000.deltacommit.requested"
    `shouldBe` Just (H.Instant "20240106120000000" H.DeltaCommit H.Requested)


instantMalformedTest :: IO ()
instantMalformedTest =
  H.parseInstantFileName "no-dots-here" `shouldBe` Nothing


sortInstantsTest :: IO ()
sortInstantsTest = do
  let unsorted =
        [ H.Instant "B" H.Commit H.Completed
        , H.Instant "A" H.Commit H.Inflight
        , H.Instant "A" H.Commit H.Requested
        , H.Instant "A" H.Commit H.Completed
        , H.Instant "B" H.DeltaCommit H.Requested
        ]
      sorted = H.sortInstants unsorted
  map (\i -> (H.instantTime i, H.instantState i)) sorted
    `shouldBe` [ ("A", H.Requested)
               , ("A", H.Inflight)
               , ("A", H.Completed)
               , ("B", H.Requested)
               , ("B", H.Completed)
               ]


completedFilterTest :: IO ()
completedFilterTest = do
  let xs =
        [ H.Instant "A" H.Commit H.Completed
        , H.Instant "B" H.DeltaCommit H.Inflight
        , H.Instant "C" H.Commit H.Completed
        ]
  map H.instantTime (H.completedInstants xs) `shouldBe` ["A", "C"]


parseCommitTest :: IO ()
parseCommitTest = case H.parseCommitJson commitPayload of
  Left err -> expectationFailure err
  Right hcm -> do
    Map.size (H.hcmPartitionToWriteStats hcm) `shouldBe` 1
    let stats = Map.findWithDefault [] "p1" (H.hcmPartitionToWriteStats hcm)
    length stats `shouldBe` 1
    let s = head stats
    H.hwsFileId s `shouldBe` Just "file-1"
    H.hwsPath s `shouldBe` Just "p1/file-1.parquet"
    H.hwsNumWrites s `shouldBe` Just 100
    H.hwsPartitionPath s `shouldBe` Just "p1"
  where
    commitPayload =
      "{\"partitionToWriteStats\":{\
      \\"p1\":[{\
      \\"fileId\":\"file-1\",\"path\":\"p1/file-1.parquet\",\
      \\"partitionPath\":\"p1\",\"numWrites\":100,\"totalWriteBytes\":4096\
      \}]\
      \},\"compacted\":false,\"operationType\":\"INSERT\"}"


applyCommitTest :: IO ()
applyCommitTest = do
  let hcm =
        H.HoodieCommitMetadata
          { H.hcmPartitionToWriteStats =
              Map.fromList
                [ ("p1", [mkStat "f1" "p1/f1.parquet" "p1"])
                , ("p2", [mkStat "f2" "p2/f2.parquet" "p2"])
                ]
          , H.hcmCompacted = Nothing
          , H.hcmExtraMetadata = mempty
          , H.hcmOperationType = Nothing
          , H.hcmTotalCreateTime = Nothing
          , H.hcmTotalUpsertTime = Nothing
          , H.hcmTotalScanTime = Nothing
          , H.hcmExtra = mempty
          }
      st = H.applyCommit "ts1" hcm H.emptyTableState
  Map.keys (H.tsPartitions st) `shouldBe` ["p1", "p2"]
  H.tsLatestInstant st `shouldBe` Just "ts1"
  let p1Slice = Map.lookup "f1" =<< Map.lookup "p1" (H.tsPartitions st)
  fmap H.fsBaseFile p1Slice `shouldBe` Just (Just "p1/f1.parquet")
  fmap H.fsLatestCommit p1Slice `shouldBe` Just "ts1"


logFileAccumTest :: IO ()
logFileAccumTest = do
  let mkCommit logs =
        H.HoodieCommitMetadata
          { H.hcmPartitionToWriteStats =
              Map.singleton
                "p"
                [mkStatWithLogs "f1" Nothing logs]
          , H.hcmCompacted = Nothing
          , H.hcmExtraMetadata = mempty
          , H.hcmOperationType = Nothing
          , H.hcmTotalCreateTime = Nothing
          , H.hcmTotalUpsertTime = Nothing
          , H.hcmTotalScanTime = Nothing
          , H.hcmExtra = mempty
          }
      st =
        H.tableStateFromCommits
          [ ("ts1", mkCommit ["log.0"])
          , ("ts2", mkCommit ["log.1"])
          ]
      slice = Map.lookup "f1" =<< Map.lookup "p" (H.tsPartitions st)
  fmap H.fsLogFiles slice `shouldBe` Just ["log.1", "log.0"]
  fmap H.fsLatestCommit slice `shouldBe` Just "ts2"


-- ============================================================
-- helpers
-- ============================================================

mkStat :: Text -> Text -> Text -> H.HoodieWriteStat
mkStat fid path partition =
  H.HoodieWriteStat
    { H.hwsFileId = Just fid
    , H.hwsPath = Just path
    , H.hwsPrevCommit = Nothing
    , H.hwsPartitionPath = Just partition
    , H.hwsNumWrites = Nothing
    , H.hwsNumDeletes = Nothing
    , H.hwsNumUpdateWrites = Nothing
    , H.hwsNumInserts = Nothing
    , H.hwsTotalWriteBytes = Nothing
    , H.hwsTotalWriteErrors = Nothing
    , H.hwsFileSizeInBytes = Nothing
    , H.hwsBaseFile = Nothing
    , H.hwsLogFiles = []
    , H.hwsTotalLogRecords = Nothing
    , H.hwsTotalLogFiles = Nothing
    , H.hwsTotalLogBlocks = Nothing
    , H.hwsExtra = mempty
    }


mkStatWithLogs :: Text -> Maybe Text -> [Text] -> H.HoodieWriteStat
mkStatWithLogs fid base logs =
  (mkStat fid "" "p")
    { H.hwsBaseFile = base
    , H.hwsLogFiles = logs
    }
