{-# LANGUAGE OverloadedStrings #-}
-- | Tests for 'Hudi.Timeline'.
module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)

import qualified Hudi.Timeline as H

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "wireform-hudi"
  [ testCase "instant filename: implicit completed" instantImplicitTest
  , testCase "instant filename: explicit state" instantExplicitTest
  , testCase "instant filename: malformed → Nothing" instantMalformedTest
  , testCase "sortInstants orders by timestamp + state" sortInstantsTest
  , testCase "completedInstants filters" completedFilterTest
  , testCase "parseCommitJson decodes WriteStats" parseCommitTest
  , testCase "applyCommit builds file slices per partition" applyCommitTest
  , testCase "applyCommit accumulates log files newest-first" logFileAccumTest
  ]

instantImplicitTest :: Assertion
instantImplicitTest =
  H.parseInstantFileName "20240106120000000.commit"
    @?= Just (H.Instant "20240106120000000" H.Commit H.Completed)

instantExplicitTest :: Assertion
instantExplicitTest =
  H.parseInstantFileName "20240106120000000.deltacommit.requested"
    @?= Just (H.Instant "20240106120000000" H.DeltaCommit H.Requested)

instantMalformedTest :: Assertion
instantMalformedTest =
  H.parseInstantFileName "no-dots-here" @?= Nothing

sortInstantsTest :: Assertion
sortInstantsTest = do
  let unsorted =
        [ H.Instant "B" H.Commit      H.Completed
        , H.Instant "A" H.Commit      H.Inflight
        , H.Instant "A" H.Commit      H.Requested
        , H.Instant "A" H.Commit      H.Completed
        , H.Instant "B" H.DeltaCommit H.Requested
        ]
      sorted = H.sortInstants unsorted
  map (\i -> (H.instantTime i, H.instantState i)) sorted
    @?= [ ("A", H.Requested)
        , ("A", H.Inflight)
        , ("A", H.Completed)
        , ("B", H.Requested)
        , ("B", H.Completed)
        ]

completedFilterTest :: Assertion
completedFilterTest = do
  let xs =
        [ H.Instant "A" H.Commit      H.Completed
        , H.Instant "B" H.DeltaCommit H.Inflight
        , H.Instant "C" H.Commit      H.Completed
        ]
  map H.instantTime (H.completedInstants xs) @?= ["A", "C"]

parseCommitTest :: Assertion
parseCommitTest = case H.parseCommitJson commitPayload of
  Left err  -> assertFailure err
  Right hcm -> do
    Map.size (H.hcmPartitionToWriteStats hcm) @?= 1
    let stats = Map.findWithDefault [] "p1" (H.hcmPartitionToWriteStats hcm)
    length stats @?= 1
    let s = head stats
    H.hwsFileId s        @?= Just "file-1"
    H.hwsPath s          @?= Just "p1/file-1.parquet"
    H.hwsNumWrites s     @?= Just 100
    H.hwsPartitionPath s @?= Just "p1"
  where
    commitPayload =
      "{\"partitionToWriteStats\":{\
      \\"p1\":[{\
      \\"fileId\":\"file-1\",\"path\":\"p1/file-1.parquet\",\
      \\"partitionPath\":\"p1\",\"numWrites\":100,\"totalWriteBytes\":4096\
      \}]\
      \},\"compacted\":false,\"operationType\":\"INSERT\"}"

applyCommitTest :: Assertion
applyCommitTest = do
  let hcm = H.HoodieCommitMetadata
        { H.hcmPartitionToWriteStats = Map.fromList
            [ ("p1", [mkStat "f1" "p1/f1.parquet" "p1"])
            , ("p2", [mkStat "f2" "p2/f2.parquet" "p2"])
            ]
        , H.hcmCompacted       = Nothing
        , H.hcmExtraMetadata   = mempty
        , H.hcmOperationType   = Nothing
        , H.hcmTotalCreateTime = Nothing
        , H.hcmTotalUpsertTime = Nothing
        , H.hcmTotalScanTime   = Nothing
        , H.hcmExtra           = mempty
        }
      st = H.applyCommit "ts1" hcm H.emptyTableState
  Map.keys (H.tsPartitions st) @?= ["p1", "p2"]
  H.tsLatestInstant st @?= Just "ts1"
  let p1Slice = Map.lookup "f1" =<< Map.lookup "p1" (H.tsPartitions st)
  fmap H.fsBaseFile     p1Slice @?= Just (Just "p1/f1.parquet")
  fmap H.fsLatestCommit p1Slice @?= Just "ts1"

logFileAccumTest :: Assertion
logFileAccumTest = do
  let mkCommit logs = H.HoodieCommitMetadata
        { H.hcmPartitionToWriteStats = Map.singleton "p"
            [ mkStatWithLogs "f1" Nothing logs ]
        , H.hcmCompacted       = Nothing
        , H.hcmExtraMetadata   = mempty
        , H.hcmOperationType   = Nothing
        , H.hcmTotalCreateTime = Nothing
        , H.hcmTotalUpsertTime = Nothing
        , H.hcmTotalScanTime   = Nothing
        , H.hcmExtra           = mempty
        }
      st = H.tableStateFromCommits
        [ ("ts1", mkCommit ["log.0"])
        , ("ts2", mkCommit ["log.1"])
        ]
      slice = Map.lookup "f1" =<< Map.lookup "p" (H.tsPartitions st)
  fmap H.fsLogFiles     slice @?= Just ["log.1", "log.0"]
  fmap H.fsLatestCommit slice @?= Just "ts2"

-- ============================================================
-- helpers
-- ============================================================

mkStat :: Text -> Text -> Text -> H.HoodieWriteStat
mkStat fid path partition = H.HoodieWriteStat
  { H.hwsFileId           = Just fid
  , H.hwsPath             = Just path
  , H.hwsPrevCommit       = Nothing
  , H.hwsPartitionPath    = Just partition
  , H.hwsNumWrites        = Nothing
  , H.hwsNumDeletes       = Nothing
  , H.hwsNumUpdateWrites  = Nothing
  , H.hwsNumInserts       = Nothing
  , H.hwsTotalWriteBytes  = Nothing
  , H.hwsTotalWriteErrors = Nothing
  , H.hwsFileSizeInBytes  = Nothing
  , H.hwsBaseFile         = Nothing
  , H.hwsLogFiles         = []
  , H.hwsTotalLogRecords  = Nothing
  , H.hwsTotalLogFiles    = Nothing
  , H.hwsTotalLogBlocks   = Nothing
  , H.hwsExtra            = mempty
  }

mkStatWithLogs :: Text -> Maybe Text -> [Text] -> H.HoodieWriteStat
mkStatWithLogs fid base logs = (mkStat fid "" "p")
  { H.hwsBaseFile = base
  , H.hwsLogFiles = logs
  }
