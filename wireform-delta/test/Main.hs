{-# LANGUAGE OverloadedStrings #-}
-- | Tests for 'Delta.Log'.
--
-- These cover the public surface — action variants, the
-- 'TableSnapshot' replay, the schema-string decoder, and the
-- @_last_checkpoint@ parser — without trying to test things
-- inherent to aeson (e.g. \"ints round-trip through JSON\").
module Main (main) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM

import qualified Delta.Log as D

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "wireform-delta"
  [ testCase "parseLogLine: add"            addLineTest
  , testCase "parseLogLine: remove"         removeLineTest
  , testCase "parseLogLine: protocol"       protocolLineTest
  , testCase "parseLogLine: metaData"       metaDataLineTest
  , testCase "parseLogLine: commitInfo"     commitInfoLineTest
  , testCase "parseLogLine: txn"            txnLineTest
  , testCase "parseLogLine: cdc"            cdcLineTest
  , testCase "snapshot fold collapses add+remove" snapshotFoldTest
  , testCase "snapshot reflects metadata + protocol" snapshotProtoMetaTest
  , testCase "schemaString decodes nested types" schemaTest
  , testCase "_last_checkpoint parser"      lastCheckpointTest
  , testCase "AddStats decoder"             addStatsTest
  ]

-- ============================================================
-- Action parser cases
-- ============================================================

addLineTest :: Assertion
addLineTest = case D.parseLogLine addJson of
  Just (D.ActionAdd a) -> do
    D.addPath a              @?= "data/file.parquet"
    D.addSize a              @?= 1024
    D.addModificationTime a  @?= 1700000000000
    D.addDataChange a        @?= True
    D.addStats a             @?= Just "{\"numRecords\":42}"
    Map.lookup "p" (D.addPartitionValues a) @?= Just (Just "v")
  _ -> assertFailure "expected ActionAdd"
  where
    addJson = "{\"add\":{\"path\":\"data/file.parquet\",\"size\":1024,\"modificationTime\":1700000000000,\"dataChange\":true,\"stats\":\"{\\\"numRecords\\\":42}\",\"partitionValues\":{\"p\":\"v\"}}}"

removeLineTest :: Assertion
removeLineTest = case D.parseLogLine removeJson of
  Just (D.ActionRemove r) -> do
    D.removePath r              @?= "data/old.parquet"
    D.removeDeletionTimestamp r @?= Just 1700000005000
  _ -> assertFailure "expected ActionRemove"
  where
    removeJson = "{\"remove\":{\"path\":\"data/old.parquet\",\"deletionTimestamp\":1700000005000,\"dataChange\":false}}"

protocolLineTest :: Assertion
protocolLineTest = case D.parseLogLine protoJson of
  Just (D.ActionProtocol p) -> do
    D.pMinReaderVersion p @?= 3
    D.pMinWriterVersion p @?= 7
    D.pReaderFeatures p   @?= ["deletionVectors"]
    D.pWriterFeatures p   @?= ["deletionVectors","columnMapping"]
  _ -> assertFailure "expected ActionProtocol"
  where
    protoJson = "{\"protocol\":{\"minReaderVersion\":3,\"minWriterVersion\":7,\"readerFeatures\":[\"deletionVectors\"],\"writerFeatures\":[\"deletionVectors\",\"columnMapping\"]}}"

metaDataLineTest :: Assertion
metaDataLineTest = case D.parseLogLine mdJson of
  Just (D.ActionMetaData md) -> do
    D.mdId md               @?= "abc-uuid"
    D.mdPartitionColumns md @?= ["p"]
    D.mdSchemaString md     @?= "{\"type\":\"struct\",\"fields\":[]}"
  _ -> assertFailure "expected ActionMetaData"
  where
    mdJson = "{\"metaData\":{\"id\":\"abc-uuid\",\"format\":{\"provider\":\"parquet\",\"options\":{}},\"schemaString\":\"{\\\"type\\\":\\\"struct\\\",\\\"fields\\\":[]}\",\"partitionColumns\":[\"p\"],\"configuration\":{}}}"

commitInfoLineTest :: Assertion
commitInfoLineTest = case D.parseLogLine ciJson of
  Just (D.ActionCommitInfo ci) -> do
    D.ciTimestamp     ci @?= Just 1700000000000
    D.ciOperation     ci @?= Just "WRITE"
    D.ciIsBlindAppend ci @?= Just True
  _ -> assertFailure "expected ActionCommitInfo"
  where
    ciJson = "{\"commitInfo\":{\"timestamp\":1700000000000,\"operation\":\"WRITE\",\"operationParameters\":{},\"isBlindAppend\":true}}"

txnLineTest :: Assertion
txnLineTest = case D.parseLogLine txnJson of
  Just (D.ActionTxn t) -> do
    D.txnAppId t   @?= "stream-1"
    D.txnVersion t @?= 7
  _ -> assertFailure "expected ActionTxn"
  where
    txnJson = "{\"txn\":{\"appId\":\"stream-1\",\"version\":7,\"lastUpdated\":1700000000000}}"

cdcLineTest :: Assertion
cdcLineTest = case D.parseLogLine cdcJson of
  Just (D.ActionCdc c) -> do
    D.cdcPath c       @?= "_change_data/cdc-1.parquet"
    D.cdcSize c       @?= 2048
    D.cdcDataChange c @?= False
  _ -> assertFailure "expected ActionCdc"
  where
    cdcJson = "{\"cdc\":{\"path\":\"_change_data/cdc-1.parquet\",\"size\":2048,\"dataChange\":false,\"partitionValues\":{}}}"

-- ============================================================
-- Snapshot fold
-- ============================================================

snapshotFoldTest :: Assertion
snapshotFoldTest = do
  let actions =
        [ D.ActionAdd D.AddAction
            { D.addPath = "data/a.parquet"
            , D.addSize = 100
            , D.addModificationTime = 1
            , D.addDataChange = True
            , D.addStats = Nothing
            , D.addPartitionValues = Map.empty
            , D.addTags = Map.empty
            , D.addDeletionVector = Nothing
            }
        , D.ActionAdd D.AddAction
            { D.addPath = "data/b.parquet"
            , D.addSize = 200
            , D.addModificationTime = 2
            , D.addDataChange = True
            , D.addStats = Nothing
            , D.addPartitionValues = Map.empty
            , D.addTags = Map.empty
            , D.addDeletionVector = Nothing
            }
        , D.ActionRemove D.RemoveAction
            { D.removePath = "data/a.parquet"
            , D.removeDeletionTimestamp = Just 3
            , D.removeDataChange = True
            , D.removeExtendedFileMetadata = Nothing
            , D.removeSize = Nothing
            , D.removePartitionValues = Map.empty
            }
        ]
      snap = D.snapshotFromActions actions
  Map.keys (D.tsFiles snap) @?= ["data/b.parquet"]
  map D.addPath (D.activeFiles actions) @?= ["data/b.parquet"]

snapshotProtoMetaTest :: Assertion
snapshotProtoMetaTest = do
  let proto = D.ActionProtocol D.ProtocolAction
        { D.pMinReaderVersion = 1
        , D.pMinWriterVersion = 2
        , D.pReaderFeatures = []
        , D.pWriterFeatures = []
        }
      md = D.ActionMetaData D.MetaDataAction
        { D.mdId               = "u"
        , D.mdName             = Just "tbl"
        , D.mdDescription      = Nothing
        , D.mdFormat           = Nothing
        , D.mdSchemaString     = "{\"type\":\"struct\",\"fields\":[]}"
        , D.mdPartitionColumns = []
        , D.mdConfiguration    = Map.empty
        , D.mdCreatedTime      = Nothing
        }
      snap = D.snapshotFromActions [proto, md]
  D.pMinWriterVersion <$> D.tsProtocol snap @?= Just 2
  fmap D.mdName (D.tsMetaData snap)         @?= Just (Just "tbl")

-- ============================================================
-- Schema-string decoder
-- ============================================================

schemaTest :: Assertion
schemaTest = case D.parseDeltaSchema schemaJson of
  Left err -> assertFailure err
  Right s  -> do
    let fs = D.dsFields s
    length fs @?= 4
    let f0 = head fs
    D.dfName f0     @?= "id"
    D.dfType f0     @?= D.DTLong
    D.dfNullable f0 @?= False
    let f1 = fs !! 1
    D.dfType f1 @?= D.DTString
    let f2 = fs !! 2
    case D.dfType f2 of
      D.DTArray D.DTString _ -> pure ()
      other -> assertFailure ("unexpected tags type: " ++ show other)
    let f3 = fs !! 3
    D.dfType f3 @?= D.DTDecimal 10 2
  where
    schemaJson =
      "{\"type\":\"struct\",\"fields\":[\
      \{\"name\":\"id\",\"type\":\"long\",\"nullable\":false,\"metadata\":{}},\
      \{\"name\":\"name\",\"type\":\"string\",\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"tags\",\"type\":{\"type\":\"array\",\"elementType\":\"string\",\"containsNull\":true},\"nullable\":true,\"metadata\":{}},\
      \{\"name\":\"amount\",\"type\":\"decimal(10,2)\",\"nullable\":true,\"metadata\":{}}\
      \]}"

-- ============================================================
-- _last_checkpoint
-- ============================================================

lastCheckpointTest :: Assertion
lastCheckpointTest = case D.parseLastCheckpoint payload of
  Nothing -> assertFailure "expected Just"
  Just lc -> do
    D.lcVersion       lc @?= 12
    D.lcSize          lc @?= 100
    D.lcParts         lc @?= Just 4
    D.lcSizeInBytes   lc @?= Just 1024
    D.lcNumOfAddFiles lc @?= Just 90
  where
    payload = "{\"version\":12,\"size\":100,\"parts\":4,\"sizeInBytes\":1024,\"numOfAddFiles\":90}"

-- ============================================================
-- AddStats
-- ============================================================

addStatsTest :: Assertion
addStatsTest = case decodeAddStats statsJson of
  Just s -> do
    D.asNumRecords s @?= Just 1000
    HM.lookup "id" (D.asNullCount s) @?= Just 3
  Nothing -> assertFailure "expected to decode stats"
  where
    statsJson = "{\"numRecords\":1000,\"minValues\":{\"id\":1},\"maxValues\":{\"id\":999},\"nullCount\":{\"id\":3}}"

decodeAddStats :: BL.ByteString -> Maybe D.AddStats
decodeAddStats bs = case Aeson.decode bs of
  Just v -> case Aeson.fromJSON v of
    Aeson.Success a -> Just a
    Aeson.Error _   -> Nothing
  Nothing -> Nothing
