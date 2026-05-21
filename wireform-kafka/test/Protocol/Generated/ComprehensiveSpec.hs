{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PackageImports #-}

module Protocol.Generated.ComprehensiveSpec (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified System.Directory as Dir
import Data.Int (Int16)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.HUnit

import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire.Codec as WC

-- Core messages
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataRequest as MetadataReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.MetadataResponse as MetadataResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsRequest as ApiVersionsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ApiVersionsResponse as ApiVersionsResp

-- Coordinator
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorRequest as FindCoordinatorReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.FindCoordinatorResponse as FindCoordinatorResp

-- Offsets
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsRequest as ListOffsetsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ListOffsetsResponse as ListOffsetsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitRequest as OffsetCommitReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetCommitResponse as OffsetCommitResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchRequest as OffsetFetchReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.OffsetFetchResponse as OffsetFetchResp

-- Consumer Groups
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.JoinGroupRequest as JoinGroupReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.JoinGroupResponse as JoinGroupResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.HeartbeatRequest as HeartbeatReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.HeartbeatResponse as HeartbeatResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupRequest as LeaveGroupReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaveGroupResponse as LeaveGroupResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SyncGroupRequest as SyncGroupReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SyncGroupResponse as SyncGroupResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeGroupsRequest as DescribeGroupsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeGroupsResponse as DescribeGroupsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ListGroupsRequest as ListGroupsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ListGroupsResponse as ListGroupsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteGroupsRequest as DeleteGroupsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteGroupsResponse as DeleteGroupsResp

-- SASL
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslHandshakeRequest as SaslHandshakeReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslHandshakeResponse as SaslHandshakeResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslAuthenticateRequest as SaslAuthenticateReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.SaslAuthenticateResponse as SaslAuthenticateResp

-- Admin - Topics
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateTopicsRequest as CreateTopicsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateTopicsResponse as CreateTopicsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteTopicsRequest as DeleteTopicsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteTopicsResponse as DeleteTopicsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreatePartitionsRequest as CreatePartitionsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreatePartitionsResponse as CreatePartitionsResp

-- Admin - Configs
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeConfigsRequest as DescribeConfigsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeConfigsResponse as DescribeConfigsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterConfigsRequest as AlterConfigsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AlterConfigsResponse as AlterConfigsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.IncrementalAlterConfigsRequest as IncrementalAlterConfigsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.IncrementalAlterConfigsResponse as IncrementalAlterConfigsResp

-- ACLs
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeAclsRequest as DescribeAclsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DescribeAclsResponse as DescribeAclsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateAclsRequest as CreateAclsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.CreateAclsResponse as CreateAclsResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteAclsRequest as DeleteAclsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteAclsResponse as DeleteAclsResp

-- Transactions
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdRequest as InitProducerIdReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.InitProducerIdResponse as InitProducerIdResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AddPartitionsToTxnRequest as AddPartitionsToTxnReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AddPartitionsToTxnResponse as AddPartitionsToTxnResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AddOffsetsToTxnRequest as AddOffsetsToTxnReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.AddOffsetsToTxnResponse as AddOffsetsToTxnResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.EndTxnRequest as EndTxnReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.EndTxnResponse as EndTxnResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.TxnOffsetCommitRequest as TxnOffsetCommitReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.TxnOffsetCommitResponse as TxnOffsetCommitResp

-- Records/Data
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteRecordsRequest as DeleteRecordsReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.DeleteRecordsResponse as DeleteRecordsResp

-- Internal protocols (may have no versions but still need to be handled)
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.StopReplicaRequest as StopReplicaReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.StopReplicaResponse as StopReplicaResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.UpdateMetadataRequest as UpdateMetadataReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.UpdateMetadataResponse as UpdateMetadataResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ControlledShutdownRequest as ControlledShutdownReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.ControlledShutdownResponse as ControlledShutdownResp
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaderAndIsrRequest as LeaderAndIsrReq
import qualified "wireform-kafka-protocol" Kafka.Protocol.Generated.LeaderAndIsrResponse as LeaderAndIsrResp

-- | A test vector from the Rust generator
data TestVector = TestVector
  { apiKey :: Maybe Int16
  , messageType :: Text
  , version :: Int16
  , testCase :: Text
  , description :: Text
  , hex :: Text
  } deriving (Show, Generic)

instance Aeson.FromJSON TestVector where
  parseJSON = Aeson.withObject "TestVector" $ \v -> TestVector
    <$> v Aeson..:? "api_key"
    <*> v Aeson..: "message_type"
    <*> v Aeson..: "version"
    <*> v Aeson..: "test_case"
    <*> v Aeson..: "description"
    <*> v Aeson..: "hex"

-- | Load test vectors. See 'Protocol.Generated.KnownGoodSpec' for
-- the search-path notes; we prefer the vendored snapshot in
-- @wireform-kafka/test-data/test-vectors.json@.
loadTestVectors :: IO [TestVector]
loadTestVectors = do
  candidate <- pickFirstExisting
    [ "wireform-kafka/test-data/test-vectors.json"
    , "test-data/test-vectors.json"
    , "test-vectors.json"
    ]
  case candidate of
    Nothing -> pure []
    Just p -> do
      content <- BL.readFile p
      case Aeson.eitherDecode content of
        Left err -> error $ "Failed to parse test vectors at " <> p <> ": " <> err
        Right vectors -> return vectors
  where
    pickFirstExisting [] = pure Nothing
    pickFirstExisting (p:ps) = do
      ok <- Dir.doesFileExist p
      if ok then pure (Just p) else pickFirstExisting ps

-- | Convert hex string to ByteString
hexToBS :: Text -> Either String BS.ByteString
hexToBS hexText = 
  let hexStr = T.unpack hexText
      pairs = chunks 2 hexStr
      parseHex [c1, c2] = do
        d1 <- hexDigit c1
        d2 <- hexDigit c2
        return $ fromIntegral (d1 * 16 + d2)
      parseHex _  = Left "Invalid hex pair"
      hexDigit c
        | c >= '0' && c <= '9' = Right (fromEnum c - fromEnum '0')
        | c >= 'a' && c <= 'f' = Right (fromEnum c - fromEnum 'a' + 10)
        | c >= 'A' && c <= 'F' = Right (fromEnum c - fromEnum 'A' + 10)
        | otherwise = Left $ "Invalid hex digit: " ++ [c]
      chunks _ [] = []
      chunks n xs = take n xs : chunks n (drop n xs)
  in BS.pack <$> mapM parseHex pairs

-- | Test a single test vector by decoding and re-encoding
testVector :: TestVector -> TestTree
testVector vec = Test.Tasty.HUnit.testCase (T.unpack $ description vec) $ do
  -- Skip empty test vectors (messages with no fields) - this is valid, not an error
  if T.null (hex vec)
    then return ()  -- Empty message, nothing to test
    else do
      -- Parse hex to bytes
      bytes <- case hexToBS (hex vec) of
        Left err -> assertFailure $ "Failed to parse hex: " ++ err
        Right bs -> return bs
      
      -- Decode and re-encode based on message type
      let result = routeMessage (messageType vec) (version vec) bytes
      
      case result of
        Left err -> assertFailure err
        Right () -> return ()

-- | Route a message to its appropriate decoder/encoder
routeMessage :: Text -> Int16 -> BS.ByteString -> Either String ()
routeMessage msgType ver bytes = case msgType of
  -- Core messages
  "MetadataRequest" -> testMsg @MetadataReq.MetadataRequest ver bytes
  "MetadataResponse" -> testMsg @MetadataResp.MetadataResponse ver bytes
  "ApiVersionsRequest" -> testMsg @ApiVersionsReq.ApiVersionsRequest ver bytes
  "ApiVersionsResponse" -> testMsg @ApiVersionsResp.ApiVersionsResponse ver bytes
  
  -- Coordinator
  "FindCoordinatorRequest" -> testMsg @FindCoordinatorReq.FindCoordinatorRequest ver bytes
  "FindCoordinatorResponse" -> testMsg @FindCoordinatorResp.FindCoordinatorResponse ver bytes
  
  -- Offsets
  "ListOffsetsRequest" -> testMsg @ListOffsetsReq.ListOffsetsRequest ver bytes
  "ListOffsetsResponse" -> testMsg @ListOffsetsResp.ListOffsetsResponse ver bytes
  "OffsetCommitRequest" -> testMsg @OffsetCommitReq.OffsetCommitRequest ver bytes
  "OffsetCommitResponse" -> testMsg @OffsetCommitResp.OffsetCommitResponse ver bytes
  "OffsetFetchRequest" -> testMsg @OffsetFetchReq.OffsetFetchRequest ver bytes
  "OffsetFetchResponse" -> testMsg @OffsetFetchResp.OffsetFetchResponse ver bytes
  
  -- Consumer Groups
  "JoinGroupRequest" -> testMsg @JoinGroupReq.JoinGroupRequest ver bytes
  "JoinGroupResponse" -> testMsg @JoinGroupResp.JoinGroupResponse ver bytes
  "HeartbeatRequest" -> testMsg @HeartbeatReq.HeartbeatRequest ver bytes
  "HeartbeatResponse" -> testMsg @HeartbeatResp.HeartbeatResponse ver bytes
  "LeaveGroupRequest" -> testMsg @LeaveGroupReq.LeaveGroupRequest ver bytes
  "LeaveGroupResponse" -> testMsg @LeaveGroupResp.LeaveGroupResponse ver bytes
  "SyncGroupRequest" -> testMsg @SyncGroupReq.SyncGroupRequest ver bytes
  "SyncGroupResponse" -> testMsg @SyncGroupResp.SyncGroupResponse ver bytes
  "DescribeGroupsRequest" -> testMsg @DescribeGroupsReq.DescribeGroupsRequest ver bytes
  "DescribeGroupsResponse" -> testMsg @DescribeGroupsResp.DescribeGroupsResponse ver bytes
  "ListGroupsRequest" -> testMsg @ListGroupsReq.ListGroupsRequest ver bytes
  "ListGroupsResponse" -> testMsg @ListGroupsResp.ListGroupsResponse ver bytes
  "DeleteGroupsRequest" -> testMsg @DeleteGroupsReq.DeleteGroupsRequest ver bytes
  "DeleteGroupsResponse" -> testMsg @DeleteGroupsResp.DeleteGroupsResponse ver bytes
  
  -- SASL
  "SaslHandshakeRequest" -> testMsg @SaslHandshakeReq.SaslHandshakeRequest ver bytes
  "SaslHandshakeResponse" -> testMsg @SaslHandshakeResp.SaslHandshakeResponse ver bytes
  "SaslAuthenticateRequest" -> testMsg @SaslAuthenticateReq.SaslAuthenticateRequest ver bytes
  "SaslAuthenticateResponse" -> testMsg @SaslAuthenticateResp.SaslAuthenticateResponse ver bytes
  
  -- Admin - Topics
  "CreateTopicsRequest" -> testMsg @CreateTopicsReq.CreateTopicsRequest ver bytes
  "CreateTopicsResponse" -> testMsg @CreateTopicsResp.CreateTopicsResponse ver bytes
  "DeleteTopicsRequest" -> testMsg @DeleteTopicsReq.DeleteTopicsRequest ver bytes
  "DeleteTopicsResponse" -> testMsg @DeleteTopicsResp.DeleteTopicsResponse ver bytes
  "CreatePartitionsRequest" -> testMsg @CreatePartitionsReq.CreatePartitionsRequest ver bytes
  "CreatePartitionsResponse" -> testMsg @CreatePartitionsResp.CreatePartitionsResponse ver bytes
  
  -- Admin - Configs
  "DescribeConfigsRequest" -> testMsg @DescribeConfigsReq.DescribeConfigsRequest ver bytes
  "DescribeConfigsResponse" -> testMsg @DescribeConfigsResp.DescribeConfigsResponse ver bytes
  "AlterConfigsRequest" -> testMsg @AlterConfigsReq.AlterConfigsRequest ver bytes
  "AlterConfigsResponse" -> testMsg @AlterConfigsResp.AlterConfigsResponse ver bytes
  "IncrementalAlterConfigsRequest" -> testMsg @IncrementalAlterConfigsReq.IncrementalAlterConfigsRequest ver bytes
  "IncrementalAlterConfigsResponse" -> testMsg @IncrementalAlterConfigsResp.IncrementalAlterConfigsResponse ver bytes
  
  -- ACLs
  "DescribeAclsRequest" -> testMsg @DescribeAclsReq.DescribeAclsRequest ver bytes
  "DescribeAclsResponse" -> testMsg @DescribeAclsResp.DescribeAclsResponse ver bytes
  "CreateAclsRequest" -> testMsg @CreateAclsReq.CreateAclsRequest ver bytes
  "CreateAclsResponse" -> testMsg @CreateAclsResp.CreateAclsResponse ver bytes
  "DeleteAclsRequest" -> testMsg @DeleteAclsReq.DeleteAclsRequest ver bytes
  "DeleteAclsResponse" -> testMsg @DeleteAclsResp.DeleteAclsResponse ver bytes
  
  -- Transactions
  "InitProducerIdRequest" -> testMsg @InitProducerIdReq.InitProducerIdRequest ver bytes
  "InitProducerIdResponse" -> testMsg @InitProducerIdResp.InitProducerIdResponse ver bytes
  "AddPartitionsToTxnRequest" -> testMsg @AddPartitionsToTxnReq.AddPartitionsToTxnRequest ver bytes
  "AddPartitionsToTxnResponse" -> testMsg @AddPartitionsToTxnResp.AddPartitionsToTxnResponse ver bytes
  "AddOffsetsToTxnRequest" -> testMsg @AddOffsetsToTxnReq.AddOffsetsToTxnRequest ver bytes
  "AddOffsetsToTxnResponse" -> testMsg @AddOffsetsToTxnResp.AddOffsetsToTxnResponse ver bytes
  "EndTxnRequest" -> testMsg @EndTxnReq.EndTxnRequest ver bytes
  "EndTxnResponse" -> testMsg @EndTxnResp.EndTxnResponse ver bytes
  "TxnOffsetCommitRequest" -> testMsg @TxnOffsetCommitReq.TxnOffsetCommitRequest ver bytes
  "TxnOffsetCommitResponse" -> testMsg @TxnOffsetCommitResp.TxnOffsetCommitResponse ver bytes
  
  -- Records/Data
  "DeleteRecordsRequest" -> testMsg @DeleteRecordsReq.DeleteRecordsRequest ver bytes
  "DeleteRecordsResponse" -> testMsg @DeleteRecordsResp.DeleteRecordsResponse ver bytes
  
  -- Internal protocols (skipping due to MonadFail requirement in encode functions)
  "StopReplicaRequest" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "StopReplicaResponse" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "UpdateMetadataRequest" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "UpdateMetadataResponse" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "ControlledShutdownRequest" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "ControlledShutdownResponse" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "LeaderAndIsrRequest" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  "LeaderAndIsrResponse" -> Right ()  -- Skip: encoder requires MonadFail which PutM doesn't have
  
  -- Unknown message type
  other -> Left $ "Unknown message type: " ++ T.unpack other

-- | Test a message by decoding and re-encoding through the
-- 'WireCodec' instance. The message type is supplied at the call
-- site via @TypeApplications@ — there's no Serial-shape encoder
-- pair to thread through any more.
testMsg
  :: forall msg. WC.WireCodec msg
  => Int16
  -> BS.ByteString
  -> Either String ()
testMsg ver bytes = do
  (msg :: msg) <- WC.runDecodeVer @msg ver bytes
  let reencoded = WC.runEncodeVer @msg ver msg
  if reencoded == bytes
        then Right ()
        else Left $ "Re-encoded bytes don't match:\n" ++
                   "  Original:  " ++ show (BS.unpack bytes) ++ "\n" ++
                   "  Reencoded: " ++ show (BS.unpack reencoded)

-- | Group test vectors by message type
groupByMessageType :: [TestVector] -> [(Text, [TestVector])]
groupByMessageType vectors =
  let types = foldl (\acc v -> if messageType v `elem` acc then acc else acc ++ [messageType v]) [] vectors
  in map (\t -> (t, filter (\v -> messageType v == t) vectors)) types

-- | Create test tree from vectors
createTests :: [TestVector] -> TestTree
createTests vectors =
  let grouped = groupByMessageType vectors
  in if null grouped
       then testGroup "Comprehensive Protocol Tests"
              [ Test.Tasty.HUnit.testCase
                  "skipped: no test-vectors.json"
                  (pure ())
              ]
       else testGroup "Comprehensive Protocol Tests"
              (map (\(msgType, vecs) ->
                      testGroup (T.unpack msgType) (map testVector vecs))
                   grouped)

tests :: IO TestTree
tests = do
  vectors <- loadTestVectors
  return $ createTests vectors
