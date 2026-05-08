{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module: Kafka.Client.Internal.TransactionCoordinator
Description: Transaction coordinator discovery and communication

Handles communication with the Kafka transaction coordinator for:
- Coordinator discovery (FindCoordinatorRequest)
- Producer ID initialization (InitProducerIdRequest)
- Transaction lifecycle (AddPartitionsToTxnRequest, EndTxnRequest)
- Offset commits (AddOffsetsToTxnRequest, TxnOffsetCommitRequest)
-}
module Kafka.Client.Internal.TransactionCoordinator
  ( TransactionCoordinator(..)
  , TransactionCoordinatorError(..)
  , interpretCoordinatorError
  , findTransactionCoordinator
  , initProducerId
  , addPartitionsToTxn
  , endTransaction
  , addOffsetsToTxn
  , txnOffsetCommit
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Kafka.Protocol.ApiVersions (ApiVersionCache)
import qualified Kafka.Protocol.ApiVersions as AV
import Kafka.Client.Consumer (TopicPartition(..))
import qualified Kafka.Client.Internal.Request as Req
import Kafka.Network.Connection (BrokerAddress(..), Connection)
import qualified Kafka.Network.Connection as Conn
import qualified Kafka.Protocol.Primitives as P

-- Import generated protocol messages
import qualified Kafka.Protocol.Generated.FindCoordinatorRequest as FCReq
import qualified Kafka.Protocol.Generated.FindCoordinatorResponse as FCResp
import qualified Kafka.Protocol.Generated.InitProducerIdRequest as IPReq
import qualified Kafka.Protocol.Generated.InitProducerIdResponse as IPResp
import qualified Kafka.Protocol.Generated.AddPartitionsToTxnRequest as APTReq
import qualified Kafka.Protocol.Generated.AddPartitionsToTxnResponse as APTResp
import qualified Kafka.Protocol.Generated.EndTxnRequest as ETReq
import qualified Kafka.Protocol.Generated.EndTxnResponse as ETResp
import qualified Kafka.Protocol.Generated.AddOffsetsToTxnRequest as AOTReq
import qualified Kafka.Protocol.Generated.AddOffsetsToTxnResponse as AOTResp
import qualified Kafka.Protocol.Generated.TxnOffsetCommitRequest as TOCReq
import qualified Kafka.Protocol.Generated.TxnOffsetCommitResponse as TOCResp

-- | Transaction coordinator information
data TransactionCoordinator = TransactionCoordinator
  { tcNodeId :: !Int32
  , tcHost :: !Text
  , tcPort :: !Int32
  } deriving (Show, Eq)

-- | Transaction coordinator errors
data TransactionCoordinatorError
  = CoordinatorNotAvailable Text
  | CoordinatorLoadInProgress Text
  | NotCoordinator Text
  | InvalidProducerIdMapping Text
  | InvalidProducerEpoch Text
  | InvalidTxnState Text
  | InvalidPartitionsInTxn Text
  | TransactionCoordinatorFenced Text
  | ConcurrentTransactions Text
  | ProducerFenced Text
  | UnknownCoordinatorError Int16 Text
  deriving (Show, Eq)

instance Exception TransactionCoordinatorError

-- | Interpret Kafka error codes into TransactionCoordinatorError
interpretCoordinatorError :: Int16 -> TransactionCoordinatorError
interpretCoordinatorError code = case code of
  15 -> CoordinatorNotAvailable "Coordinator not available"
  14 -> CoordinatorLoadInProgress "Coordinator load in progress"
  16 -> NotCoordinator "Not coordinator for this resource"
  47 -> InvalidProducerIdMapping "Invalid producer ID mapping"
  51 -> InvalidProducerEpoch "Invalid producer epoch"
  24 -> InvalidTxnState "Invalid transaction state"
  48 -> InvalidPartitionsInTxn "Invalid partitions in transaction"
  32 -> TransactionCoordinatorFenced "Transaction coordinator fenced"
  90 -> ProducerFenced "Producer fenced by another instance"
  96 -> ConcurrentTransactions "Concurrent transactions"
  _  -> UnknownCoordinatorError code $ "Unknown error code: " <> T.pack (show code)

-- | Find the transaction coordinator for a given transactional ID
-- Uses FindCoordinatorRequest (API key 10) with coordinator type TRANSACTION (1)
findTransactionCoordinator :: Conn.ConnectionManager
                           -> AV.ApiVersionCache
                           -> TVar Int32          -- ^ Correlation ID source
                           -> BrokerAddress      -- ^ Bootstrap broker
                           -> Text               -- ^ Client ID
                           -> Text               -- ^ Transactional ID
                           -> IO (Either TransactionCoordinatorError TransactionCoordinator)
findTransactionCoordinator connMgr versionCache corrIdVar bootstrapBroker clientId transactionalId = do
  -- Get connection to bootstrap broker
  connResult <- Conn.getOrCreateConnection connMgr bootstrapBroker Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $ 
      "Failed to connect to bootstrap broker: " <> T.pack err
    
    Right conn -> do
      -- Get correlation ID
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      
      let apiKey = 10  -- FindCoordinator API key
          clientMaxVersion = 3  -- We support up to v3
      
      -- Version negotiation
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache bootstrapBroker apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 1  -- Default to v1 (has keyType field)
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 1
              Just v -> v
      
      -- Build FindCoordinatorRequest
      let request = FCReq.FindCoordinatorRequest
            { FCReq.findCoordinatorRequestKey = P.mkKafkaString transactionalId
            , FCReq.findCoordinatorRequestKeyType = 1  -- TRANSACTION type
            , FCReq.findCoordinatorRequestCoordinatorKeys = P.KafkaArray (P.NotNull V.empty)
            }
          
          requestBody = runPutS $ FCReq.encodeFindCoordinatorRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $ 
          "FindCoordinator request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          -- Parse response
          case runGetS (FCResp.decodeFindCoordinatorResponse apiVersion) responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse FindCoordinatorResponse: " <> T.pack err
            
            Right response -> do
              let errorCode = FCResp.findCoordinatorResponseErrorCode response
              
              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else do
                  let nodeId = FCResp.findCoordinatorResponseNodeId response
                      host = case P.unKafkaString $ FCResp.findCoordinatorResponseHost response of
                        P.NotNull h -> h
                        P.Null -> ""  -- Should not happen for successful response
                      port = FCResp.findCoordinatorResponsePort response
                  
                  return $ Right $ TransactionCoordinator
                    { tcNodeId = nodeId
                    , tcHost = host
                    , tcPort = port
                    }

-- | Initialize a producer ID for transactional or idempotent producer
-- Uses InitProducerIdRequest (API key 22)
initProducerId :: Conn.ConnectionManager
               -> AV.ApiVersionCache
               -> TVar Int32              -- ^ Correlation ID source
               -> Text                    -- ^ Client ID
               -> TransactionCoordinator  -- ^ Transaction coordinator
               -> Maybe Text              -- ^ Transactional ID (Nothing for idempotent-only)
               -> Int32                   -- ^ Transaction timeout ms
               -> Maybe Int64             -- ^ Producer ID (for fencing)
               -> Maybe Int16             -- ^ Producer epoch (for fencing)
               -> IO (Either TransactionCoordinatorError (Int64, Int16))
               -- ^ Returns (ProducerId, ProducerEpoch)
initProducerId connMgr versionCache corrIdVar clientId coordinator transactionalId timeoutMs maybeProducerId maybeEpoch = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)
  
  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $
      "Failed to connect to coordinator: " <> T.pack err
    
    Right conn -> do
      -- Get correlation ID
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      
      let apiKey = 22  -- InitProducerId API key
          clientMaxVersion = 3  -- We support up to v3
      
      -- Version negotiation
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0  -- Default to v0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v
      
      -- Build InitProducerIdRequest
      let request = IPReq.InitProducerIdRequest
            { IPReq.initProducerIdRequestTransactionalId =
                case transactionalId of
                  Nothing -> P.KafkaString P.Null
                  Just txnId -> P.mkKafkaString txnId
            , IPReq.initProducerIdRequestTransactionTimeoutMs = timeoutMs
            , IPReq.initProducerIdRequestProducerId = maybe (-1) id maybeProducerId
            , IPReq.initProducerIdRequestProducerEpoch = maybe (-1) id maybeEpoch
            , IPReq.initProducerIdRequestEnable2Pc = False  -- v6+ only
            , IPReq.initProducerIdRequestKeepPreparedTxn = False  -- v6+ only
            }
          
          requestBody = runPutS $ IPReq.encodeInitProducerIdRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "InitProducerId request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          -- Parse response
          case runGetS (IPResp.decodeInitProducerIdResponse apiVersion) responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse InitProducerIdResponse: " <> T.pack err
            
            Right response -> do
              let errorCode = IPResp.initProducerIdResponseErrorCode response
              
              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else do
                  let producerId = IPResp.initProducerIdResponseProducerId response
                      producerEpoch = IPResp.initProducerIdResponseProducerEpoch response
                  
                  return $ Right (producerId, producerEpoch)

-- | Add partitions to the current transaction
-- Uses AddPartitionsToTxnRequest (API key 24)
addPartitionsToTxn :: Conn.ConnectionManager
                   -> AV.ApiVersionCache
                   -> TVar Int32              -- ^ Correlation ID source
                   -> Text                    -- ^ Client ID
                   -> TransactionCoordinator  -- ^ Transaction coordinator
                   -> Text                    -- ^ Transactional ID
                   -> Int64                   -- ^ Producer ID
                   -> Int16                   -- ^ Producer epoch
                   -> [TopicPartition]        -- ^ Partitions to add
                   -> IO (Either TransactionCoordinatorError ())
addPartitionsToTxn connMgr versionCache corrIdVar clientId coordinator transactionalId producerId epoch partitions = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)
  
  -- Group partitions by topic
  let byTopic = Map.fromListWith (++)
        [(tpTopic tp, [tpPartition tp]) | tp <- partitions]
      
      topics = V.fromList
        [APTReq.AddPartitionsToTxnTopic
          { APTReq.addPartitionsToTxnTopicName = P.mkKafkaString topic
          , APTReq.addPartitionsToTxnTopicPartitions = P.mkKafkaArray (V.fromList partIds)
          }
        | (topic, partIds) <- Map.toList byTopic
        ]
  
  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $
      "Failed to connect to coordinator: " <> T.pack err
    
    Right conn -> do
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      
      let apiKey = 24
          clientMaxVersion = 3  -- Use v3 (flexible but simpler than v4+)
      
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v
      
      let request = APTReq.AddPartitionsToTxnRequest
            { APTReq.addPartitionsToTxnRequestTransactions = P.mkKafkaArray V.empty  -- v4+ only
            , APTReq.addPartitionsToTxnRequestV3AndBelowTransactionalId = P.mkKafkaString transactionalId
            , APTReq.addPartitionsToTxnRequestV3AndBelowProducerId = producerId
            , APTReq.addPartitionsToTxnRequestV3AndBelowProducerEpoch = epoch
            , APTReq.addPartitionsToTxnRequestV3AndBelowTopics = P.mkKafkaArray topics
            }
          
          requestBody = runPutS $ APTReq.encodeAddPartitionsToTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "AddPartitionsToTxn request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          case runGetS (APTResp.decodeAddPartitionsToTxnResponse apiVersion) responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse AddPartitionsToTxnResponse: " <> T.pack err
            
            Right response -> do
              -- Check for top-level error (v4+) or topic-level errors (v0-3)
              -- For simplicity, we'll check the first topic's first partition error if using v0-3
              let resultTopics = case P.unKafkaArray (APTResp.addPartitionsToTxnResponseResultsByTopicV3AndBelow response) of
                    P.NotNull v -> v
                    P.Null -> V.empty
              
              if V.null resultTopics
                then return $ Right ()  -- No topics means success
                else do
                  let firstTopic = V.head resultTopics
                      resultPartitions = case P.unKafkaArray (APTResp.addPartitionsToTxnTopicResultResultsByPartition firstTopic) of
                        P.NotNull v -> v
                        P.Null -> V.empty
                  
                  if V.null resultPartitions
                    then return $ Right ()
                    else do
                      let firstPart = V.head resultPartitions
                          errorCode = APTResp.addPartitionsToTxnPartitionResultPartitionErrorCode firstPart
                      
                      if errorCode /= 0
                        then return $ Left $ interpretCoordinatorError errorCode
                        else return $ Right ()

-- | End a transaction (commit or abort)
-- Uses EndTxnRequest (API key 26)
endTransaction :: Conn.ConnectionManager
               -> AV.ApiVersionCache
               -> TVar Int32              -- ^ Correlation ID source
               -> Text                    -- ^ Client ID
               -> TransactionCoordinator  -- ^ Transaction coordinator
               -> Text                    -- ^ Transactional ID
               -> Int64                   -- ^ Producer ID
               -> Int16                   -- ^ Producer epoch
               -> Bool                    -- ^ True = commit, False = abort
               -> IO (Either TransactionCoordinatorError ())
endTransaction connMgr versionCache corrIdVar clientId coordinator transactionalId producerId epoch committed = do
  let coordAddr = BrokerAddress (T.unpack $ tcHost coordinator) (fromIntegral $ tcPort coordinator)
  
  -- Get connection to coordinator
  connResult <- Conn.getOrCreateConnection connMgr coordAddr Conn.defaultConnectionConfig
  
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $
      "Failed to connect to coordinator: " <> T.pack err
    
    Right conn -> do
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      
      let apiKey = 26  -- EndTxn API key
          clientMaxVersion = 3
      
      brokerVersionM <- atomically $ AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v -> v
      
      let request = ETReq.EndTxnRequest
            { ETReq.endTxnRequestTransactionalId = P.mkKafkaString transactionalId
            , ETReq.endTxnRequestProducerId = producerId
            , ETReq.endTxnRequestProducerEpoch = epoch
            , ETReq.endTxnRequestCommitted = committed
            }
          
          requestBody = runPutS $ ETReq.encodeEndTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "EndTxn request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          case runGetS (ETResp.decodeEndTxnResponse apiVersion) responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse EndTxnResponse: " <> T.pack err
            
            Right response -> do
              let errorCode = ETResp.endTxnResponseErrorCode response
              
              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else return $ Right ()

-- | Add consumer group offsets to the transaction
-- Uses AddOffsetsToTxnRequest (API key 25)
addOffsetsToTxn :: TransactionCoordinator  -- ^ Transaction coordinator
                -> Text                    -- ^ Transactional ID
                -> Int64                   -- ^ Producer ID
                -> Int16                   -- ^ Producer epoch
                -> Text                    -- ^ Consumer group ID
                -> IO (Either TransactionCoordinatorError ())
addOffsetsToTxn coordinator transactionalId producerId epoch groupId = do
  -- TODO: Implement AddOffsetsToTxnRequest
  -- 1. Create AddOffsetsToTxnRequest with:
  --    - transactionalId
  --    - producerId
  --    - producerEpoch
  --    - groupId
  -- 2. Send to transaction coordinator
  -- 3. Parse AddOffsetsToTxnResponse
  -- 4. Handle error codes:
  --    - 0: NO_ERROR
  --    - 16: NOT_COORDINATOR
  --    - 15: COORDINATOR_NOT_AVAILABLE
  --    - 14: COORDINATOR_LOAD_IN_PROGRESS
  --    - 51: INVALID_PRODUCER_EPOCH
  --    - 82: PRODUCER_FENCED
  --    - 24: INVALID_TXN_STATE
  --    - 32: TRANSACTIONAL_ID_AUTHORIZATION_FAILED
  --    - 30: GROUP_AUTHORIZATION_FAILED
  
  return $ Left $ CoordinatorNotAvailable $
    "AddOffsetsToTxnRequest not yet implemented for group: " <> groupId

-- | Commit consumer group offsets as part of a transaction
-- Uses TxnOffsetCommitRequest (API key 28)
-- This is sent to the consumer group coordinator, not the transaction coordinator
txnOffsetCommit :: BrokerAddress           -- ^ Consumer group coordinator
                -> Text                    -- ^ Transactional ID
                -> Text                    -- ^ Consumer group ID
                -> Int64                   -- ^ Producer ID
                -> Int16                   -- ^ Producer epoch
                -> [(TopicPartition, Int64)]  -- ^ Offsets to commit
                -> IO (Either TransactionCoordinatorError ())
txnOffsetCommit groupCoordinator transactionalId groupId producerId epoch offsets = do
  -- TODO: Implement TxnOffsetCommitRequest
  -- 1. Create TxnOffsetCommitRequest with:
  --    - transactionalId
  --    - groupId
  --    - producerId
  --    - producerEpoch
  --    - topics (grouped by topic with partition/offset/metadata)
  -- 2. Send to consumer group coordinator (NOT transaction coordinator!)
  -- 3. Parse TxnOffsetCommitResponse
  -- 4. Handle per-partition error codes:
  --    - 0: NO_ERROR
  --    - 25: ILLEGAL_GENERATION
  --    - 27: REBALANCE_IN_PROGRESS
  --    - 15: COORDINATOR_NOT_AVAILABLE
  --    - 14: COORDINATOR_LOAD_IN_PROGRESS
  --    - 16: NOT_COORDINATOR
  --    - 51: INVALID_PRODUCER_EPOCH
  --    - 82: PRODUCER_FENCED
  --    - 32: TRANSACTIONAL_ID_AUTHORIZATION_FAILED
  --    - 30: GROUP_AUTHORIZATION_FAILED
  --    - 3: UNKNOWN_TOPIC_OR_PARTITION
  
  return $ Left $ CoordinatorNotAvailable $
    "TxnOffsetCommitRequest not yet implemented for " <> T.pack (show (length offsets)) <> " offsets"

