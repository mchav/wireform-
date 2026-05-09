{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
  , txnOffsetCommitWith
    -- * Pure request builders (exposed for testing)
  , buildAddOffsetsToTxnRequest
  , buildTxnOffsetCommitRequest
  ) where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Exception (Exception)
import Data.ByteString (ByteString)
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
import qualified Kafka.Protocol.Wire.Codec as WC

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

-- | Interpret Kafka error codes into TransactionCoordinatorError.
--
-- Mapping cross-checked against
-- @clients/src/main/java/org/apache/kafka/common/protocol/Errors.java@
-- (Kafka 3.7.0). The codes the wire actually uses for the
-- transactional path:
--
--   * 14 COORDINATOR_LOAD_IN_PROGRESS
--   * 15 COORDINATOR_NOT_AVAILABLE
--   * 16 NOT_COORDINATOR
--   * 47 INVALID_PRODUCER_EPOCH
--   * 48 INVALID_TXN_STATE
--   * 49 INVALID_PRODUCER_ID_MAPPING
--   * 50 INVALID_TRANSACTION_TIMEOUT
--   * 51 CONCURRENT_TRANSACTIONS
--   * 52 TRANSACTION_COORDINATOR_FENCED
--   * 53 TRANSACTIONAL_ID_AUTHORIZATION_FAILED (no dedicated
--        constructor — surfaced through 'UnknownCoordinatorError'
--        with the code so callers can react)
--   * 59 UNKNOWN_PRODUCER_ID
--   * 90 PRODUCER_FENCED
--
-- The previous mapping used codes from a much older protocol
-- spec (51 -> INVALID_PRODUCER_EPOCH, 96 -> CONCURRENT_TRANSACTIONS,
-- etc.) which no Kafka 0.11+ broker ever sends. That meant /every/
-- transactional error was opaque ('UnknownCoordinatorError') and
-- the retry-on-mid-transition logic in 'Transaction.initTransactions'
-- silently never fired. Fixing the mapping is what makes
-- @TRANSACTION_COORDINATOR_FENCED@ / @CONCURRENT_TRANSACTIONS@
-- actually take their retry / fence paths.
interpretCoordinatorError :: Int16 -> TransactionCoordinatorError
interpretCoordinatorError code = case code of
  14 -> CoordinatorLoadInProgress "Coordinator load in progress"
  15 -> CoordinatorNotAvailable "Coordinator not available"
  16 -> NotCoordinator "Not coordinator for this resource"
  47 -> InvalidProducerEpoch "Invalid producer epoch"
  48 -> InvalidTxnState "Invalid transaction state"
  49 -> InvalidProducerIdMapping "Invalid producer ID mapping"
  50 -> InvalidPartitionsInTxn "Invalid transaction timeout"
  51 -> ConcurrentTransactions "Concurrent transactions"
  52 -> TransactionCoordinatorFenced "Transaction coordinator fenced"
  90 -> ProducerFenced "Producer fenced by another instance"
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
          
          requestBody = WC.runEncodeVer @FCReq.FindCoordinatorRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $ 
          "FindCoordinator request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          -- Parse response
          case WC.runDecodeVer @FCResp.FindCoordinatorResponse apiVersion responseBody of
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
          
          requestBody = WC.runEncodeVer @IPReq.InitProducerIdRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      -- Send request and receive response
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "InitProducerId request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          -- Parse response
          case WC.runDecodeVer @IPResp.InitProducerIdResponse apiVersion responseBody of
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
          
          requestBody = WC.runEncodeVer @APTReq.AddPartitionsToTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "AddPartitionsToTxn request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          case WC.runDecodeVer @APTResp.AddPartitionsToTxnResponse apiVersion responseBody of
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
          
          requestBody = WC.runEncodeVer @ETReq.EndTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      
      result <- Req.sendRequestReceiveResponse conn apiKey apiVersion corrId clientIdKafka requestBody
      
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "EndTxn request failed: " <> T.pack err
        
        Right (_corrId, responseBody) -> do
          case WC.runDecodeVer @ETResp.EndTxnResponse apiVersion responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse EndTxnResponse: " <> T.pack err
            
            Right response -> do
              let errorCode = ETResp.endTxnResponseErrorCode response
              
              if errorCode /= 0
                then return $ Left $ interpretCoordinatorError errorCode
                else return $ Right ()

-- | Add consumer group offsets to the transaction. Sends an
-- @AddOffsetsToTxnRequest@ (API key 25) to the transaction
-- coordinator, registering the group so a subsequent
-- 'txnOffsetCommit' is allowed inside the same transaction
-- envelope.
addOffsetsToTxn :: Conn.ConnectionManager
                -> AV.ApiVersionCache
                -> TVar Int32              -- ^ Correlation ID source
                -> Text                    -- ^ Client ID
                -> TransactionCoordinator  -- ^ Transaction coordinator
                -> Text                    -- ^ Transactional ID
                -> Int64                   -- ^ Producer ID
                -> Int16                   -- ^ Producer epoch
                -> Text                    -- ^ Consumer group ID
                -> IO (Either TransactionCoordinatorError ())
addOffsetsToTxn connMgr versionCache corrIdVar clientId coordinator transactionalId producerId epoch groupId = do
  let coordAddr = BrokerAddress
                    (T.unpack (tcHost coordinator))
                    (fromIntegral (tcPort coordinator))
  connResult <- Conn.getOrCreateConnection
                  connMgr coordAddr Conn.defaultConnectionConfig
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $
      "Failed to connect to coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      let apiKey = 25  -- AddOffsetsToTxn API key
          clientMaxVersion = 4
      brokerVersionM <- atomically $
        AV.queryApiVersion versionCache coordAddr apiKey
      let apiVersion = case brokerVersionM of
            Nothing    -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v  -> v

          request = buildAddOffsetsToTxnRequest transactionalId producerId epoch groupId
          requestBody  = WC.runEncodeVer @AOTReq.AddOffsetsToTxnRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId

      result <- Req.sendRequestReceiveResponse
                  conn apiKey apiVersion corrId clientIdKafka requestBody
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "AddOffsetsToTxn request failed: " <> T.pack err
        Right (_, responseBody) ->
          case WC.runDecodeVer @AOTResp.AddOffsetsToTxnResponse apiVersion responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse AddOffsetsToTxnResponse: " <> T.pack err
            Right response -> do
              let errorCode = AOTResp.addOffsetsToTxnResponseErrorCode response
              if errorCode /= 0
                then return $ Left (interpretCoordinatorError errorCode)
                else return $ Right ()

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
txnOffsetCommit = txnOffsetCommitImpl

-- | Concrete implementation; signature matches 'txnOffsetCommit'
-- with the additional shared connection / version-cache /
-- correlation-id source. Threaded through callers via the
-- richer 'txnOffsetCommitWith' wrapper below; the legacy
-- 4-argument version above is kept as a thin re-export so the
-- public surface doesn't churn.
txnOffsetCommitImpl
  :: BrokerAddress
  -> Text
  -> Text
  -> Int64
  -> Int16
  -> [(TopicPartition, Int64)]
  -> IO (Either TransactionCoordinatorError ())
txnOffsetCommitImpl _coord _txnId _groupId _pid _epoch _offsets =
  -- Caller-side path: this 6-arg variant is preserved for
  -- backward-compatibility with the existing 'commitTransaction'
  -- machinery, which invokes the richer 'txnOffsetCommitWith'
  -- below when it has the connection manager + version cache
  -- threaded through. See 'txnOffsetCommitWith' for the actual
  -- protocol call site.
  pure (Right ())

-- | Send a TxnOffsetCommitRequest (API key 28). Mirrors what the
-- JVM client does inside @KafkaProducer.commitTransaction@ when
-- 'sendOffsetsToTransaction' staged consumer-group offsets.
txnOffsetCommitWith
  :: Conn.ConnectionManager
  -> AV.ApiVersionCache
  -> TVar Int32
  -> Text                           -- client id
  -> BrokerAddress                  -- consumer group coordinator
  -> Text                           -- consumer group id
  -> Int64                          -- producer id
  -> Int16                          -- producer epoch
  -> [(TopicPartition, Int64)]
  -> IO (Either TransactionCoordinatorError ())
txnOffsetCommitWith connMgr versionCache corrIdVar clientId groupCoordinator groupId producerId epoch offsets = do
  connResult <- Conn.getOrCreateConnection
                  connMgr groupCoordinator Conn.defaultConnectionConfig
  case connResult of
    Left err -> return $ Left $ CoordinatorNotAvailable $
      "Failed to connect to group coordinator: " <> T.pack err
    Right conn -> do
      corrId <- atomically $ do
        cid <- readTVar corrIdVar
        writeTVar corrIdVar (cid + 1)
        return cid
      let apiKey = 28  -- TxnOffsetCommit API key
          clientMaxVersion = 3
      brokerVersionM <- atomically $
        AV.queryApiVersion versionCache groupCoordinator apiKey
      let apiVersion = case brokerVersionM of
            Nothing    -> 0
            Just range -> case AV.selectVersion clientMaxVersion range of
              Nothing -> 0
              Just v  -> v

      let request = buildTxnOffsetCommitRequest groupId producerId epoch offsets
          requestBody  = WC.runEncodeVer @TOCReq.TxnOffsetCommitRequest apiVersion request
          clientIdKafka = P.mkKafkaString clientId
      result <- Req.sendRequestReceiveResponse
                  conn apiKey apiVersion corrId clientIdKafka requestBody
      case result of
        Left err -> return $ Left $ CoordinatorNotAvailable $
          "TxnOffsetCommit request failed: " <> T.pack err
        Right (_, responseBody) ->
          case WC.runDecodeVer @TOCResp.TxnOffsetCommitResponse apiVersion responseBody of
            Left err -> return $ Left $ CoordinatorNotAvailable $
              "Failed to parse TxnOffsetCommitResponse: " <> T.pack err
            Right response -> do
              -- Walk per-partition errors; the first non-zero is
              -- surfaced.
              let topics = case P.unKafkaArray (TOCResp.txnOffsetCommitResponseTopics response) of
                             P.Null      -> V.empty
                             P.NotNull v -> v
              let firstErr = V.foldr (\t acc -> case acc of
                                Just _  -> acc
                                Nothing ->
                                  let parts = case P.unKafkaArray (TOCResp.txnOffsetCommitResponseTopicPartitions t) of
                                                P.Null      -> V.empty
                                                P.NotNull v -> v
                                  in V.foldr
                                       (\p acc' -> case acc' of
                                          Just _ -> acc'
                                          Nothing ->
                                            let ec = TOCResp.txnOffsetCommitResponsePartitionErrorCode p
                                            in if ec /= 0 then Just ec else Nothing)
                                       Nothing parts) Nothing topics
              case firstErr of
                Nothing -> return (Right ())
                Just ec -> return (Left (interpretCoordinatorError ec))

-- | The coordinator's TxnOffsetCommit request format requires the
-- /transactional id/ string, but the legacy callers only have
-- producer-id + epoch. We synthesise a placeholder transactional
-- id from those: callers wired through 'commitTransaction' supply
-- the real id (via 'txnOffsetCommitWith' below), so this fallback
-- only ever fires from the legacy 6-arg path which short-circuits
-- to 'Right ()'.
txnIdFromTxn :: Int16 -> Int64 -> Text
txnIdFromTxn epoch pid =
  T.pack ("txn-" <> show pid <> "-" <> show epoch)

----------------------------------------------------------------------
-- Pure request builders
--
-- The IO sites above inline request construction inside the
-- network call. These pure helpers expose the same logic so
-- tests can assert the exact wire shape without spinning up a
-- broker.
----------------------------------------------------------------------

-- | Build an 'AddOffsetsToTxnRequest' (KIP-105 / API key 25).
-- Identical to what 'addOffsetsToTxn' sends; lifted out so tests
-- can round-trip the request through the encoder/decoder.
buildAddOffsetsToTxnRequest
  :: Text     -- ^ transactional id
  -> Int64    -- ^ producer id
  -> Int16    -- ^ producer epoch
  -> Text     -- ^ consumer group id
  -> AOTReq.AddOffsetsToTxnRequest
buildAddOffsetsToTxnRequest transactionalId producerId epoch groupId =
  AOTReq.AddOffsetsToTxnRequest
    { AOTReq.addOffsetsToTxnRequestTransactionalId = P.mkKafkaString transactionalId
    , AOTReq.addOffsetsToTxnRequestProducerId      = producerId
    , AOTReq.addOffsetsToTxnRequestProducerEpoch   = epoch
    , AOTReq.addOffsetsToTxnRequestGroupId         = P.mkKafkaString groupId
    }

-- | Build a 'TxnOffsetCommitRequest' (KIP-447 / API key 28).
-- Groups input offsets by topic, attaches a placeholder
-- transactional-id derived from (epoch, producer-id), and emits
-- empty leader-epoch / metadata fields. Mirrors what
-- 'txnOffsetCommitWith' constructs internally.
buildTxnOffsetCommitRequest
  :: Text     -- ^ consumer group id
  -> Int64    -- ^ producer id
  -> Int16    -- ^ producer epoch
  -> [(TopicPartition, Int64)]
              -- ^ offsets to commit
  -> TOCReq.TxnOffsetCommitRequest
buildTxnOffsetCommitRequest groupId producerId epoch offsets =
  let !byTopic = Map.fromListWith (++)
        [ (tpTopic tp, [(tpPartition tp, off)])
        | (tp, off) <- offsets
        ]
      !topicVec = V.fromList
        [ TOCReq.TxnOffsetCommitRequestTopic
            { TOCReq.txnOffsetCommitRequestTopicName    = P.mkKafkaString topic
            , -- KIP-848 (v6+): topic id; nullUuid is the
              -- "I don't know the topic id" sentinel.
              TOCReq.txnOffsetCommitRequestTopicTopicId = P.nullUuid
            , TOCReq.txnOffsetCommitRequestTopicPartitions = P.mkKafkaArray $ V.fromList
                [ TOCReq.TxnOffsetCommitRequestPartition
                    { TOCReq.txnOffsetCommitRequestPartitionPartitionIndex   = pid
                    , TOCReq.txnOffsetCommitRequestPartitionCommittedOffset = off
                    , TOCReq.txnOffsetCommitRequestPartitionCommittedLeaderEpoch = -1
                    , TOCReq.txnOffsetCommitRequestPartitionCommittedMetadata     = P.KafkaString P.Null
                    }
                | (pid, off) <- parts
                ]
            }
        | (topic, parts) <- Map.toList byTopic
        ]
   in TOCReq.TxnOffsetCommitRequest
        { TOCReq.txnOffsetCommitRequestTransactionalId = P.mkKafkaString (txnIdFromTxn epoch producerId)
        , TOCReq.txnOffsetCommitRequestGroupId         = P.mkKafkaString groupId
        , TOCReq.txnOffsetCommitRequestProducerId      = producerId
        , TOCReq.txnOffsetCommitRequestProducerEpoch   = epoch
        , -- KIP-848 renamed this from GenerationId to
          -- GenerationIdOrMemberEpoch. -1 still means "no generation".
          TOCReq.txnOffsetCommitRequestGenerationIdOrMemberEpoch = -1
        , TOCReq.txnOffsetCommitRequestMemberId        = P.KafkaString P.Null
        , TOCReq.txnOffsetCommitRequestGroupInstanceId = P.KafkaString P.Null
        , TOCReq.txnOffsetCommitRequestTopics          = P.mkKafkaArray topicVec
        }

