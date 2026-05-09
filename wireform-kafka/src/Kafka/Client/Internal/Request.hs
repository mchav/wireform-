{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Internal.Request
Description : Internal request/response handling utilities
Copyright   : (c) 2025
License     : BSD-3-Clause

Internal utilities for sending Kafka protocol requests and receiving responses.
This module provides low-level primitives for the higher-level client APIs.

-}
module Kafka.Client.Internal.Request
  ( -- * Request/Response Operations
    sendRequestReceiveResponse
  , sendRawRequest
  , receiveRawResponse
    -- * Frame Construction
  , frameRequest
  , parseResponseFrame
  ) where

import Control.Monad (when)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize, deserialize)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32)
import Network.Connection (Connection, connectionGet, connectionPut)

import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Generated.RequestHeader as RH

-- | Frame a request with its length prefix and header.
-- 
-- Kafka wire protocol format:
-- [4 bytes: message length] [header] [request body]
--
-- The message length is the size of header + request body.
frameRequest 
  :: Int16          -- ^ API key
  -> Int16          -- ^ API version
  -> Int32          -- ^ Correlation ID
  -> P.KafkaString  -- ^ Client ID
  -> ByteString     -- ^ Serialized request body
  -> ByteString
frameRequest apiKey apiVersion correlationId clientId requestBody =
  let
    -- Create the request header
    header = RH.RequestHeader
      { RH.requestHeaderRequestApiKey = apiKey
      , RH.requestHeaderRequestApiVersion = apiVersion
      , RH.requestHeaderCorrelationId = correlationId
      , RH.requestHeaderClientId = clientId
      }

    -- The Kafka request header has its own version: v0 (for the
    -- ApiVersions request, which deliberately stays at v0 so the
    -- broker can negotiate from any client); v1 (no tagged fields,
    -- matches every non-flexible API); v2 (flexible — adds an
    -- empty TaggedFields trailer). The choice is determined by
    -- whether the API key + version pair has flexible-version
    -- support; sending a v1 header for a flexible body makes the
    -- broker read garbage off the wire and close the connection
    -- with @InvalidRequestException@ + BufferUnderflow.
    headerVersion = requestHeaderVersionFor apiKey apiVersion

    headerBytes = runPutS $ RH.encodeRequestHeader headerVersion header

    -- Combine header and body
    messageBytes = headerBytes <> requestBody
    messageSize = BS.length messageBytes

    -- Serialize the size prefix (4 bytes, big-endian Int32)
    sizeBytes = runPutS $ serialize (fromIntegral messageSize :: Int32)
  in
    sizeBytes <> messageBytes

-- | Pick the right Kafka request-header version for a given API
-- key + body version. Encodes the per-API flexible-version
-- thresholds upstream sets in the JSON message defs.
--
-- Header v0 is special-cased for 'ApiVersions' (api key 18) so a
-- broker that doesn't yet know which body versions we support can
-- still parse the request.
--
-- Anything in the table at body version >= the flexible-from
-- threshold uses header v2 (which adds an empty 'TaggedFields'
-- trailer); everything else uses header v1.
requestHeaderVersionFor :: Int16 -> Int16 -> Int16
requestHeaderVersionFor apiKey apiVersion = case apiKey of
  18 -> 0  -- ApiVersions: header v0 regardless of body version
  _  -> case lookup apiKey flexibleVersionTable of
          Nothing  -> 1                       -- non-flexible API
          Just t   -> if apiVersion >= t then 2 else 1

-- | Per-API flexible-from version (i.e. the lowest body version
-- that uses the v2 request header). Sourced from the upstream
-- @data/messages/*Request.json@ "flexibleVersions" field.
--
-- This is a denormalised mirror of what the generated
-- @messageFlexibleVersion@ on each request type carries; we keep
-- a simple Int -> Int table here so the framing layer doesn't
-- have to reach back into 'KafkaMessage' for every send.
flexibleVersionTable :: [(Int16, Int16)]
flexibleVersionTable =
  [ (0, 9)   -- Produce
  , (1, 12)  -- Fetch
  , (2, 6)   -- ListOffsets
  , (3, 9)   -- Metadata
  , (8, 8)   -- OffsetCommit
  , (9, 6)   -- OffsetFetch
  , (10, 4)  -- FindCoordinator
  , (11, 6)  -- JoinGroup
  , (12, 4)  -- Heartbeat
  , (13, 4)  -- LeaveGroup
  , (14, 4)  -- SyncGroup
  , (15, 5)  -- DescribeGroups
  , (16, 3)  -- ListGroups
  , (17, 2)  -- SaslHandshake (still header v1; SaslHandshake stays non-flexible)
    -- Note: SaslHandshake actually never goes flexible. The
    -- entry is here to document the choice.
  , (19, 5)  -- CreateTopics
  , (20, 4)  -- DeleteTopics
  , (21, 2)  -- DeleteRecords
  , (22, 2)  -- InitProducerId
  , (23, 4)  -- OffsetForLeaderEpoch
  , (24, 3)  -- AddPartitionsToTxn
  , (25, 3)  -- AddOffsetsToTxn
  , (26, 3)  -- EndTxn
  , (27, 1)  -- WriteTxnMarkers
  , (28, 3)  -- TxnOffsetCommit
  , (29, 2)  -- DescribeAcls
  , (30, 2)  -- CreateAcls
  , (31, 2)  -- DeleteAcls
  , (32, 4)  -- DescribeConfigs
  , (33, 2)  -- AlterConfigs
  , (34, 2)  -- AlterReplicaLogDirs
  , (35, 2)  -- DescribeLogDirs
  , (37, 2)  -- CreatePartitions
  , (38, 2)  -- CreateDelegationToken
  , (39, 2)  -- RenewDelegationToken
  , (40, 2)  -- ExpireDelegationToken
  , (41, 2)  -- DescribeDelegationToken
  , (42, 2)  -- DeleteGroups
  , (43, 2)  -- ElectLeaders
  , (44, 1)  -- IncrementalAlterConfigs
  , (45, 0)  -- AlterPartitionReassignments
  , (46, 0)  -- ListPartitionReassignments
  , (47, 0)  -- OffsetDelete (kept v1; never went flexible)
  , (48, 1)  -- DescribeClientQuotas
  , (49, 1)  -- AlterClientQuotas
  , (50, 0)  -- DescribeUserScramCredentials
  , (51, 0)  -- AlterUserScramCredentials
  , (60, 0)  -- DescribeCluster
  , (61, 0)  -- DescribeProducers
  , (65, 0)  -- DescribeTransactions
  , (66, 0)  -- ListTransactions
  , (68, 0)  -- ConsumerGroupHeartbeat
  , (69, 0)  -- ConsumerGroupDescribe
  , (71, 0)  -- GetTelemetrySubscriptions
  , (72, 0)  -- PushTelemetry
  ]

-- | Parse a response frame, extracting the correlation ID and response body.
--
-- Kafka response format:
-- [4 bytes: message length] [response header] [response body]
parseResponseFrame :: ByteString -> Either String (Int32, ByteString)
parseResponseFrame bs = do
  -- Read the size prefix
  when (BS.length bs < 4) $
    Left "Response too short: missing size prefix"
  
  let sizeResult = runGetS deserialize (BS.take 4 bs)
  messageSize <- sizeResult
  
  let remainingBytes = BS.drop 4 bs
  when (BS.length remainingBytes < fromIntegral (messageSize :: Int32)) $
    Left $ "Response too short: expected " ++ show messageSize ++ " bytes, got " ++ show (BS.length remainingBytes)
  
  let messageBytes = BS.take (fromIntegral messageSize) remainingBytes
  
  -- Parse response header (version 0 is simplest - just correlation ID)
  -- Response header v0: correlation_id (4 bytes)
  when (BS.length messageBytes < 4) $
    Left "Response message too short: missing correlation ID"
  
  let correlationIdResult = runGetS deserialize (BS.take 4 messageBytes)
  correlationId <- correlationIdResult
  
  let responseBody = BS.drop 4 messageBytes
  
  return (correlationId, responseBody)

-- | Send a raw framed request to the connection.
sendRawRequest :: Connection -> ByteString -> IO ()
sendRawRequest conn framedRequest = do
  connectionPut conn framedRequest

-- | Read exactly N bytes from the connection.
-- Keep reading until we have all the bytes or the connection closes.
readExactly :: Connection -> Int -> IO ByteString
readExactly conn n = go BS.empty n 0
  where
    go acc remaining emptyReads
      | remaining <= 0 = return acc
      | emptyReads >= 3 = fail $ "Connection appears closed: received " ++ show emptyReads ++ " consecutive empty reads"
      | otherwise = do
          chunk <- connectionGet conn remaining
          if BS.null chunk
            then go acc remaining (emptyReads + 1)  -- Retry on empty read, but count it
            else do
              let newAcc = acc <> chunk
                  newRemaining = remaining - BS.length chunk
              go newAcc newRemaining 0  -- Reset empty read counter on successful read

-- | Receive a raw framed response from the connection.
-- This reads the size prefix first, then reads exactly that many bytes.
receiveRawResponse :: Connection -> IO ByteString
receiveRawResponse conn = do
  -- Read the 4-byte size prefix
  sizeBytes <- readExactly conn 4
  
  let sizeResult = runGetS deserialize sizeBytes
  messageSize <- case sizeResult of
    Left err -> fail $ "Failed to parse response size: " ++ err
    Right size -> return (size :: Int32)
  
  -- Read the message body
  messageBytes <- readExactly conn (fromIntegral messageSize)
  
  -- Return the complete framed response (size + message)
  return $ sizeBytes <> messageBytes

-- | Send a request and receive the corresponding response.
--
-- This is a synchronous request/response operation. For pipelined requests,
-- use the Pipeline module instead.
sendRequestReceiveResponse
  :: Connection
  -> Int16          -- ^ API key
  -> Int16          -- ^ API version
  -> Int32          -- ^ Correlation ID
  -> P.KafkaString  -- ^ Client ID
  -> ByteString     -- ^ Serialized request body
  -> IO (Either String (Int32, ByteString))
sendRequestReceiveResponse conn apiKey apiVersion correlationId clientId requestBody = do
  -- Frame and send the request
  let framedRequest = frameRequest apiKey apiVersion correlationId clientId requestBody
  sendRawRequest conn framedRequest
  
  -- Receive the response
  response <- receiveRawResponse conn
  
  -- Parse the response frame
  return $ parseResponseFrame response

