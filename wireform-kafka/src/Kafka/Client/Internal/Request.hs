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
    
    -- Serialize the header (use version 1 for simplicity - supports all fields we need)
    headerBytes = runPutS $ RH.encodeRequestHeader 1 header
    
    -- Combine header and body
    messageBytes = headerBytes <> requestBody
    messageSize = BS.length messageBytes
    
    -- Serialize the size prefix (4 bytes, big-endian Int32)
    sizeBytes = runPutS $ serialize (fromIntegral messageSize :: Int32)
  in
    sizeBytes <> messageBytes

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

