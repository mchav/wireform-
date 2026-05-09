{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Protocol.ApiVersions
Description : API version negotiation and management
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module handles API version negotiation with Kafka brokers.

When connecting to a broker, clients should:
1. Send an ApiVersionsRequest (using a compatible version like v0 or v3)
2. Receive ApiVersionsResponse with supported versions for all APIs
3. Cache this information per broker
4. Use the cached versions to select appropriate API versions for requests

The client should use: min(clientMaxVersion, brokerMaxVersion) for each API.
-}
module Kafka.Protocol.ApiVersions
  ( -- * Version Cache
    ApiVersionCache
  , createVersionCache
  , queryApiVersion
    -- * Version Negotiation
  , negotiateVersions
  , ApiVersionRange(..)
    -- * Utilities
  , selectVersion
  , isVersionSupported
  ) where

import Control.Concurrent.STM
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Int
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.Connection (Connection)
import qualified StmContainers.Map as StmMap

import Kafka.Client.Internal.Request
import Kafka.Network.Connection (BrokerAddress)
import qualified Kafka.Protocol.Encoding as E
import qualified Kafka.Protocol.Generated.ApiVersionsRequest as AVReq
import qualified Kafka.Protocol.Generated.ApiVersionsResponse as AVResp
import qualified Kafka.Protocol.Primitives as P

-- | Range of supported versions for an API
data ApiVersionRange = ApiVersionRange
  { rangeMinVersion :: !Int16
    -- ^ Minimum supported version (inclusive)
  , rangeMaxVersion :: !Int16
    -- ^ Maximum supported version (inclusive)
  } deriving (Eq, Show, Generic)

-- | Cache of API versions per broker
-- Maps BrokerAddress -> (API Key -> ApiVersionRange)
newtype ApiVersionCache = ApiVersionCache
  { unVersionCache :: StmMap.Map BrokerAddress (Map Int16 ApiVersionRange)
  }

-- | Create a new empty version cache
createVersionCache :: IO ApiVersionCache
createVersionCache = ApiVersionCache <$> StmMap.newIO

-- | Query the version cache for a specific broker and API key
-- Returns Nothing if the broker hasn't been queried yet
queryApiVersion
  :: ApiVersionCache
  -> BrokerAddress
  -> Int16  -- ^ API key
  -> STM (Maybe ApiVersionRange)
queryApiVersion (ApiVersionCache cache) brokerAddr apiKey = do
  brokerVersionsM <- StmMap.lookup brokerAddr cache
  case brokerVersionsM of
    Nothing -> return Nothing
    Just brokerVersions -> return $ Map.lookup apiKey brokerVersions

-- | Negotiate API versions with a broker
--
-- This sends an ApiVersionsRequest and caches the response.
-- Returns the map of API key -> version range.
negotiateVersions
  :: Connection
  -> BrokerAddress
  -> ApiVersionCache
  -> Int32  -- ^ Correlation ID
  -> IO (Either String (Map Int16 ApiVersionRange))
negotiateVersions conn brokerAddr (ApiVersionCache cache) correlationId = do
  -- Use version 3 for ApiVersionsRequest (supports flexible encoding and client info)
  let apiVersion = 3
      request = AVReq.ApiVersionsRequest
        { AVReq.apiVersionsRequestClientSoftwareName = P.mkKafkaString "kafka-native"
        , AVReq.apiVersionsRequestClientSoftwareVersion = P.mkKafkaString "0.1.0"
        }
      requestBody = runPutS $ AVReq.encodeApiVersionsRequest apiVersion request
      clientId = P.mkKafkaString "kafka-native"
  
  result <- sendRequestReceiveResponse
    conn
    18  -- ApiVersions API key
    (fromIntegral apiVersion)
    correlationId
    clientId
    requestBody
  
  case result of
    Left err -> return $ Left err
    Right (respCorrelationId, respBody) ->
      if respCorrelationId /= correlationId
        then return $ Left $ "Correlation ID mismatch: expected " ++ show correlationId ++ ", got " ++ show respCorrelationId
        else case runGetS (AVResp.decodeApiVersionsResponse apiVersion) respBody of
          Left err -> return $ Left $ "Failed to decode ApiVersions response: " ++ err
          Right response -> do
            -- Extract version ranges from response
            let versionMap = extractVersionRanges response
            
            -- Store in cache
            atomically $ StmMap.insert versionMap brokerAddr cache
            
            return $ Right versionMap

-- | Extract API version ranges from an ApiVersionsResponse
extractVersionRanges :: AVResp.ApiVersionsResponse -> Map Int16 ApiVersionRange
extractVersionRanges response =
  let apiVersions = case P.unKafkaArray (AVResp.apiVersionsResponseApiKeys response) of
        P.Null -> []
        P.NotNull vec -> V.toList vec
  in Map.fromList $ map extractRange apiVersions
  where
    extractRange :: AVResp.ApiVersion -> (Int16, ApiVersionRange)
    extractRange av =
      ( AVResp.apiVersionApiKey av
      , ApiVersionRange
          { rangeMinVersion = AVResp.apiVersionMinVersion av
          , rangeMaxVersion = AVResp.apiVersionMaxVersion av
          }
      )

-- | Select an appropriate API version to use
--
-- Returns the minimum of the client's maximum supported version and the broker's maximum version.
-- Returns Nothing if the API is not supported by the broker or if no common version exists.
selectVersion
  :: Int16  -- ^ Client's maximum supported version
  -> ApiVersionRange  -- ^ Broker's supported range
  -> Maybe Int16
selectVersion clientMaxVersion ApiVersionRange{..}
  | clientMaxVersion < rangeMinVersion = Nothing  -- Client too old
  | otherwise = Just $ min clientMaxVersion rangeMaxVersion

-- | Check if a specific version is supported by the broker
isVersionSupported :: Int16 -> ApiVersionRange -> Bool
isVersionSupported version ApiVersionRange{..} =
  version >= rangeMinVersion && version <= rangeMaxVersion
