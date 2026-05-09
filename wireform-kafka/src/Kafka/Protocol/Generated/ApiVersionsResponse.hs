{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ApiVersionsResponse
Description : Kafka ApiVersionsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 18.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ApiVersionsResponse
  (
    ApiVersionsResponse(..),
    ApiVersion(..),
    SupportedFeatureKey(..),
    FinalizedFeatureKey(..),
    encodeApiVersionsResponse,
    decodeApiVersionsResponse,
    maxApiVersionsResponseVersion
  ) where

import Control.Monad (when)
import qualified Data.Bytes.Get
import Data.Bytes.Get (MonadGet)
import qualified Data.Bytes.Put
import Data.Bytes.Put (MonadPut)
import Data.Bytes.Serial (Serial(..), serialize, deserialize)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Kafka.Protocol.Primitives as P
import Kafka.Protocol.Primitives
  ( VarInt(..), VarLong(..), UVarInt(..)
  , KafkaString, KafkaBytes, KafkaArray, KafkaUuid
  , CompactString, CompactBytes, CompactArray
  , TaggedFields, emptyTaggedFields, Nullable(..)
  , toCompactString, toCompactBytes, toCompactArray
  )
import qualified Kafka.Protocol.Encoding as E
import Kafka.Protocol.Message (KafkaMessage(..))
import qualified Kafka.Protocol.Wire.Codec as WC
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word8)
import qualified Data.ByteString
import qualified Data.Int
import qualified Data.Map.Strict
import qualified Data.Word
import qualified Kafka.Protocol.Wire as W
import qualified Kafka.Protocol.Wire.Primitives as WP


-- | The APIs supported by the broker.
data ApiVersion = ApiVersion
  {

  -- | The API index.

  -- Versions: 0+
  apiVersionApiKey :: !(Int16)
,

  -- | The minimum supported version, inclusive.

  -- Versions: 0+
  apiVersionMinVersion :: !(Int16)
,

  -- | The maximum supported version, inclusive.

  -- Versions: 0+
  apiVersionMaxVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode ApiVersion with version-aware field handling.
encodeApiVersion :: MonadPut m => E.ApiVersion -> ApiVersion -> m ()
encodeApiVersion version amsg =
  do
    serialize (apiVersionApiKey amsg)
    serialize (apiVersionMinVersion amsg)
    serialize (apiVersionMaxVersion amsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ApiVersion with version-aware field handling.
decodeApiVersion :: MonadGet m => E.ApiVersion -> m ApiVersion
decodeApiVersion version =
  do
    fieldapikey <- deserialize
    fieldminversion <- deserialize
    fieldmaxversion <- deserialize
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ApiVersion
      {
      apiVersionApiKey = fieldapikey
      ,
      apiVersionMinVersion = fieldminversion
      ,
      apiVersionMaxVersion = fieldmaxversion
      }


-- | Features supported by the broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.
data SupportedFeatureKey = SupportedFeatureKey
  {

  -- | The name of the feature.

  -- Versions: 3+
  supportedFeatureKeyName :: !(KafkaString)
,

  -- | The minimum supported version for the feature.

  -- Versions: 3+
  supportedFeatureKeyMinVersion :: !(Int16)
,

  -- | The maximum supported version for the feature.

  -- Versions: 3+
  supportedFeatureKeyMaxVersion :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode SupportedFeatureKey with version-aware field handling.
encodeSupportedFeatureKey :: MonadPut m => E.ApiVersion -> SupportedFeatureKey -> m ()
encodeSupportedFeatureKey version smsg =
  do
    when (version >= 3) $
      if version >= 3 then serialize (toCompactString (supportedFeatureKeyName smsg)) else serialize (supportedFeatureKeyName smsg)
    when (version >= 3) $
      serialize (supportedFeatureKeyMinVersion smsg)
    when (version >= 3) $
      serialize (supportedFeatureKeyMaxVersion smsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode SupportedFeatureKey with version-aware field handling.
decodeSupportedFeatureKey :: MonadGet m => E.ApiVersion -> m SupportedFeatureKey
decodeSupportedFeatureKey version =
  do
    fieldname <- if version >= 3
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldminversion <- if version >= 3
      then deserialize
      else pure (0)
    fieldmaxversion <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure SupportedFeatureKey
      {
      supportedFeatureKeyName = fieldname
      ,
      supportedFeatureKeyMinVersion = fieldminversion
      ,
      supportedFeatureKeyMaxVersion = fieldmaxversion
      }


-- | List of cluster-wide finalized features. The information is valid only if FinalizedFeaturesEpoch >= 0.
data FinalizedFeatureKey = FinalizedFeatureKey
  {

  -- | The name of the feature.

  -- Versions: 3+
  finalizedFeatureKeyName :: !(KafkaString)
,

  -- | The cluster-wide finalized max version level for the feature.

  -- Versions: 3+
  finalizedFeatureKeyMaxVersionLevel :: !(Int16)
,

  -- | The cluster-wide finalized min version level for the feature.

  -- Versions: 3+
  finalizedFeatureKeyMinVersionLevel :: !(Int16)

  }
  deriving (Eq, Show, Generic)


-- | Encode FinalizedFeatureKey with version-aware field handling.
encodeFinalizedFeatureKey :: MonadPut m => E.ApiVersion -> FinalizedFeatureKey -> m ()
encodeFinalizedFeatureKey version fmsg =
  do
    when (version >= 3) $
      if version >= 3 then serialize (toCompactString (finalizedFeatureKeyName fmsg)) else serialize (finalizedFeatureKeyName fmsg)
    when (version >= 3) $
      serialize (finalizedFeatureKeyMaxVersionLevel fmsg)
    when (version >= 3) $
      serialize (finalizedFeatureKeyMinVersionLevel fmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode FinalizedFeatureKey with version-aware field handling.
decodeFinalizedFeatureKey :: MonadGet m => E.ApiVersion -> m FinalizedFeatureKey
decodeFinalizedFeatureKey version =
  do
    fieldname <- if version >= 3
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldmaxversionlevel <- if version >= 3
      then deserialize
      else pure (0)
    fieldminversionlevel <- if version >= 3
      then deserialize
      else pure (0)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure FinalizedFeatureKey
      {
      finalizedFeatureKeyName = fieldname
      ,
      finalizedFeatureKeyMaxVersionLevel = fieldmaxversionlevel
      ,
      finalizedFeatureKeyMinVersionLevel = fieldminversionlevel
      }



data ApiVersionsResponse = ApiVersionsResponse
  {

  -- | The top-level error code.

  -- Versions: 0+
  apiVersionsResponseErrorCode :: !(Int16)
,

  -- | The APIs supported by the broker.

  -- Versions: 0+
  apiVersionsResponseApiKeys :: !(KafkaArray (ApiVersion))
,

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  apiVersionsResponseThrottleTimeMs :: !(Int32)
,

  -- | Features supported by the broker. Note: in v0-v3, features with MinSupportedVersion = 0 are omitted.

  -- Versions: 3+
  apiVersionsResponseSupportedFeatures :: !(KafkaArray (SupportedFeatureKey))
,

  -- | The monotonically increasing epoch for the finalized features information. Valid values are >= 0. A 

  -- Versions: 3+
  apiVersionsResponseFinalizedFeaturesEpoch :: !(Int64)
,

  -- | List of cluster-wide finalized features. The information is valid only if FinalizedFeaturesEpoch >= 

  -- Versions: 3+
  apiVersionsResponseFinalizedFeatures :: !(KafkaArray (FinalizedFeatureKey))
,

  -- | Set by a KRaft controller if the required configurations for ZK migration are present.

  -- Versions: 3+
  apiVersionsResponseZkMigrationReady :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ApiVersionsResponse.
maxApiVersionsResponseVersion :: Int16
maxApiVersionsResponseVersion = 5

-- | KafkaMessage instance for ApiVersionsResponse.
instance KafkaMessage ApiVersionsResponse where
  messageApiKey = 18
  messageMinVersion = 0
  messageMaxVersion = 5
  messageFlexibleVersion = Just 3

-- | Encode ApiVersionsResponse with the given API version.
encodeApiVersionsResponse :: MonadPut m => E.ApiVersion -> ApiVersionsResponse -> m ()
encodeApiVersionsResponse version msg
  | version == 0 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 1 && version <= 2 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (apiVersionsResponseThrottleTimeMs msg)


  | version >= 3 && version <= 5 =
    do
      serialize (apiVersionsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeApiVersion (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (apiVersionsResponseThrottleTimeMs msg)
      do
        let _entries = (if version >= 3 then [(0, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeSupportedFeatureKey (case P.unKafkaArray (apiVersionsResponseSupportedFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else []) ++ (if version >= 3 then [(1, Data.Bytes.Put.runPutS (serialize (apiVersionsResponseFinalizedFeaturesEpoch msg)))] else []) ++ (if version >= 3 then [(2, Data.Bytes.Put.runPutS (E.encodeVersionedArray version 999 encodeFinalizedFeatureKey (case P.unKafkaArray (apiVersionsResponseFinalizedFeatures msg) of { P.NotNull v -> v; P.Null -> V.empty })))] else []) ++ (if version >= 3 then [(3, Data.Bytes.Put.runPutS (serialize (apiVersionsResponseZkMigrationReady msg)))] else [])
        P.serializeTaggedFieldEntries _entries
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ApiVersionsResponse with the given API version.
decodeApiVersionsResponse :: MonadGet m => E.ApiVersion -> m ApiVersionsResponse
decodeApiVersionsResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = 0
        ,
        apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = (-1)
        ,
        apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseZkMigrationReady = False
        }

  | version >= 1 && version <= 2 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      fieldthrottletimems <- deserialize
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = (-1)
        ,
        apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty
        ,
        apiVersionsResponseZkMigrationReady = False
        }

  | version >= 3 && version <= 5 =
    do
      fielderrorcode <- deserialize
      fieldapikeys <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeApiVersion
      fieldthrottletimems <- deserialize
      _taggedFields <- (deserialize :: MonadGet m => m TaggedFields)
      let fieldsupportedfeatures =
            if version >= 3
              then case P.lookupTaggedField 0 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeSupportedFeatureKey) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      let fieldfinalizedfeaturesepoch =
            if version >= 3
              then case P.lookupTaggedField 1 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> ((-1))
                Nothing  -> ((-1))
              else ((-1))
      let fieldfinalizedfeatures =
            if version >= 3
              then case P.lookupTaggedField 2 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (P.mkKafkaArray <$> E.decodeVersionedArray version 999 decodeFinalizedFeatureKey) _bs of
                    Right _v -> _v
                    Left  _  -> (P.mkKafkaArray V.empty)
                Nothing  -> (P.mkKafkaArray V.empty)
              else (P.mkKafkaArray V.empty)
      let fieldzkmigrationready =
            if version >= 3
              then case P.lookupTaggedField 3 _taggedFields of
                Just _bs -> case Data.Bytes.Get.runGetS (deserialize) _bs of
                    Right _v -> _v
                    Left  _  -> (False)
                Nothing  -> (False)
              else (False)
      pure ApiVersionsResponse
        {
        apiVersionsResponseErrorCode = fielderrorcode
        ,
        apiVersionsResponseApiKeys = fieldapikeys
        ,
        apiVersionsResponseThrottleTimeMs = fieldthrottletimems
        ,
        apiVersionsResponseSupportedFeatures = fieldsupportedfeatures
        ,
        apiVersionsResponseFinalizedFeaturesEpoch = fieldfinalizedfeaturesepoch
        ,
        apiVersionsResponseFinalizedFeatures = fieldfinalizedfeatures
        ,
        apiVersionsResponseZkMigrationReady = fieldzkmigrationready
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a ApiVersion.
wireMaxSizeApiVersion :: Int -> ApiVersion -> Int
wireMaxSizeApiVersion _version msg =
  0
  + 2
  + 2
  + 2
  + 1

-- | Direct-poke encoder for ApiVersion.
wirePokeApiVersion :: Int -> Ptr Word8 -> ApiVersion -> IO (Ptr Word8)
wirePokeApiVersion version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (apiVersionApiKey msg)
  p2 <- W.pokeInt16BE p1 (apiVersionMinVersion msg)
  p3 <- W.pokeInt16BE p2 (apiVersionMaxVersion msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for ApiVersion.
wirePeekApiVersion :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ApiVersion, Ptr Word8)
wirePeekApiVersion version _fp _basePtr p0 endPtr = do
  (f0_apikey, p1) <- W.peekInt16BE p0 endPtr
  (f1_minversion, p2) <- W.peekInt16BE p1 endPtr
  (f2_maxversion, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (ApiVersion { apiVersionApiKey = f0_apikey, apiVersionMinVersion = f1_minversion, apiVersionMaxVersion = f2_maxversion }, pTagsEnd)

-- | Worst-case wire size of a SupportedFeatureKey.
wireMaxSizeSupportedFeatureKey :: Int -> SupportedFeatureKey -> Int
wireMaxSizeSupportedFeatureKey _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (supportedFeatureKeyName msg))
  + 2
  + 2
  + 1

-- | Direct-poke encoder for SupportedFeatureKey.
wirePokeSupportedFeatureKey :: Int -> Ptr Word8 -> SupportedFeatureKey -> IO (Ptr Word8)
wirePokeSupportedFeatureKey version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (supportedFeatureKeyName msg))
  p2 <- W.pokeInt16BE p1 (supportedFeatureKeyMinVersion msg)
  p3 <- W.pokeInt16BE p2 (supportedFeatureKeyMaxVersion msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for SupportedFeatureKey.
wirePeekSupportedFeatureKey :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (SupportedFeatureKey, Ptr Word8)
wirePeekSupportedFeatureKey version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_minversion, p2) <- W.peekInt16BE p1 endPtr
  (f2_maxversion, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (SupportedFeatureKey { supportedFeatureKeyName = f0_name, supportedFeatureKeyMinVersion = f1_minversion, supportedFeatureKeyMaxVersion = f2_maxversion }, pTagsEnd)

-- | Worst-case wire size of a FinalizedFeatureKey.
wireMaxSizeFinalizedFeatureKey :: Int -> FinalizedFeatureKey -> Int
wireMaxSizeFinalizedFeatureKey _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (finalizedFeatureKeyName msg))
  + 2
  + 2
  + 1

-- | Direct-poke encoder for FinalizedFeatureKey.
wirePokeFinalizedFeatureKey :: Int -> Ptr Word8 -> FinalizedFeatureKey -> IO (Ptr Word8)
wirePokeFinalizedFeatureKey version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (finalizedFeatureKeyName msg))
  p2 <- W.pokeInt16BE p1 (finalizedFeatureKeyMaxVersionLevel msg)
  p3 <- W.pokeInt16BE p2 (finalizedFeatureKeyMinVersionLevel msg)
  if version >= 3 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for FinalizedFeatureKey.
wirePeekFinalizedFeatureKey :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (FinalizedFeatureKey, Ptr Word8)
wirePeekFinalizedFeatureKey version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_maxversionlevel, p2) <- W.peekInt16BE p1 endPtr
  (f2_minversionlevel, p3) <- W.peekInt16BE p2 endPtr
  pTagsEnd <- if version >= 3 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (FinalizedFeatureKey { finalizedFeatureKeyName = f0_name, finalizedFeatureKeyMaxVersionLevel = f1_maxversionlevel, finalizedFeatureKeyMinVersionLevel = f2_minversionlevel }, pTagsEnd)

-- | Worst-case wire size of a ApiVersionsResponse.
wireMaxSizeApiVersionsResponse :: Int -> ApiVersionsResponse -> Int
wireMaxSizeApiVersionsResponse _version msg =
  0
  + 2
  + (5 + (case P.unKafkaArray (apiVersionsResponseApiKeys msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeApiVersion _version x ) v); P.Null -> 0 }))
  + 4
  + (5 + (case P.unKafkaArray (apiVersionsResponseSupportedFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeSupportedFeatureKey _version x ) v); P.Null -> 0 }))
  + 8
  + (5 + (case P.unKafkaArray (apiVersionsResponseFinalizedFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFinalizedFeatureKey _version x ) v); P.Null -> 0 }))
  + 1
  + 1

-- | Direct-poke encoder for ApiVersionsResponse.
wirePokeApiVersionsResponse :: Int -> Ptr Word8 -> ApiVersionsResponse -> IO (Ptr Word8)
wirePokeApiVersionsResponse version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (apiVersionsResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeApiVersion version p x) p1 (apiVersionsResponseApiKeys msg)
    pure p2
  | version >= 1 && version <= 2 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (apiVersionsResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeApiVersion version p x) p1 (apiVersionsResponseApiKeys msg)
    p3 <- W.pokeInt32BE p2 (apiVersionsResponseThrottleTimeMs msg)
    pure p3
  | version >= 3 && version <= 5 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt16BE p0 (apiVersionsResponseErrorCode msg)
    p2 <- WP.pokeVersionedArray version 3 (\p x -> wirePokeApiVersion version p x) p1 (apiVersionsResponseApiKeys msg)
    p3 <- W.pokeInt32BE p2 (apiVersionsResponseThrottleTimeMs msg)
    let !_taggedEntries = (if version >= 3 then [(0, W.runWirePokeWith (5 + (case P.unKafkaArray (apiVersionsResponseSupportedFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeSupportedFeatureKey version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeSupportedFeatureKey version p_ x) p (apiVersionsResponseSupportedFeatures msg)))] else []) ++ (if version >= 3 then [(1, W.runWirePut (apiVersionsResponseFinalizedFeaturesEpoch msg))] else []) ++ (if version >= 3 then [(2, W.runWirePokeWith (5 + (case P.unKafkaArray (apiVersionsResponseFinalizedFeatures msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeFinalizedFeatureKey version x) v); P.Null -> 0 })) (\p -> WP.pokeCompactArray (\p_ x -> wirePokeFinalizedFeatureKey version p_ x) p (apiVersionsResponseFinalizedFeatures msg)))] else []) ++ (if version >= 3 then [(3, W.runWirePut (apiVersionsResponseZkMigrationReady msg))] else [])
    WP.pokeTaggedFieldEntries p3 _taggedEntries
  | otherwise = error $ "wirePoke ApiVersionsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for ApiVersionsResponse.
wirePeekApiVersionsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (ApiVersionsResponse, Ptr Word8)
wirePeekApiVersionsResponse version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_apikeys, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekApiVersion version _fp _basePtr p e) p1 endPtr
    pure (ApiVersionsResponse { apiVersionsResponseErrorCode = f0_errorcode, apiVersionsResponseApiKeys = f1_apikeys, apiVersionsResponseThrottleTimeMs = 0, apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty, apiVersionsResponseFinalizedFeaturesEpoch = 0, apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty, apiVersionsResponseZkMigrationReady = False }, p2)
  | version >= 1 && version <= 2 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_apikeys, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekApiVersion version _fp _basePtr p e) p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    pure (ApiVersionsResponse { apiVersionsResponseErrorCode = f0_errorcode, apiVersionsResponseApiKeys = f1_apikeys, apiVersionsResponseThrottleTimeMs = f2_throttletimems, apiVersionsResponseSupportedFeatures = P.mkKafkaArray V.empty, apiVersionsResponseFinalizedFeaturesEpoch = 0, apiVersionsResponseFinalizedFeatures = P.mkKafkaArray V.empty, apiVersionsResponseZkMigrationReady = False }, p3)
  | version >= 3 && version <= 5 = do
    (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
    (f1_apikeys, p2) <- WP.peekVersionedArray version 3 (\p e -> wirePeekApiVersion version _fp _basePtr p e) p1 endPtr
    (f2_throttletimems, p3) <- W.peekInt32BE p2 endPtr
    (_taggedMap, pTagsEnd) <- WP.peekTaggedFieldsMap p3 endPtr
    let !_tag_supportedfeatures = if version >= 3 then case Data.Map.Strict.lookup 0 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekSupportedFeatureKey version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    let !_tag_finalizedfeaturesepoch = if version >= 3 then case Data.Map.Strict.lookup 1 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String Data.Int.Int64) _bs of { Right _v -> _v ; Left _ -> 0}; Nothing -> 0} else 0
    let !_tag_finalizedfeatures = if version >= 3 then case Data.Map.Strict.lookup 2 _taggedMap of { Just _bs -> case (W.runWireGetWith (\_fp _bp p e -> WP.peekCompactArray (\p e -> wirePeekFinalizedFeatureKey version _fp _bp p e) p e)) _bs of { Right _v -> _v ; Left _ -> P.mkKafkaArray V.empty}; Nothing -> P.mkKafkaArray V.empty} else P.mkKafkaArray V.empty
    let !_tag_zkmigrationready = if version >= 3 then case Data.Map.Strict.lookup 3 _taggedMap of { Just _bs -> case (W.runWireGet :: Data.ByteString.ByteString -> Either String Bool) _bs of { Right _v -> _v ; Left _ -> False}; Nothing -> False} else False
    pure (ApiVersionsResponse { apiVersionsResponseErrorCode = f0_errorcode, apiVersionsResponseApiKeys = f1_apikeys, apiVersionsResponseThrottleTimeMs = f2_throttletimems, apiVersionsResponseSupportedFeatures = _tag_supportedfeatures, apiVersionsResponseFinalizedFeaturesEpoch = _tag_finalizedfeaturesepoch, apiVersionsResponseFinalizedFeatures = _tag_finalizedfeatures, apiVersionsResponseZkMigrationReady = _tag_zkmigrationready }, pTagsEnd)
  | otherwise = error $ "wirePeek ApiVersionsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec ApiVersionsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeApiVersionsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeApiVersionsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekApiVersionsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}