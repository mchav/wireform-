{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeConfigsResponse
Description : Kafka DescribeConfigsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 32.



Valid versions: 1-4
Flexible versions: 4+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeConfigsResponse
  (
    DescribeConfigsResponse(..),
    DescribeConfigsResult(..),
    DescribeConfigsResourceResult(..),
    DescribeConfigsSynonym(..),
    encodeDescribeConfigsResponse,
    decodeDescribeConfigsResponse,
    maxDescribeConfigsResponseVersion
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


-- | The synonyms for this configuration key.
data DescribeConfigsSynonym = DescribeConfigsSynonym
  {

  -- | The synonym name.

  -- Versions: 1+
  describeConfigsSynonymName :: !(KafkaString)
,

  -- | The synonym value.

  -- Versions: 1+
  describeConfigsSynonymValue :: !(KafkaString)
,

  -- | The synonym source.

  -- Versions: 1+
  describeConfigsSynonymSource :: !(Int8)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsSynonym with version-aware field handling.
encodeDescribeConfigsSynonym :: MonadPut m => E.ApiVersion -> DescribeConfigsSynonym -> m ()
encodeDescribeConfigsSynonym version dmsg =
  do
    when (version >= 1) $
      if version >= 4 then serialize (toCompactString (describeConfigsSynonymName dmsg)) else serialize (describeConfigsSynonymName dmsg)
    when (version >= 1) $
      if version >= 4 then serialize (toCompactString (describeConfigsSynonymValue dmsg)) else serialize (describeConfigsSynonymValue dmsg)
    when (version >= 1) $
      serialize (describeConfigsSynonymSource dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsSynonym with version-aware field handling.
decodeDescribeConfigsSynonym :: MonadGet m => E.ApiVersion -> m DescribeConfigsSynonym
decodeDescribeConfigsSynonym version =
  do
    fieldname <- if version >= 1
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldvalue <- if version >= 1
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldsource <- if version >= 1
      then deserialize
      else pure (0)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsSynonym
      {
      describeConfigsSynonymName = fieldname
      ,
      describeConfigsSynonymValue = fieldvalue
      ,
      describeConfigsSynonymSource = fieldsource
      }


-- | Each listed configuration.
data DescribeConfigsResourceResult = DescribeConfigsResourceResult
  {

  -- | The configuration name.

  -- Versions: 0+
  describeConfigsResourceResultName :: !(KafkaString)
,

  -- | The configuration value.

  -- Versions: 0+
  describeConfigsResourceResultValue :: !(KafkaString)
,

  -- | True if the configuration is read-only.

  -- Versions: 0+
  describeConfigsResourceResultReadOnly :: !(Bool)
,

  -- | The configuration source.

  -- Versions: 1+
  describeConfigsResourceResultConfigSource :: !(Int8)
,

  -- | True if this configuration is sensitive.

  -- Versions: 0+
  describeConfigsResourceResultIsSensitive :: !(Bool)
,

  -- | The synonyms for this configuration key.

  -- Versions: 1+
  describeConfigsResourceResultSynonyms :: !(KafkaArray (DescribeConfigsSynonym))
,

  -- | The configuration data type. Type can be one of the following values - BOOLEAN, STRING, INT, SHORT, 

  -- Versions: 3+
  describeConfigsResourceResultConfigType :: !(Int8)
,

  -- | The configuration documentation.

  -- Versions: 3+
  describeConfigsResourceResultDocumentation :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsResourceResult with version-aware field handling.
encodeDescribeConfigsResourceResult :: MonadPut m => E.ApiVersion -> DescribeConfigsResourceResult -> m ()
encodeDescribeConfigsResourceResult version dmsg =
  do
    if version >= 4 then serialize (toCompactString (describeConfigsResourceResultName dmsg)) else serialize (describeConfigsResourceResultName dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResourceResultValue dmsg)) else serialize (describeConfigsResourceResultValue dmsg)
    serialize (describeConfigsResourceResultReadOnly dmsg)
    when (version >= 1) $
      serialize (describeConfigsResourceResultConfigSource dmsg)
    serialize (describeConfigsResourceResultIsSensitive dmsg)
    when (version >= 1) $
      E.encodeVersionedArray version 4 encodeDescribeConfigsSynonym (case P.unKafkaArray (describeConfigsResourceResultSynonyms dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 3) $
      serialize (describeConfigsResourceResultConfigType dmsg)
    when (version >= 3) $
      if version >= 4 then serialize (toCompactString (describeConfigsResourceResultDocumentation dmsg)) else serialize (describeConfigsResourceResultDocumentation dmsg)
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsResourceResult with version-aware field handling.
decodeDescribeConfigsResourceResult :: MonadGet m => E.ApiVersion -> m DescribeConfigsResourceResult
decodeDescribeConfigsResourceResult version =
  do
    fieldname <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldvalue <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldreadonly <- deserialize
    fieldconfigsource <- if version >= 1
      then deserialize
      else pure ((-1))
    fieldissensitive <- deserialize
    fieldsynonyms <- if version >= 1
      then P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsSynonym
      else pure (P.mkKafkaArray V.empty)
    fieldconfigtype <- if version >= 3
      then deserialize
      else pure (0)
    fielddocumentation <- if version >= 3
      then if version >= 4 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsResourceResult
      {
      describeConfigsResourceResultName = fieldname
      ,
      describeConfigsResourceResultValue = fieldvalue
      ,
      describeConfigsResourceResultReadOnly = fieldreadonly
      ,
      describeConfigsResourceResultConfigSource = fieldconfigsource
      ,
      describeConfigsResourceResultIsSensitive = fieldissensitive
      ,
      describeConfigsResourceResultSynonyms = fieldsynonyms
      ,
      describeConfigsResourceResultConfigType = fieldconfigtype
      ,
      describeConfigsResourceResultDocumentation = fielddocumentation
      }


-- | The results for each resource.
data DescribeConfigsResult = DescribeConfigsResult
  {

  -- | The error code, or 0 if we were able to successfully describe the configurations.

  -- Versions: 0+
  describeConfigsResultErrorCode :: !(Int16)
,

  -- | The error message, or null if we were able to successfully describe the configurations.

  -- Versions: 0+
  describeConfigsResultErrorMessage :: !(KafkaString)
,

  -- | The resource type.

  -- Versions: 0+
  describeConfigsResultResourceType :: !(Int8)
,

  -- | The resource name.

  -- Versions: 0+
  describeConfigsResultResourceName :: !(KafkaString)
,

  -- | Each listed configuration.

  -- Versions: 0+
  describeConfigsResultConfigs :: !(KafkaArray (DescribeConfigsResourceResult))

  }
  deriving (Eq, Show, Generic)


-- | Encode DescribeConfigsResult with version-aware field handling.
encodeDescribeConfigsResult :: MonadPut m => E.ApiVersion -> DescribeConfigsResult -> m ()
encodeDescribeConfigsResult version dmsg =
  do
    serialize (describeConfigsResultErrorCode dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResultErrorMessage dmsg)) else serialize (describeConfigsResultErrorMessage dmsg)
    serialize (describeConfigsResultResourceType dmsg)
    if version >= 4 then serialize (toCompactString (describeConfigsResultResourceName dmsg)) else serialize (describeConfigsResultResourceName dmsg)
    E.encodeVersionedArray version 4 encodeDescribeConfigsResourceResult (case P.unKafkaArray (describeConfigsResultConfigs dmsg) of { P.NotNull v -> v; P.Null -> V.empty })
    when (version >= 4) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode DescribeConfigsResult with version-aware field handling.
decodeDescribeConfigsResult :: MonadGet m => E.ApiVersion -> m DescribeConfigsResult
decodeDescribeConfigsResult version =
  do
    fielderrorcode <- deserialize
    fielderrormessage <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldresourcetype <- deserialize
    fieldresourcename <- if version >= 4 then P.fromCompactString <$> deserialize else deserialize
    fieldconfigs <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResourceResult
    _ <- if version >= 4 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure DescribeConfigsResult
      {
      describeConfigsResultErrorCode = fielderrorcode
      ,
      describeConfigsResultErrorMessage = fielderrormessage
      ,
      describeConfigsResultResourceType = fieldresourcetype
      ,
      describeConfigsResultResourceName = fieldresourcename
      ,
      describeConfigsResultConfigs = fieldconfigs
      }



data DescribeConfigsResponse = DescribeConfigsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 0+
  describeConfigsResponseThrottleTimeMs :: !(Int32)
,

  -- | The results for each resource.

  -- Versions: 0+
  describeConfigsResponseResults :: !(KafkaArray (DescribeConfigsResult))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeConfigsResponse.
maxDescribeConfigsResponseVersion :: Int16
maxDescribeConfigsResponseVersion = 4

-- | KafkaMessage instance for DescribeConfigsResponse.
instance KafkaMessage DescribeConfigsResponse where
  messageApiKey = 32
  messageMinVersion = 1
  messageMaxVersion = 4
  messageFlexibleVersion = Just 4

-- | Encode DescribeConfigsResponse with the given API version.
encodeDescribeConfigsResponse :: MonadPut m => E.ApiVersion -> DescribeConfigsResponse -> m ()
encodeDescribeConfigsResponse version msg
  | version == 4 =
    do
      serialize (describeConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDescribeConfigsResult (case P.unKafkaArray (describeConfigsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)

  | version >= 1 && version <= 3 =
    do
      serialize (describeConfigsResponseThrottleTimeMs msg)
      E.encodeVersionedArray version 4 encodeDescribeConfigsResult (case P.unKafkaArray (describeConfigsResponseResults msg) of { P.NotNull v -> v; P.Null -> V.empty })

  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeConfigsResponse with the given API version.
decodeDescribeConfigsResponse :: MonadGet m => E.ApiVersion -> m DescribeConfigsResponse
decodeDescribeConfigsResponse version
  | version == 4 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResult
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeConfigsResponse
        {
        describeConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeConfigsResponseResults = fieldresults
        }

  | version >= 1 && version <= 3 =
    do
      fieldthrottletimems <- deserialize
      fieldresults <- P.mkKafkaArray <$> E.decodeVersionedArray version 4 decodeDescribeConfigsResult
      pure DescribeConfigsResponse
        {
        describeConfigsResponseThrottleTimeMs = fieldthrottletimems
        ,
        describeConfigsResponseResults = fieldresults
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Worst-case wire size of a DescribeConfigsSynonym.
wireMaxSizeDescribeConfigsSynonym :: Int -> DescribeConfigsSynonym -> Int
wireMaxSizeDescribeConfigsSynonym _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsSynonymName msg))
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsSynonymValue msg))
  + 1
  + 1

-- | Direct-poke encoder for DescribeConfigsSynonym.
wirePokeDescribeConfigsSynonym :: Int -> Ptr Word8 -> DescribeConfigsSynonym -> IO (Ptr Word8)
wirePokeDescribeConfigsSynonym version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeConfigsSynonymName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeConfigsSynonymValue msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (describeConfigsSynonymSource msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p3 else pure p3

-- | Direct-poke decoder for DescribeConfigsSynonym.
wirePeekDescribeConfigsSynonym :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsSynonym, Ptr Word8)
wirePeekDescribeConfigsSynonym version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_source, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p3 endPtr else pure p3
  pure (DescribeConfigsSynonym { describeConfigsSynonymName = f0_name, describeConfigsSynonymValue = f1_value, describeConfigsSynonymSource = f2_source }, pTagsEnd)

-- | Worst-case wire size of a DescribeConfigsResourceResult.
wireMaxSizeDescribeConfigsResourceResult :: Int -> DescribeConfigsResourceResult -> Int
wireMaxSizeDescribeConfigsResourceResult _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsResourceResultName msg))
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsResourceResultValue msg))
  + 1
  + 1
  + 1
  + (5 + (case P.unKafkaArray (describeConfigsResourceResultSynonyms msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeConfigsSynonym _version x ) v); P.Null -> 0 }))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsResourceResultDocumentation msg))
  + 1

-- | Direct-poke encoder for DescribeConfigsResourceResult.
wirePokeDescribeConfigsResourceResult :: Int -> Ptr Word8 -> DescribeConfigsResourceResult -> IO (Ptr Word8)
wirePokeDescribeConfigsResourceResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- WP.pokeCompactString p0 (P.toCompactString (describeConfigsResourceResultName msg))
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeConfigsResourceResultValue msg))
  p3 <- W.pokeWord8 p2 (if (describeConfigsResourceResultReadOnly msg) then 1 else 0)
  p4 <- W.pokeWord8 p3 (fromIntegral (describeConfigsResourceResultConfigSource msg))
  p5 <- W.pokeWord8 p4 (if (describeConfigsResourceResultIsSensitive msg) then 1 else 0)
  p6 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsSynonym version p x) p5 (describeConfigsResourceResultSynonyms msg)
  p7 <- W.pokeWord8 p6 (fromIntegral (describeConfigsResourceResultConfigType msg))
  p8 <- WP.pokeCompactString p7 (P.toCompactString (describeConfigsResourceResultDocumentation msg))
  if version >= 4 then WP.pokeEmptyTaggedFields p8 else pure p8

-- | Direct-poke decoder for DescribeConfigsResourceResult.
wirePeekDescribeConfigsResourceResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsResourceResult, Ptr Word8)
wirePeekDescribeConfigsResourceResult version _fp _basePtr p0 endPtr = do
  (f0_name, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
  (f1_value, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_readonly, p3) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p2 endPtr
  (f3_configsource, p4) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p3 endPtr
  (f4_issensitive, p5) <- (\(w, p') -> (w /= 0, p')) <$> W.peekWord8 p4 endPtr
  (f5_synonyms, p6) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsSynonym version _fp _basePtr p e) p5 endPtr
  (f6_configtype, p7) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p6 endPtr
  (f7_documentation, p8) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p7 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p8 endPtr else pure p8
  pure (DescribeConfigsResourceResult { describeConfigsResourceResultName = f0_name, describeConfigsResourceResultValue = f1_value, describeConfigsResourceResultReadOnly = f2_readonly, describeConfigsResourceResultConfigSource = f3_configsource, describeConfigsResourceResultIsSensitive = f4_issensitive, describeConfigsResourceResultSynonyms = f5_synonyms, describeConfigsResourceResultConfigType = f6_configtype, describeConfigsResourceResultDocumentation = f7_documentation }, pTagsEnd)

-- | Worst-case wire size of a DescribeConfigsResult.
wireMaxSizeDescribeConfigsResult :: Int -> DescribeConfigsResult -> Int
wireMaxSizeDescribeConfigsResult _version msg =
  0
  + 2
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsResultErrorMessage msg))
  + 1
  + WP.compactStringMaxSize (P.toCompactString (describeConfigsResultResourceName msg))
  + (5 + (case P.unKafkaArray (describeConfigsResultConfigs msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeConfigsResourceResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeConfigsResult.
wirePokeDescribeConfigsResult :: Int -> Ptr Word8 -> DescribeConfigsResult -> IO (Ptr Word8)
wirePokeDescribeConfigsResult version basePtr msg = do
  p0 <- pure basePtr
  p1 <- W.pokeInt16BE p0 (describeConfigsResultErrorCode msg)
  p2 <- WP.pokeCompactString p1 (P.toCompactString (describeConfigsResultErrorMessage msg))
  p3 <- W.pokeWord8 p2 (fromIntegral (describeConfigsResultResourceType msg))
  p4 <- WP.pokeCompactString p3 (P.toCompactString (describeConfigsResultResourceName msg))
  p5 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResourceResult version p x) p4 (describeConfigsResultConfigs msg)
  if version >= 4 then WP.pokeEmptyTaggedFields p5 else pure p5

-- | Direct-poke decoder for DescribeConfigsResult.
wirePeekDescribeConfigsResult :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsResult, Ptr Word8)
wirePeekDescribeConfigsResult version _fp _basePtr p0 endPtr = do
  (f0_errorcode, p1) <- W.peekInt16BE p0 endPtr
  (f1_errormessage, p2) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p1 endPtr
  (f2_resourcetype, p3) <- (\(w, p') -> (fromIntegral w :: Int8, p')) <$> W.peekWord8 p2 endPtr
  (f3_resourcename, p4) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p3 endPtr
  (f4_configs, p5) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResourceResult version _fp _basePtr p e) p4 endPtr
  pTagsEnd <- if version >= 4 then WP.peekAndSkipTaggedFields p5 endPtr else pure p5
  pure (DescribeConfigsResult { describeConfigsResultErrorCode = f0_errorcode, describeConfigsResultErrorMessage = f1_errormessage, describeConfigsResultResourceType = f2_resourcetype, describeConfigsResultResourceName = f3_resourcename, describeConfigsResultConfigs = f4_configs }, pTagsEnd)

-- | Worst-case wire size of a DescribeConfigsResponse.
wireMaxSizeDescribeConfigsResponse :: Int -> DescribeConfigsResponse -> Int
wireMaxSizeDescribeConfigsResponse _version msg =
  0
  + 4
  + (5 + (case P.unKafkaArray (describeConfigsResponseResults msg) of { P.NotNull v -> sum (fmap (\x -> wireMaxSizeDescribeConfigsResult _version x ) v); P.Null -> 0 }))
  + 1

-- | Direct-poke encoder for DescribeConfigsResponse.
wirePokeDescribeConfigsResponse :: Int -> Ptr Word8 -> DescribeConfigsResponse -> IO (Ptr Word8)
wirePokeDescribeConfigsResponse version basePtr msg
  | version == 4 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResult version p x) p1 (describeConfigsResponseResults msg)
    WP.pokeEmptyTaggedFields p2
  | version >= 1 && version <= 3 = do
    p0 <- pure basePtr
    p1 <- W.pokeInt32BE p0 (describeConfigsResponseThrottleTimeMs msg)
    p2 <- WP.pokeVersionedArray version 4 (\p x -> wirePokeDescribeConfigsResult version p x) p1 (describeConfigsResponseResults msg)
    pure p2
  | otherwise = error $ "wirePoke DescribeConfigsResponse : unsupported version: " ++ show version

-- | Direct-poke decoder for DescribeConfigsResponse.
wirePeekDescribeConfigsResponse :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (DescribeConfigsResponse, Ptr Word8)
wirePeekDescribeConfigsResponse version _fp _basePtr p0 endPtr
  | version == 4 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResult version _fp _basePtr p e) p1 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p2 endPtr
    pure (DescribeConfigsResponse { describeConfigsResponseThrottleTimeMs = f0_throttletimems, describeConfigsResponseResults = f1_results }, pTagsEnd)
  | version >= 1 && version <= 3 = do
    (f0_throttletimems, p1) <- W.peekInt32BE p0 endPtr
    (f1_results, p2) <- WP.peekVersionedArray version 4 (\p e -> wirePeekDescribeConfigsResult version _fp _basePtr p e) p1 endPtr
    pure (DescribeConfigsResponse { describeConfigsResponseThrottleTimeMs = f0_throttletimems, describeConfigsResponseResults = f1_results }, p2)
  | otherwise = error $ "wirePeek DescribeConfigsResponse : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec DescribeConfigsResponse where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeDescribeConfigsResponse (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeDescribeConfigsResponse (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekDescribeConfigsResponse (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}