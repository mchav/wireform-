{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DescribeAclsRequest
Description : Kafka DescribeAclsRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 29.



Valid versions: 1-3
Flexible versions: 2+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DescribeAclsRequest
  (
    DescribeAclsRequest(..),
    encodeDescribeAclsRequest,
    decodeDescribeAclsRequest,
    maxDescribeAclsRequestVersion
  ) where

import Control.Monad (when)
import Data.Bytes.Get (MonadGet)
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
import qualified Kafka.Protocol.Wire.Codec as WC




data DescribeAclsRequest = DescribeAclsRequest
  {

  -- | The resource type.

  -- Versions: 0+
  describeAclsRequestResourceTypeFilter :: !(Int8)
,

  -- | The resource name, or null to match any resource name.

  -- Versions: 0+
  describeAclsRequestResourceNameFilter :: !(KafkaString)
,

  -- | The resource pattern to match.

  -- Versions: 1+
  describeAclsRequestPatternTypeFilter :: !(Int8)
,

  -- | The principal to match, or null to match any principal.

  -- Versions: 0+
  describeAclsRequestPrincipalFilter :: !(KafkaString)
,

  -- | The host to match, or null to match any host.

  -- Versions: 0+
  describeAclsRequestHostFilter :: !(KafkaString)
,

  -- | The operation to match.

  -- Versions: 0+
  describeAclsRequestOperation :: !(Int8)
,

  -- | The permission type to match.

  -- Versions: 0+
  describeAclsRequestPermissionType :: !(Int8)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DescribeAclsRequest.
maxDescribeAclsRequestVersion :: Int16
maxDescribeAclsRequestVersion = 3

-- | Encode DescribeAclsRequest with the given API version.
encodeDescribeAclsRequest :: MonadPut m => E.ApiVersion -> DescribeAclsRequest -> m ()
encodeDescribeAclsRequest version msg
  | version == 1 =
    do
      serialize (describeAclsRequestResourceTypeFilter msg)
      serialize (describeAclsRequestResourceNameFilter msg)
      serialize (describeAclsRequestPatternTypeFilter msg)
      serialize (describeAclsRequestPrincipalFilter msg)
      serialize (describeAclsRequestHostFilter msg)
      serialize (describeAclsRequestOperation msg)
      serialize (describeAclsRequestPermissionType msg)


  | version >= 2 && version <= 3 =
    do
      serialize (describeAclsRequestResourceTypeFilter msg)
      serialize (toCompactString (describeAclsRequestResourceNameFilter msg))
      serialize (describeAclsRequestPatternTypeFilter msg)
      serialize (toCompactString (describeAclsRequestPrincipalFilter msg))
      serialize (toCompactString (describeAclsRequestHostFilter msg))
      serialize (describeAclsRequestOperation msg)
      serialize (describeAclsRequestPermissionType msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DescribeAclsRequest with the given API version.
decodeDescribeAclsRequest :: MonadGet m => E.ApiVersion -> m DescribeAclsRequest
decodeDescribeAclsRequest version
  | version == 1 =
    do
      fieldresourcetypefilter <- deserialize
      fieldresourcenamefilter <- deserialize
      fieldpatterntypefilter <- deserialize
      fieldprincipalfilter <- deserialize
      fieldhostfilter <- deserialize
      fieldoperation <- deserialize
      fieldpermissiontype <- deserialize
      pure DescribeAclsRequest
        {
        describeAclsRequestResourceTypeFilter = fieldresourcetypefilter
        ,
        describeAclsRequestResourceNameFilter = fieldresourcenamefilter
        ,
        describeAclsRequestPatternTypeFilter = fieldpatterntypefilter
        ,
        describeAclsRequestPrincipalFilter = fieldprincipalfilter
        ,
        describeAclsRequestHostFilter = fieldhostfilter
        ,
        describeAclsRequestOperation = fieldoperation
        ,
        describeAclsRequestPermissionType = fieldpermissiontype
        }

  | version >= 2 && version <= 3 =
    do
      fieldresourcetypefilter <- deserialize
      fieldresourcenamefilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldpatterntypefilter <- deserialize
      fieldprincipalfilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldhostfilter <- if version >= 2 then P.fromCompactString <$> deserialize else deserialize
      fieldoperation <- deserialize
      fieldpermissiontype <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DescribeAclsRequest
        {
        describeAclsRequestResourceTypeFilter = fieldresourcetypefilter
        ,
        describeAclsRequestResourceNameFilter = fieldresourcenamefilter
        ,
        describeAclsRequestPatternTypeFilter = fieldpatterntypefilter
        ,
        describeAclsRequestPrincipalFilter = fieldprincipalfilter
        ,
        describeAclsRequestHostFilter = fieldhostfilter
        ,
        describeAclsRequestOperation = fieldoperation
        ,
        describeAclsRequestPermissionType = fieldpermissiontype
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DescribeAclsRequest where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
