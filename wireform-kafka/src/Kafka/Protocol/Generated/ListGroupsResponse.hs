{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.ListGroupsResponse
Description : Kafka ListGroupsResponse message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka response for API key 16.



Valid versions: 0-5
Flexible versions: 3+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.ListGroupsResponse
  (
    ListGroupsResponse(..),
    ListedGroup(..),
    encodeListGroupsResponse,
    decodeListGroupsResponse,
    maxListGroupsResponseVersion
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


-- | Each group in the response.
data ListedGroup = ListedGroup
  {

  -- | The group ID.

  -- Versions: 0+
  listedGroupGroupId :: !(KafkaString)
,

  -- | The group protocol type.

  -- Versions: 0+
  listedGroupProtocolType :: !(KafkaString)
,

  -- | The group state name.

  -- Versions: 4+
  listedGroupGroupState :: !(KafkaString)
,

  -- | The group type name.

  -- Versions: 5+
  listedGroupGroupType :: !(KafkaString)

  }
  deriving (Eq, Show, Generic)


-- | Encode ListedGroup with version-aware field handling.
encodeListedGroup :: MonadPut m => E.ApiVersion -> ListedGroup -> m ()
encodeListedGroup version lmsg =
  do
    if version >= 3 then serialize (toCompactString (listedGroupGroupId lmsg)) else serialize (listedGroupGroupId lmsg)
    if version >= 3 then serialize (toCompactString (listedGroupProtocolType lmsg)) else serialize (listedGroupProtocolType lmsg)
    when (version >= 4) $
      if version >= 3 then serialize (toCompactString (listedGroupGroupState lmsg)) else serialize (listedGroupGroupState lmsg)
    when (version >= 5) $
      if version >= 3 then serialize (toCompactString (listedGroupGroupType lmsg)) else serialize (listedGroupGroupType lmsg)
    when (version >= 3) $ serialize (emptyTaggedFields :: TaggedFields)


-- | Decode ListedGroup with version-aware field handling.
decodeListedGroup :: MonadGet m => E.ApiVersion -> m ListedGroup
decodeListedGroup version =
  do
    fieldgroupid <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldprotocoltype <- if version >= 3 then P.fromCompactString <$> deserialize else deserialize
    fieldgroupstate <- if version >= 4
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    fieldgrouptype <- if version >= 5
      then if version >= 3 then P.fromCompactString <$> deserialize else deserialize
      else pure (P.KafkaString Null)
    _ <- if version >= 3 then (deserialize :: MonadGet m => m TaggedFields) else pure emptyTaggedFields
    pure ListedGroup
      {
      listedGroupGroupId = fieldgroupid
      ,
      listedGroupProtocolType = fieldprotocoltype
      ,
      listedGroupGroupState = fieldgroupstate
      ,
      listedGroupGroupType = fieldgrouptype
      }



data ListGroupsResponse = ListGroupsResponse
  {

  -- | The duration in milliseconds for which the request was throttled due to a quota violation, or zero i

  -- Versions: 1+
  listGroupsResponseThrottleTimeMs :: !(Int32)
,

  -- | The error code, or 0 if there was no error.

  -- Versions: 0+
  listGroupsResponseErrorCode :: !(Int16)
,

  -- | Each group in the response.

  -- Versions: 0+
  listGroupsResponseGroups :: !(KafkaArray (ListedGroup))

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for ListGroupsResponse.
maxListGroupsResponseVersion :: Int16
maxListGroupsResponseVersion = 5

-- | Encode ListGroupsResponse with the given API version.
encodeListGroupsResponse :: MonadPut m => E.ApiVersion -> ListGroupsResponse -> m ()
encodeListGroupsResponse version msg
  | version == 0 =
    do
      serialize (listGroupsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeListedGroup (case P.unKafkaArray (listGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 1 && version <= 2 =
    do
      serialize (listGroupsResponseThrottleTimeMs msg)
      serialize (listGroupsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeListedGroup (case P.unKafkaArray (listGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })


  | version >= 3 && version <= 5 =
    do
      serialize (listGroupsResponseThrottleTimeMs msg)
      serialize (listGroupsResponseErrorCode msg)
      E.encodeVersionedArray version 3 encodeListedGroup (case P.unKafkaArray (listGroupsResponseGroups msg) of { P.NotNull v -> v; P.Null -> V.empty })
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode ListGroupsResponse with the given API version.
decodeListGroupsResponse :: MonadGet m => E.ApiVersion -> m ListGroupsResponse
decodeListGroupsResponse version
  | version == 0 =
    do
      fielderrorcode <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeListedGroup
      pure ListGroupsResponse
        {
        listGroupsResponseThrottleTimeMs = 0
        ,
        listGroupsResponseErrorCode = fielderrorcode
        ,
        listGroupsResponseGroups = fieldgroups
        }

  | version >= 1 && version <= 2 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeListedGroup
      pure ListGroupsResponse
        {
        listGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listGroupsResponseErrorCode = fielderrorcode
        ,
        listGroupsResponseGroups = fieldgroups
        }

  | version >= 3 && version <= 5 =
    do
      fieldthrottletimems <- deserialize
      fielderrorcode <- deserialize
      fieldgroups <- P.mkKafkaArray <$> E.decodeVersionedArray version 3 decodeListedGroup
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure ListGroupsResponse
        {
        listGroupsResponseThrottleTimeMs = fieldthrottletimems
        ,
        listGroupsResponseErrorCode = fielderrorcode
        ,
        listGroupsResponseGroups = fieldgroups
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec ListGroupsResponse where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
