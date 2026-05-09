{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.RemoveRaftVoterRequest
Description : Kafka RemoveRaftVoterRequest message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka request for API key 81.



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.RemoveRaftVoterRequest
  (
    RemoveRaftVoterRequest(..),
    encodeRemoveRaftVoterRequest,
    decodeRemoveRaftVoterRequest,
    maxRemoveRaftVoterRequestVersion
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




data RemoveRaftVoterRequest = RemoveRaftVoterRequest
  {

  -- | The cluster id of the request.

  -- Versions: 0+
  removeRaftVoterRequestClusterId :: !(KafkaString)
,

  -- | The replica id of the voter getting removed from the topic partition.

  -- Versions: 0+
  removeRaftVoterRequestVoterId :: !(Int32)
,

  -- | The directory id of the voter getting removed from the topic partition.

  -- Versions: 0+
  removeRaftVoterRequestVoterDirectoryId :: !(KafkaUuid)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for RemoveRaftVoterRequest.
maxRemoveRaftVoterRequestVersion :: Int16
maxRemoveRaftVoterRequestVersion = 0

-- | KafkaMessage instance for RemoveRaftVoterRequest.
instance KafkaMessage RemoveRaftVoterRequest where
  messageApiKey = 81
  messageMinVersion = 0
  messageMaxVersion = 0
  messageFlexibleVersion = Just 0

-- | Encode RemoveRaftVoterRequest with the given API version.
encodeRemoveRaftVoterRequest :: MonadPut m => E.ApiVersion -> RemoveRaftVoterRequest -> m ()
encodeRemoveRaftVoterRequest version msg
  | version == 0 =
    do
      serialize (toCompactString (removeRaftVoterRequestClusterId msg))
      serialize (removeRaftVoterRequestVoterId msg)
      serialize (removeRaftVoterRequestVoterDirectoryId msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode RemoveRaftVoterRequest with the given API version.
decodeRemoveRaftVoterRequest :: MonadGet m => E.ApiVersion -> m RemoveRaftVoterRequest
decodeRemoveRaftVoterRequest version
  | version == 0 =
    do
      fieldclusterid <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldvoterid <- deserialize
      fieldvoterdirectoryid <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure RemoveRaftVoterRequest
        {
        removeRaftVoterRequestClusterId = fieldclusterid
        ,
        removeRaftVoterRequestVoterId = fieldvoterid
        ,
        removeRaftVoterRequestVoterDirectoryId = fieldvoterdirectoryid
        }
  | otherwise = fail $ "Unsupported version: " ++ show version


-- | Worst-case wire size of a RemoveRaftVoterRequest.
wireMaxSizeRemoveRaftVoterRequest :: Int -> RemoveRaftVoterRequest -> Int
wireMaxSizeRemoveRaftVoterRequest _version msg =
  0
  + WP.compactStringMaxSize (P.toCompactString (removeRaftVoterRequestClusterId msg))
  + 4
  + 16
  + 1

-- | Direct-poke encoder for RemoveRaftVoterRequest.
wirePokeRemoveRaftVoterRequest :: Int -> Ptr Word8 -> RemoveRaftVoterRequest -> IO (Ptr Word8)
wirePokeRemoveRaftVoterRequest version basePtr msg
  | version == 0 = do
    p0 <- pure basePtr
    p1 <- WP.pokeCompactString p0 (P.toCompactString (removeRaftVoterRequestClusterId msg))
    p2 <- W.pokeInt32BE p1 (removeRaftVoterRequestVoterId msg)
    p3 <- WP.pokeKafkaUuid p2 (removeRaftVoterRequestVoterDirectoryId msg)
    WP.pokeEmptyTaggedFields p3
  | otherwise = error $ "wirePoke RemoveRaftVoterRequest : unsupported version: " ++ show version

-- | Direct-poke decoder for RemoveRaftVoterRequest.
wirePeekRemoveRaftVoterRequest :: Int -> ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> IO (RemoveRaftVoterRequest, Ptr Word8)
wirePeekRemoveRaftVoterRequest version _fp _basePtr p0 endPtr
  | version == 0 = do
    (f0_clusterid, p1) <- (\(cs, p') -> (P.fromCompactString cs, p')) <$> WP.peekCompactString p0 endPtr
    (f1_voterid, p2) <- W.peekInt32BE p1 endPtr
    (f2_voterdirectoryid, p3) <- WP.peekKafkaUuid p2 endPtr
    pTagsEnd <- WP.peekAndSkipTaggedFields p3 endPtr
    pure (RemoveRaftVoterRequest { removeRaftVoterRequestClusterId = f0_clusterid, removeRaftVoterRequestVoterId = f1_voterid, removeRaftVoterRequestVoterDirectoryId = f2_voterdirectoryid }, pTagsEnd)
  | otherwise = error $ "wirePeek RemoveRaftVoterRequest : unsupported version: " ++ show version


-- | Native 'WC.WireCodec' instance: 'WC.runEncodeVer' /
-- 'WC.runDecodeVer' dispatch into the direct-poke functions
-- generated above. There is no Serial fallback path.
instance WC.WireCodec RemoveRaftVoterRequest where
  wireCodec = WC.WireCodecImpl
    { WC.wireMaxSizeFor = \v msg -> wireMaxSizeRemoveRaftVoterRequest (fromIntegral v) msg
    , WC.wirePokeFor    = \v p msg -> wirePokeRemoveRaftVoterRequest (fromIntegral v) p msg
    , WC.wirePeekFor    = \v fp basePtr p endPtr ->
        wirePeekRemoveRaftVoterRequest (fromIntegral v) fp basePtr p endPtr
    }
  {-# INLINE wireCodec #-}