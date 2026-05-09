{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-|
Module      : Kafka.Protocol.Generated.DefaultPrincipalData
Description : Kafka DefaultPrincipalData message
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka data (no API key).



Valid versions: 0
Flexible versions: 0+

This code is auto-generated from Kafka protocol definitions.
-}

module Kafka.Protocol.Generated.DefaultPrincipalData
  (
    DefaultPrincipalData(..),
    encodeDefaultPrincipalData,
    decodeDefaultPrincipalData,
    maxDefaultPrincipalDataVersion
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




data DefaultPrincipalData = DefaultPrincipalData
  {

  -- | The principal type.

  -- Versions: 0+
  defaultPrincipalDataType :: !(KafkaString)
,

  -- | The principal name.

  -- Versions: 0+
  defaultPrincipalDataName :: !(KafkaString)
,

  -- | Whether the principal was authenticated by a delegation token on the forwarding broker.

  -- Versions: 0+
  defaultPrincipalDataTokenAuthenticated :: !(Bool)

  }
  deriving (Eq, Show, Generic)

-- | Maximum supported version for DefaultPrincipalData.
maxDefaultPrincipalDataVersion :: Int16
maxDefaultPrincipalDataVersion = 0

-- | Encode DefaultPrincipalData with the given API version.
encodeDefaultPrincipalData :: MonadPut m => E.ApiVersion -> DefaultPrincipalData -> m ()
encodeDefaultPrincipalData version msg
  | version == 0 =
    do
      serialize (toCompactString (defaultPrincipalDataType msg))
      serialize (toCompactString (defaultPrincipalDataName msg))
      serialize (defaultPrincipalDataTokenAuthenticated msg)
      serialize (emptyTaggedFields :: TaggedFields)
  | otherwise = error $ "Unsupported version: " ++ show version

-- | Decode DefaultPrincipalData with the given API version.
decodeDefaultPrincipalData :: MonadGet m => E.ApiVersion -> m DefaultPrincipalData
decodeDefaultPrincipalData version
  | version == 0 =
    do
      fieldtype <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldname <- if version >= 0 then P.fromCompactString <$> deserialize else deserialize
      fieldtokenauthenticated <- deserialize
      _ <- (deserialize :: MonadGet m => m TaggedFields)
      pure DefaultPrincipalData
        {
        defaultPrincipalDataType = fieldtype
        ,
        defaultPrincipalDataName = fieldname
        ,
        defaultPrincipalDataTokenAuthenticated = fieldtokenauthenticated
        }
  | otherwise = fail $ "Unsupported version: " ++ show version

-- | Default 'WC.WireCodec' instance: 'wireCodec = Nothing' makes
-- 'WC.runEncodeVer' / 'WC.runDecodeVer' fall through to the
-- 'Data.Bytes.Serial' encoders / decoders defined above. Modules
-- migrated to a native 'Wire' codec override this with a
-- 'Just'-valued 'WireCodecImpl'.
instance WC.WireCodec DefaultPrincipalData where
  wireCodec = Nothing
  {-# INLINE wireCodec #-}
