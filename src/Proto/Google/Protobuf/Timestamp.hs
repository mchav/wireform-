{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Proto.Google.Protobuf.Timestamp
  ( Timestamp (..)
  , defaultTimestamp
  ) where

import Data.Int (Int32, Int64)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Message (IsMessage(..))
import Proto.Schema
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize)

data Timestamp = Timestamp
  { seconds :: {-# UNPACK #-} !Int64
  , nanos   :: {-# UNPACK #-} !Int32
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

defaultTimestamp :: Timestamp
defaultTimestamp = Timestamp 0 0

instance MessageEncode Timestamp where
  buildMessage (Timestamp s n) =
    (if s == 0 then mempty else encodeFieldVarint 1 (fromIntegral s)) <>
    (if n == 0 then mempty else encodeFieldVarint 2 (fromIntegral n))
  {-# INLINE buildMessage #-}

instance MessageSize Timestamp where
  messageSize (Timestamp s n) =
    (if s == 0 then 0 else fieldVarintSize 1 (fromIntegral s)) +
    (if n == 0 then 0 else fieldVarintSize 2 (fromIntegral n))
  {-# INLINE messageSize #-}

instance MessageDecode Timestamp where
  messageDecoder = loop 0 0
    where
      loop !s !n = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Timestamp s n)
          Just (Tag fn wt) -> case fn of
            1 -> do v <- getVarint; loop (fromIntegral v) n
            2 -> do v <- getVarint; loop s (fromIntegral v)
            _ -> skipField wt >> loop s n
  {-# INLINE messageDecoder #-}

instance IsMessage Timestamp where
  messageTypeName _ = "google.protobuf.Timestamp"

instance ProtoMessage Timestamp where
  protoMessageName _ = "google.protobuf.Timestamp"
  protoPackageName _ = "google.protobuf"
  protoDefaultValue = defaultTimestamp
  protoFieldDescriptors _ = Map.fromList
    [ (1, SomeField FieldDescriptor
        { fdName = "seconds", fdNumber = 1
        , fdTypeDesc = ScalarType Int64Field
        , fdLabel = LabelOptional
        , fdGet = seconds, fdSet = \v m -> m { seconds = v }
        })
    , (2, SomeField FieldDescriptor
        { fdName = "nanos", fdNumber = 2
        , fdTypeDesc = ScalarType Int32Field
        , fdLabel = LabelOptional
        , fdGet = nanos, fdSet = \v m -> m { nanos = v }
        })
    ]

instance HasField Timestamp "seconds" Int64 where
  getField = seconds
  setField v m = m { seconds = v }
  fieldDescriptor _ _ = FieldDescriptor
    { fdName = "seconds", fdNumber = 1
    , fdTypeDesc = ScalarType Int64Field
    , fdLabel = LabelOptional
    , fdGet = seconds, fdSet = \v m -> m { seconds = v }
    }

instance HasField Timestamp "nanos" Int32 where
  getField = nanos
  setField v m = m { nanos = v }
  fieldDescriptor _ _ = FieldDescriptor
    { fdName = "nanos", fdNumber = 2
    , fdTypeDesc = ScalarType Int32Field
    , fdLabel = LabelOptional
    , fdGet = nanos, fdSet = \v m -> m { nanos = v }
    }
