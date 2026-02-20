{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Wrappers
  ( DoubleValue (..), defaultDoubleValue
  , FloatValue (..), defaultFloatValue
  , Int64Value (..), defaultInt64Value
  , UInt64Value (..), defaultUInt64Value
  , Int32Value (..), defaultInt32Value
  , UInt32Value (..), defaultUInt32Value
  , BoolValue (..), defaultBoolValue
  , StringValue (..), defaultStringValue
  , BytesValue (..), defaultBytesValue
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize, fieldBoolSize, fieldFloatSize,
  fieldDoubleSize, fieldTextSize, fieldBytesSize)

-- Each wrapper is a message with a single field "value" at field number 1.

newtype DoubleValue = DoubleValue { doubleValue :: Double }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultDoubleValue :: DoubleValue
defaultDoubleValue = DoubleValue 0

instance MessageEncode DoubleValue where
  buildMessage (DoubleValue v) = if v == 0 then mempty else encodeFieldDouble 1 v
instance MessageSize DoubleValue where
  messageSize (DoubleValue v) = if v == 0 then 0 else fieldDoubleSize 1
instance MessageDecode DoubleValue where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (DoubleValue v)
          Just (Tag 1 _) -> getDouble >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop v

newtype FloatValue = FloatValue { floatValue :: Float }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultFloatValue :: FloatValue
defaultFloatValue = FloatValue 0

instance MessageEncode FloatValue where
  buildMessage (FloatValue v) = if v == 0 then mempty else encodeFieldFloat 1 v
instance MessageSize FloatValue where
  messageSize (FloatValue v) = if v == 0 then 0 else fieldFloatSize 1
instance MessageDecode FloatValue where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (FloatValue v)
          Just (Tag 1 _) -> getFloat >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop v

newtype Int64Value = Int64Value { int64Value :: Int64 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultInt64Value :: Int64Value
defaultInt64Value = Int64Value 0

instance MessageEncode Int64Value where
  buildMessage (Int64Value v) = if v == 0 then mempty else encodeFieldVarint 1 (fromIntegral v)
instance MessageSize Int64Value where
  messageSize (Int64Value v) = if v == 0 then 0 else fieldVarintSize 1 (fromIntegral v)
instance MessageDecode Int64Value where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Int64Value v)
          Just (Tag 1 _) -> getVarint >>= \x -> loop (fromIntegral x)
          Just (Tag _ wt) -> skipField wt >> loop v

newtype UInt64Value = UInt64Value { uint64Value :: Word64 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultUInt64Value :: UInt64Value
defaultUInt64Value = UInt64Value 0

instance MessageEncode UInt64Value where
  buildMessage (UInt64Value v) = if v == 0 then mempty else encodeFieldVarint 1 v
instance MessageSize UInt64Value where
  messageSize (UInt64Value v) = if v == 0 then 0 else fieldVarintSize 1 v
instance MessageDecode UInt64Value where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (UInt64Value v)
          Just (Tag 1 _) -> getVarint >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop v

newtype Int32Value = Int32Value { int32Value :: Int32 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultInt32Value :: Int32Value
defaultInt32Value = Int32Value 0

instance MessageEncode Int32Value where
  buildMessage (Int32Value v) = if v == 0 then mempty else encodeFieldVarint 1 (fromIntegral v)
instance MessageSize Int32Value where
  messageSize (Int32Value v) = if v == 0 then 0 else fieldVarintSize 1 (fromIntegral v)
instance MessageDecode Int32Value where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Int32Value v)
          Just (Tag 1 _) -> getVarint >>= \x -> loop (fromIntegral x)
          Just (Tag _ wt) -> skipField wt >> loop v

newtype UInt32Value = UInt32Value { uint32Value :: Word32 }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultUInt32Value :: UInt32Value
defaultUInt32Value = UInt32Value 0

instance MessageEncode UInt32Value where
  buildMessage (UInt32Value v) = if v == 0 then mempty else encodeFieldVarint 1 (fromIntegral v)
instance MessageSize UInt32Value where
  messageSize (UInt32Value v) = if v == 0 then 0 else fieldVarintSize 1 (fromIntegral v)
instance MessageDecode UInt32Value where
  messageDecoder = loop 0
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (UInt32Value v)
          Just (Tag 1 _) -> getVarint >>= \x -> loop (fromIntegral x)
          Just (Tag _ wt) -> skipField wt >> loop v

newtype BoolValue = BoolValue { boolValue :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultBoolValue :: BoolValue
defaultBoolValue = BoolValue False

instance MessageEncode BoolValue where
  buildMessage (BoolValue v) = if not v then mempty else encodeFieldBool 1 v
instance MessageSize BoolValue where
  messageSize (BoolValue v) = if not v then 0 else fieldBoolSize 1
instance MessageDecode BoolValue where
  messageDecoder = loop False
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (BoolValue v)
          Just (Tag 1 _) -> getVarint >>= \x -> loop (x /= 0)
          Just (Tag _ wt) -> skipField wt >> loop v

newtype StringValue = StringValue { stringValue :: Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultStringValue :: StringValue
defaultStringValue = StringValue ""

instance MessageEncode StringValue where
  buildMessage (StringValue v) = if v == "" then mempty else encodeFieldString 1 v
instance MessageSize StringValue where
  messageSize (StringValue v) = if v == "" then 0 else fieldTextSize 1 v
instance MessageDecode StringValue where
  messageDecoder = loop ""
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (StringValue v)
          Just (Tag 1 _) -> decodeFieldString >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop v

newtype BytesValue = BytesValue { bytesValue :: ByteString }
  deriving stock (Show, Eq, Generic)
  deriving anyclass NFData

defaultBytesValue :: BytesValue
defaultBytesValue = BytesValue ""

instance MessageEncode BytesValue where
  buildMessage (BytesValue v) = if BS.null v then mempty else encodeFieldBytes 1 v
instance MessageSize BytesValue where
  messageSize (BytesValue v) = if BS.null v then 0 else fieldBytesSize 1 v
instance MessageDecode BytesValue where
  messageDecoder = loop ""
    where
      loop !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (BytesValue v)
          Just (Tag 1 _) -> decodeFieldBytes >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop v
