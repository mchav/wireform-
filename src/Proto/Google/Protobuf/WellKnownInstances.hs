{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | IsMessage instances for well-known protobuf types.
-- These associate each type with its fully-qualified proto name,
-- enabling type-safe Any packing/unpacking.
module Proto.Google.Protobuf.WellKnownInstances () where

import Proto.Message (IsMessage(..))
import Proto.Google.Protobuf.Any (Any(..))
import Proto.Google.Protobuf.Timestamp (Timestamp(..))
import Proto.Google.Protobuf.Duration (Duration(..))
import Proto.Google.Protobuf.Empty (Empty(..))
import Proto.Google.Protobuf.Struct (Struct(..), Value(..), ListValue(..))
import Proto.Google.Protobuf.FieldMask (FieldMask(..))
import Proto.Google.Protobuf.SourceContext (SourceContext(..))
import Proto.Google.Protobuf.Wrappers

instance IsMessage Any where
  messageTypeName _ = "google.protobuf.Any"

instance IsMessage Timestamp where
  messageTypeName _ = "google.protobuf.Timestamp"

instance IsMessage Duration where
  messageTypeName _ = "google.protobuf.Duration"

instance IsMessage Empty where
  messageTypeName _ = "google.protobuf.Empty"

instance IsMessage Struct where
  messageTypeName _ = "google.protobuf.Struct"

instance IsMessage Value where
  messageTypeName _ = "google.protobuf.Value"

instance IsMessage ListValue where
  messageTypeName _ = "google.protobuf.ListValue"

instance IsMessage FieldMask where
  messageTypeName _ = "google.protobuf.FieldMask"

instance IsMessage SourceContext where
  messageTypeName _ = "google.protobuf.SourceContext"

instance IsMessage DoubleValue where
  messageTypeName _ = "google.protobuf.DoubleValue"

instance IsMessage FloatValue where
  messageTypeName _ = "google.protobuf.FloatValue"

instance IsMessage Int64Value where
  messageTypeName _ = "google.protobuf.Int64Value"

instance IsMessage UInt64Value where
  messageTypeName _ = "google.protobuf.UInt64Value"

instance IsMessage Int32Value where
  messageTypeName _ = "google.protobuf.Int32Value"

instance IsMessage UInt32Value where
  messageTypeName _ = "google.protobuf.UInt32Value"

instance IsMessage BoolValue where
  messageTypeName _ = "google.protobuf.BoolValue"

instance IsMessage StringValue where
  messageTypeName _ = "google.protobuf.StringValue"

instance IsMessage BytesValue where
  messageTypeName _ = "google.protobuf.BytesValue"
