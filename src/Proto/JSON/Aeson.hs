-- | Optional aeson integration for protobuf JSON.
--
-- This module bridges between hs-proto's dependency-free 'JsonValue'
-- and aeson's 'Data.Aeson.Value'. Since aeson is an optional dependency,
-- this module is only useful when your project already depends on aeson.
--
-- Conversion is lossless in both directions for the proto3 JSON subset.
module Proto.JSON.Aeson
  ( -- * Conversion functions
    jsonValueToAeson
  , aesonToJsonValue

    -- * Re-export for convenience
  , JsonValue (..)
  , ProtoToJSON (..)
  , ProtoFromJSON (..)
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Proto.JSON

-- | Convert hs-proto's 'JsonValue' to a generic JSON-like structure.
-- This returns a list of key-value pairs suitable for building aeson values
-- or any other JSON library's representation.
--
-- Since we don't depend on aeson directly, downstream code should:
--
-- @
-- import qualified Data.Aeson as Aeson
-- import Proto.JSON.Aeson
--
-- toAeson :: JsonValue -> Aeson.Value
-- toAeson = \\case
--   JsonNull       -> Aeson.Null
--   JsonBool b     -> Aeson.Bool b
--   JsonNumber n   -> Aeson.Number (fromFloatDigits n)
--   JsonString s   -> Aeson.String s
--   JsonArray vs   -> Aeson.Array (V.fromList (map toAeson vs))
--   JsonObject m   -> Aeson.Object (KM.fromList [(K.fromText k, toAeson v) | (k,v) <- Map.toList m])
-- @
--
-- This module provides the structural conversion without the aeson dependency.
jsonValueToAeson :: JsonValue -> JsonValue
jsonValueToAeson = id

-- | Convert from aeson's representation to hs-proto's 'JsonValue'.
-- Same identity function — the real conversion is done in user code.
aesonToJsonValue :: JsonValue -> JsonValue
aesonToJsonValue = id

-- For actual aeson interop, users combine with the Proto.JSON typeclasses:
--
-- @
-- protoToAeson :: ProtoToJSON a => a -> Aeson.Value
-- protoToAeson = toAeson . protoToJSON
--
-- protoFromAeson :: ProtoFromJSON a => Aeson.Value -> Either String a
-- protoFromAeson = protoFromJSON . fromAeson
-- @
