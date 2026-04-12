-- | Amazon Ion binary value representation.
--
-- Amazon Ion is a richly-typed, self-describing data format used by AWS
-- services. This module defines a dynamically-typed value covering Ion's
-- core types: null, bool, int, float, string, blob, clob, list, struct,
-- symbol, and annotation.
--
-- @
-- import qualified Ion.Value as I
-- import qualified Ion.Encode as IE
-- import qualified Ion.Decode as ID
-- import qualified Data.Vector as V
--
-- let val = I.Struct (V.fromList [(\"name\", I.String \"Alice\"), (\"age\", I.Int 30)])
-- let bytes = IE.encode val
-- let Right decoded = ID.decode bytes
-- @
module Ion.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int64)
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

data Value
  = Null
  | Bool       !Bool
  | Int        {-# UNPACK #-} !Int64
  | Float      {-# UNPACK #-} !Double
  | String     !Text
  | Blob       !ByteString
  | Clob       !ByteString
  | List       !(Vector Value)
  | Struct     !(Vector (Text, Value))
  | Symbol     !Text
  | Annotation !Text !Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Value where
  toJSON Null             = Aeson.Null
  toJSON (Bool b)         = Aeson.Bool b
  toJSON (Int n)          = Aeson.Number (fromIntegral n)
  toJSON (Float d)        = Aeson.Number (fromFloatDigits d)
  toJSON (String t)       = Aeson.String t
  toJSON (Blob bs)        = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (Clob bs)        = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (List vs)        = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (Struct kvs)     = Aeson.Object $ KM.fromList
    [(Key.fromText k, Aeson.toJSON v) | (k, v) <- V.toList kvs]
  toJSON (Symbol t)       = Aeson.String t
  toJSON (Annotation _ v) = Aeson.toJSON v

instance Aeson.FromJSON Value where
  parseJSON Aeson.Null       = pure Null
  parseJSON (Aeson.Bool b)   = pure (Bool b)
  parseJSON (Aeson.String t) = pure (String t)
  parseJSON (Aeson.Number n) = pure $ case toBoundedInteger n :: Maybe Int64 of
    Just i  -> Int i
    Nothing -> Float (toRealFloat n)
  parseJSON (Aeson.Array arr) = List <$> V.mapM Aeson.parseJSON arr
  parseJSON (Aeson.Object obj) = pure $ Struct $ V.fromList
    [(Key.toText k, ionFromAesonValue v) | (k, v) <- KM.toList obj]

ionFromAesonValue :: Aeson.Value -> Value
ionFromAesonValue Aeson.Null       = Null
ionFromAesonValue (Aeson.Bool b)   = Bool b
ionFromAesonValue (Aeson.String t) = String t
ionFromAesonValue (Aeson.Number n) = case toBoundedInteger n :: Maybe Int64 of
  Just i  -> Int i
  Nothing -> Float (toRealFloat n)
ionFromAesonValue (Aeson.Array arr) = List (V.map ionFromAesonValue arr)
ionFromAesonValue (Aeson.Object obj) = Struct $ V.fromList
  [(Key.toText k, ionFromAesonValue v) | (k, v) <- KM.toList obj]
