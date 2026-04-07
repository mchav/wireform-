-- | BSON (Binary JSON) value representation.
--
-- BSON is the binary serialization format used by MongoDB. This module
-- defines a dynamically-typed value covering all BSON element types:
-- doubles, strings, documents (ordered key-value maps), arrays, binary
-- data, booleans, datetimes, null, 32-bit and 64-bit integers, ObjectIds,
-- and regular expressions.
--
-- @
-- import qualified BSON.Value as B
-- import qualified BSON.Encode as BE
-- import qualified BSON.Decode as BD
-- import qualified Data.Vector as V
--
-- let doc = B.Document (V.fromList [(\"name\", B.String \"Alice\"), (\"age\", B.Int32 30)])
-- let bytes = BE.encode doc
-- let Right decoded = BD.decode bytes
-- @
module BSON.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int32, Int64)
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
  = Double   {-# UNPACK #-} !Double
  | String   !Text
  | Document !(Vector (Text, Value))
  | Array    !(Vector Value)
  | Binary   !ByteString
  | Bool     !Bool
  | DateTime {-# UNPACK #-} !Int64
  | Null
  | Int32    {-# UNPACK #-} !Int32
  | Int64    {-# UNPACK #-} !Int64
  | ObjectId !ByteString
  | Regex    !Text !Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)

instance Aeson.ToJSON Value where
  toJSON (Double d)      = Aeson.Number (fromFloatDigits d)
  toJSON (String t)      = Aeson.String t
  toJSON (Document kvs)  = Aeson.Object $ KM.fromList
    [(Key.fromText k, Aeson.toJSON v) | (k, v) <- V.toList kvs]
  toJSON (Array vs)      = Aeson.Array (V.map Aeson.toJSON vs)
  toJSON (Binary bs)     = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (Bool b)        = Aeson.Bool b
  toJSON (DateTime ms)   = Aeson.object [(Key.fromText "$date", Aeson.Number (fromIntegral ms))]
  toJSON Null            = Aeson.Null
  toJSON (Int32 n)       = Aeson.Number (fromIntegral n)
  toJSON (Int64 n)       = Aeson.Number (fromIntegral n)
  toJSON (ObjectId bs)   = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
  toJSON (Regex pat opts) = Aeson.object
    [(Key.fromText "$regex", Aeson.String pat), (Key.fromText "$options", Aeson.String opts)]

instance Aeson.FromJSON Value where
  parseJSON Aeson.Null       = pure Null
  parseJSON (Aeson.Bool b)   = pure (Bool b)
  parseJSON (Aeson.String t) = pure (String t)
  parseJSON (Aeson.Number n) = pure $ case toBoundedInteger n :: Maybe Int32 of
    Just i  -> Int32 i
    Nothing -> case toBoundedInteger n :: Maybe Int64 of
      Just i  -> Int64 i
      Nothing -> Double (toRealFloat n)
  parseJSON (Aeson.Array arr) = Array <$> V.mapM Aeson.parseJSON arr
  parseJSON (Aeson.Object obj) = pure $ Document $ V.fromList
    [(Key.toText k, fromAesonValue v) | (k, v) <- KM.toList obj]

fromAesonValue :: Aeson.Value -> Value
fromAesonValue Aeson.Null       = Null
fromAesonValue (Aeson.Bool b)   = Bool b
fromAesonValue (Aeson.String t) = String t
fromAesonValue (Aeson.Number n) = case toBoundedInteger n :: Maybe Int32 of
  Just i  -> Int32 i
  Nothing -> case toBoundedInteger n :: Maybe Int64 of
    Just i  -> Int64 i
    Nothing -> Double (toRealFloat n)
fromAesonValue (Aeson.Array arr) = Array (V.map fromAesonValue arr)
fromAesonValue (Aeson.Object obj) = Document $ V.fromList
  [(Key.toText k, fromAesonValue v) | (k, v) <- KM.toList obj]
