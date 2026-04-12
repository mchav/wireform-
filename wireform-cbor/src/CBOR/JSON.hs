{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | CBOR / JSON interconversion (RFC 8949 Section 6.1).
--
-- Converts between 'CBOR.Value.Value' and 'Data.Aeson.Value' following
-- the preferred serialization mapping from RFC 8949 Section 6.1. The
-- mapping is lossy: CBOR byte strings are base64url-encoded, tags are
-- dropped, and negative integers use JSON numbers.
module CBOR.JSON
  ( toJSON
  , fromJSON
  ) where

import Prelude hiding (map)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64)

import qualified CBOR.Value as C

-- | Convert a CBOR 'C.Value' to a JSON 'Aeson.Value'.
toJSON :: C.Value -> Aeson.Value
toJSON (C.UInt n)      = Aeson.Number (fromIntegral n)
toJSON (C.NInt n)      = Aeson.Number (fromIntegral (negate (fromIntegral n :: Integer) - 1))
toJSON (C.Bool b)      = Aeson.Bool b
toJSON C.Null          = Aeson.Null
toJSON C.Undefined     = Aeson.Null
toJSON (C.Float16 f)   = floatToJSON f
toJSON (C.Float32 f)   = floatToJSON f
toJSON (C.Float64 d)   = doubleToJSON d
toJSON (C.ByteString bs) = Aeson.String (TE.decodeUtf8Lenient (B64.encode bs))
toJSON (C.TextString t)  = Aeson.String t
toJSON (C.Array vec)     = Aeson.Array (V.map toJSON vec)
toJSON (C.Map vec)       = mapToJSON vec
toJSON (C.Tag tagNum content) =
  Aeson.Object $ KM.fromList
    [ (Key.fromText "tag", Aeson.Number (fromIntegral tagNum))
    , (Key.fromText "value", toJSON content)
    ]
toJSON (C.Simple n) = Aeson.Number (fromIntegral n)

-- | Convert a JSON 'Aeson.Value' to a CBOR 'C.Value'.
fromJSON :: Aeson.Value -> C.Value
fromJSON Aeson.Null       = C.Null
fromJSON (Aeson.Bool b)   = C.Bool b
fromJSON (Aeson.String t) = C.TextString t
fromJSON (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int of
    Just i
      | i >= 0    -> C.UInt (fromIntegral i)
      | otherwise -> C.NInt (fromIntegral (negate (fromIntegral i :: Integer) - 1))
    Nothing -> C.Float64 (toRealFloat n)
fromJSON (Aeson.Array arr) = C.Array (V.map fromJSON arr)
fromJSON (Aeson.Object obj) =
  case (KM.lookup (Key.fromText "tag") obj, KM.lookup (Key.fromText "value") obj) of
    (Just (Aeson.Number tn), Just val) ->
      case toBoundedInteger tn :: Maybe Word64 of
        Just tagN -> C.Tag tagN (fromJSON val)
        Nothing   -> objToMap obj
    _ -> objToMap obj

objToMap :: KM.KeyMap Aeson.Value -> C.Value
objToMap obj = C.Map $ V.fromList
  [ (C.TextString (Key.toText k), fromJSON v)
  | (k, v) <- KM.toList obj
  ]

mapToJSON :: V.Vector (C.Value, C.Value) -> Aeson.Value
mapToJSON vec
  | V.all isTextKey vec =
      Aeson.Object $ KM.fromList
        [ (Key.fromText k, toJSON v)
        | (C.TextString k, v) <- V.toList vec
        ]
  | otherwise =
      Aeson.Array $ V.concatMap (\(k, v) -> V.fromList [toJSON k, toJSON v]) vec
  where
    isTextKey (C.TextString _, _) = True
    isTextKey _                   = False

floatToJSON :: Float -> Aeson.Value
floatToJSON !f
  | isNaN f               = Aeson.String "NaN"
  | isInfinite f && f > 0 = Aeson.String "Infinity"
  | isInfinite f          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits f)

doubleToJSON :: Double -> Aeson.Value
doubleToJSON !d
  | isNaN d               = Aeson.String "NaN"
  | isInfinite d && d > 0 = Aeson.String "Infinity"
  | isInfinite d          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits d)

instance Aeson.ToJSON C.Value where
  toJSON = toJSON

instance Aeson.FromJSON C.Value where
  parseJSON = pure . fromJSON
