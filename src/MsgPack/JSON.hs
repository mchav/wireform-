{-# LANGUAGE BangPatterns #-}
-- | MessagePack / JSON interop.
--
-- Converts between 'MsgPack.Value.Value' and 'Data.Aeson.Value'.
-- Lossy in the general case because JSON has no binary, ext, or
-- unsigned-integer types.  The mapping is:
--
-- * 'MV.Nil'       ↔ 'Aeson.Null'
-- * 'MV.Bool'      ↔ 'Aeson.Bool'
-- * 'MV.Int'       ↔ 'Aeson.Number'
-- * 'MV.Word'      ↔ 'Aeson.Number' (large Word64 values that exceed
--                     JSON safe-integer range are encoded as strings)
-- * 'MV.Float'     ↔ 'Aeson.Number'
-- * 'MV.Double'    ↔ 'Aeson.Number'
-- * 'MV.String'    ↔ 'Aeson.String'
-- * 'MV.Binary'    ↔ 'Aeson.String' (base64)
-- * 'MV.Array'     ↔ 'Aeson.Array'
-- * 'MV.Map'       ↔ 'Aeson.Object' (string keys) or 'Aeson.Array' of pairs
-- * 'MV.Ext'       ↔ object @{"type": n, "data": "<base64>"}@
-- * 'MV.Timestamp' ↔ object @{"seconds": n, "nanoseconds": n}@
module MsgPack.JSON
  ( toJSON
  , fromJSON
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Base64 as Base64
import Data.Int (Int64)
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import qualified Data.Scientific as Sci
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word64)

import qualified MsgPack.Value as MV

--------------------------------------------------------------------------------
-- To JSON
--------------------------------------------------------------------------------

toJSON :: MV.Value -> Aeson.Value
toJSON MV.Nil          = Aeson.Null
toJSON (MV.Bool b)     = Aeson.Bool b
toJSON (MV.Int n)      = Aeson.Number (fromIntegral n)
toJSON (MV.Word n)
  | n > 9007199254740992 = Aeson.String (T.pack (show n))
  | otherwise            = Aeson.Number (fromIntegral n)
toJSON (MV.Float f)    = Aeson.Number (fromFloatDigits f)
toJSON (MV.Double d)   = Aeson.Number (fromFloatDigits d)
toJSON (MV.String t)   = Aeson.String t
toJSON (MV.Binary bs)  = Aeson.String (TE.decodeUtf8 (Base64.encode bs))
toJSON (MV.Array vs)   = Aeson.Array (V.map toJSON vs)
toJSON (MV.Map kvs)    = case tryObjectMap kvs of
  Just obj -> Aeson.Object obj
  Nothing  -> Aeson.Array (V.map (\(k, v) -> Aeson.Array (V.fromList [toJSON k, toJSON v])) kvs)
toJSON (MV.Ext ty bs) = Aeson.object
  [ (Key.fromText "type", Aeson.Number (fromIntegral ty))
  , (Key.fromText "data", Aeson.String (TE.decodeUtf8 (Base64.encode bs)))
  ]
toJSON (MV.Timestamp s ns) = Aeson.object
  [ (Key.fromText "seconds", Aeson.Number (fromIntegral s))
  , (Key.fromText "nanoseconds", Aeson.Number (fromIntegral ns))
  ]

tryObjectMap :: V.Vector (MV.Value, MV.Value) -> Maybe (KM.KeyMap Aeson.Value)
tryObjectMap kvs
  | V.all (\(k, _) -> isString k) kvs =
      Just $ KM.fromList
        [ (Key.fromText t, toJSON v)
        | (MV.String t, v) <- V.toList kvs
        ]
  | otherwise = Nothing
  where
    isString (MV.String _) = True
    isString _             = False

--------------------------------------------------------------------------------
-- From JSON
--------------------------------------------------------------------------------

fromJSON :: Aeson.Value -> MV.Value
fromJSON Aeson.Null       = MV.Nil
fromJSON (Aeson.Bool b)   = MV.Bool b
fromJSON (Aeson.Number n) = numToMsgPack n
fromJSON (Aeson.String t) = MV.String t
fromJSON (Aeson.Array vs) = MV.Array (V.map fromJSON vs)
fromJSON (Aeson.Object obj) = MV.Map $ V.fromList
  [ (MV.String (Key.toText k), fromJSON v)
  | (k, v) <- KM.toList obj
  ]

numToMsgPack :: Sci.Scientific -> MV.Value
numToMsgPack n
  | Sci.isInteger n = case toBoundedInteger n :: Maybe Int64 of
      Just i  -> MV.Int i
      Nothing -> case toBoundedInteger n :: Maybe Word64 of
        Just w  -> MV.Word w
        Nothing -> MV.Double (toRealFloat n)
  | otherwise = MV.Double (toRealFloat n)
