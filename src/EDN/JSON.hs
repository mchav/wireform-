{-# LANGUAGE BangPatterns #-}
-- | EDN / JSON interconversion.
--
-- Converts between 'EDN.Value.Value' and 'Data.Aeson.Value'. The mapping
-- is lossy: EDN keywords, symbols, characters, sets, and tagged literals
-- have no direct JSON equivalent and are mapped to strings or arrays.
module EDN.JSON
  ( toJSON
  , fromJSON
  ) where

import Prelude hiding (map)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (fromFloatDigits, toBoundedInteger, toRealFloat)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified EDN.Value as E

-- | Convert an EDN 'E.Value' to a JSON 'Aeson.Value'.
toJSON :: E.Value -> Aeson.Value
toJSON E.Nil            = Aeson.Null
toJSON (E.Bool b)       = Aeson.Bool b
toJSON (E.Integer n)    = Aeson.Number (fromIntegral n)
toJSON (E.Float d)      = doubleToJSON d
toJSON (E.String t)     = Aeson.String t
toJSON (E.Char c)       = Aeson.String (T.singleton c)
toJSON (E.Keyword ns n) = Aeson.String (keywordToText ns n)
toJSON (E.Symbol ns n)  = Aeson.String (symbolToText ns n)
toJSON (E.List vs)      = Aeson.Array (V.map toJSON vs)
toJSON (E.Vector vs)    = Aeson.Array (V.map toJSON vs)
toJSON (E.Map pairs)    = mapToJSON pairs
toJSON (E.Set vs)       = Aeson.Array (V.map toJSON vs)
toJSON (E.Tagged ns tag val) =
  Aeson.Object $ KM.fromList
    [ (Key.fromText "__tag__", Aeson.String (tagToText ns tag))
    , (Key.fromText "__value__", toJSON val)
    ]

-- | Convert a JSON 'Aeson.Value' to an EDN 'E.Value'.
fromJSON :: Aeson.Value -> E.Value
fromJSON Aeson.Null       = E.Nil
fromJSON (Aeson.Bool b)   = E.Bool b
fromJSON (Aeson.String t) = E.String t
fromJSON (Aeson.Number n) =
  case toBoundedInteger n :: Maybe Int of
    Just i  -> E.Integer (fromIntegral i)
    Nothing -> E.Float (toRealFloat n)
fromJSON (Aeson.Array arr) = E.Vector (V.map fromJSON arr)
fromJSON (Aeson.Object obj) =
  case (KM.lookup (Key.fromText "__tag__") obj, KM.lookup (Key.fromText "__value__") obj) of
    (Just (Aeson.String tag), Just val) ->
      case T.breakOn "/" tag of
        (ns, rest)
          | T.null rest -> E.Tagged T.empty tag (fromJSON val)
          | otherwise   -> E.Tagged ns (T.drop 1 rest) (fromJSON val)
    _ -> objToMap obj

keywordToText :: Maybe Text -> Text -> Text
keywordToText Nothing  n = T.cons ':' n
keywordToText (Just ns) n = T.cons ':' (ns <> "/" <> n)

symbolToText :: Maybe Text -> Text -> Text
symbolToText Nothing  n = n
symbolToText (Just ns) n = ns <> "/" <> n

tagToText :: Text -> Text -> Text
tagToText ns tag
  | T.null ns = tag
  | otherwise = ns <> "/" <> tag

mapToJSON :: V.Vector (E.Value, E.Value) -> Aeson.Value
mapToJSON pairs
  | V.all isKeywordKey pairs =
      Aeson.Object $ KM.fromList
        [ (Key.fromText (kwName ns n), toJSON v)
        | (E.Keyword ns n, v) <- V.toList pairs
        ]
  | V.all isStringKey pairs =
      Aeson.Object $ KM.fromList
        [ (Key.fromText k, toJSON v)
        | (E.String k, v) <- V.toList pairs
        ]
  | otherwise =
      Aeson.Array $ V.concatMap (\(k, v) -> V.fromList [toJSON k, toJSON v]) pairs
  where
    isKeywordKey (E.Keyword _ _, _) = True
    isKeywordKey _                  = False
    isStringKey (E.String _, _)     = True
    isStringKey _                   = False
    kwName Nothing  n = n
    kwName (Just ns) n = ns <> "/" <> n

objToMap :: KM.KeyMap Aeson.Value -> E.Value
objToMap obj = E.Map $ V.fromList
  [ (E.String (Key.toText k), fromJSON v)
  | (k, v) <- KM.toList obj
  ]

doubleToJSON :: Double -> Aeson.Value
doubleToJSON !d
  | isNaN d               = Aeson.String "NaN"
  | isInfinite d && d > 0 = Aeson.String "Infinity"
  | isInfinite d          = Aeson.String "-Infinity"
  | otherwise             = Aeson.Number (fromFloatDigits d)
