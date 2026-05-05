-- | Lossy bridge between YAML 'YV.Value' and Aeson 'A.Value'.
--
-- YAML is a superset of JSON in the data model, but the YAML core
-- schema admits scalars (notably non-string mapping keys) that JSON
-- cannot represent directly. We follow the same conventions other
-- @<Format>.JSON@ modules use:
--
-- * Mapping keys are coerced to text via 'showKey'. Keys that are
--   already 'YV.YString' pass through verbatim; numeric, boolean and
--   null keys are stringified.
-- * Tags / anchors are stripped (they are part of the YAML
--   representation graph, not the data model).
module YAML.JSON
  ( yamlToJSON
  , jsonToYAML
  ) where

import qualified Data.Aeson           as A
import qualified Data.Aeson.Key       as AK
import qualified Data.Aeson.KeyMap    as AKM
import qualified Data.Scientific      as Sci
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified YAML.Value as YV

yamlToJSON :: YV.Value -> A.Value
yamlToJSON v = case YV.unwrap v of
  YV.YNull       -> A.Null
  YV.YBool b     -> A.Bool b
  YV.YInt n      -> A.Number (fromIntegral n)
  YV.YFloat d    -> A.Number (Sci.fromFloatDigits d)
  YV.YString t   -> A.String t
  YV.YSeq xs     -> A.Array (V.map yamlToJSON xs)
  YV.YMap kvs    -> A.Object (AKM.fromList (V.toList (V.map mkPair kvs)))
  YV.YTagged _ v' -> yamlToJSON v'
  YV.YAnchored _ v' -> yamlToJSON v'
  where
    mkPair (k, val) = (AK.fromText (showKey (YV.unwrap k)), yamlToJSON val)

showKey :: YV.Value -> Text
showKey YV.YNull       = T.pack "null"
showKey (YV.YBool b)   = if b then T.pack "true" else T.pack "false"
showKey (YV.YInt n)    = T.pack (show n)
showKey (YV.YFloat d)  = T.pack (show d)
showKey (YV.YString s) = s
showKey _              = T.pack "<complex>"

jsonToYAML :: A.Value -> YV.Value
jsonToYAML = \case
  A.Null      -> YV.YNull
  A.Bool b    -> YV.YBool b
  A.String t  -> YV.YString t
  A.Number n  ->
    case Sci.floatingOrInteger n of
      Left  d -> YV.YFloat (d :: Double)
      Right i -> YV.YInt (fromIntegral (i :: Integer))
  A.Array xs  -> YV.YSeq (V.map jsonToYAML xs)
  A.Object o  ->
    YV.YMap (V.fromList (fmap mkPair (AKM.toList o)))
  where
    mkPair (k, val) = (YV.YString (AK.toText k), jsonToYAML val)
