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
yamlToJSON v = case v of
  YV.YNull       -> A.Null
  YV.YBool b     -> A.Bool b
  YV.YInt n      -> A.Number (fromIntegral n)
  YV.YFloat d    -> A.Number (Sci.fromFloatDigits d)
  YV.YString t   -> A.String t
  YV.YSeq xs     -> A.Array (V.map yamlToJSON xs)
  YV.YMap kvs    -> A.Object (AKM.fromList (V.toList (V.map mkPair kvs)))
  YV.YTagged tag inner -> applyTag tag inner
  YV.YAnchored _ inner -> yamlToJSON inner
  where
    mkPair (k, val) = (AK.fromText (showKey (YV.unwrap k)), yamlToJSON val)

-- | Resolve the value under an explicit YAML tag. The standard
-- @tag:yaml.org,2002:str@ tag with a null body produces @""@,
-- because @!!str@ on an empty source means "force-string-typed
-- empty scalar"; without this rule round-tripping
-- @!!str@-tagged null values gives the wrong JSON shape.
applyTag :: YV.Tag -> YV.Value -> A.Value
applyTag (YV.Tag t) v = case (t, YV.unwrap v) of
  ("tag:yaml.org,2002:str", YV.YNull) -> A.String (T.pack "")
  ("tag:yaml.org,2002:str", inner)    -> A.String (forceString inner)
  ("tag:yaml.org,2002:int", YV.YString s) ->
    case reads (T.unpack s) :: [(Integer, String)] of
      [(i, "")] -> A.Number (fromIntegral i)
      _         -> yamlToJSON v
  ("tag:yaml.org,2002:float", YV.YString s) ->
    case reads (T.unpack s) :: [(Double, String)] of
      [(d, "")] -> A.Number (Sci.fromFloatDigits d)
      _         -> yamlToJSON v
  ("tag:yaml.org,2002:bool", YV.YString s)
    | s == T.pack "true"  -> A.Bool True
    | s == T.pack "false" -> A.Bool False
  ("tag:yaml.org,2002:null", _) -> A.Null
  _ -> yamlToJSON v
  where
    forceString YV.YNull       = T.pack ""
    forceString (YV.YString s) = s
    forceString (YV.YBool b)   = if b then T.pack "true" else T.pack "false"
    forceString (YV.YInt n)    = T.pack (show n)
    forceString (YV.YFloat d)  = T.pack (show d)
    forceString _              = T.pack ""

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
