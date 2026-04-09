-- | XSD (XML Schema Definition) types for code generation.
--
-- Represents a subset of XSD sufficient for generating Haskell types
-- from XML Schema files.
module XML.Schema
  ( XSDSchema(..)
  , XSDType(..)
  , SimpleTypeRestriction(..)
  , ComplexContent(..)
  , XSDElement(..)
  , Occurrence(..)
  , XSDAttribute(..)
  , parseXSD
  ) where

import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import XML.Value (Node(..), Name(..), Attribute(..), Document(..))
import qualified XML.Decode as XD

data XSDSchema = XSDSchema !(Vector XSDType)
  deriving stock (Show, Eq, Generic)

instance NFData XSDSchema where
  rnf (XSDSchema ts) = rnf ts

data XSDType
  = XSDSimple !Text !SimpleTypeRestriction
  | XSDComplex !Text !ComplexContent
  deriving stock (Show, Eq, Generic)

instance NFData XSDType where
  rnf (XSDSimple n r) = rnf n `seq` rnf r
  rnf (XSDComplex n c) = rnf n `seq` rnf c

data SimpleTypeRestriction
  = STRString
  | STRInt
  | STRInteger
  | STRDecimal
  | STRFloat
  | STRDouble
  | STRDate
  | STRDateTime
  | STRBoolean
  | STRBase64Binary
  | STROther !Text
  deriving stock (Show, Eq, Generic)

instance NFData SimpleTypeRestriction

data ComplexContent
  = CCSequence !(Vector XSDElement)
  | CCChoice !(Vector XSDElement)
  | CCAll !(Vector XSDElement)
  | CCSimpleContent !Text
  | CCEmpty
  deriving stock (Show, Eq, Generic)

instance NFData ComplexContent where
  rnf (CCSequence es) = rnf es
  rnf (CCChoice es) = rnf es
  rnf (CCAll es) = rnf es
  rnf (CCSimpleContent t) = rnf t
  rnf CCEmpty = ()

data XSDElement = XSDElement
  { xsdElemName :: !Text
  , xsdElemType :: !Text
  , xsdElemNillable :: !Bool
  , xsdElemOccurrence :: !Occurrence
  } deriving stock (Show, Eq, Generic)

instance NFData XSDElement where
  rnf (XSDElement n t ni o) = rnf n `seq` rnf t `seq` rnf ni `seq` rnf o

data Occurrence
  = Once
  | Optional
  | Unbounded
  | Range !Int !Int
  deriving stock (Show, Eq, Generic)

instance NFData Occurrence

data XSDAttribute = XSDAttribute
  { xsdAttrName :: !Text
  , xsdAttrType :: !Text
  , xsdAttrRequired :: !Bool
  } deriving stock (Show, Eq, Generic)

instance NFData XSDAttribute where
  rnf (XSDAttribute n t r) = rnf n `seq` rnf t `seq` rnf r

-- | Parse an XSD document into an XSDSchema.
parseXSD :: Text -> Either String XSDSchema
parseXSD txt = do
  doc <- XD.decodeText txt
  extractSchema (docRoot doc)

extractSchema :: Node -> Either String XSDSchema
extractSchema root = do
  let children = case root of
        Element _ _ cs -> cs
        _ -> V.empty
  types <- V.mapM extractType (V.filter isTypeNode children)
  Right (XSDSchema types)

isTypeNode :: Node -> Bool
isTypeNode (Element name _ _) =
  nameLocal name == "simpleType" || nameLocal name == "complexType" || nameLocal name == "element"
isTypeNode _ = False

extractType :: Node -> Either String XSDType
extractType (Element name attrs cs)
  | nameLocal name == "simpleType" = do
      let typeName = maybe "" id (attrVal "name" attrs)
      restriction <- extractSimpleRestriction cs
      Right (XSDSimple typeName restriction)
  | nameLocal name == "complexType" = do
      let typeName = maybe "" id (attrVal "name" attrs)
      content <- extractComplexContent cs
      Right (XSDComplex typeName content)
  | nameLocal name == "element" = do
      let elemName = maybe "" id (attrVal "name" attrs)
          elemType = attrVal "type" attrs
      case elemType of
        Just t -> Right (XSDComplex elemName (CCSimpleContent t))
        Nothing -> do
          content <- extractComplexContent cs
          Right (XSDComplex elemName content)
  | otherwise = Left $ "Unknown type node: " ++ T.unpack (nameLocal name)
extractType _ = Left "Expected element node for type"

extractSimpleRestriction :: Vector Node -> Either String SimpleTypeRestriction
extractSimpleRestriction cs =
  case V.find isRestriction cs of
    Just (Element _ attrs _) ->
      let base = maybe "string" id (attrVal "base" attrs)
      in Right (parseBaseType base)
    _ -> Right STRString
  where
    isRestriction (Element n _ _) = nameLocal n == "restriction"
    isRestriction _ = False

extractComplexContent :: Vector Node -> Either String ComplexContent
extractComplexContent cs
  | Just seq' <- V.find (isElem "sequence") cs =
      CCSequence <$> extractElements (elemChildren seq')
  | Just ch <- V.find (isElem "choice") cs =
      CCChoice <$> extractElements (elemChildren ch)
  | Just allE <- V.find (isElem "all") cs =
      CCAll <$> extractElements (elemChildren allE)
  | Just sc <- V.find (isElem "simpleContent") cs =
      Right (CCSimpleContent (maybe "" id (extractExtBase sc)))
  | otherwise = Right CCEmpty

isElem :: Text -> Node -> Bool
isElem target (Element n _ _) = nameLocal n == target
isElem _ _ = False

elemChildren :: Node -> Vector Node
elemChildren (Element _ _ cs) = cs
elemChildren _ = V.empty

extractExtBase :: Node -> Maybe Text
extractExtBase (Element _ _ cs) =
  case V.find (isElem "extension") cs of
    Just (Element _ attrs _) -> attrVal "base" attrs
    _ -> Nothing
extractExtBase _ = Nothing

extractElements :: Vector Node -> Either String (Vector XSDElement)
extractElements cs = V.mapM extractElement (V.filter (isElem "element") cs)

extractElement :: Node -> Either String XSDElement
extractElement (Element _ attrs _) =
  let name = maybe "" id (attrVal "name" attrs)
      typeName = maybe "xs:string" id (attrVal "type" attrs)
      nillable = maybe False (== "true") (attrVal "nillable" attrs)
      minOcc = maybe "1" id (attrVal "minOccurs" attrs)
      maxOcc = maybe "1" id (attrVal "maxOccurs" attrs)
      occ = parseOccurrence minOcc maxOcc
  in Right (XSDElement name typeName nillable occ)
extractElement _ = Left "Expected element node"

parseOccurrence :: Text -> Text -> Occurrence
parseOccurrence "0" "1" = Optional
parseOccurrence "0" "unbounded" = Unbounded
parseOccurrence "1" "1" = Once
parseOccurrence minT maxT =
  case (readMaybe minT, readMaybe maxT) of
    (Just lo, Just hi) -> Range lo hi
    _ -> Once
  where
    readMaybe t = case reads (T.unpack t) of
      [(v, "")] -> Just v
      _ -> Nothing

parseBaseType :: Text -> SimpleTypeRestriction
parseBaseType t =
  let local = case T.breakOnEnd ":" t of
        (_, l) | T.null l -> t
        (_, l) -> l
  in case local of
    "string"       -> STRString
    "int"          -> STRInt
    "integer"      -> STRInteger
    "decimal"      -> STRDecimal
    "float"        -> STRFloat
    "double"       -> STRDouble
    "date"         -> STRDate
    "dateTime"     -> STRDateTime
    "boolean"      -> STRBoolean
    "base64Binary" -> STRBase64Binary
    other          -> STROther other

attrVal :: Text -> Vector Attribute -> Maybe Text
attrVal name attrs = go 0
  where
    !len = V.length attrs
    go !i
      | i >= len = Nothing
      | Attribute aname val <- attrs V.! i
      , nameLocal aname == name = Just val
      | otherwise = go (i + 1)
