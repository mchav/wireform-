-- | Generate Haskell types from XSD schemas.
--
-- Produces Haskell source text with data types and ToXML\/FromXML instances
-- corresponding to XSD complex types.
module XML.CodeGen
  ( generateXMLTypes
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import XML.Schema

-- | Generate Haskell module text from an XSD schema.
generateXMLTypes :: XSDSchema -> Text
generateXMLTypes (XSDSchema types) =
  T.intercalate "\n\n" (prelude : V.toList (V.map generateType types))

prelude :: Text
prelude = T.unlines
  [ "{-# LANGUAGE DeriveGeneric #-}"
  , "{-# LANGUAGE DerivingStrategies #-}"
  , "{-# LANGUAGE DeriveAnyClass #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "module Generated.XSD where"
  , ""
  , "import Data.Text (Text)"
  , "import Data.Vector (Vector)"
  , "import GHC.Generics (Generic)"
  , "import XML.Class (ToXML, FromXML)"
  ]

generateType :: XSDType -> Text
generateType (XSDSimple name _restriction) =
  "type " <> sanitizeName name <> " = Text"
generateType (XSDComplex name content) =
  generateComplexType name content

generateComplexType :: Text -> ComplexContent -> Text
generateComplexType name (CCSequence elements) =
  generateRecord name elements
generateComplexType name (CCChoice elements) =
  generateSumType name elements
generateComplexType name (CCAll elements) =
  generateRecord name elements
generateComplexType name (CCSimpleContent baseType) =
  "type " <> sanitizeName name <> " = " <> mapType baseType
generateComplexType name CCEmpty =
  "data " <> sanitizeName name <> " = " <> sanitizeName name
  <> "\n  deriving stock (Show, Eq, Generic)"
  <> "\n  deriving anyclass (ToXML, FromXML)"

generateRecord :: Text -> Vector XSDElement -> Text
generateRecord name elements
  | V.null elements =
      "data " <> sname <> " = " <> sname
      <> "\n  deriving stock (Show, Eq, Generic)"
      <> "\n  deriving anyclass (ToXML, FromXML)"
  | otherwise =
      "data " <> sname <> " = " <> sname
      <> "\n  { " <> T.intercalate "\n  , " (V.toList (V.map genField elements))
      <> "\n  } deriving stock (Show, Eq, Generic)"
      <> "\n    deriving anyclass (ToXML, FromXML)"
  where
    sname = sanitizeName name

generateSumType :: Text -> Vector XSDElement -> Text
generateSumType name elements =
  "data " <> sname
  <> "\n  = " <> T.intercalate "\n  | " (V.toList (V.map genVariant elements))
  <> "\n  deriving stock (Show, Eq, Generic)"
  where
    sname = sanitizeName name

genField :: XSDElement -> Text
genField (XSDElement fname ftype _nillable occ) =
  let fieldName = sanitizeFieldName fname
      typeName = wrapOccurrence occ (mapType ftype)
  in fieldName <> " :: !" <> typeName

genVariant :: XSDElement -> Text
genVariant (XSDElement vname vtype _nillable _occ) =
  sanitizeName (T.toTitle vname) <> " !" <> mapType vtype

wrapOccurrence :: Occurrence -> Text -> Text
wrapOccurrence Once t = t
wrapOccurrence Optional t = "(Maybe " <> t <> ")"
wrapOccurrence Unbounded t = "(Vector " <> t <> ")"
wrapOccurrence (Range _ _) t = "(Vector " <> t <> ")"

mapType :: Text -> Text
mapType t =
  let local = case T.breakOnEnd ":" t of
        (_, l) | T.null l -> t
        (_, l) -> l
  in case local of
    "string"       -> "Text"
    "int"          -> "Int"
    "integer"      -> "Integer"
    "decimal"      -> "Double"
    "float"        -> "Float"
    "double"       -> "Double"
    "date"         -> "Text"
    "dateTime"     -> "Text"
    "boolean"      -> "Bool"
    "base64Binary" -> "ByteString"
    other          -> sanitizeName other

sanitizeName :: Text -> Text
sanitizeName t
  | T.null t = "Unnamed"
  | otherwise =
      let first = T.toUpper (T.take 1 t)
          rest = T.drop 1 t
      in first <> T.filter isIdChar rest

sanitizeFieldName :: Text -> Text
sanitizeFieldName t
  | T.null t = "unnamed"
  | otherwise =
      let first = T.toLower (T.take 1 t)
          rest = T.drop 1 t
      in first <> T.filter isIdChar rest

isIdChar :: Char -> Bool
isIdChar c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' ||
             c >= '0' && c <= '9' || c == '_'
