{-# LANGUAGE TemplateHaskell #-}

{- | Generate Haskell types from XSD schemas.

Produces Haskell source text with data types and ToXML\/FromXML instances
corresponding to XSD complex types.

== Text generation

'generateXMLTypes' takes an 'XSDSchema' and produces Haskell source
as 'Text'.

== Template Haskell

'deriveXSD' generates declarations at compile time from an 'XSDSchema'.
-}
module XML.CodeGen (
  generateXMLTypes,
  deriveXSD,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Language.Haskell.TH
import XML.Schema


-- | Generate Haskell module text from an XSD schema.
generateXMLTypes :: XSDSchema -> Text
generateXMLTypes (XSDSchema types) =
  T.intercalate "\n\n" (prelude : V.toList (V.map generateType types))


prelude :: Text
prelude =
  T.unlines
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
  "data "
    <> sanitizeName name
    <> " = "
    <> sanitizeName name
    <> "\n  deriving stock (Show, Eq, Generic)"
    <> "\n  deriving anyclass (ToXML, FromXML)"


generateRecord :: Text -> Vector XSDElement -> Text
generateRecord name elements
  | V.null elements =
      "data "
        <> sname
        <> " = "
        <> sname
        <> "\n  deriving stock (Show, Eq, Generic)"
        <> "\n  deriving anyclass (ToXML, FromXML)"
  | otherwise =
      "data "
        <> sname
        <> " = "
        <> sname
        <> "\n  { "
        <> T.intercalate "\n  , " (V.toList (V.map genField elements))
        <> "\n  } deriving stock (Show, Eq, Generic)"
        <> "\n    deriving anyclass (ToXML, FromXML)"
  where
    sname = sanitizeName name


generateSumType :: Text -> Vector XSDElement -> Text
generateSumType name elements =
  "data "
    <> sname
    <> "\n  = "
    <> T.intercalate "\n  | " (V.toList (V.map genVariant elements))
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
       "string" -> "Text"
       "int" -> "Int"
       "integer" -> "Integer"
       "decimal" -> "Double"
       "float" -> "Float"
       "double" -> "Double"
       "date" -> "Text"
       "dateTime" -> "Text"
       "boolean" -> "Bool"
       "base64Binary" -> "ByteString"
       other -> sanitizeName other


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
isIdChar c =
  c >= 'a' && c <= 'z'
    || c >= 'A' && c <= 'Z'
    || c >= '0' && c <= '9'
    || c == '_'


-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

{- | Generate Haskell declarations from an 'XSDSchema' at compile time.
Produces data types with @Show@, @Eq@, @Generic@ deriving and
@ToXML@\/@FromXML@ instances via @DeriveAnyClass@.
-}
deriveXSD :: XSDSchema -> Q [Dec]
deriveXSD (XSDSchema types) =
  concat <$> mapM deriveXSDType (V.toList types)


deriveXSDType :: XSDType -> Q [Dec]
deriveXSDType (XSDSimple name _restriction) = do
  let tyName = mkName (T.unpack (sanitizeName name))
      target = ConT (mkName "Text")
  pure [TySynD tyName [] target]
deriveXSDType (XSDComplex name content) =
  deriveXSDComplex name content


deriveXSDComplex :: Text -> ComplexContent -> Q [Dec]
deriveXSDComplex name (CCSequence elements) =
  deriveXSDRecord name elements
deriveXSDComplex name (CCChoice elements) =
  deriveXSDSumType name elements
deriveXSDComplex name (CCAll elements) =
  deriveXSDRecord name elements
deriveXSDComplex name (CCSimpleContent baseType) = do
  let tyName = mkName (T.unpack (sanitizeName name))
  target <- mapTypeToTH baseType
  pure [TySynD tyName [] target]
deriveXSDComplex name CCEmpty = do
  let sname = sanitizeName name
      tyName = mkName (T.unpack sname)
      conName = mkName (T.unpack sname)
      dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [NormalC conName []]
          [ DerivClause
              (Just StockStrategy)
              [ConT ''Show, ConT ''Eq, ConT (mkName "Generic")]
          , DerivClause
              (Just AnyclassStrategy)
              [ConT (mkName "ToXML"), ConT (mkName "FromXML")]
          ]
  pure [dataDec]


deriveXSDRecord :: Text -> Vector XSDElement -> Q [Dec]
deriveXSDRecord name elements
  | V.null elements = deriveXSDComplex name CCEmpty
  | otherwise = do
      let sname = sanitizeName name
          tyName = mkName (T.unpack sname)
          conName = mkName (T.unpack sname)
      fields <- mapM mkXSDRecordField (V.toList elements)
      let dataDec =
            DataD
              []
              tyName
              []
              Nothing
              [RecC conName fields]
              [ DerivClause
                  (Just StockStrategy)
                  [ConT ''Show, ConT ''Eq, ConT (mkName "Generic")]
              , DerivClause
                  (Just AnyclassStrategy)
                  [ConT (mkName "ToXML"), ConT (mkName "FromXML")]
              ]
      pure [dataDec]


mkXSDRecordField :: XSDElement -> Q VarBangType
mkXSDRecordField (XSDElement fname ftype _nillable occ) = do
  let fieldName = mkName (T.unpack (sanitizeFieldName fname))
  innerTy <- mapTypeToTH ftype
  let wrappedTy = wrapOccurrenceTH occ innerTy
      strictBang = Bang NoSourceUnpackedness SourceStrict
  pure (fieldName, strictBang, wrappedTy)


deriveXSDSumType :: Text -> Vector XSDElement -> Q [Dec]
deriveXSDSumType name elements = do
  let sname = sanitizeName name
      tyName = mkName (T.unpack sname)
  cons <- mapM mkXSDVariant (V.toList elements)
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          cons
          [ DerivClause
              (Just StockStrategy)
              [ConT ''Show, ConT ''Eq, ConT (mkName "Generic")]
          ]
  pure [dataDec]


mkXSDVariant :: XSDElement -> Q Con
mkXSDVariant (XSDElement vname vtype _nillable _occ) = do
  ty <- mapTypeToTH vtype
  let conName = mkName (T.unpack (sanitizeName (T.toTitle vname)))
      strictBang = Bang NoSourceUnpackedness SourceStrict
  pure (NormalC conName [(strictBang, ty)])


wrapOccurrenceTH :: Occurrence -> Type -> Type
wrapOccurrenceTH Once t = t
wrapOccurrenceTH Optional t = AppT (ConT ''Maybe) t
wrapOccurrenceTH Unbounded t = AppT (ConT (mkName "Vector")) t
wrapOccurrenceTH (Range _ _) t = AppT (ConT (mkName "Vector")) t


mapTypeToTH :: Text -> Q Type
mapTypeToTH t = do
  let local = case T.breakOnEnd ":" t of
        (_, l) | T.null l -> t
        (_, l) -> l
  case local of
    "string" -> pure (ConT (mkName "Text"))
    "int" -> pure (ConT ''Int)
    "integer" -> pure (ConT ''Integer)
    "decimal" -> pure (ConT ''Double)
    "float" -> pure (ConT ''Float)
    "double" -> pure (ConT ''Double)
    "date" -> pure (ConT (mkName "Text"))
    "dateTime" -> pure (ConT (mkName "Text"))
    "boolean" -> pure (ConT ''Bool)
    "base64Binary" -> pure (ConT (mkName "ByteString"))
    other -> pure (ConT (mkName (T.unpack (sanitizeName other))))
