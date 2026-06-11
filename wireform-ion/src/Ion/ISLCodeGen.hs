{-# LANGUAGE TemplateHaskell #-}

{- | Ion Schema Language code generation — generates Haskell data types and
ToIon\/FromIon stub instances from ISL schemas.
Struct types become records with field constraints as comments.
-}
module Ion.ISLCodeGen (
  generateISLTypes,
  deriveISL,
) where

import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Ion.ISLSchema
import Language.Haskell.TH


-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateISLTypes :: ISLSchema -> Text
generateISLTypes schema =
  let decls = concatMap genISLType (V.toList (islTypes schema))
  in T.intercalate "\n\n" decls


genISLType :: ISLType -> [Text]
genISLType ist = case islFields ist of
  Just fields ->
    let name = upperFirst (islTypeName ist)
        fieldList = V.toList fields
    in [ genStructRecord name fieldList ist
       , genToIonStruct name fieldList
       , genFromIonStruct name fieldList
       ]
  Nothing -> case islValidValues ist of
    Just (EnumVal syms) ->
      let name = upperFirst (islTypeName ist)
          symList = V.toList syms
      in [ genISLEnum name symList
         , genToIonEnum name symList
         , genFromIonEnum name symList
         ]
    _ ->
      let name = upperFirst (islTypeName ist)
      in [genISLSimpleType name ist]


-- ---------------------------------------------------------------------------
-- Struct -> record
-- ---------------------------------------------------------------------------

genStructRecord :: Text -> [ISLField] -> ISLType -> Text
genStructRecord name fields parentType =
  T.unlines $
    ["-- | Generated from ISL type @" <> islTypeName parentType <> "@"]
      <> case islBaseType parentType of
        Just base -> ["-- Base type: " <> base]
        Nothing -> []
      <> ["data " <> name <> " = " <> name]
      <> case fields of
        [] ->
          ["  deriving stock (Show, Eq, Generic)"]
        (f : fs) ->
          ["  { " <> genISLFieldDecl name f]
            <> map (\fld -> "  , " <> genISLFieldDecl name fld) fs
            <> ["  } deriving stock (Show, Eq, Generic)"]


genISLFieldDecl :: Text -> ISLField -> Text
genISLFieldDecl recName (ISLField fieldName fieldType) =
  let accessor = islFieldAccessorName recName fieldName
      hsType = islFieldHsType fieldType
      constraint = islFieldConstraintComment fieldType
  in accessor <> " :: " <> hsType <> constraint


islFieldAccessorName :: Text -> Text -> Text
islFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)


islFieldHsType :: ISLType -> Text
islFieldHsType ist = case islOccurs ist of
  Just OOptional -> "!(Maybe " <> islBaseHsType ist <> ")"
  _ -> "!" <> islBaseHsType ist


islBaseHsType :: ISLType -> Text
islBaseHsType ist = case islBaseType ist of
  Just "string" -> "Text"
  Just "int" -> "Int64"
  Just "float" -> "Double"
  Just "bool" -> "Bool"
  Just "blob" -> "ByteString"
  Just "symbol" -> "Text"
  Just "list" -> "(Vector IonValue)"
  Just other -> upperFirst other
  Nothing -> "IonValue"


islFieldConstraintComment :: ISLType -> Text
islFieldConstraintComment ist = case islValidValues ist of
  Just (RangeVal lo hi) ->
    " -- ^ range: [" <> maybe "min" (T.pack . show) lo <> ", " <> maybe "max" (T.pack . show) hi <> "]"
  Just (EnumVal syms) ->
    " -- ^ valid values: " <> T.intercalate ", " (V.toList syms)
  Nothing -> ""


-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

genISLEnum :: Text -> [Text] -> Text
genISLEnum name syms =
  T.unlines $
    ["data " <> name]
      <> case syms of
        [] -> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]
        (s : ss) ->
          ["  = " <> islEnumConName name s]
            <> map (\sym -> "  | " <> islEnumConName name sym) ss
            <> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]


islEnumConName :: Text -> Text -> Text
islEnumConName enumName sym =
  enumName <> upperFirst (snakeToCamel sym)


-- ---------------------------------------------------------------------------
-- Simple type alias
-- ---------------------------------------------------------------------------

genISLSimpleType :: Text -> ISLType -> Text
genISLSimpleType name ist =
  "type " <> name <> " = " <> islBaseHsType ist


-- ---------------------------------------------------------------------------
-- ToIon / FromIon stub instances (text)
-- ---------------------------------------------------------------------------

genToIonStruct :: Text -> [ISLField] -> Text
genToIonStruct name _ =
  T.unlines
    [ "instance ToIon " <> name <> " where"
    , "  toIon _ = error \"ToIon " <> name <> ": stub\""
    ]


genFromIonStruct :: Text -> [ISLField] -> Text
genFromIonStruct name _ =
  T.unlines
    [ "instance FromIon " <> name <> " where"
    , "  fromIon _ = Left \"FromIon " <> name <> ": stub\""
    ]


genToIonEnum :: Text -> [Text] -> Text
genToIonEnum name syms =
  T.unlines $
    ["instance ToIon " <> name <> " where"]
      <> map
        ( \sym ->
            "  toIon " <> islEnumConName name sym <> " = Ion.Value.Symbol " <> T.pack (show sym)
        )
        syms


genFromIonEnum :: Text -> [Text] -> Text
genFromIonEnum name syms =
  T.unlines $
    ["instance FromIon " <> name <> " where"]
      <> map
        ( \sym ->
            "  fromIon (Ion.Value.Symbol " <> T.pack (show sym) <> ") = Right " <> islEnumConName name sym
        )
        syms
      <> ["  fromIon _ = Left \"FromIon " <> name <> ": expected Symbol\""]


-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveISL :: ISLSchema -> Q [Dec]
deriveISL schema = do
  concat <$> mapM deriveISLType (V.toList (islTypes schema))


deriveISLType :: ISLType -> Q [Dec]
deriveISLType ist = case islFields ist of
  Just fields -> deriveISLStructTH (upperFirst (islTypeName ist)) (V.toList fields) ist
  Nothing -> case islValidValues ist of
    Just (EnumVal syms) -> deriveISLEnumTH (upperFirst (islTypeName ist)) (V.toList syms)
    _ -> pure []


-- ---------------------------------------------------------------------------
-- TH: Struct -> record
-- ---------------------------------------------------------------------------

deriveISLStructTH :: Text -> [ISLField] -> ISLType -> Q [Dec]
deriveISLStructTH name fields _ = do
  let tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
  fieldDecs <- mapM (mkISLRecordField name) fields
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [RecC conName fieldDecs]
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkISLRecordField :: Text -> ISLField -> Q VarBangType
mkISLRecordField recName (ISLField fieldName fieldType) = do
  let accessor = islFieldAccessorName recName fieldName
      accName = mkName (T.unpack accessor)
  hsTy <- case islOccurs fieldType of
    Just OOptional -> do
      inner <- islTypeToTH fieldType
      pure (AppT (ConT ''Maybe) inner)
    _ -> islTypeToTH fieldType
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)


islTypeToTH :: ISLType -> Q Type
islTypeToTH ist = case islBaseType ist of
  Just "string" -> [t|Text|]
  Just "int" -> [t|Int64|]
  Just "float" -> [t|Double|]
  Just "bool" -> [t|Bool|]
  Just "blob" -> [t|ByteString|]
  Just "symbol" -> [t|Text|]
  Just other -> pure (ConT (mkName (T.unpack (upperFirst other))))
  Nothing -> [t|()|]


-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveISLEnumTH :: Text -> [Text] -> Q [Dec]
deriveISLEnumTH name syms = do
  let tyName = mkName (T.unpack name)
      cons = map (\sym -> NormalC (mkName (T.unpack (islEnumConName name sym))) []) syms
      dataDec =
        DataD
          []
          tyName
          []
          Nothing
          cons
          [ DerivClause
              (Just StockStrategy)
              [ConT ''Show, ConT ''Eq, ConT ''Ord, ConT ''Enum, ConT ''Bounded]
          ]
  pure [dataDec]


-- ---------------------------------------------------------------------------
-- Name helpers
-- ---------------------------------------------------------------------------

lowerFirst :: Text -> Text
lowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing -> s


upperFirst :: Text -> Text
upperFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> s


snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn "_" t
  in case parts of
       [] -> t
       (p : ps) -> T.concat (lowerFirst p : map titleCase ps)


titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s
