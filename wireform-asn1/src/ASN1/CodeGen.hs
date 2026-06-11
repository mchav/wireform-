{-# LANGUAGE TemplateHaskell #-}

{- | ASN.1 code generation — generates Haskell data types from ASN.1 modules.
SEQUENCE -> records, CHOICE -> sum types, ENUMERATED -> enum types,
OPTIONAL -> Maybe, SEQUENCE OF -> Vector.
-}
module ASN1.CodeGen (
  generateASN1Types,
  deriveASN1,
) where

import ASN1.Schema
import Data.ByteString (ByteString)
import Data.Char (toLower, toUpper)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH


-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateASN1Types :: ASN1Module -> Text
generateASN1Types modl =
  let decls = concatMap genAssignment (V.toList (asnAssignments modl))
  in T.intercalate "\n\n" decls


genAssignment :: TypeAssignment -> [Text]
genAssignment (TypeAssignment name td) = genTypeDef name td


genTypeDef :: Text -> ASN1TypeDef -> [Text]
genTypeDef name = \case
  TDSequence components ->
    let fields = V.toList components
    in [genSequenceDecl name fields]
  TDChoice components ->
    let alts = V.toList components
    in [genChoiceDecl name alts]
  TDEnumerated vals ->
    let enumVals = V.toList vals
    in [genEnumeratedDecl name enumVals]
  TDSequenceOf inner ->
    ["type " <> name <> " = Vector " <> asn1InnerHsType inner]
  TDSetOf inner ->
    ["type " <> name <> " = Vector " <> asn1InnerHsType inner]
  TDNamedType ref ->
    ["type " <> name <> " = " <> ref]
  _ -> ["type " <> name <> " = ASN1Value"]


-- ---------------------------------------------------------------------------
-- SEQUENCE -> record
-- ---------------------------------------------------------------------------

genSequenceDecl :: Text -> [ComponentType] -> Text
genSequenceDecl name components =
  T.unlines $
    ["data " <> name <> " = " <> name]
      <> case components of
        [] ->
          ["  deriving stock (Show, Eq, Generic)"]
        (c : cs) ->
          ["  { " <> genComponentField name c]
            <> map (\comp -> "  , " <> genComponentField name comp) cs
            <> ["  } deriving stock (Show, Eq, Generic)"]


genComponentField :: Text -> ComponentType -> Text
genComponentField recName (ComponentType fieldName td isOptional) =
  let accessor = asnFieldAccessorName recName fieldName
      hsType =
        if isOptional || isOptionalType td
          then "!(Maybe " <> asn1InnerHsType (unwrapOptional td) <> ")"
          else asn1StrictHsType td
  in accessor <> " :: " <> hsType


isOptionalType :: ASN1TypeDef -> Bool
isOptionalType (TDOptional _) = True
isOptionalType (TDDefault _ _) = True
isOptionalType _ = False


unwrapOptional :: ASN1TypeDef -> ASN1TypeDef
unwrapOptional (TDOptional inner) = inner
unwrapOptional (TDDefault inner _) = inner
unwrapOptional other = other


-- ---------------------------------------------------------------------------
-- CHOICE -> sum type
-- ---------------------------------------------------------------------------

genChoiceDecl :: Text -> [ComponentType] -> Text
genChoiceDecl name components =
  T.unlines $
    ["data " <> name]
      <> case components of
        [] -> ["  deriving stock (Show, Eq, Generic)"]
        (c : cs) ->
          ["  = " <> genChoiceAlt name c]
            <> map (\comp -> "  | " <> genChoiceAlt name comp) cs
            <> ["  deriving stock (Show, Eq, Generic)"]


genChoiceAlt :: Text -> ComponentType -> Text
genChoiceAlt parentName (ComponentType altName td _) =
  let conName = parentName <> upperFirst (snakeToCamel altName)
  in conName <> " " <> asn1StrictHsType (unwrapOptional td)


-- ---------------------------------------------------------------------------
-- ENUMERATED -> enum
-- ---------------------------------------------------------------------------

genEnumeratedDecl :: Text -> [(Text, Maybe Int)] -> Text
genEnumeratedDecl name vals =
  T.unlines $
    ["data " <> name]
      <> case vals of
        [] -> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]
        ((sym, _) : rest) ->
          ["  = " <> asn1EnumConName name sym]
            <> map (\(s, _) -> "  | " <> asn1EnumConName name s) rest
            <> ["  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)"]


asn1EnumConName :: Text -> Text -> Text
asn1EnumConName enumName valName =
  enumName <> upperFirst (snakeToCamel valName)


-- ---------------------------------------------------------------------------
-- Type mapping
-- ---------------------------------------------------------------------------

asnFieldAccessorName :: Text -> Text -> Text
asnFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)


asn1StrictHsType :: ASN1TypeDef -> Text
asn1StrictHsType = \case
  TDBoolean -> "!Bool"
  TDInteger _ -> "{-# UNPACK #-} !Int64"
  TDNULL -> "()"
  TDOctetString _ -> "!ByteString"
  TDBitString -> "!ByteString"
  TDUTF8String -> "!Text"
  TDPrintableString -> "!Text"
  TDIA5String -> "!Text"
  TDVisibleString -> "!Text"
  TDNamedType n -> "!" <> n
  TDSequenceOf inner -> "!(Vector " <> asn1InnerHsType inner <> ")"
  TDSetOf inner -> "!(Vector " <> asn1InnerHsType inner <> ")"
  TDOptional inner -> "!(Maybe " <> asn1InnerHsType inner <> ")"
  TDDefault inner _ -> "!(Maybe " <> asn1InnerHsType inner <> ")"
  TDSequence _ -> "!ASN1Value"
  TDChoice _ -> "!ASN1Value"
  TDEnumerated _ -> "!ASN1Value"


asn1InnerHsType :: ASN1TypeDef -> Text
asn1InnerHsType = \case
  TDBoolean -> "Bool"
  TDInteger _ -> "Int64"
  TDNULL -> "()"
  TDOctetString _ -> "ByteString"
  TDBitString -> "ByteString"
  TDUTF8String -> "Text"
  TDPrintableString -> "Text"
  TDIA5String -> "Text"
  TDVisibleString -> "Text"
  TDNamedType n -> n
  TDSequenceOf inner -> "(Vector " <> asn1InnerHsType inner <> ")"
  TDSetOf inner -> "(Vector " <> asn1InnerHsType inner <> ")"
  TDOptional inner -> "(Maybe " <> asn1InnerHsType inner <> ")"
  TDDefault inner _ -> "(Maybe " <> asn1InnerHsType inner <> ")"
  TDSequence _ -> "ASN1Value"
  TDChoice _ -> "ASN1Value"
  TDEnumerated _ -> "ASN1Value"


-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveASN1 :: ASN1Module -> Q [Dec]
deriveASN1 modl = do
  let assignments = V.toList (asnAssignments modl)
  concat <$> mapM deriveAssignment assignments


deriveAssignment :: TypeAssignment -> Q [Dec]
deriveAssignment (TypeAssignment name td) = deriveTypeDef name td


deriveTypeDef :: Text -> ASN1TypeDef -> Q [Dec]
deriveTypeDef name = \case
  TDSequence components -> deriveSequenceTH name (V.toList components)
  TDChoice components -> deriveChoiceTH name (V.toList components)
  TDEnumerated vals -> deriveEnumeratedTH name (V.toList vals)
  TDSequenceOf inner -> deriveTypeAlias name (asn1InnerHsType inner) True
  TDSetOf inner -> deriveTypeAlias name (asn1InnerHsType inner) True
  TDNamedType ref -> deriveTypeAlias name ref False
  _ -> pure []


deriveTypeAlias :: Text -> Text -> Bool -> Q [Dec]
deriveTypeAlias name target isVector = do
  let tyName = mkName (T.unpack name)
      targetTy =
        if isVector
          then AppT (ConT ''V.Vector) (ConT (mkName (T.unpack target)))
          else ConT (mkName (T.unpack target))
  pure [TySynD tyName [] targetTy]


-- ---------------------------------------------------------------------------
-- TH: SEQUENCE -> record
-- ---------------------------------------------------------------------------

deriveSequenceTH :: Text -> [ComponentType] -> Q [Dec]
deriveSequenceTH name components = do
  let tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
  fieldDecs <- mapM (mkASN1RecordField name) components
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          [RecC conName fieldDecs]
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkASN1RecordField :: Text -> ComponentType -> Q VarBangType
mkASN1RecordField recName (ComponentType fieldName td isOptional) = do
  let accessor = asnFieldAccessorName recName fieldName
      accName = mkName (T.unpack accessor)
  hsTy <-
    if isOptional || isOptionalType td
      then do
        inner <- asn1TypeToTH (unwrapOptional td)
        pure (AppT (ConT ''Maybe) inner)
      else asn1TypeToTH td
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)


-- ---------------------------------------------------------------------------
-- TH: CHOICE -> sum type
-- ---------------------------------------------------------------------------

deriveChoiceTH :: Text -> [ComponentType] -> Q [Dec]
deriveChoiceTH name components = do
  let tyName = mkName (T.unpack name)
  cons <- mapM (mkChoiceCon name) components
  let dataDec =
        DataD
          []
          tyName
          []
          Nothing
          cons
          [DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq]]
  pure [dataDec]


mkChoiceCon :: Text -> ComponentType -> Q Con
mkChoiceCon parentName (ComponentType altName td _) = do
  let conName = mkName (T.unpack (parentName <> upperFirst (snakeToCamel altName)))
  argTy <- asn1TypeToTH (unwrapOptional td)
  let bangArg = (Bang NoSourceUnpackedness SourceStrict, argTy)
  pure (NormalC conName [bangArg])


-- ---------------------------------------------------------------------------
-- TH: ENUMERATED
-- ---------------------------------------------------------------------------

deriveEnumeratedTH :: Text -> [(Text, Maybe Int)] -> Q [Dec]
deriveEnumeratedTH name vals = do
  let tyName = mkName (T.unpack name)
      cons = map (\(sym, _) -> NormalC (mkName (T.unpack (asn1EnumConName name sym))) []) vals
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
-- TH type mapping
-- ---------------------------------------------------------------------------

asn1TypeToTH :: ASN1TypeDef -> Q Type
asn1TypeToTH = \case
  TDBoolean -> [t|Bool|]
  TDInteger _ -> [t|Int64|]
  TDNULL -> [t|()|]
  TDOctetString _ -> [t|ByteString|]
  TDBitString -> [t|ByteString|]
  TDUTF8String -> [t|Text|]
  TDPrintableString -> [t|Text|]
  TDIA5String -> [t|Text|]
  TDVisibleString -> [t|Text|]
  TDNamedType n -> pure (ConT (mkName (T.unpack n)))
  TDSequenceOf inner -> do
    innerTy <- asn1TypeToTH inner
    pure (AppT (ConT ''V.Vector) innerTy)
  TDSetOf inner -> do
    innerTy <- asn1TypeToTH inner
    pure (AppT (ConT ''V.Vector) innerTy)
  TDOptional inner -> do
    innerTy <- asn1TypeToTH inner
    pure (AppT (ConT ''Maybe) innerTy)
  TDDefault inner _ -> do
    innerTy <- asn1TypeToTH inner
    pure (AppT (ConT ''Maybe) innerTy)
  _ -> [t|()|]


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
