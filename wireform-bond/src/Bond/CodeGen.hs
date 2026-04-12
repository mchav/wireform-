{-# LANGUAGE TemplateHaskell #-}
-- | Bond code generation — generates Haskell data types and
-- ToBond\/FromBond stub instances from Bond schemas.
module Bond.CodeGen
  ( generateBondTypes
  , generateBondTypesWithRegistry
  , deriveBond
  ) where

import Data.Char (toLower, toUpper)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Language.Haskell.TH

import Bond.Schema
import Bond.Registry

-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateBondTypes :: BondSchema -> Text
generateBondTypes = generateBondTypesWithRegistry defaultBondRegistry

-- | Generate Haskell source code using a custom 'BondRegistry'.
-- When a field has an attribute matching a registered handler, the handler's
-- type transformation and extra code generation are applied.
generateBondTypesWithRegistry :: BondRegistry -> BondSchema -> Text
generateBondTypesWithRegistry reg schema =
  let decls = concatMap (genDeclWithRegistry reg) (bondDecls schema)
  in T.intercalate "\n\n" decls

genDecl :: BondDecl -> [Text]
genDecl = genDeclWithRegistry defaultBondRegistry

genDeclWithRegistry :: BondRegistry -> BondDecl -> [Text]
genDeclWithRegistry reg (BondDeclStruct s) = genBondStructWithRegistry reg s
genDeclWithRegistry _reg (BondDeclEnum e) = genBondEnum e

-- ---------------------------------------------------------------------------
-- Struct generation (text)
-- ---------------------------------------------------------------------------

genBondStruct :: BondStruct -> [Text]
genBondStruct = genBondStructWithRegistry defaultBondRegistry

genBondStructWithRegistry :: BondRegistry -> BondStruct -> [Text]
genBondStructWithRegistry reg bs =
  let name = bsName bs
      fields = bsFields bs
      extraCode = concatMap (\fld ->
        concatMap (\(k, mv) ->
          case Map.lookup k (brAttributeHandlers reg) of
            Just handler -> Bond.Registry.hExtraCode handler k mv
            Nothing -> []
          ) (V.toList (bfAttributes fld))
        ) fields
  in [ genStructDataDeclWithRegistry reg name fields
     , genToBondStruct name fields
     , genFromBondStruct name fields
     ]
     <> if null extraCode then [] else [T.unlines extraCode]

genStructDataDecl :: Text -> [BondField] -> Text
genStructDataDecl = genStructDataDeclWithRegistry defaultBondRegistry

genStructDataDeclWithRegistry :: BondRegistry -> Text -> [BondField] -> Text
genStructDataDeclWithRegistry reg name fields = T.unlines $
  [ "data " <> name <> " = " <> name ]
  <> case fields of
    [] ->
      [ "  deriving stock (Show, Eq, Generic)" ]
    (f:fs) ->
      [ "  { " <> genFieldDeclWithRegistry reg name f ]
      <> map (\fld -> "  , " <> genFieldDeclWithRegistry reg name fld) fs
      <> [ "  } deriving stock (Show, Eq, Generic)" ]

genFieldDecl :: Text -> BondField -> Text
genFieldDecl = genFieldDeclWithRegistry defaultBondRegistry

genFieldDeclWithRegistry :: BondRegistry -> Text -> BondField -> Text
genFieldDeclWithRegistry reg recName fld =
  let accessor = bondFieldAccessorName recName (bfName fld)
      baseType = bondFieldHsType (bfType fld) (bfModifier fld)
      hsType = applyBondAttributeTransforms reg fld baseType
  in accessor <> " :: " <> hsType

applyBondAttributeTransforms :: BondRegistry -> BondField -> Text -> Text
applyBondAttributeTransforms reg fld ty =
  foldl (\acc (k, _mv) ->
    case Map.lookup k (brAttributeHandlers reg) of
      Just handler -> Bond.Registry.hTransformType handler acc
      Nothing -> acc
    ) ty (V.toList (bfAttributes fld))

bondFieldAccessorName :: Text -> Text -> Text
bondFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)

bondFieldHsType :: BondFieldType -> BondFieldModifier -> Text
bondFieldHsType ty modifier = case modifier of
  BondOptional -> "!(Maybe " <> bondInnerHsType ty <> ")"
  _            -> bondStrictHsType ty

bondStrictHsType :: BondFieldType -> Text
bondStrictHsType = \case
  BFTBool   -> "!Bool"
  BFTInt8   -> "{-# UNPACK #-} !Int8"
  BFTInt16  -> "{-# UNPACK #-} !Int16"
  BFTInt32  -> "{-# UNPACK #-} !Int32"
  BFTInt64  -> "{-# UNPACK #-} !Int64"
  BFTUInt8  -> "{-# UNPACK #-} !Word8"
  BFTUInt16 -> "{-# UNPACK #-} !Word16"
  BFTUInt32 -> "{-# UNPACK #-} !Word32"
  BFTUInt64 -> "{-# UNPACK #-} !Word64"
  BFTFloat  -> "{-# UNPACK #-} !Float"
  BFTDouble -> "{-# UNPACK #-} !Double"
  BFTString -> "!Text"
  BFTWString -> "!Text"
  BFTBlob   -> "!ByteString"
  BFTNamed n -> "!" <> n
  BFTList elemTy -> "!(Vector " <> bondInnerHsType elemTy <> ")"
  BFTSet elemTy -> "!(Vector " <> bondInnerHsType elemTy <> ")"
  BFTMap keyTy valTy -> "!(Map " <> bondInnerHsType keyTy <> " " <> bondInnerHsType valTy <> ")"
  BFTNullable inner -> "!(Maybe " <> bondInnerHsType inner <> ")"

bondInnerHsType :: BondFieldType -> Text
bondInnerHsType = \case
  BFTBool   -> "Bool"
  BFTInt8   -> "Int8"
  BFTInt16  -> "Int16"
  BFTInt32  -> "Int32"
  BFTInt64  -> "Int64"
  BFTUInt8  -> "Word8"
  BFTUInt16 -> "Word16"
  BFTUInt32 -> "Word32"
  BFTUInt64 -> "Word64"
  BFTFloat  -> "Float"
  BFTDouble -> "Double"
  BFTString -> "Text"
  BFTWString -> "Text"
  BFTBlob   -> "ByteString"
  BFTNamed n -> n
  BFTList elemTy -> "(Vector " <> bondInnerHsType elemTy <> ")"
  BFTSet elemTy -> "(Vector " <> bondInnerHsType elemTy <> ")"
  BFTMap keyTy valTy -> "(Map " <> bondInnerHsType keyTy <> " " <> bondInnerHsType valTy <> ")"
  BFTNullable inner -> "(Maybe " <> bondInnerHsType inner <> ")"

-- ---------------------------------------------------------------------------
-- ToBond / FromBond instances (text)
-- ---------------------------------------------------------------------------

genToBondStruct :: Text -> [BondField] -> Text
genToBondStruct name _fields = T.unlines
  [ "instance ToBond " <> name <> " where"
  , "  toBond _ = error \"ToBond " <> name <> ": stub\""
  ]

genFromBondStruct :: Text -> [BondField] -> Text
genFromBondStruct name _fields = T.unlines
  [ "instance FromBond " <> name <> " where"
  , "  fromBond _ = Left \"FromBond " <> name <> ": stub\""
  ]

-- ---------------------------------------------------------------------------
-- Enum generation (text)
-- ---------------------------------------------------------------------------

genBondEnum :: BondEnum -> [Text]
genBondEnum be =
  let name = beName be
      vals = beValues be
  in [ genEnumDataDecl name vals
     , genToBondEnum name vals
     , genFromBondEnum name vals
     ]

genEnumDataDecl :: Text -> [BondEnumValue] -> Text
genEnumDataDecl name vals = T.unlines $
  [ "data " <> name ]
  <> case vals of
    [] -> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" ]
    (v:vs) ->
      [ "  = " <> bondEnumConName name (bevName v) ]
      <> map (\val -> "  | " <> bondEnumConName name (bevName val)) vs
      <> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" ]

bondEnumConName :: Text -> Text -> Text
bondEnumConName enumName valName =
  enumName <> upperFirst (snakeToCamel (T.toLower valName))

genToBondEnum :: Text -> [BondEnumValue] -> Text
genToBondEnum name vals = T.unlines $
  [ "instance ToBond " <> name <> " where" ]
  <> zipWith (\i v ->
      "  toBond " <> bondEnumConName name (bevName v) <> " = Bond.Value.Int32 " <> T.pack (show (i :: Int))
    ) [0..] vals

genFromBondEnum :: Text -> [BondEnumValue] -> Text
genFromBondEnum name vals = T.unlines $
  [ "instance FromBond " <> name <> " where" ]
  <> zipWith (\i v ->
      "  fromBond (Bond.Value.Int32 " <> T.pack (show (i :: Int)) <> ") = Right " <> bondEnumConName name (bevName v)
    ) [0..] vals
  <> [ "  fromBond _ = Left \"FromBond " <> name <> ": expected Int32 enum\"" ]

-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveBond :: BondSchema -> Q [Dec]
deriveBond schema = do
  let decls = bondDecls schema
  concat <$> mapM deriveBondDecl decls

deriveBondDecl :: BondDecl -> Q [Dec]
deriveBondDecl (BondDeclStruct s) = deriveBondStructTH s
deriveBondDecl (BondDeclEnum e)   = deriveBondEnumTH e

-- ---------------------------------------------------------------------------
-- TH: Struct
-- ---------------------------------------------------------------------------

deriveBondStructTH :: BondStruct -> Q [Dec]
deriveBondStructTH bs = do
  let name = bsName bs
      tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      fields = bsFields bs
  fieldDecs <- mapM (mkBondRecordField name) fields
  let dataDec = DataD [] tyName [] Nothing
        [RecC conName fieldDecs]
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  pure [dataDec]

mkBondRecordField :: Text -> BondField -> Q VarBangType
mkBondRecordField recName fld = do
  let accessor = bondFieldAccessorName recName (bfName fld)
      accName = mkName (T.unpack accessor)
  hsTy <- bondFieldToTHType (bfType fld) (bfModifier fld)
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)

bondFieldToTHType :: BondFieldType -> BondFieldModifier -> Q Type
bondFieldToTHType ty modifier = case modifier of
  BondOptional -> do
    inner <- bondTypeToTH ty
    pure (AppT (ConT ''Maybe) inner)
  _ -> bondTypeToTH ty

bondTypeToTH :: BondFieldType -> Q Type
bondTypeToTH = \case
  BFTBool   -> [t| Bool |]
  BFTInt8   -> [t| Int8 |]
  BFTInt16  -> [t| Int16 |]
  BFTInt32  -> [t| Int32 |]
  BFTInt64  -> [t| Int64 |]
  BFTUInt8  -> [t| Word8 |]
  BFTUInt16 -> [t| Word16 |]
  BFTUInt32 -> [t| Word32 |]
  BFTUInt64 -> [t| Word64 |]
  BFTFloat  -> [t| Float |]
  BFTDouble -> [t| Double |]
  BFTString -> [t| Text |]
  BFTWString -> [t| Text |]
  BFTBlob   -> [t| ByteString |]
  BFTNamed n -> pure (ConT (mkName (T.unpack n)))
  BFTList elemTy -> do
    inner <- bondTypeToTH elemTy
    pure (AppT (ConT ''V.Vector) inner)
  BFTSet elemTy -> do
    inner <- bondTypeToTH elemTy
    pure (AppT (ConT ''V.Vector) inner)
  BFTMap keyTy valTy -> do
    k <- bondTypeToTH keyTy
    v <- bondTypeToTH valTy
    pure (AppT (AppT (ConT ''Map) k) v)
  BFTNullable inner -> do
    innerTy <- bondTypeToTH inner
    pure (AppT (ConT ''Maybe) innerTy)

-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveBondEnumTH :: BondEnum -> Q [Dec]
deriveBondEnumTH be = do
  let name = beName be
      tyName = mkName (T.unpack name)
      vals = beValues be
      cons = map (\v -> NormalC (mkName (T.unpack (bondEnumConName name (bevName v)))) []) vals
      dataDec = DataD [] tyName [] Nothing cons
        [ DerivClause (Just StockStrategy)
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
    (p:ps) -> T.concat (lowerFirst p : map titleCase ps)

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s
