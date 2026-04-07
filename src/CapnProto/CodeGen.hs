{-# LANGUAGE TemplateHaskell #-}
-- | Cap'n Proto code generation — generates Haskell data types from
-- Cap'n Proto schemas.
module CapnProto.CodeGen
  ( generateCapnProtoTypes
  , deriveCapnProto
  ) where

import Data.Char (toLower, toUpper)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Language.Haskell.TH

import CapnProto.Schema

-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateCapnProtoTypes :: CapnProtoSchema -> Text
generateCapnProtoTypes schema =
  let decls = concatMap genDecl (V.toList (csDecls schema))
  in T.intercalate "\n\n" decls

genDecl :: Declaration -> [Text]
genDecl (DStruct s)       = genCapnStruct s
genDecl (DEnum e)         = genCapnEnum e
genDecl (DInterface _)    = []
genDecl (DConst _ _ _)    = []
genDecl (DAnnotation _ _) = []

-- ---------------------------------------------------------------------------
-- Struct generation (text)
-- ---------------------------------------------------------------------------

genCapnStruct :: StructDef -> [Text]
genCapnStruct sd =
  let name = sdName sd
      fields = V.toList (sdFields sd)
      nested = concatMap genNestedDecl (V.toList (sdNested sd))
      unions = concatMap (genUnionDecl name) (V.toList (sdUnions sd))
  in nested <> unions <>
     [ genStructDataDecl name fields ]

genNestedDecl :: Declaration -> [Text]
genNestedDecl = genDecl

genUnionDecl :: Text -> UnionDef -> [Text]
genUnionDecl parentName ud =
  let unionName = parentName <> "Union"
      fields = V.toList (udFields ud)
      cons = map (\f -> unionName <> upperFirst (snakeToCamel (fdName f))) fields
  in [ T.unlines $
       [ "data " <> unionName ]
       <> case zip cons fields of
         [] -> [ "  deriving stock (Show, Eq, Generic)" ]
         ((c, f):rest) ->
           [ "  = " <> c <> " " <> capnStrictHsType (fdType f) ]
           <> map (\(cn, fn) -> "  | " <> cn <> " " <> capnStrictHsType (fdType fn)) rest
           <> [ "  deriving stock (Show, Eq, Generic)" ]
     ]

genStructDataDecl :: Text -> [FieldDef] -> Text
genStructDataDecl name fields = T.unlines $
  [ "data " <> name <> " = " <> name ]
  <> case fields of
    [] ->
      [ "  deriving stock (Show, Eq, Generic)" ]
    (f:fs) ->
      [ "  { " <> genFieldDecl name f ]
      <> map (\fld -> "  , " <> genFieldDecl name fld) fs
      <> [ "  } deriving stock (Show, Eq, Generic)" ]

genFieldDecl :: Text -> FieldDef -> Text
genFieldDecl recName fld =
  let accessor = capnFieldAccessorName recName (fdName fld)
      hsType = capnStrictHsType (fdType fld)
  in accessor <> " :: " <> hsType

capnFieldAccessorName :: Text -> Text -> Text
capnFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)

capnStrictHsType :: CapnType -> Text
capnStrictHsType = \case
  CTVoid   -> "()"
  CTBool   -> "!Bool"
  CTInt8   -> "{-# UNPACK #-} !Int8"
  CTInt16  -> "{-# UNPACK #-} !Int16"
  CTInt32  -> "{-# UNPACK #-} !Int32"
  CTInt64  -> "{-# UNPACK #-} !Int64"
  CTUInt8  -> "{-# UNPACK #-} !Word8"
  CTUInt16 -> "{-# UNPACK #-} !Word16"
  CTUInt32 -> "{-# UNPACK #-} !Word32"
  CTUInt64 -> "{-# UNPACK #-} !Word64"
  CTFloat32 -> "{-# UNPACK #-} !Float"
  CTFloat64 -> "{-# UNPACK #-} !Double"
  CTText   -> "!Text"
  CTData   -> "!ByteString"
  CTList inner -> "!(Vector " <> capnInnerHsType inner <> ")"
  CTNamed n -> "!" <> n

capnInnerHsType :: CapnType -> Text
capnInnerHsType = \case
  CTVoid   -> "()"
  CTBool   -> "Bool"
  CTInt8   -> "Int8"
  CTInt16  -> "Int16"
  CTInt32  -> "Int32"
  CTInt64  -> "Int64"
  CTUInt8  -> "Word8"
  CTUInt16 -> "Word16"
  CTUInt32 -> "Word32"
  CTUInt64 -> "Word64"
  CTFloat32 -> "Float"
  CTFloat64 -> "Double"
  CTText   -> "Text"
  CTData   -> "ByteString"
  CTList inner -> "(Vector " <> capnInnerHsType inner <> ")"
  CTNamed n -> n

-- ---------------------------------------------------------------------------
-- Enum generation (text)
-- ---------------------------------------------------------------------------

genCapnEnum :: EnumDef -> [Text]
genCapnEnum ed =
  let name = edName ed
      vals = V.toList (edValues ed)
  in [ T.unlines $
       [ "data " <> name ]
       <> case vals of
         [] -> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" ]
         ((sym, _):rest) ->
           [ "  = " <> capnEnumConName name sym ]
           <> map (\(s, _) -> "  | " <> capnEnumConName name s) rest
           <> [ "  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)" ]
     ]

capnEnumConName :: Text -> Text -> Text
capnEnumConName enumName valName =
  enumName <> upperFirst (snakeToCamel valName)

-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveCapnProto :: CapnProtoSchema -> Q [Dec]
deriveCapnProto schema = do
  let decls = V.toList (csDecls schema)
  concat <$> mapM deriveCapnDecl decls

deriveCapnDecl :: Declaration -> Q [Dec]
deriveCapnDecl (DStruct s) = deriveCapnStructTH s
deriveCapnDecl (DEnum e)   = deriveCapnEnumTH e
deriveCapnDecl _           = pure []

-- ---------------------------------------------------------------------------
-- TH: Struct
-- ---------------------------------------------------------------------------

deriveCapnStructTH :: StructDef -> Q [Dec]
deriveCapnStructTH sd = do
  let name = sdName sd
      tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      fields = V.toList (sdFields sd)
  nestedDecs <- concat <$> mapM deriveCapnDecl (V.toList (sdNested sd))
  fieldDecs <- mapM (mkCapnRecordField name) fields
  let dataDec = DataD [] tyName [] Nothing
        [RecC conName fieldDecs]
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  pure (nestedDecs <> [dataDec])

mkCapnRecordField :: Text -> FieldDef -> Q VarBangType
mkCapnRecordField recName fld = do
  let accessor = capnFieldAccessorName recName (fdName fld)
      accName = mkName (T.unpack accessor)
  hsTy <- capnTypeToTH (fdType fld)
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)

capnTypeToTH :: CapnType -> Q Type
capnTypeToTH = \case
  CTVoid   -> [t| () |]
  CTBool   -> [t| Bool |]
  CTInt8   -> [t| Int8 |]
  CTInt16  -> [t| Int16 |]
  CTInt32  -> [t| Int32 |]
  CTInt64  -> [t| Int64 |]
  CTUInt8  -> [t| Word8 |]
  CTUInt16 -> [t| Word16 |]
  CTUInt32 -> [t| Word32 |]
  CTUInt64 -> [t| Word64 |]
  CTFloat32 -> [t| Float |]
  CTFloat64 -> [t| Double |]
  CTText   -> [t| Text |]
  CTData   -> [t| ByteString |]
  CTList inner -> do
    innerTy <- capnTypeToTH inner
    pure (AppT (ConT ''V.Vector) innerTy)
  CTNamed n -> pure (ConT (mkName (T.unpack n)))

-- ---------------------------------------------------------------------------
-- TH: Enum
-- ---------------------------------------------------------------------------

deriveCapnEnumTH :: EnumDef -> Q [Dec]
deriveCapnEnumTH ed = do
  let name = edName ed
      tyName = mkName (T.unpack name)
      vals = V.toList (edValues ed)
      cons = map (\(sym, _) -> NormalC (mkName (T.unpack (capnEnumConName name sym))) []) vals
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
