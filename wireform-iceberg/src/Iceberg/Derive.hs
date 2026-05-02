{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Annotation-driven Template Haskell deriver for Apache Iceberg.
--
-- Iceberg sits one layer above Parquet \/ ORC \/ Avro: a table's
-- /metadata/ is the deriver's main concern, while the row data is
-- left to one of the underlying file formats. This module therefore
-- only emits a /schema/ for a record type — there is no row class.
--
-- * 'icebergSchemaFor' splices an 'I.Schema' value (struct) for a
--   record. Field IDs are auto-assigned starting from 1 in
--   declaration order, but a 'tag' modifier overrides that.
-- * 'icebergFieldsFor' splices the inner @[(Text, IcebergType)]@ if
--   you want the bare type list without the surrounding 'I.Schema'
--   envelope (e.g. for a nested 'I.TStruct').
-- * 'deriveIceberg' generates a 'HasIcebergSchema' instance for the
--   named type so call sites can recover the schema from a 'Proxy'
--   without re-running the splice. Records get a fully-baked
--   'I.Schema' literal; newtypes pass through to the inner type's
--   'HasIcebergSchema' instance.
--
-- == Limitations
--
-- * Records (and newtypes around them) only. Sums \/ enums fail at
--   splice time — Iceberg does not represent variant constructors
--   at the schema level.
-- * Only the recognised flat scalar types are supported (@Int32 \/
--   Int64 \/ Int \/ Float \/ Double \/ Bool \/ Text \/ String \/
--   ByteString@). 'Maybe' wraps the field in @sfRequired = False@.
module Iceberg.Derive
  ( -- * Splice helpers
    icebergSchemaFor
  , icebergFieldsFor
    -- * Class + instance deriver
  , HasIcebergSchema (..)
  , deriveIceberg
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32, Int64)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH

import qualified Iceberg.Types as I

import Wireform.Derive.Backend (backendIceberg)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Public splice entry points
-- ---------------------------------------------------------------------------

-- | Splice an 'I.Schema' for the given record (or newtype around a
-- record) type. The schema's @schemaId@ is set to @0@ and
-- @schemaIdentifierFieldIds@ is empty; adjust after splicing if you
-- need a different table-level id or identity columns.
--
-- Newtypes defer to the inner type's 'HasIcebergSchema' instance, so
-- the inner type must already have one (e.g. via 'deriveIceberg').
icebergSchemaFor :: Name -> Q Exp
icebergSchemaFor nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord  c -> recordSchemaE c
    TypeShapeNewtype c -> newtypeSchemaE c
    TypeShapeEnum    _ -> fail (rejectShape nm "enum")
    TypeShapeSum     _ -> fail (rejectShape nm "sum")

-- | Splice @[(Text, IcebergType)]@ for the given record type — the
-- bare field list without the 'I.Schema' wrapper. Useful when the
-- record is being embedded inside another schema (e.g. the @value@
-- column of a @TList@ or a nested @TStruct@).
icebergFieldsFor :: Name -> Q Exp
icebergFieldsFor nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord c -> do
      pairs <- mapM fieldNameTypePair (conInfoFields c)
      pure (ListE pairs)
    _ -> fail $ "Iceberg.Derive.icebergFieldsFor: " ++ nameBase nm
             ++ " must be a single-constructor record"

-- ---------------------------------------------------------------------------
-- Schema construction
-- ---------------------------------------------------------------------------

recordSchemaE :: ConInfo -> Q Exp
recordSchemaE c = do
  fieldsE <- structFieldsE c
  [| I.Schema
       { I.schemaId                 = 0
       , I.schemaFields             = $(pure fieldsE)
       , I.schemaIdentifierFieldIds = V.empty
       } |]

newtypeSchemaE :: ConInfo -> Q Exp
newtypeSchemaE c = case conInfoFields c of
  [FieldInfo _ ty] ->
    [| icebergSchema (Proxy :: Proxy $(pure ty)) |]
  _ -> fail "Iceberg.Derive: newtype must have exactly one field"

rejectShape :: Name -> String -> String
rejectShape nm shape =
  "Iceberg.Derive: cannot derive Iceberg schema for " ++ shape
    ++ " type " ++ show nm
    ++ ". Iceberg's row schema is record-shaped; model variants "
    ++ "explicitly with a discriminator column and payload columns."

-- ---------------------------------------------------------------------------
-- Building the Vector StructField for icebergSchemaFor
-- ---------------------------------------------------------------------------

structFieldsE :: ConInfo -> Q Exp
structFieldsE c = do
  let assigned = assignFieldIds (conInfoFields c)
  sfExps <- mapM structFieldE assigned
  [| V.fromList $(pure (ListE sfExps)) |]

-- | Assign field IDs in declaration order. If a field has a
-- @tag :: Int@ modifier, that wins; otherwise we fill with the next
-- unused integer starting at 1.
assignFieldIds :: [FieldInfo] -> [(Int, FieldInfo)]
assignFieldIds = go 1
  where
    go _ []     = []
    go k (f:fs) = (k, f) : go (k + 1) fs

structFieldE :: (Int, FieldInfo) -> Q Exp
structFieldE (autoId, FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendIceberg selName
  let selBase = T.pack (nameBase selName)
      fid     = case miTag mi of
                  Just t  -> t
                  Nothing -> autoId
      idLit   = litE (integerL (fromIntegral fid))
  nameExp <- renderWireKey mi selBase
  let (isOptional, innerTy) = unwrapMaybe ty
      requiredE = if isOptional then [| False |] else [| True |]
  tyExp <- icebergTypeE selName innerTy
  [| I.StructField
       { I.sfId             = $idLit
       , I.sfName           = $(pure nameExp)
       , I.sfRequired       = $requiredE
       , I.sfType           = $(pure tyExp)
       , I.sfDoc            = Nothing
       , I.sfInitialDefault = Nothing
       , I.sfWriteDefault   = Nothing
       } |]

fieldNameTypePair :: FieldInfo -> Q Exp
fieldNameTypePair (FieldInfo mSel ty) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendIceberg selName
  let selBase = T.pack (nameBase selName)
  nameExp <- renderWireKey mi selBase
  let (_, innerTy) = unwrapMaybe ty
  tyExp <- icebergTypeE selName innerTy
  [| ($(pure nameExp), $(pure tyExp)) |]

-- ---------------------------------------------------------------------------
-- Field-type lookup
-- ---------------------------------------------------------------------------

icebergTypeE :: Name -> Type -> Q Exp
icebergTypeE selName ty = case typeBaseName ty of
  Just "Int32"      -> [| I.TInt    |]
  Just "Int64"      -> [| I.TLong   |]
  Just "Int"        -> [| I.TLong   |]
  Just "Float"      -> [| I.TFloat  |]
  Just "Double"     -> [| I.TDouble |]
  Just "Bool"       -> [| I.TBoolean |]
  Just "Text"       -> [| I.TString |]
  Just "String"     -> [| I.TString |]
  Just "ByteString" -> [| I.TBinary |]
  _ -> fail $ "Iceberg.Derive: unsupported field type for "
           ++ nameBase selName ++ ": " ++ pprint ty

_unused :: (Int32, Int64, ByteString)
_unused = (0, 0, mempty)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

unwrapMaybe :: Type -> (Bool, Type)
unwrapMaybe (AppT (ConT n) t) | n == ''Maybe = (True, t)
unwrapMaybe t                                 = (False, t)

typeBaseName :: Type -> Maybe String
typeBaseName = \case
  ConT n   -> Just (nameBase n)
  AppT t _ -> typeBaseName t
  _        -> Nothing

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "Iceberg.Derive: cannot derive Iceberg for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT

-- ---------------------------------------------------------------------------
-- HasIcebergSchema class + deriveIceberg
-- ---------------------------------------------------------------------------

-- | Reflect a Haskell type onto its Iceberg 'I.Schema'.
--
-- The 'Proxy' argument carries no value-level information; it is
-- there so callers can write @'icebergSchema' ('Proxy' :: 'Proxy'
-- MyType)@ at a use site without 'TypeApplications'. The deriver
-- 'deriveIceberg' emits one instance per type it touches.
class HasIcebergSchema a where
  icebergSchema :: proxy a -> I.Schema

-- | Derive a 'HasIcebergSchema' instance for the named type.
--
-- Records get a fully-baked 'I.Schema' literal. Newtypes get a
-- pass-through instance that defers to the inner type's
-- 'HasIcebergSchema' instance — letting inner-type evolution
-- happen without re-deriving every wrapper.
deriveIceberg :: Name -> Q [Dec]
deriveIceberg nm = do
  ti   <- reifyTypeInfo nm
  body <- case typeInfoShape ti of
    TypeShapeRecord  c -> recordSchemaE c
    TypeShapeNewtype c -> newtypeSchemaE c
    TypeShapeEnum    _ -> fail (rejectShape nm "enum")
    TypeShapeSum     _ -> fail (rejectShape nm "sum")
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''HasIcebergSchema) typ)
              [FunD 'icebergSchema [Clause [WildP] (NormalB body) []]]
  pure [decl]
