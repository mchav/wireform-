{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Annotation-driven Template Haskell deriver for Apache Arrow
-- 'Arrow.Record.Table' values.
--
-- Arrow is a /columnar/ format: records become rows in a columnar
-- batch rather than recursive trees. The deriver therefore
-- produces row-level instances:
--
-- * For a record type, it emits a 'HasTable' instance so that
--   @'encodeTable' 'hasTable'@ and @'decodeTable' 'hasTable'@
--   round-trip a @'Data.Vector.Vector'@ of the record through an
--   Arrow 'Schema' + @'Data.Vector.Vector' 'ColumnArray'@.
-- * For a @newtype@, it emits 'HasEncoder' and 'HasDecoder'
--   instances that pass through to the inner type. This lets the
--   newtype be used as a record field by the record deriver.
--
-- Sum types and enums fail at splice time because Arrow has no
-- first-class column-level union representation.
--
-- Modifiers honoured (resolved against 'backendArrow'):
--
-- * 'rename', 'renameStyle', 'renameWith' — name of the column in
--   the emitted schema.
-- * 'skip' — drop the column entirely from both encode and decode
--   sides; if combined with 'defaults', the decoder fills the
--   field from the named default function.
-- * 'defaults' — the default value used for skipped fields when
--   decoding.
-- * 'coerced' — the field is encoded/decoded via the named target
--   type's 'HasEncoder' / 'HasDecoder'; @Data.Coerce.coerce@ wraps
--   the boundary in both directions.
module Arrow.Derive
  ( -- * Class
    HasTable (..)
    -- * Derivers
  , deriveArrow
  , deriveArrowTable
  , deriveArrowColumn
  ) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Functor.Contravariant (contramap)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Language.Haskell.TH

import Arrow.Record
  ( Decoder
  , Encoder
  , Table
  , columnD
  , fieldE
  , table
  )
import Arrow.Record.Generic (HasDecoder (..), HasEncoder (..))

import Wireform.Derive.Backend (backendArrow)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- HasTable
-- ---------------------------------------------------------------------------

-- | A record type with a derived Arrow 'Table'.
--
-- Analogous to 'HasEncoder' / 'HasDecoder' from
-- "Arrow.Record.Generic", but operates at the row level rather
-- than the column level: a 'Table' bundles a 'RowEncoder' and a
-- 'RowDecoder' for the same record type.
class HasTable a where
  hasTable :: Table a

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive Arrow instances for the named type.
--
-- Dispatches on the data shape:
--
-- * Records get a 'HasTable' instance.
-- * Newtypes get 'HasEncoder' + 'HasDecoder' instances that
--   pass through to the inner type.
-- * Sum types and enums fail at splice time.
deriveArrow :: Name -> Q [Dec]
deriveArrow nm = do
  ti <- reifyTypeInfo nm
  case typeInfoShape ti of
    TypeShapeRecord  c -> deriveRecordTable ti c
    TypeShapeNewtype c -> deriveNewtypeColumn ti c
    TypeShapeEnum    _ -> fail (rejectEnumOrSum nm "enum")
    TypeShapeSum     _ -> fail (rejectEnumOrSum nm "sum")

-- | Synonym for 'deriveArrow', used when calling sites want to
-- emphasise the record code path.
deriveArrowTable :: Name -> Q [Dec]
deriveArrowTable = deriveArrow

-- | Synonym for 'deriveArrow', used when calling sites want to
-- emphasise the newtype (column) code path.
deriveArrowColumn :: Name -> Q [Dec]
deriveArrowColumn = deriveArrow

rejectEnumOrSum :: Name -> String -> String
rejectEnumOrSum nm shape =
  "Arrow.Derive: cannot derive Arrow instances for " ++ shape
    ++ " type " ++ show nm
    ++ ". Arrow has no first-class column-level union representation; "
    ++ "model alternatives as a discriminator + payload columns by hand."

-- ---------------------------------------------------------------------------
-- Records → HasTable
-- ---------------------------------------------------------------------------

deriveRecordTable :: TypeInfo -> ConInfo -> Q [Dec]
deriveRecordTable ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  encExp <- buildRowEncoder c
  decExp <- buildRowDecoder c
  body   <- [| table $(pure encExp) $(pure decExp) |]
  let decl = InstanceD Nothing []
              (AppT (ConT ''HasTable) typ)
              [FunD 'hasTable [Clause [] (NormalB body) []]]
  pure [decl]

-- | Build a 'RowEncoder' expression. Skipped fields drop out
-- entirely; if every field is skipped the encoder collapses to
-- 'mempty' (a no-column batch).
buildRowEncoder :: ConInfo -> Q Exp
buildRowEncoder c = do
  pieces <- mapM mkFieldEncoder (conInfoFields c)
  case catMaybes pieces of
    []     -> [| mempty |]
    (e:es) -> foldlM (\acc x -> [| $(pure acc) <> $(pure x) |]) e es

mkFieldEncoder :: FieldInfo -> Q (Maybe Exp)
mkFieldEncoder fi@(FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendArrow selName
  if miSkip mi
    then pure Nothing
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      enc    <- case miCoerce mi of
        Nothing      ->
          [| fieldE $(pure keyExp) $(varE selName) hasEncoder |]
        Just tgtName ->
          let tgtTy = ConT tgtName
              srcTy = fieldInfoType fi
          in  [| fieldE $(pure keyExp)
                        ((coerce :: $(pure srcTy) -> $(pure tgtTy))
                            . $(varE selName))
                        (hasEncoder :: Encoder $(pure tgtTy)) |]
      pure (Just enc)

-- | Build a 'RowDecoder' expression. Every field of the
-- constructor needs a slot in the applicative chain; skipped
-- fields use @pure <default>@ to fill in.
buildRowDecoder :: ConInfo -> Q Exp
buildRowDecoder c = do
  let conName = conInfoName c
      fields  = conInfoFields c
  case fields of
    [] -> [| pure $(conE conName) |]
    (f0 : fs) -> do
      e0 <- mkFieldDecoder f0
      hd <- [| $(conE conName) <$> $(pure e0) |]
      foldlM
        (\acc f -> do
            ef <- mkFieldDecoder f
            [| $(pure acc) <*> $(pure ef) |])
        hd
        fs

mkFieldDecoder :: FieldInfo -> Q Exp
mkFieldDecoder fi@(FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi      <- reifyModifierInfoFor backendArrow selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm ->
        [| pure $(varE defNm) |]
      Nothing -> fail $
        "Arrow.Derive: skipped field " ++ show selName
          ++ " requires a 'defaults' modifier so the decoder can "
          ++ "fill the missing value."
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      case miCoerce mi of
        Nothing ->
          [| columnD $(pure keyExp) hasDecoder |]
        Just tgtName ->
          let tgtTy = ConT tgtName
              srcTy = fieldInfoType fi
          in  [| (coerce :: $(pure tgtTy) -> $(pure srcTy))
                  <$> columnD $(pure keyExp)
                              (hasDecoder :: Decoder $(pure tgtTy)) |]

-- ---------------------------------------------------------------------------
-- Newtypes → HasEncoder / HasDecoder pass-through
-- ---------------------------------------------------------------------------

deriveNewtypeColumn :: TypeInfo -> ConInfo -> Q [Dec]
deriveNewtypeColumn ti c = do
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
  innerTy <- case conInfoFields c of
    [FieldInfo _ ty] -> pure ty
    _ -> fail $
      "Arrow.Derive: newtype " ++ show (typeInfoName ti)
        ++ " must have exactly one field"
  encBody <- [| contramap (coerce :: $(pure typ) -> $(pure innerTy))
                          (hasEncoder :: Encoder $(pure innerTy)) |]
  decBody <- [| (coerce :: $(pure innerTy) -> $(pure typ))
                  <$> (hasDecoder :: Decoder $(pure innerTy)) |]
  let encInst = InstanceD Nothing []
                  (AppT (ConT ''HasEncoder) typ)
                  [FunD 'hasEncoder [Clause [] (NormalB encBody) []]]
      decInst = InstanceD Nothing []
                  (AppT (ConT ''HasDecoder) typ)
                  [FunD 'hasDecoder [Clause [] (NormalB decBody) []]]
  pure [encInst, decInst]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "Arrow.Derive: cannot derive Arrow instances for a positional \
       \(non-record) constructor; Arrow columns are addressed by name."

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT

