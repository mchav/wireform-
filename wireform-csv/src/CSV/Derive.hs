{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for CSV
'CSV.Class.ToCSV' / 'CSV.Class.FromCSV' instances.

CSV is fundamentally **flat** — each Haskell record becomes one
CSV row of 'Text' cells. The deriver therefore:

* supports only 'TypeShapeRecord' (records) and 'TypeShapeNewtype'
  (single field, pass-through);
* cannot derive for 'TypeShapeSum' or multi-constructor enums (CSV
  has no notion of \"which constructor\"). The deriver fails at
  splice time with a clear message rather than emitting code that
  would break at runtime;
* encodes / decodes each field via 'CSV.Class.CSVField' from the
  class library — so any user-defined scalar type with a
  'CSVField' instance plugs in automatically.

== Modifiers honoured

* 'skip' — omits the field from both the row and the header,
  defaulting on decode via 'defaults' (required, like the other
  derivers).
* 'rename' / 'renameStyle' — used **only** for the header text
  reported by 'csvHeaderFor'. Field order on the row is positional
  regardless of renames.
* 'coerced' — wraps encode \/ decode in 'Data.Coerce.coerce', so
  newtype wrappers can transparently round-trip through the inner
  field's 'CSVField' instance.
-}
module CSV.Derive (
  deriveCSV,
  deriveToCSV,
  deriveFromCSV,
  csvHeaderFor,
) where

import CSV.Class qualified as C
import Data.Coerce (coerce)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive both 'C.ToCSV' and 'C.FromCSV' for a record / newtype.
deriveCSV :: Name -> Q [Dec]
deriveCSV nm = (++) <$> deriveToCSV nm <*> deriveFromCSV nm


deriveToCSV :: Name -> Q [Dec]
deriveToCSV nm = do
  ti <- reifyTypeInfo nm
  body <- toCSVBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''C.ToCSV) typ)
          [FunD 'C.toCSVRow [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromCSV :: Name -> Q [Dec]
deriveFromCSV nm = do
  ti <- reifyTypeInfo nm
  body <- fromCSVBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''C.FromCSV) typ)
          [FunD 'C.fromCSVRow [Clause [] (NormalB body) []]]
  pure [decl]


{- | Splice a 'Vector' of header column names for a record type, in
the same order as 'C.toCSVRow' produces them. Useful when writing
a 'CSVDocument' header row alongside the records:

@
let doc = CSVDocument (Just $(csvHeaderFor \''User)) (V.map C.toCSVRow users)
@
-}
csvHeaderFor :: Name -> Q Exp
csvHeaderFor nm = do
  ti <- reifyTypeInfo nm
  fields <- recordFieldsOnly ti
  headers <- mapM headerFor fields
  let nonEmpty = [h | Just h <- headers]
  [|V.fromList $(pure (ListE nonEmpty))|]
  where
    headerFor :: FieldInfo -> Q (Maybe Exp)
    headerFor (FieldInfo mSel _) = do
      selName <- requireSelector mSel
      mi <- reifyModifierInfoFor backendCSV selName
      if miSkip mi
        then pure Nothing
        else do
          let selBase = T.pack (nameBase selName)
          keyExp <- renderWireKey mi selBase
          pure (Just keyExp)


-- ---------------------------------------------------------------------------
-- ToCSV
-- ---------------------------------------------------------------------------

toCSVBody :: TypeInfo -> Q Exp
toCSVBody ti = case typeInfoShape ti of
  TypeShapeRecord c -> toCSVRecord c
  TypeShapeNewtype c -> toCSVNewtype c
  TypeShapeEnum _ ->
    fail "CSV.Derive: enums cannot be derived; CSV has no constructor tag"
  TypeShapeSum _ ->
    fail "CSV.Derive: sum types cannot be derived; CSV has no constructor tag"


toCSVNewtype :: ConInfo -> Q Exp
toCSVNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE
      [varP x]
      [|V.singleton (C.toCSVField ($(varE sel) $(varE x)))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE
      [conP (conInfoName c) [varP x]]
      [|V.singleton (C.toCSVField $(varE x))|]
  _ -> fail "CSV.Derive: newtype must have exactly one field"


toCSVRecord :: ConInfo -> Q Exp
toCSVRecord c = do
  x <- newName "x"
  cellsE <- recordCells (varE x) c
  lamE [varP x] [|V.fromList $(pure cellsE)|]


recordCells :: Q Exp -> ConInfo -> Q Exp
recordCells varExp c = do
  cellExpss <- mapM (toCSVField varExp) (conInfoFields c)
  pure (ListE (concat cellExpss))


toCSVField :: Q Exp -> FieldInfo -> Q [Exp]
toCSVField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendCSV selName
  if miSkip mi
    then pure []
    else do
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|C.toCSVField $getter|]
            Just _ -> [|C.toCSVField (coerce $getter)|]
      cell <- encoded
      pure [cell]


-- ---------------------------------------------------------------------------
-- FromCSV
-- ---------------------------------------------------------------------------

fromCSVBody :: TypeInfo -> Q Exp
fromCSVBody ti = case typeInfoShape ti of
  TypeShapeRecord c -> fromCSVRecord c
  TypeShapeNewtype c -> fromCSVNewtype c
  TypeShapeEnum _ ->
    fail "CSV.Derive: enums cannot be derived; CSV has no constructor tag"
  TypeShapeSum _ ->
    fail "CSV.Derive: sum types cannot be derived; CSV has no constructor tag"


fromCSVNewtype :: ConInfo -> Q Exp
fromCSVNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> do
    cells <- newName "cells"
    lamE
      [varP cells]
      [|
        if V.length $(varE cells) >= 1
          then fmap $(conE (conInfoName c)) (C.fromCSVField ($(varE cells) V.! 0))
          else Left "CSV.Derive: newtype expected at least 1 cell"
        |]
  _ -> fail "CSV.Derive: newtype must have exactly one field"


fromCSVRecord :: ConInfo -> Q Exp
fromCSVRecord c = do
  cells <- newName "cells"
  body <- recordParser cells c
  lamE [varP cells] (pure body)


{- | Build a positional parser: each field is read from the next cell
in turn. Fields with 'skip' don't advance the index and instead
grab their value from 'defaults'.
-}
recordParser :: Name -> ConInfo -> Q Exp
recordParser cells c = do
  let conName = conInfoName c
      fields = conInfoFields c
  case fields of
    [] -> [|Right $(conE conName)|]
    _ -> buildSequence cells conName fields


{- | Walk the field list building a chain of @>>= \\v -> …@. The
positional cell index advances only for non-skipped fields.
-}
buildSequence
  :: Name
  -- ^ cell vector
  -> Name
  -- ^ constructor
  -> [FieldInfo]
  -> Q Exp
buildSequence cells conName = go 0 []
  where
    go :: Int -> [Name] -> [FieldInfo] -> Q Exp
    go _ acc [] = do
      let assemble =
            foldl
              (\e vN -> AppE e (VarE vN))
              (ConE conName)
              (reverse acc)
      [|Right $(pure assemble)|]
    go pos acc (f : fs) = do
      vName <- newName "v"
      (cellExp, advance) <- fieldCell cells pos f
      restExp <- go (pos + advance) (vName : acc) fs
      [|$(pure cellExp) >>= \ $(varP vName) -> $(pure restExp)|]


{- | Produce @(parserExp, advance)@ for a single field. 'advance' is
0 when the field is skipped (no cell consumed) and 1 otherwise.
-}
fieldCell :: Name -> Int -> FieldInfo -> Q (Exp, Int)
fieldCell cells pos (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendCSV selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> do
        e <- [|Right $(varE defNm)|]
        pure (e, 0)
      Nothing ->
        let msg =
              "CSV.Derive: missing 'defaults' for skipped field "
                ++ nameBase selName
        in (\e -> (e, 0)) <$> [|Left $(litE (stringL msg))|]
    else do
      let posLit = litE (integerL (fromIntegral pos))
          base =
            [|
              if V.length $(varE cells) > $posLit
                then C.fromCSVField ($(varE cells) V.! $posLit)
                else
                  Left
                    ( "CSV.Derive: row missing cell at index "
                        ++ show ($posLit :: Int)
                    )
              |]
      e <- case miCoerce mi of
        Nothing -> base
        Just _ -> [|fmap coerce $base|]
      pure (e, 1)


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

recordFieldsOnly :: TypeInfo -> Q [FieldInfo]
recordFieldsOnly ti = case typeInfoShape ti of
  TypeShapeRecord c -> pure (conInfoFields c)
  TypeShapeNewtype c -> pure (conInfoFields c)
  _ -> fail "CSV.Derive: csvHeaderFor only supports record / newtype types"


requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "CSV.Derive: cannot derive CSV for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
