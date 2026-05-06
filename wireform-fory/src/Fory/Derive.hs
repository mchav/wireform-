{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Annotation-driven Template Haskell deriver for Apache Fory
-- 'Fory.Class.ToFory' / 'Fory.Class.FromFory' instances.
--
-- The deriver consults the same annotation vocabulary as every
-- other per-format deriver in this repo (see
-- @Wireform.Derive.Modifier@). The default rename style for the
-- 'backendFory' backend is @snake_case@, matching the xlang spec\'s
-- requirement that field names be converted to @snake_case@ before
-- being written as a meta-string.
--
-- Encoding shape:
--
-- * 'TypeShapeNewtype' — pass-through to the inner field's
--   'Fory.Class.ToFory' instance.
-- * 'TypeShapeRecord'  — Fory @NAMED_STRUCT@ where the namespace is
--   the type's defining module name and the type name is the
--   constructor name. Record fields become @(meta-string field name,
--   value)@ pairs in source order.
-- * 'TypeShapeEnum'    — Fory 'StringVal' carrying the renamed
--   constructor name.
-- * 'TypeShapeSum'     — Fory @NAMED_STRUCT@ with two fields:
--   @tag@ (the renamed constructor name) and @contents@ (the
--   payload — 'NoneVal' for nullary, the inner value for unary,
--   a 'ListVal' for n-ary).
--
-- Modifiers honoured: 'rename', 'renameStyle', 'renameWith',
-- 'skip', 'defaults', 'optional', 'coerced'.
module Fory.Derive
  ( deriveFory
  , deriveToFory
  , deriveFromFory
  ) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import qualified Data.Text as T
import qualified Data.Vector as V
import Language.Haskell.TH

import qualified Fory.Class as F
import qualified Fory.Value as VV

import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveFory :: Name -> Q [Dec]
deriveFory nm = (++) <$> deriveToFory nm <*> deriveFromFory nm

deriveToFory :: Name -> Q [Dec]
deriveToFory nm = do
  ti   <- reifyTypeInfo nm
  body <- toForyBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''F.ToFory) typ)
              [FunD 'F.toFory [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromFory :: Name -> Q [Dec]
deriveFromFory nm = do
  ti   <- reifyTypeInfo nm
  body <- fromForyBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''F.FromFory) typ)
              [FunD 'F.fromFory [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- Common helpers
-- ---------------------------------------------------------------------------

-- | Splice-time literal: the module the type was declared in.
--
-- The xlang spec uses the type's defining module as the
-- 'namespace' meta string. We resolve that by reifying the type
-- name and pulling 'nameModule' off whatever 'DataD' / 'NewtypeD'
-- comes back. (Note: 'thisModule' would point at the splice site
-- instead of the declaration site, which is wrong when the user
-- splices the deriver from a different module.)
namespaceLitForName :: Name -> Q Exp
namespaceLitForName n = do
  info <- reify n
  let modPart = case info of
        TyConI (DataD _ tn _ _ _ _)    -> nameModule tn
        TyConI (NewtypeD _ tn _ _ _ _) -> nameModule tn
        _                              -> Nothing
      ns = maybe "" T.pack modPart
  [| ns |]

-- ---------------------------------------------------------------------------
-- ToFory
-- ---------------------------------------------------------------------------

toForyBody :: TypeInfo -> Q Exp
toForyBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toForyNewtype c
  TypeShapeRecord  c   -> toForyRecord (typeInfoName ti) c
  TypeShapeEnum    cs  -> toForyEnum cs
  TypeShapeSum     cs  -> toForySum (typeInfoName ti) cs

toForyNewtype :: ConInfo -> Q Exp
toForyNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| F.toFory ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| F.toFory $(varE x) |]
  _ -> fail "Fory.Derive: newtype must have exactly one field"

toForyRecord :: Name -> ConInfo -> Q Exp
toForyRecord tyName c = do
  x <- newName "x"
  pairs <- recordFieldPairs (varE x) c
  nsE   <- namespaceLitForName tyName
  let typeNmLit = LitE (StringL (nameBase (conInfoName c)))
  lamE [varP x]
    [| VV.StructVal $(pure nsE) (T.pack $(pure typeNmLit))
         (V.fromList $(pure pairs)) |]

recordFieldPairs :: Q Exp -> ConInfo -> Q Exp
recordFieldPairs varExp c = do
  pairExpss <- mapM (toForyField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))

toForyField :: Q Exp -> FieldInfo -> Q [Exp]
toForyField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendFory selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [| F.toFory $getter |]
            Just _  -> [| F.toFory (coerce $getter) |]
      pair <- [| ($(pure keyExp), $encoded) |]
      pure [pair]

toForyEnum :: [ConInfo] -> Q Exp
toForyEnum cs = do
  v <- newName "v"
  matches <- mapM enumToForyMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

enumToForyMatch :: ConInfo -> Q Match
enumToForyMatch c = do
  mi <- reifyModifierInfoFor backendFory (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [| VV.StringVal $(pure keyExp) |]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])

toForySum :: Name -> [ConInfo] -> Q Exp
toForySum tyName cs = do
  v <- newName "v"
  nsE <- namespaceLitForName tyName
  let tyNmLit = LitE (StringL (nameBase tyName))
  matches <- mapM (sumCtorToFory (pure nsE) (pure tyNmLit)) cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

sumCtorToFory :: Q Exp -> Q Exp -> ConInfo -> Q Match
sumCtorToFory nsE tyNmE c = do
  mi <- reifyModifierInfoFor backendFory (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  body <- case fieldNames of
    [] ->
      [| VV.StructVal $nsE (T.pack $tyNmE)
           (V.fromList
             [ (T.pack "tag",      VV.StringVal $(pure keyExp))
             , (T.pack "contents", VV.NoneVal)
             ]) |]
    [n] ->
      [| VV.StructVal $nsE (T.pack $tyNmE)
           (V.fromList
             [ (T.pack "tag",      VV.StringVal $(pure keyExp))
             , (T.pack "contents", F.toFory $(varE n))
             ]) |]
    ns ->
      [| VV.StructVal $nsE (T.pack $tyNmE)
           (V.fromList
             [ (T.pack "tag",      VV.StringVal $(pure keyExp))
             , (T.pack "contents",
                 VV.ListVal (V.fromList
                   $(pure (ListE (map (AppE (VarE 'F.toFory) . VarE) ns)))))
             ]) |]
  pure (Match pat (NormalB body) [])

-- ---------------------------------------------------------------------------
-- FromFory
-- ---------------------------------------------------------------------------

fromForyBody :: TypeInfo -> Q Exp
fromForyBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromForyNewtype c
  TypeShapeRecord  c   -> fromForyRecord c
  TypeShapeEnum    cs  -> fromForyEnum cs
  TypeShapeSum     cs  -> fromForySum cs

fromForyNewtype :: ConInfo -> Q Exp
fromForyNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . F.fromFory |]
  _               -> fail "Fory.Derive: newtype must have exactly one field"

fromForyRecord :: ConInfo -> Q Exp
fromForyRecord c = do
  v   <- newName "v"
  fields <- newName "fields"
  bodyE <- recordParser fields c
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'VV.StructVal [wildP, wildP, varP fields])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "Fory.Derive: expected NamedStruct for record type" |])
               []
       ])

recordParser :: Name -> ConInfo -> Q Exp
recordParser fields c = do
  let conName = conInfoName c
      fs      = conInfoFields c
  case fs of
    []        -> [| Right $(conE conName) |]
    (f0 : rs) -> do
      e0 <- fieldParser fields f0
      hd <- [| $(conE conName) <$> $(pure e0) |]
      foldlM
        (\acc f -> do
            ef <- fieldParser fields f
            [| $(pure acc) <*> $(pure ef) |])
        hd
        rs

fieldParser :: Name -> FieldInfo -> Q Exp
fieldParser fields (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendFory selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [| Right $(varE defNm) |]
      Nothing    -> [| Left ("Fory.Derive: missing 'defaults' for skipped field "
                              ++ $(litE (stringL (nameBase selName)))) |]
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then [| case lookupForyField $(pure keyExp) $(varE fields) of
                    Nothing -> Right Nothing
                    Just  v -> fmap Just (F.fromFory v) |]
          else [| case lookupForyField $(pure keyExp) $(varE fields) of
                    Nothing -> Left ("Fory.Derive: missing field "
                                     ++ T.unpack $(pure keyExp))
                    Just v  -> F.fromFory v |]
      case miCoerce mi of
        Nothing -> pure base
        Just _  -> [| fmap coerce $(pure base) |]

-- | Linear scan for a field name in a struct's field list. Lifted
-- to the module level so generated code can reference it without
-- splice-time inlining games.
lookupForyField :: T.Text -> V.Vector (T.Text, VV.Value) -> Maybe VV.Value
lookupForyField name fields = V.foldr step Nothing fields
  where
    step (k, v) acc | k == name = Just v
                    | otherwise = acc

fromForyEnum :: [ConInfo] -> Q Exp
fromForyEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (AppE (VarE 'mappend)
                  (LitE (StringL "Fory.Derive: unknown enum value ")))
               (AppE (VarE 'show) (VarE s))
        )
      multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'VV.StringVal [varP s])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "Fory.Derive: enum expected String" |])
               []
       ])

enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendFory (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp  = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)

fromForySum :: [ConInfo] -> Q Exp
fromForySum cs = do
  v       <- newName "v"
  fields  <- newName "fields"
  tagVar  <- newName "tag"
  cVar    <- newName "c"
  branches <- mapM (sumDispatch tagVar cVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "Fory.Derive: unknown sum tag ")))
                  (AppE (VarE 'show) (VarE tagVar)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'VV.StructVal [wildP, wildP, varP fields])
               (normalB
                  [| do
                       $(varP tagVar) <-
                         case lookupForyField (T.pack "tag") $(varE fields) of
                           Just (VV.StringVal t) -> Right t
                           _ -> Left "Fory.Derive: sum missing 'tag'"
                       $(varP cVar) <-
                         case lookupForyField (T.pack "contents") $(varE fields) of
                           Just x  -> Right x
                           Nothing -> Right VV.NoneVal
                       $(pure multi)
                  |])
               []
       , match wildP
               (normalB [| Left "Fory.Derive: sum expected NamedStruct" |])
               []
       ])

sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendFory (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    []     -> [| Right $(conE (conInfoName c)) |]
    [_one] -> [| fmap $(conE (conInfoName c)) (F.fromFory $(varE cVar)) |]
    many   -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)

sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [| F.fromFory ($(varE arr) V.! $(litE (integerL (fromIntegral i)))) |]
  hd <- do
    e0 <- parseI 0
    [| $(conE conName) <$> $(pure e0) |]
  body <- foldlM
    (\acc i -> do
        ei <- parseI i
        [| $(pure acc) <*> $(pure ei) |])
    hd
    [1 .. arity - 1]
  let conNameStr = nameBase conName
      arityStr   = show arity
  [| case $(varE cVar) of
       VV.ListVal $(varP arr)
         | V.length $(varE arr) == $(litE (integerL (fromIntegral arity)))
             -> $(pure body)
         | otherwise
             -> Left ("Fory.Derive: " ++ conNameStr
                      ++ " expected " ++ arityStr ++ " contents, got "
                      ++ show (V.length $(varE arr)))
       _ -> Left ("Fory.Derive: " ++ conNameStr
                  ++ " expected ListVal contents")
   |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "Fory.Derive: cannot derive Fory for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
