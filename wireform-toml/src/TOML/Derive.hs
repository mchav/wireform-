{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Annotation-driven Template Haskell deriver for TOML
-- 'TOML.Class.ToTOML' / 'TOML.Class.FromTOML' instances.
--
-- Encoding shape:
--
-- * 'TypeShapeNewtype' — pass-through to the inner field's instance.
-- * 'TypeShapeRecord'  — TOML 'TV.TTable' with text-string keys.
-- * 'TypeShapeEnum'    — TOML 'TV.TString' carrying the (possibly
--   renamed) constructor name.
-- * 'TypeShapeSum'     — TOML table @{ \"tag\": \"Ctor\", \"contents\": ... }@.
--   TOML has no @Null@, so nullary constructors omit the @contents@ key
--   entirely and the decoder treats an absent @contents@ as the nullary
--   case.
--
-- Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
-- 'defaults', 'optional', 'coerced'.
module TOML.Derive
  ( deriveTOML
  , deriveToTOML
  , deriveFromTOML
  ) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Language.Haskell.TH

import qualified TOML.Class as T
import qualified TOML.Value as TV

import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveTOML :: Name -> Q [Dec]
deriveTOML nm = (++) <$> deriveToTOML nm <*> deriveFromTOML nm

deriveToTOML :: Name -> Q [Dec]
deriveToTOML nm = do
  ti   <- reifyTypeInfo nm
  body <- toTOMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''T.ToTOML) typ)
              [FunD 'T.toTOML [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromTOML :: Name -> Q [Dec]
deriveFromTOML nm = do
  ti   <- reifyTypeInfo nm
  body <- fromTOMLBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''T.FromTOML) typ)
              [FunD 'T.fromTOML [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- ToTOML
-- ---------------------------------------------------------------------------

toTOMLBody :: TypeInfo -> Q Exp
toTOMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toTOMLNewtype c
  TypeShapeRecord  c   -> toTOMLRecord  c
  TypeShapeEnum    cs  -> toTOMLEnum    cs
  TypeShapeSum     cs  -> toTOMLSum     cs

toTOMLNewtype :: ConInfo -> Q Exp
toTOMLNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| T.toTOML ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| T.toTOML $(varE x) |]
  _ -> fail "TOML.Derive: newtype must have exactly one field"

toTOMLRecord :: ConInfo -> Q Exp
toTOMLRecord c = do
  x <- newName "x"
  pairs <- recordToTOMLPairs (varE x) c
  lamE [varP x]
    [| TV.TTable (V.fromList $(pure pairs)) |]

recordToTOMLPairs :: Q Exp -> ConInfo -> Q Exp
recordToTOMLPairs varExp c = do
  pairExpss <- mapM (toTOMLField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))

toTOMLField :: Q Exp -> FieldInfo -> Q [Exp]
toTOMLField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendTOML selName
  if miSkip mi
    then pure []
    else do
      let selBase = Text.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [| T.toTOML $getter |]
            Just _  -> [| T.toTOML (coerce $getter) |]
      pair <- [| ($(pure keyExp), $encoded) |]
      pure [pair]

toTOMLEnum :: [ConInfo] -> Q Exp
toTOMLEnum cs = do
  v <- newName "v"
  matches <- mapM enumToTOMLMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

enumToTOMLMatch :: ConInfo -> Q Match
enumToTOMLMatch c = do
  mi <- reifyModifierInfoFor backendTOML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  body <- [| TV.TString $(pure keyExp) |]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])

toTOMLSum :: [ConInfo] -> Q Exp
toTOMLSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToTOML cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)

sumCtorToTOML :: ConInfo -> Q Match
sumCtorToTOML c = do
  mi <- reifyModifierInfoFor backendTOML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  body <- case fieldNames of
    []   ->
      [| TV.TTable (V.fromList
            [ (Text.pack "tag", TV.TString $(pure keyExp))
            ]) |]
    [n]  ->
      [| TV.TTable (V.fromList
            [ (Text.pack "tag",      TV.TString $(pure keyExp))
            , (Text.pack "contents", T.toTOML $(varE n))
            ]) |]
    ns   ->
      [| TV.TTable (V.fromList
            [ (Text.pack "tag",      TV.TString $(pure keyExp))
            , (Text.pack "contents",
                TV.TArray (V.fromList
                  $(pure (ListE (map (AppE (VarE 'T.toTOML) . VarE) ns)))))
            ]) |]
  pure (Match pat (NormalB body) [])

-- ---------------------------------------------------------------------------
-- FromTOML
-- ---------------------------------------------------------------------------

fromTOMLBody :: TypeInfo -> Q Exp
fromTOMLBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromTOMLNewtype c
  TypeShapeRecord  c   -> fromTOMLRecord c
  TypeShapeEnum    cs  -> fromTOMLEnum cs
  TypeShapeSum     cs  -> fromTOMLSum  cs

fromTOMLNewtype :: ConInfo -> Q Exp
fromTOMLNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . T.fromTOML |]
  _               -> fail "TOML.Derive: newtype must have exactly one field"

fromTOMLRecord :: ConInfo -> Q Exp
fromTOMLRecord c = do
  v   <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.TTable [varP kvs])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "TOML.Derive: expected TTable for record type" |])
               []
       ])

recordParser :: Name -> ConInfo -> Q Exp
recordParser kvs c = do
  let conName = conInfoName c
      fields  = conInfoFields c
  case fields of
    []        -> [| Right $(conE conName) |]
    (f0 : fs) -> do
      e0 <- fieldParser kvs f0
      hd <- [| $(conE conName) <$> $(pure e0) |]
      foldlM
        (\acc f -> do
            ef <- fieldParser kvs f
            [| $(pure acc) <*> $(pure ef) |])
        hd
        fs

fieldParser :: Name -> FieldInfo -> Q Exp
fieldParser kvs (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendTOML selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [| Right $(varE defNm) |]
      Nothing    -> [| Left ("TOML.Derive: missing 'defaults' for skipped field " ++
                              $(litE (stringL (nameBase selName)))) |]
    else do
      let selBase = Text.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then [| case lookupTOMLField $(pure keyExp) $(varE kvs) of
                    Nothing -> Right Nothing
                    Just  v -> fmap Just (T.fromTOML v) |]
          else [| case lookupTOMLField $(pure keyExp) $(varE kvs) of
                    Nothing -> Left ("TOML.Derive: missing field "
                                     ++ Text.unpack $(pure keyExp))
                    Just v  -> T.fromTOML v |]
      case miCoerce mi of
        Nothing -> pure base
        Just _  -> [| fmap coerce $(pure base) |]

-- | Linear scan for a key in a 'TV.TTable'.
lookupTOMLField :: Text.Text -> V.Vector (Text.Text, TV.Value) -> Maybe TV.Value
lookupTOMLField name kvs = V.foldr step Nothing kvs
  where
    step (k, v) acc | k == name = Just v
                    | otherwise = acc

fromTOMLEnum :: [ConInfo] -> Q Exp
fromTOMLEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (AppE (VarE 'mappend)
                  (LitE (StringL "TOML.Derive: unknown enum value ")))
               (AppE (VarE 'show) (VarE s))
        )
      multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.TString [varP s])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "TOML.Derive: enum expected TString" |])
               []
       ])

enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendTOML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp  = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)

fromTOMLSum :: [ConInfo] -> Q Exp
fromTOMLSum cs = do
  v       <- newName "v"
  kvs     <- newName "kvs"
  tagVar  <- newName "tag"
  cVar    <- newName "c"
  branches <- mapM (sumDispatch tagVar cVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE (ConE 'Left)
            (AppE (AppE (VarE 'mappend)
                      (LitE (StringL "TOML.Derive: unknown sum tag ")))
                  (AppE (VarE 'show) (VarE tagVar)))
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.TTable [varP kvs])
               (normalB
                  [| do
                       $(varP tagVar) <-
                         case lookupTOMLField (Text.pack "tag") $(varE kvs) of
                           Just (TV.TString t) -> Right t
                           _ -> Left "TOML.Derive: sum missing 'tag'"
                       let $(varP cVar) =
                             lookupTOMLField (Text.pack "contents") $(varE kvs)
                       $(pure multi)
                  |])
               []
       , match wildP
               (normalB [| Left "TOML.Derive: sum expected TTable" |])
               []
       ])

sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendTOML (conInfoName c)
  keyExp <- renderWireKey mi (Text.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    []     ->
      let conNameStr = nameBase (conInfoName c)
      in [| case $(varE cVar) of
              Nothing -> Right $(conE (conInfoName c))
              Just _  -> Left ("TOML.Derive: " ++ conNameStr
                               ++ " is nullary; unexpected 'contents'")
         |]
    [_one] ->
      let conNameStr = nameBase (conInfoName c)
      in [| case $(varE cVar) of
              Just inner -> fmap $(conE (conInfoName c)) (T.fromTOML inner)
              Nothing    -> Left ("TOML.Derive: " ++ conNameStr
                                  ++ " missing 'contents'")
         |]
    many   -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)

sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [| T.fromTOML ($(varE arr) V.! $(litE (integerL (fromIntegral i)))) |]
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
  [| case $(varE cVar) of
       Just (TV.TArray $(varP arr))
         | V.length $(varE arr) == $(litE (integerL (fromIntegral arity)))
             -> $(pure body)
         | otherwise
             -> Left ("TOML.Derive: " ++ conNameStr
                      ++ " expected " ++ show arity ++ " contents, got "
                      ++ show (V.length $(varE arr)))
       Just _  -> Left ("TOML.Derive: " ++ conNameStr
                        ++ " expected TArray contents")
       Nothing -> Left ("TOML.Derive: " ++ conNameStr
                        ++ " missing 'contents'")
   |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "TOML.Derive: cannot derive TOML for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
