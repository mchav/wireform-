{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for ION
'Ion.Class.ToIon' / 'Ion.Class.FromIon' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — Ion 'IV.Struct' with text-string keys.
* 'TypeShapeEnum'    — Ion 'IV.String' carrying the (possibly
  renamed) constructor name.
* 'TypeShapeSum'     — Ion struct
  @{ \"tag\": \"Ctor\", \"contents\": ... }@.

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'.
-}
module Ion.Derive (
  deriveIon,
  deriveToIon,
  deriveFromIon,
) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Data.Vector qualified as V
import Ion.Class qualified as I
import Ion.Value qualified as IV
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveIon :: Name -> Q [Dec]
deriveIon nm = (++) <$> deriveToIon nm <*> deriveFromIon nm


deriveToIon :: Name -> Q [Dec]
deriveToIon nm = do
  ti <- reifyTypeInfo nm
  body <- toIonBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''I.ToIon) typ)
          [FunD 'I.toIon [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromIon :: Name -> Q [Dec]
deriveFromIon nm = do
  ti <- reifyTypeInfo nm
  body <- fromIonBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''I.FromIon) typ)
          [FunD 'I.fromIon [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToIon
-- ---------------------------------------------------------------------------

toIonBody :: TypeInfo -> Q Exp
toIonBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toIonNewtype c
  TypeShapeRecord c -> toIonRecord c
  TypeShapeEnum cs -> toIonEnum cs
  TypeShapeSum cs -> toIonSum cs


toIonNewtype :: ConInfo -> Q Exp
toIonNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|I.toIon ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|I.toIon $(varE x)|]
  _ -> fail "Ion.Derive: newtype must have exactly one field"


toIonRecord :: ConInfo -> Q Exp
toIonRecord c = do
  x <- newName "x"
  pairs <- recordToIonPairs (varE x) c
  lamE
    [varP x]
    [|IV.Struct (V.fromList $(pure pairs))|]


recordToIonPairs :: Q Exp -> ConInfo -> Q Exp
recordToIonPairs varExp c = do
  pairExpss <- mapM (toIonField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toIonField :: Q Exp -> FieldInfo -> Q [Exp]
toIonField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendION selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|I.toIon $getter|]
            Just _ -> [|I.toIon (coerce $getter)|]
      pair <- [|($(pure keyExp), $encoded)|]
      pure [pair]


toIonEnum :: [ConInfo] -> Q Exp
toIonEnum cs = do
  v <- newName "v"
  matches <- mapM enumToIonMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToIonMatch :: ConInfo -> Q Match
enumToIonMatch c = do
  mi <- reifyModifierInfoFor backendION (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|IV.String $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toIonSum :: [ConInfo] -> Q Exp
toIonSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToIon cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToIon :: ConInfo -> Q Match
sumCtorToIon c = do
  mi <- reifyModifierInfoFor backendION (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|IV.Null|]
    [n] -> [|I.toIon $(varE n)|]
    ns ->
      [|
        IV.List
          ( V.fromList
              $(pure (ListE (map (AppE (VarE 'I.toIon) . VarE) ns)))
          )
        |]
  body <-
    [|
      IV.Struct
        ( V.fromList
            [ (T.pack "tag", IV.String $(pure keyExp))
            , (T.pack "contents", $(pure contentsE))
            ]
        )
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromIon
-- ---------------------------------------------------------------------------

fromIonBody :: TypeInfo -> Q Exp
fromIonBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromIonNewtype c
  TypeShapeRecord c -> fromIonRecord c
  TypeShapeEnum cs -> fromIonEnum cs
  TypeShapeSum cs -> fromIonSum cs


fromIonNewtype :: ConInfo -> Q Exp
fromIonNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . I.fromIon|]
  _ -> fail "Ion.Derive: newtype must have exactly one field"


fromIonRecord :: ConInfo -> Q Exp
fromIonRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'IV.Struct [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "Ion.Derive: expected Struct for record type"|]
            )
            []
        ]
    )


recordParser :: Name -> ConInfo -> Q Exp
recordParser kvs c = do
  let conName = conInfoName c
      fields = conInfoFields c
  case fields of
    [] -> [|Right $(conE conName)|]
    (f0 : fs) -> do
      e0 <- fieldParser kvs f0
      hd <- [|$(conE conName) <$> $(pure e0)|]
      foldlM
        ( \acc f -> do
            ef <- fieldParser kvs f
            [|$(pure acc) <*> $(pure ef)|]
        )
        hd
        fs


fieldParser :: Name -> FieldInfo -> Q Exp
fieldParser kvs (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendION selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "Ion.Derive: missing 'defaults' for skipped field "
                ++ $(litE (stringL (nameBase selName)))
            )
          |]
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then
            [|
              case lookupIonField $(pure keyExp) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (I.fromIon v)
              |]
          else
            [|
              case lookupIonField $(pure keyExp) $(varE kvs) of
                Nothing ->
                  Left
                    ( "Ion.Derive: missing field "
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> I.fromIon v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


-- | Linear scan for a key in an 'IV.Struct'.
lookupIonField :: T.Text -> V.Vector (T.Text, IV.Value) -> Maybe IV.Value
lookupIonField name kvs = V.foldr step Nothing kvs
  where
    step (k, v) acc
      | k == name = Just v
      | otherwise = acc


fromIonEnum :: [ConInfo] -> Q Exp
fromIonEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "Ion.Derive: unknown enum value "))
            )
            (AppE (VarE 'show) (VarE s))
        )
      multi =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
              ++ [(NormalG (ConE 'True), AppE (ConE 'Left) (snd fallback))]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'IV.String [varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "Ion.Derive: enum expected String"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendION (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromIonSum :: [ConInfo] -> Q Exp
fromIonSum cs = do
  v <- newName "v"
  kvs <- newName "kvs"
  tagVar <- newName "tag"
  cVar <- newName "c"
  branches <- mapM (sumDispatch tagVar cVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (ConE 'Left)
            ( AppE
                ( AppE
                    (VarE 'mappend)
                    (LitE (StringL "Ion.Derive: unknown sum tag "))
                )
                (AppE (VarE 'show) (VarE tagVar))
            )
        )
      multi = MultiIfE (branches ++ [fallback])
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'IV.Struct [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupIonField (T.pack "tag") $(varE kvs) of
                        Just (IV.String t) -> Right t
                        _ -> Left "Ion.Derive: sum missing 'tag'"
                    $(varP cVar) <-
                      case lookupIonField (T.pack "contents") $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right IV.Null
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "Ion.Derive: sum expected Struct"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendION (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (I.fromIon $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|I.fromIon ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
  hd <- do
    e0 <- parseI 0
    [|$(conE conName) <$> $(pure e0)|]
  body <-
    foldlM
      ( \acc i -> do
          ei <- parseI i
          [|$(pure acc) <*> $(pure ei)|]
      )
      hd
      [1 .. arity - 1]
  let conNameStr = nameBase conName
  [|
    case $(varE cVar) of
      IV.List $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "Ion.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ show arity
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "Ion.Derive: "
              ++ conNameStr
              ++ " expected List contents"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "Ion.Derive: cannot derive Ion for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
