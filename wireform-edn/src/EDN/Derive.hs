{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for EDN
'EDN.Class.ToEDN' / 'EDN.Class.FromEDN' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — EDN 'EV.Map' with @Keyword Nothing key@ keys.
* 'TypeShapeEnum'    — EDN 'EV.Keyword' carrying the (possibly
  renamed) constructor name as an unqualified keyword.
* 'TypeShapeSum'     — EDN map
  @{ :tag :ctor, :contents ... }@ where both the @tag@\/@contents@
  keys and the tag value are keywords.

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'.
-}
module EDN.Derive (
  deriveEDN,
  deriveToEDN,
  deriveFromEDN,
) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Data.Vector qualified as V
import EDN.Class qualified as E
import EDN.Value qualified as EV
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveEDN :: Name -> Q [Dec]
deriveEDN nm = (++) <$> deriveToEDN nm <*> deriveFromEDN nm


deriveToEDN :: Name -> Q [Dec]
deriveToEDN nm = do
  ti <- reifyTypeInfo nm
  body <- toEDNBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''E.ToEDN) typ)
          [FunD 'E.toEDN [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromEDN :: Name -> Q [Dec]
deriveFromEDN nm = do
  ti <- reifyTypeInfo nm
  body <- fromEDNBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''E.FromEDN) typ)
          [FunD 'E.fromEDN [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToEDN
-- ---------------------------------------------------------------------------

toEDNBody :: TypeInfo -> Q Exp
toEDNBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toEDNNewtype c
  TypeShapeRecord c -> toEDNRecord c
  TypeShapeEnum cs -> toEDNEnum cs
  TypeShapeSum cs -> toEDNSum cs


toEDNNewtype :: ConInfo -> Q Exp
toEDNNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|E.toEDN ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|E.toEDN $(varE x)|]
  _ -> fail "EDN.Derive: newtype must have exactly one field"


toEDNRecord :: ConInfo -> Q Exp
toEDNRecord c = do
  x <- newName "x"
  pairs <- recordToEDNPairs (varE x) c
  lamE
    [varP x]
    [|EV.Map (V.fromList $(pure pairs))|]


recordToEDNPairs :: Q Exp -> ConInfo -> Q Exp
recordToEDNPairs varExp c = do
  pairExpss <- mapM (toEDNField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toEDNField :: Q Exp -> FieldInfo -> Q [Exp]
toEDNField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendEDN selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|E.toEDN $getter|]
            Just _ -> [|E.toEDN (coerce $getter)|]
      pair <- [|(EV.Keyword Nothing $(pure keyExp), $encoded)|]
      pure [pair]


toEDNEnum :: [ConInfo] -> Q Exp
toEDNEnum cs = do
  v <- newName "v"
  matches <- mapM enumToEDNMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToEDNMatch :: ConInfo -> Q Match
enumToEDNMatch c = do
  mi <- reifyModifierInfoFor backendEDN (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|EV.Keyword Nothing $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toEDNSum :: [ConInfo] -> Q Exp
toEDNSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToEDN cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToEDN :: ConInfo -> Q Match
sumCtorToEDN c = do
  mi <- reifyModifierInfoFor backendEDN (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|EV.Nil|]
    [n] -> [|E.toEDN $(varE n)|]
    ns ->
      [|
        EV.Vector
          ( V.fromList
              $(pure (ListE (map (AppE (VarE 'E.toEDN) . VarE) ns)))
          )
        |]
  body <-
    [|
      EV.Map
        ( V.fromList
            [
              ( EV.Keyword Nothing (T.pack "tag")
              , EV.Keyword Nothing $(pure keyExp)
              )
            ,
              ( EV.Keyword Nothing (T.pack "contents")
              , $(pure contentsE)
              )
            ]
        )
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromEDN
-- ---------------------------------------------------------------------------

fromEDNBody :: TypeInfo -> Q Exp
fromEDNBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromEDNNewtype c
  TypeShapeRecord c -> fromEDNRecord c
  TypeShapeEnum cs -> fromEDNEnum cs
  TypeShapeSum cs -> fromEDNSum cs


fromEDNNewtype :: ConInfo -> Q Exp
fromEDNNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . E.fromEDN|]
  _ -> fail "EDN.Derive: newtype must have exactly one field"


fromEDNRecord :: ConInfo -> Q Exp
fromEDNRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'EV.Map [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "EDN.Derive: expected Map for record type"|]
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
  mi <- reifyModifierInfoFor backendEDN selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "EDN.Derive: missing 'defaults' for skipped field "
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
              case lookupEDNField
                (EV.Keyword Nothing $(pure keyExp))
                $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (E.fromEDN v)
              |]
          else
            [|
              case lookupEDNField
                (EV.Keyword Nothing $(pure keyExp))
                $(varE kvs) of
                Nothing ->
                  Left
                    ( "EDN.Derive: missing field :"
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> E.fromEDN v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


{- | Linear scan for a key in an EDN map. Keys are full 'EV.Value's
because EDN allows arbitrary values as map keys (typically a
@Keyword Nothing "field"@ for record fields).
-}
lookupEDNField :: EV.Value -> V.Vector (EV.Value, EV.Value) -> Maybe EV.Value
lookupEDNField name kvs = V.foldr step Nothing kvs
  where
    step (k, v) acc
      | k == name = Just v
      | otherwise = acc


fromEDNEnum :: [ConInfo] -> Q Exp
fromEDNEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "EDN.Derive: unknown enum keyword "))
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
            (conP 'EV.Keyword [wildP, varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "EDN.Derive: enum expected Keyword"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendEDN (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromEDNSum :: [ConInfo] -> Q Exp
fromEDNSum cs = do
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
                    (LitE (StringL "EDN.Derive: unknown sum tag "))
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
            (conP 'EV.Map [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupEDNField
                        (EV.Keyword Nothing (T.pack "tag"))
                        $(varE kvs) of
                        Just (EV.Keyword _ t) -> Right t
                        _ -> Left "EDN.Derive: sum missing 'tag' keyword"
                    $(varP cVar) <-
                      case lookupEDNField
                        (EV.Keyword Nothing (T.pack "contents"))
                        $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right EV.Nil
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "EDN.Derive: sum expected Map"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendEDN (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (E.fromEDN $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|E.fromEDN ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
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
      EV.Vector $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "EDN.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ show arity
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "EDN.Derive: "
              ++ conNameStr
              ++ " expected Vector contents"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "EDN.Derive: cannot derive EDN for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
