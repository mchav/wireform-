{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for BSON
'BSON.Class.ToBSON' / 'BSON.Class.FromBSON' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — BSON 'BV.Document' with text-string keys.
* 'TypeShapeEnum'    — BSON 'BV.String' carrying the (possibly
  renamed) constructor name.
* 'TypeShapeSum'     — BSON document
  @{ \"tag\": \"Ctor\", \"contents\": ... }@.

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'.
-}
module BSON.Derive (
  deriveBSON,
  deriveToBSON,
  deriveFromBSON,
) where

import BSON.Class qualified as B
import BSON.Value qualified as BV
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveBSON :: Name -> Q [Dec]
deriveBSON nm = (++) <$> deriveToBSON nm <*> deriveFromBSON nm


deriveToBSON :: Name -> Q [Dec]
deriveToBSON nm = do
  ti <- reifyTypeInfo nm
  body <- toBSONBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''B.ToBSON) typ)
          [FunD 'B.toBSON [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromBSON :: Name -> Q [Dec]
deriveFromBSON nm = do
  ti <- reifyTypeInfo nm
  body <- fromBSONBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''B.FromBSON) typ)
          [FunD 'B.fromBSON [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToBSON
-- ---------------------------------------------------------------------------

toBSONBody :: TypeInfo -> Q Exp
toBSONBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toBSONNewtype c
  TypeShapeRecord c -> toBSONRecord c
  TypeShapeEnum cs -> toBSONEnum cs
  TypeShapeSum cs -> toBSONSum cs


toBSONNewtype :: ConInfo -> Q Exp
toBSONNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|B.toBSON ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|B.toBSON $(varE x)|]
  _ -> fail "BSON.Derive: newtype must have exactly one field"


toBSONRecord :: ConInfo -> Q Exp
toBSONRecord c = do
  x <- newName "x"
  pairs <- recordToBSONPairs (varE x) c
  lamE
    [varP x]
    [|BV.Document (V.fromList $(pure pairs))|]


recordToBSONPairs :: Q Exp -> ConInfo -> Q Exp
recordToBSONPairs varExp c = do
  pairExpss <- mapM (toBSONField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toBSONField :: Q Exp -> FieldInfo -> Q [Exp]
toBSONField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendBSON selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|B.toBSON $getter|]
            Just _ -> [|B.toBSON (coerce $getter)|]
      pair <- [|($(pure keyExp), $encoded)|]
      pure [pair]


toBSONEnum :: [ConInfo] -> Q Exp
toBSONEnum cs = do
  v <- newName "v"
  matches <- mapM enumToBSONMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToBSONMatch :: ConInfo -> Q Match
enumToBSONMatch c = do
  mi <- reifyModifierInfoFor backendBSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|BV.String $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toBSONSum :: [ConInfo] -> Q Exp
toBSONSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToBSON cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToBSON :: ConInfo -> Q Match
sumCtorToBSON c = do
  mi <- reifyModifierInfoFor backendBSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|BV.Null|]
    [n] -> [|B.toBSON $(varE n)|]
    ns ->
      [|
        BV.Array
          ( V.fromList
              $(pure (ListE (map (AppE (VarE 'B.toBSON) . VarE) ns)))
          )
        |]
  body <-
    [|
      BV.Document
        ( V.fromList
            [ (T.pack "tag", BV.String $(pure keyExp))
            , (T.pack "contents", $(pure contentsE))
            ]
        )
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromBSON
-- ---------------------------------------------------------------------------

fromBSONBody :: TypeInfo -> Q Exp
fromBSONBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromBSONNewtype c
  TypeShapeRecord c -> fromBSONRecord c
  TypeShapeEnum cs -> fromBSONEnum cs
  TypeShapeSum cs -> fromBSONSum cs


fromBSONNewtype :: ConInfo -> Q Exp
fromBSONNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . B.fromBSON|]
  _ -> fail "BSON.Derive: newtype must have exactly one field"


fromBSONRecord :: ConInfo -> Q Exp
fromBSONRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'BV.Document [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "BSON.Derive: expected Document for record type"|]
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
  mi <- reifyModifierInfoFor backendBSON selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "BSON.Derive: missing 'defaults' for skipped field "
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
              case lookupBSONField $(pure keyExp) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (B.fromBSON v)
              |]
          else
            [|
              case lookupBSONField $(pure keyExp) $(varE kvs) of
                Nothing ->
                  Left
                    ( "BSON.Derive: missing field "
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> B.fromBSON v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


-- | Linear scan for a key in a 'BV.Document'.
lookupBSONField :: T.Text -> V.Vector (T.Text, BV.Value) -> Maybe BV.Value
lookupBSONField name kvs = V.foldr step Nothing kvs
  where
    step (k, v) acc
      | k == name = Just v
      | otherwise = acc


fromBSONEnum :: [ConInfo] -> Q Exp
fromBSONEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "BSON.Derive: unknown enum value "))
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
            (conP 'BV.String [varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "BSON.Derive: enum expected String"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendBSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromBSONSum :: [ConInfo] -> Q Exp
fromBSONSum cs = do
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
                    (LitE (StringL "BSON.Derive: unknown sum tag "))
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
            (conP 'BV.Document [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupBSONField (T.pack "tag") $(varE kvs) of
                        Just (BV.String t) -> Right t
                        _ -> Left "BSON.Derive: sum missing 'tag'"
                    $(varP cVar) <-
                      case lookupBSONField (T.pack "contents") $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right BV.Null
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "BSON.Derive: sum expected Document"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendBSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (B.fromBSON $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|B.fromBSON ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
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
      BV.Array $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "BSON.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ show arity
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "BSON.Derive: "
              ++ conNameStr
              ++ " expected Array contents"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "BSON.Derive: cannot derive BSON for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
