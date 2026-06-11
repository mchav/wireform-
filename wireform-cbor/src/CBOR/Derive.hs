{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for CBOR
'CBOR.Class.ToCBOR' / 'CBOR.Class.FromCBOR' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — CBOR map with text-string keys.
* 'TypeShapeEnum'    — CBOR text-string of the (possibly renamed)
  constructor name.
* 'TypeShapeSum'     — CBOR map @{ "tag": "Ctor", "contents": ... }@,
  matching the wireform JSON / proto3 conventions for portability.

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'. Modifiers irrelevant to CBOR
(e.g. 'tag' for proto field numbers) are silently ignored.
-}
module CBOR.Derive (
  deriveCBOR,
  deriveToCBOR,
  deriveFromCBOR,
) where

import CBOR.Class qualified as C
import CBOR.Value qualified as CV
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

-- | Derive both 'C.ToCBOR' and 'C.FromCBOR' for a type.
deriveCBOR :: Name -> Q [Dec]
deriveCBOR nm = (++) <$> deriveToCBOR nm <*> deriveFromCBOR nm


-- | Derive only 'C.ToCBOR'.
deriveToCBOR :: Name -> Q [Dec]
deriveToCBOR nm = do
  ti <- reifyTypeInfo nm
  body <- toCBORBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''C.ToCBOR) typ)
          [FunD 'C.toCBOR [Clause [] (NormalB body) []]]
  pure [decl]


-- | Derive only 'C.FromCBOR'.
deriveFromCBOR :: Name -> Q [Dec]
deriveFromCBOR nm = do
  ti <- reifyTypeInfo nm
  body <- fromCBORBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''C.FromCBOR) typ)
          [FunD 'C.fromCBOR [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToCBOR
-- ---------------------------------------------------------------------------

toCBORBody :: TypeInfo -> Q Exp
toCBORBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toCBORNewtype c
  TypeShapeRecord c -> toCBORRecord c
  TypeShapeEnum cs -> toCBOREnum cs
  TypeShapeSum cs -> toCBORSum cs


toCBORNewtype :: ConInfo -> Q Exp
toCBORNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|C.toCBOR ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|C.toCBOR $(varE x)|]
  _ -> fail "CBOR.Derive: newtype must have exactly one field"


toCBORRecord :: ConInfo -> Q Exp
toCBORRecord c = do
  x <- newName "x"
  pairs <- recordToCBORPairs (varE x) c
  lamE
    [varP x]
    [|CV.Map (V.fromList $(pure pairs))|]


recordToCBORPairs :: Q Exp -> ConInfo -> Q Exp
recordToCBORPairs varExp c = do
  pairExpss <- mapM (toCBORField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toCBORField :: Q Exp -> FieldInfo -> Q [Exp]
toCBORField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendCBOR selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|C.toCBOR $getter|]
            Just _ -> [|C.toCBOR (coerce $getter)|]
      pair <- [|(CV.TextString $(pure keyExp), $encoded)|]
      pure [pair]


toCBOREnum :: [ConInfo] -> Q Exp
toCBOREnum cs = do
  v <- newName "v"
  matches <- mapM enumToCBORMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToCBORMatch :: ConInfo -> Q Match
enumToCBORMatch c = do
  mi <- reifyModifierInfoFor backendCBOR (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|CV.TextString $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toCBORSum :: [ConInfo] -> Q Exp
toCBORSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToCBOR cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToCBOR :: ConInfo -> Q Match
sumCtorToCBOR c = do
  mi <- reifyModifierInfoFor backendCBOR (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|CV.Null|]
    [n] -> [|C.toCBOR $(varE n)|]
    ns ->
      [|
        CV.Array
          ( V.fromList
              $(pure (ListE (map (AppE (VarE 'C.toCBOR) . VarE) ns)))
          )
        |]
  body <-
    [|
      CV.Map
        ( V.fromList
            [
              ( CV.TextString (T.pack "tag")
              , CV.TextString $(pure keyExp)
              )
            ,
              ( CV.TextString (T.pack "contents")
              , $(pure contentsE)
              )
            ]
        )
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromCBOR
-- ---------------------------------------------------------------------------

fromCBORBody :: TypeInfo -> Q Exp
fromCBORBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromCBORNewtype c
  TypeShapeRecord c -> fromCBORRecord c
  TypeShapeEnum cs -> fromCBOREnum cs
  TypeShapeSum cs -> fromCBORSum cs


fromCBORNewtype :: ConInfo -> Q Exp
fromCBORNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . C.fromCBOR|]
  _ -> fail "CBOR.Derive: newtype must have exactly one field"


fromCBORRecord :: ConInfo -> Q Exp
fromCBORRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'CV.Map [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "CBOR.Derive: expected Map for record type"|]
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
  mi <- reifyModifierInfoFor backendCBOR selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "CBOR.Derive: missing 'defaults' for skipped field "
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
              case lookupCBORField $(pure keyExp) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (C.fromCBOR v)
              |]
          else
            [|
              case lookupCBORField $(pure keyExp) $(varE kvs) of
                Nothing ->
                  Left
                    ( "CBOR.Derive: missing field "
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> C.fromCBOR v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


{- | Linear scan for a key in the map. Inlined helper exposed at the
module level so generated code can reference it.
-}
lookupCBORField :: T.Text -> V.Vector (CV.Value, CV.Value) -> Maybe CV.Value
lookupCBORField name kvs = V.foldr step Nothing kvs
  where
    step (CV.TextString k, v) acc
      | k == name = Just v
      | otherwise = acc
    step _ acc = acc


fromCBOREnum :: [ConInfo] -> Q Exp
fromCBOREnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "CBOR.Derive: unknown enum value "))
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
            (conP 'CV.TextString [varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "CBOR.Derive: enum expected TextString"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendCBOR (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromCBORSum :: [ConInfo] -> Q Exp
fromCBORSum cs = do
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
                    (LitE (StringL "CBOR.Derive: unknown sum tag "))
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
            (conP 'CV.Map [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupCBORField (T.pack "tag") $(varE kvs) of
                        Just (CV.TextString t) -> Right t
                        _ -> Left "CBOR.Derive: sum missing 'tag'"
                    $(varP cVar) <-
                      case lookupCBORField (T.pack "contents") $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right CV.Null
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "CBOR.Derive: sum expected Map"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendCBOR (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (C.fromCBOR $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|C.fromCBOR ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
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
      CV.Array $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "CBOR.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ show arity
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "CBOR.Derive: "
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
  fail "CBOR.Derive: cannot derive CBOR for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
