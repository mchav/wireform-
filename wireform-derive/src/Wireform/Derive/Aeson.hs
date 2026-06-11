{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Aeson 'ToJSON' / 'FromJSON' deriver.

Adapted from riz0id's @aeson-th@ but reimplemented on top of the
'Wireform.Derive' core so a single set of @ANN@ pragmas can drive
every wireform-supported format simultaneously. Riz0id's original
left sum-of-products unfinished; this deriver implements them as
aeson's @TaggedObject@ shape (the proto3-JSON-friendly default).

The deriver covers four type shapes:

* 'TypeShapeNewtype' — the inner field's instance is reused.
* 'TypeShapeRecord'  — encoded as @{ "k1": v1, ... }@.
* 'TypeShapeEnum'    — encoded as a JSON string of the constructor
  name (with rename modifiers applied).
* 'TypeShapeSum'     — encoded as @{ "tag": "Ctor", "contents": ... }@.
  When a constructor has zero fields, @contents@ is 'Aeson.Null';
  one field, the field's JSON; multiple fields, a JSON array.

== Quick start

@
data Person = Person
  { personName :: !Text
  , personAge  :: !Int
  } deriving (Eq, Show)

{\-\# ANN type Person   (rename "person")           \#-\}
{\-\# ANN personName    (rename "name")             \#-\}
{\-\# ANN personAge     (renameStyle SnakeCase)     \#-\}

'deriveJSON' ''Person
@
-}
module Wireform.Derive.Aeson (
  deriveJSON,
  deriveToJSON,
  deriveFromJSON,
) where

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive both 'ToJSON' and 'FromJSON' for the given type.
deriveJSON :: Name -> Q [Dec]
deriveJSON nm = (++) <$> deriveToJSON nm <*> deriveFromJSON nm


-- | Derive only 'ToJSON'.
deriveToJSON :: Name -> Q [Dec]
deriveToJSON nm = do
  ti <- reifyTypeInfo nm
  body <- toJSONBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''ToJSON) typ)
          [FunD 'toJSON [Clause [] (NormalB body) []]]
  pure [decl]


-- | Derive only 'FromJSON'.
deriveFromJSON :: Name -> Q [Dec]
deriveFromJSON nm = do
  ti <- reifyTypeInfo nm
  body <- fromJSONBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FromJSON) typ)
          [FunD 'parseJSON [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToJSON dispatch
-- ---------------------------------------------------------------------------

toJSONBody :: TypeInfo -> Q Exp
toJSONBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toJSONNewtype c
  TypeShapeRecord c -> toJSONRecord c
  TypeShapeEnum cs -> toJSONEnum cs
  TypeShapeSum cs -> toJSONSum cs


toJSONNewtype :: ConInfo -> Q Exp
toJSONNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|toJSON ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|toJSON $(varE x)|]
  _ -> fail "Wireform.Derive.Aeson: newtype must have exactly one field"


toJSONRecord :: ConInfo -> Q Exp
toJSONRecord c = do
  x <- newName "x"
  pairsExp <- recordToJSONPairs (varE x) c
  lamE [varP x] [|Aeson.object $(pure pairsExp)|]


recordToJSONPairs :: Q Exp -> ConInfo -> Q Exp
recordToJSONPairs varExp c = do
  pairExpss <- mapM (toJSONField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toJSONField :: Q Exp -> FieldInfo -> Q [Exp]
toJSONField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendJSON selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|toJSON $getter|]
            Just _ -> [|toJSON (coerce $getter)|]
      pair <- [|(Aeson.Key.fromText $(pure keyExp) .= ($encoded :: Aeson.Value))|]
      pure [pair]


{- | Enum: encode as a JSON string. Each constructor's wire key is
baked at splice time.
-}
toJSONEnum :: [ConInfo] -> Q Exp
toJSONEnum cs = do
  v <- newName "v"
  matches <- mapM enumToJSONMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToJSONMatch :: ConInfo -> Q Match
enumToJSONMatch c = do
  mi <- reifyModifierInfoFor backendJSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|toJSON ($(pure keyExp) :: T.Text)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


{- | Sum: tagged-object encoding with @tag@ + @contents@ fields. The
@contents@ payload is 'Aeson.Null' for nullary constructors, the
single field for unary constructors, and a JSON array for n-ary
constructors.
-}
toJSONSum :: [ConInfo] -> Q Exp
toJSONSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToJSON cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToJSON :: ConInfo -> Q Match
sumCtorToJSON c = do
  mi <- reifyModifierInfoFor backendJSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|Aeson.Null|]
    [n] -> [|toJSON $(varE n)|]
    ns ->
      [|
        toJSON
          ( $(pure (ListE (map (AppE (VarE 'toJSON) . VarE) ns)))
              :: [Aeson.Value]
          )
        |]
  body <-
    [|
      Aeson.object
        [ Aeson.Key.fromText (T.pack "tag") .= ($(pure keyExp) :: T.Text)
        , Aeson.Key.fromText (T.pack "contents") .= ($(pure contentsE) :: Aeson.Value)
        ]
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromJSON dispatch
-- ---------------------------------------------------------------------------

fromJSONBody :: TypeInfo -> Q Exp
fromJSONBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromJSONNewtype c
  TypeShapeRecord c -> fromJSONRecord (nameBase (typeInfoName ti)) c
  TypeShapeEnum cs -> fromJSONEnum cs
  TypeShapeSum cs -> fromJSONSum cs


fromJSONNewtype :: ConInfo -> Q Exp
fromJSONNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . parseJSON|]
  _ -> fail "Wireform.Derive.Aeson: newtype must have exactly one field"


fromJSONRecord :: String -> ConInfo -> Q Exp
fromJSONRecord typeName c = do
  obj <- newName "o"
  parserExp <- recordParser obj c
  [|Aeson.withObject typeName (\ $(varP obj) -> $(pure parserExp))|]


recordParser :: Name -> ConInfo -> Q Exp
recordParser obj c = do
  let conName = conInfoName c
      fields = conInfoFields c
  case fields of
    [] -> [|pure $(conE conName)|]
    (f0 : fs) -> do
      e0 <- fieldParser obj f0
      hd <- [|$(conE conName) <$> $(pure e0)|]
      foldlM
        ( \acc f -> do
            ef <- fieldParser obj f
            [|$(pure acc) <*> $(pure ef)|]
        )
        hd
        fs


fieldParser :: Name -> FieldInfo -> Q Exp
fieldParser obj (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendJSON selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|pure $(varE defNm)|]
      Nothing -> [|pure (error "Wireform.Derive.Aeson: missing 'defaults' for skipped field")|]
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then [|$(varE obj) .:? Aeson.Key.fromText $(pure keyExp)|]
          else [|$(varE obj) .: Aeson.Key.fromText $(pure keyExp)|]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


{- | Enum: dispatch on the JSON string against each constructor's
rendered key. Built as a `MultiWayIf` so that runtime-rendered keys
(from 'renameWith') participate naturally.
-}
fromJSONEnum :: [ConInfo] -> Q Exp
fromJSONEnum cs = do
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (VarE 'fail)
            ( InfixE
                (Just (LitE (StringL "Wireform.Derive.Aeson: unknown enum value ")))
                (VarE '(++))
                (Just (AppE (VarE 'show) (VarE s)))
            )
        )
  let multi = MultiIfE (branches ++ [fallback])
  [|Aeson.withText "enum" (\ $(varP s) -> $(pure multi))|]


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendJSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = AppE (VarE 'pure) (ConE (conInfoName c))
  pure (NormalG guardExp, bodyExp)


{- | Sum: read @tag@ + @contents@ from the object, then dispatch via
a `MultiWayIf` on the rendered tag key for each constructor.
-}
fromJSONSum :: [ConInfo] -> Q Exp
fromJSONSum cs = do
  obj <- newName "o"
  tagVar <- newName "tag"
  cVar <- newName "c"
  branches <- mapM (sumDispatch tagVar cVar) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            (VarE 'fail)
            ( InfixE
                (Just (LitE (StringL "Wireform.Derive.Aeson: unknown sum tag ")))
                (VarE '(++))
                (Just (AppE (VarE 'show) (VarE tagVar)))
            )
        )
      multi = MultiIfE (branches ++ [fallback])
  [|
    Aeson.withObject
      "sum"
      ( \ $(varP obj) -> do
          ($(varP tagVar) :: T.Text) <-
            $(varE obj) .: Aeson.Key.fromText (T.pack "tag")
          ($(varP cVar) :: Aeson.Value) <-
            $(varE obj) .: Aeson.Key.fromText (T.pack "contents")
          $(pure multi)
      )
    |]


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendJSON (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  bodyExp <- case conInfoFields c of
    [] -> [|pure $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (parseJSON $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, bodyExp)


{- | Build a parser for an n-ary sum constructor: parse @contents@ as
a JSON array, then apply each element through 'parseJSON' and
combine via @ConE c \<$> parse e0 \<*> parse e1 \<*> ...@.
-}
sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|parseJSON ($(varE arr) !! $(litE (integerL (fromIntegral i))))|]
  hd <- do
    e0 <- parseI 0
    [|$(conE conName) <$> $(pure e0)|]
  tail' <-
    foldlM
      ( \acc i -> do
          ei <- parseI i
          [|$(pure acc) <*> $(pure ei)|]
      )
      hd
      [1 .. arity - 1]
  let conNameStr = nameBase conName
  [|
    do
      ($(varP arr) :: [Aeson.Value]) <- parseJSON $(varE cVar)
      if length $(varE arr) /= $(litE (integerL (fromIntegral arity)))
        then
          fail
            ( "Wireform.Derive.Aeson: "
                ++ conNameStr
                ++ " expected "
                ++ show ($(litE (integerL (fromIntegral arity))) :: Int)
                ++ " contents, got "
                ++ show (length $(varE arr))
            )
        else $(pure tail')
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "Wireform.Derive.Aeson: cannot derive JSON for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
