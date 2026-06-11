{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for MessagePack
'MsgPack.Class.ToMsgPack' / 'MsgPack.Class.FromMsgPack' instances.

Mirrors "CBOR.Derive". Modifiers honoured: 'rename', 'renameStyle',
'renameWith', 'skip', 'defaults', 'optional', 'coerced'. The CBOR
and MessagePack derivers are deliberately structural twins so a
single set of @ANN@ pragmas can drive both formats.
-}
module MsgPack.Derive (
  deriveMsgPack,
  deriveToMsgPack,
  deriveFromMsgPack,
) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Data.Vector qualified as V
import Language.Haskell.TH
import MsgPack.Class qualified as M
import MsgPack.Value qualified as MV
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveMsgPack :: Name -> Q [Dec]
deriveMsgPack nm = (++) <$> deriveToMsgPack nm <*> deriveFromMsgPack nm


deriveToMsgPack :: Name -> Q [Dec]
deriveToMsgPack nm = do
  ti <- reifyTypeInfo nm
  body <- toMPBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''M.ToMsgPack) typ)
          [FunD 'M.toMsgPack [Clause [] (NormalB body) []]]
  pure [decl]


deriveFromMsgPack :: Name -> Q [Dec]
deriveFromMsgPack nm = do
  ti <- reifyTypeInfo nm
  body <- fromMPBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''M.FromMsgPack) typ)
          [FunD 'M.fromMsgPack [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToMsgPack
-- ---------------------------------------------------------------------------

toMPBody :: TypeInfo -> Q Exp
toMPBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toMPNewtype c
  TypeShapeRecord c -> toMPRecord c
  TypeShapeEnum cs -> toMPEnum cs
  TypeShapeSum cs -> toMPSum cs


toMPNewtype :: ConInfo -> Q Exp
toMPNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|M.toMsgPack ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|M.toMsgPack $(varE x)|]
  _ -> fail "MsgPack.Derive: newtype must have exactly one field"


toMPRecord :: ConInfo -> Q Exp
toMPRecord c = do
  x <- newName "x"
  pairs <- recordToMPPairs (varE x) c
  lamE
    [varP x]
    [|MV.Map (V.fromList $(pure pairs))|]


recordToMPPairs :: Q Exp -> ConInfo -> Q Exp
recordToMPPairs varExp c = do
  pairExpss <- mapM (toMPField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toMPField :: Q Exp -> FieldInfo -> Q [Exp]
toMPField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendMsgPack selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|M.toMsgPack $getter|]
            Just _ -> [|M.toMsgPack (coerce $getter)|]
      pair <- [|(MV.String $(pure keyExp), $encoded)|]
      pure [pair]


toMPEnum :: [ConInfo] -> Q Exp
toMPEnum cs = do
  v <- newName "v"
  matches <- mapM enumToMPMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToMPMatch :: ConInfo -> Q Match
enumToMPMatch c = do
  mi <- reifyModifierInfoFor backendMsgPack (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|MV.String $(pure keyExp)|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toMPSum :: [ConInfo] -> Q Exp
toMPSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToMP cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToMP :: ConInfo -> Q Match
sumCtorToMP c = do
  mi <- reifyModifierInfoFor backendMsgPack (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  contentsE <- case fieldNames of
    [] -> [|MV.Nil|]
    [n] -> [|M.toMsgPack $(varE n)|]
    ns ->
      [|
        MV.Array
          ( V.fromList
              $(pure (ListE (map (AppE (VarE 'M.toMsgPack) . VarE) ns)))
          )
        |]
  body <-
    [|
      MV.Map
        ( V.fromList
            [
              ( MV.String (T.pack "tag")
              , MV.String $(pure keyExp)
              )
            ,
              ( MV.String (T.pack "contents")
              , $(pure contentsE)
              )
            ]
        )
      |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromMsgPack
-- ---------------------------------------------------------------------------

fromMPBody :: TypeInfo -> Q Exp
fromMPBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromMPNewtype c
  TypeShapeRecord c -> fromMPRecord c
  TypeShapeEnum cs -> fromMPEnum cs
  TypeShapeSum cs -> fromMPSum cs


fromMPNewtype :: ConInfo -> Q Exp
fromMPNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . M.fromMsgPack|]
  _ -> fail "MsgPack.Derive: newtype must have exactly one field"


fromMPRecord :: ConInfo -> Q Exp
fromMPRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'MV.Map [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "MsgPack.Derive: expected Map for record type"|]
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
  mi <- reifyModifierInfoFor backendMsgPack selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "MsgPack.Derive: missing 'defaults' for skipped field "
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
              case lookupMPField $(pure keyExp) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (M.fromMsgPack v)
              |]
          else
            [|
              case lookupMPField $(pure keyExp) $(varE kvs) of
                Nothing ->
                  Left
                    ( "MsgPack.Derive: missing field "
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> M.fromMsgPack v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


lookupMPField :: T.Text -> V.Vector (MV.Value, MV.Value) -> Maybe MV.Value
lookupMPField name kvs = V.foldr step Nothing kvs
  where
    step (MV.String k, v) acc
      | k == name = Just v
      | otherwise = acc
    step _ acc = acc


fromMPEnum :: [ConInfo] -> Q Exp
fromMPEnum cs = do
  v <- newName "v"
  s <- newName "s"
  branches <- mapM (enumDispatch s) cs
  let multi =
        MultiIfE
          ( map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
              ++ [
                   ( NormalG (ConE 'True)
                   , AppE
                       (ConE 'Left)
                       ( AppE
                           ( AppE
                               (VarE 'mappend)
                               (LitE (StringL "MsgPack.Derive: unknown enum value "))
                           )
                           (AppE (VarE 'show) (VarE s))
                       )
                   )
                 ]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'MV.String [varP s])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "MsgPack.Derive: enum expected String"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendMsgPack (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromMPSum :: [ConInfo] -> Q Exp
fromMPSum cs = do
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
                    (LitE (StringL "MsgPack.Derive: unknown sum tag "))
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
            (conP 'MV.Map [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupMPField (T.pack "tag") $(varE kvs) of
                        Just (MV.String t) -> Right t
                        _ -> Left "MsgPack.Derive: sum missing 'tag'"
                    $(varP cVar) <-
                      case lookupMPField (T.pack "contents") $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right MV.Nil
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "MsgPack.Derive: sum expected Map"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendMsgPack (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (M.fromMsgPack $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|M.fromMsgPack ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
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
      MV.Array $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "MsgPack.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ show arity
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "MsgPack.Derive: "
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
  fail "MsgPack.Derive: cannot derive MsgPack for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
