{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for Bencode
'Bencode.Class.ToBencode' / 'Bencode.Class.FromBencode' instances.

Encoding shape:

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — Bencode 'BV.BDict' with UTF-8 byte-string keys.
* 'TypeShapeEnum'    — Bencode 'BV.BString' carrying the (possibly
  renamed) constructor name (UTF-8 encoded).
* 'TypeShapeSum'     — Bencode dict
  @{ \"tag\": \"Ctor\", \"contents\": ... }@, except for nullary
  constructors, which emit @{ \"tag\": \"Ctor\" }@ (no @contents@
  entry, because Bencode has no Null).

Modifiers honoured: 'rename', 'renameStyle', 'renameWith', 'skip',
'defaults', 'optional', 'coerced'.
-}
module Bencode.Derive (
  deriveBencode,
  deriveToBencode,
  deriveFromBencode,
) where

import Bencode.Class qualified as B
import Bencode.Value qualified as BV
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive both 'B.ToBencode' and 'B.FromBencode' for a type.
deriveBencode :: Name -> Q [Dec]
deriveBencode nm = (++) <$> deriveToBencode nm <*> deriveFromBencode nm


-- | Derive only 'B.ToBencode'.
deriveToBencode :: Name -> Q [Dec]
deriveToBencode nm = do
  ti <- reifyTypeInfo nm
  body <- toBencodeBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''B.ToBencode) typ)
          [FunD 'B.toBencode [Clause [] (NormalB body) []]]
  pure [decl]


-- | Derive only 'B.FromBencode'.
deriveFromBencode :: Name -> Q [Dec]
deriveFromBencode nm = do
  ti <- reifyTypeInfo nm
  body <- fromBencodeBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''B.FromBencode) typ)
          [FunD 'B.fromBencode [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToBencode
-- ---------------------------------------------------------------------------

toBencodeBody :: TypeInfo -> Q Exp
toBencodeBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toBencodeNewtype c
  TypeShapeRecord c -> toBencodeRecord c
  TypeShapeEnum cs -> toBencodeEnum cs
  TypeShapeSum cs -> toBencodeSum cs


toBencodeNewtype :: ConInfo -> Q Exp
toBencodeNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|B.toBencode ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|B.toBencode $(varE x)|]
  _ -> fail "Bencode.Derive: newtype must have exactly one field"


toBencodeRecord :: ConInfo -> Q Exp
toBencodeRecord c = do
  x <- newName "x"
  pairs <- recordToBencodePairs (varE x) c
  lamE
    [varP x]
    [|BV.BDict (V.fromList $(pure pairs))|]


recordToBencodePairs :: Q Exp -> ConInfo -> Q Exp
recordToBencodePairs varExp c = do
  pairExpss <- mapM (toBencodeField varExp) (conInfoFields c)
  pure (ListE (concat pairExpss))


toBencodeField :: Q Exp -> FieldInfo -> Q [Exp]
toBencodeField varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendBencode selName
  if miSkip mi
    then pure []
    else do
      let selBase = T.pack (nameBase selName)
      keyExp <- renderWireKey mi selBase
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|B.toBencode $getter|]
            Just _ -> [|B.toBencode (coerce $getter)|]
      pair <- [|(TE.encodeUtf8 $(pure keyExp), $encoded)|]
      pure [pair]


toBencodeEnum :: [ConInfo] -> Q Exp
toBencodeEnum cs = do
  v <- newName "v"
  matches <- mapM enumToBencodeMatch cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


enumToBencodeMatch :: ConInfo -> Q Match
enumToBencodeMatch c = do
  mi <- reifyModifierInfoFor backendBencode (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  body <- [|BV.BString (TE.encodeUtf8 $(pure keyExp))|]
  pure (Match (ConP (conInfoName c) [] []) (NormalB body) [])


toBencodeSum :: [ConInfo] -> Q Exp
toBencodeSum cs = do
  v <- newName "v"
  matches <- mapM sumCtorToBencode cs
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)


sumCtorToBencode :: ConInfo -> Q Match
sumCtorToBencode c = do
  mi <- reifyModifierInfoFor backendBencode (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
  let pat = ConP (conInfoName c) [] (map VarP fieldNames)
  body <- case fieldNames of
    [] ->
      -- Bencode has no Null; drop the 'contents' entry entirely so
      -- nullary constructors round-trip as a single-key dict.
      [|
        BV.BDict
          ( V.fromList
              [ (BS8.pack "tag", BV.BString (TE.encodeUtf8 $(pure keyExp)))
              ]
          )
        |]
    [n] ->
      [|
        BV.BDict
          ( V.fromList
              [ (BS8.pack "tag", BV.BString (TE.encodeUtf8 $(pure keyExp)))
              , (BS8.pack "contents", B.toBencode $(varE n))
              ]
          )
        |]
    ns ->
      [|
        BV.BDict
          ( V.fromList
              [ (BS8.pack "tag", BV.BString (TE.encodeUtf8 $(pure keyExp)))
              ,
                ( BS8.pack "contents"
                , BV.BList
                    ( V.fromList
                        $(pure (ListE (map (AppE (VarE 'B.toBencode) . VarE) ns)))
                    )
                )
              ]
          )
        |]
  pure (Match pat (NormalB body) [])


-- ---------------------------------------------------------------------------
-- FromBencode
-- ---------------------------------------------------------------------------

fromBencodeBody :: TypeInfo -> Q Exp
fromBencodeBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromBencodeNewtype c
  TypeShapeRecord c -> fromBencodeRecord c
  TypeShapeEnum cs -> fromBencodeEnum cs
  TypeShapeSum cs -> fromBencodeSum cs


fromBencodeNewtype :: ConInfo -> Q Exp
fromBencodeNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . B.fromBencode|]
  _ -> fail "Bencode.Derive: newtype must have exactly one field"


fromBencodeRecord :: ConInfo -> Q Exp
fromBencodeRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs c
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'BV.BDict [varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "Bencode.Derive: expected BDict for record type"|]
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
  mi <- reifyModifierInfoFor backendBencode selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "Bencode.Derive: missing 'defaults' for skipped field "
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
              case lookupBencodeField (TE.encodeUtf8 $(pure keyExp)) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (B.fromBencode v)
              |]
          else
            [|
              case lookupBencodeField (TE.encodeUtf8 $(pure keyExp)) $(varE kvs) of
                Nothing ->
                  Left
                    ( "Bencode.Derive: missing field "
                        ++ T.unpack $(pure keyExp)
                    )
                Just v -> B.fromBencode v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


{- | Linear scan for a key in a 'BV.BDict'. Inlined helper exposed at
the module level so generated code can reference it.
-}
lookupBencodeField :: ByteString -> V.Vector (ByteString, BV.Value) -> Maybe BV.Value
lookupBencodeField name kvs = V.foldr step Nothing kvs
  where
    step (k, v) acc
      | k == name = Just v
      | otherwise = acc


fromBencodeEnum :: [ConInfo] -> Q Exp
fromBencodeEnum cs = do
  v <- newName "v"
  s <- newName "s"
  t <- newName "t"
  branches <- mapM (enumDispatch t) cs
  let fallback =
        ( NormalG (ConE 'True)
        , AppE
            ( AppE
                (VarE 'mappend)
                (LitE (StringL "Bencode.Derive: unknown enum value "))
            )
            (AppE (VarE 'show) (VarE t))
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
            (conP 'BV.BString [varP s])
            ( normalB
                [|
                  case TE.decodeUtf8' $(varE s) of
                    Left _ -> Left "Bencode.Derive: enum key not valid UTF-8"
                    Right $(varP t) -> $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "Bencode.Derive: enum expected BString"|])
            []
        ]
    )


enumDispatch :: Name -> ConInfo -> Q (Guard, Exp)
enumDispatch sVar c = do
  mi <- reifyModifierInfoFor backendBencode (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE sVar)) (VarE '(==)) (Just keyExp)
      bodyExp = ConE (conInfoName c)
  pure (NormalG guardExp, bodyExp)


fromBencodeSum :: [ConInfo] -> Q Exp
fromBencodeSum cs = do
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
                    (LitE (StringL "Bencode.Derive: unknown sum tag "))
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
            (conP 'BV.BDict [varP kvs])
            ( normalB
                [|
                  do
                    $(varP tagVar) <-
                      case lookupBencodeField (BS8.pack "tag") $(varE kvs) of
                        Just (BV.BString raw) -> case TE.decodeUtf8' raw of
                          Left _ -> Left "Bencode.Derive: sum 'tag' not valid UTF-8"
                          Right t -> Right t
                        _ -> Left "Bencode.Derive: sum missing 'tag'"
                    -- Contents may be absent for nullary constructors;
                    -- we provide a harmless placeholder that those
                    -- branches never inspect.
                    $(varP cVar) <-
                      case lookupBencodeField (BS8.pack "contents") $(varE kvs) of
                        Just x -> Right x
                        Nothing -> Right (BV.BList V.empty)
                    $(pure multi)
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "Bencode.Derive: sum expected BDict"|])
            []
        ]
    )


sumDispatch :: Name -> Name -> ConInfo -> Q (Guard, Exp)
sumDispatch tagVar cVar c = do
  mi <- reifyModifierInfoFor backendBencode (conInfoName c)
  keyExp <- renderWireKey mi (T.pack (nameBase (conInfoName c)))
  let guardExp = InfixE (Just (VarE tagVar)) (VarE '(==)) (Just keyExp)
  body <- case conInfoFields c of
    [] -> [|Right $(conE (conInfoName c))|]
    [_one] -> [|fmap $(conE (conInfoName c)) (B.fromBencode $(varE cVar))|]
    many -> sumNAry cVar (conInfoName c) (length many)
  pure (NormalG guardExp, body)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry cVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|B.fromBencode ($(varE arr) V.! $(litE (integerL (fromIntegral i))))|]
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
      arityStr = show arity
  [|
    case $(varE cVar) of
      BV.BList $(varP arr)
        | V.length $(varE arr) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "Bencode.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ arityStr
                  ++ " contents, got "
                  ++ show (V.length $(varE arr))
              )
      _ ->
        Left
          ( "Bencode.Derive: "
              ++ conNameStr
              ++ " expected BList contents"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "Bencode.Derive: cannot derive Bencode for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
