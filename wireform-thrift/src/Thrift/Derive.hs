{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Annotation-driven Template Haskell deriver for Thrift
-- 'Thrift.Class.ToThrift' / 'Thrift.Class.FromThrift' instances.
--
-- Encoding shape:
--
-- * 'TypeShapeNewtype' — pass-through.
-- * 'TypeShapeRecord'  — Thrift @Struct@ with one entry per record
--   field. Field IDs default to the field's positional index
--   starting at @1@; an explicit @tag N@ modifier overrides it.
-- * 'TypeShapeEnum'    — encoded as a Thrift @I32@. Each constructor
--   maps to its zero-based positional index unless overridden by
--   @tag N@.
-- * 'TypeShapeSum'     — encoded as a Thrift @Struct@ where the
--   single set field's ID identifies the constructor (Thrift's
--   union convention). Constructor field IDs are positional unless
--   overridden by @tag N@.
--
-- Modifiers honoured: 'tag' (field id / enum value override),
-- 'rename' (ignored — Thrift is structural), 'skip', 'defaults',
-- 'optional', 'required', 'coerced'. Modifiers irrelevant to Thrift
-- (e.g. 'renameStyle') are silently ignored on Thrift-only paths.
module Thrift.Derive
  ( deriveThrift
  , deriveToThrift
  , deriveFromThrift
  ) where

import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Int (Int16, Int32)
import qualified Data.Vector as Vector
import Language.Haskell.TH

import qualified Thrift.Class as TC
import qualified Thrift.Value as TV

import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

deriveThrift :: Name -> Q [Dec]
deriveThrift nm = (++) <$> deriveToThrift nm <*> deriveFromThrift nm

deriveToThrift :: Name -> Q [Dec]
deriveToThrift nm = do
  ti   <- reifyTypeInfo nm
  body <- toThriftBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''TC.ToThrift) typ)
              [FunD 'TC.toThrift [Clause [] (NormalB body) []]]
  pure [decl]

deriveFromThrift :: Name -> Q [Dec]
deriveFromThrift nm = do
  ti   <- reifyTypeInfo nm
  body <- fromThriftBody ti
  let typ  = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl = InstanceD Nothing []
              (AppT (ConT ''TC.FromThrift) typ)
              [FunD 'TC.fromThrift [Clause [] (NormalB body) []]]
  pure [decl]

-- ---------------------------------------------------------------------------
-- ToThrift
-- ---------------------------------------------------------------------------

toThriftBody :: TypeInfo -> Q Exp
toThriftBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> toThriftNewtype c
  TypeShapeRecord  c   -> toThriftRecord  c
  TypeShapeEnum    cs  -> toThriftEnum    cs
  TypeShapeSum     cs  -> toThriftSum     cs

toThriftNewtype :: ConInfo -> Q Exp
toThriftNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [| TC.toThrift ($(varE sel) $(varE x)) |]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [| TC.toThrift $(varE x) |]
  _ -> fail "Thrift.Derive: newtype must have exactly one field"

toThriftRecord :: ConInfo -> Q Exp
toThriftRecord c = do
  x <- newName "x"
  pairs <- recordToThriftPairs (varE x) (conInfoFields c)
  lamE [varP x]
    [| TV.Struct (Vector.fromList $(pure pairs)) |]

recordToThriftPairs :: Q Exp -> [FieldInfo] -> Q Exp
recordToThriftPairs varExp fields = do
  pairExpss <- mapM go (zip [1..] fields)
  pure (ListE (concat pairExpss))
  where
    go :: (Int16, FieldInfo) -> Q [Exp]
    go (defaultId, FieldInfo mSel _) = do
      selName <- requireSelector mSel
      mi <- reifyModifierInfoFor backendThrift selName
      if miSkip mi
        then pure []
        else do
          let fid = case miTag mi of
                Just n  -> fromIntegral n :: Int16
                Nothing -> defaultId
              getter = appE (varE selName) varExp
              encoded = case miCoerce mi of
                Nothing -> [| TC.toThrift $getter |]
                Just _  -> [| TC.toThrift (coerce $getter) |]
          pair <- [| ($(litE (integerL (fromIntegral fid))) :: Int16, $encoded) |]
          pure [pair]

-- | Enums encoded as I32. Default values are zero-based positional
-- indices unless overridden by 'tag'.
toThriftEnum :: [ConInfo] -> Q Exp
toThriftEnum cs = do
  v <- newName "v"
  matches <- mapM enumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatchTo :: (Int32, ConInfo) -> Q Match
    enumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendThrift (conInfoName c)
      let n = case miTag mi of
            Just t  -> fromIntegral t :: Int32
            Nothing -> defaultIdx
      bodyE <- [| TV.I32 $(litE (integerL (fromIntegral n))) |]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])

-- | Sums encoded as Thrift unions: a Struct with exactly one entry
-- whose field ID identifies the constructor.
toThriftSum :: [ConInfo] -> Q Exp
toThriftSum cs = do
  v <- newName "v"
  matches <- mapM sumMatchTo (zip [1 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    sumMatchTo :: (Int16, ConInfo) -> Q Match
    sumMatchTo (defaultId, c) = do
      mi <- reifyModifierInfoFor backendThrift (conInfoName c)
      let fid = case miTag mi of
            Just t  -> fromIntegral t :: Int16
            Nothing -> defaultId
      fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
      let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      payload <- case fieldNames of
        []  -> [| TV.Bool True |]
        [n] -> [| TC.toThrift $(varE n) |]
        ns  -> [| TC.toThrift
                    ($(pure (ListE (map (AppE (VarE 'TC.toThrift) . VarE) ns)))
                      :: [TV.Value]) |]
      bodyE <- [| TV.Struct (Vector.singleton
                    ($(litE (integerL (fromIntegral fid))) :: Int16, $(pure payload))) |]
      pure (Match pat (NormalB bodyE) [])

-- ---------------------------------------------------------------------------
-- FromThrift
-- ---------------------------------------------------------------------------

fromThriftBody :: TypeInfo -> Q Exp
fromThriftBody ti = case typeInfoShape ti of
  TypeShapeNewtype c   -> fromThriftNewtype c
  TypeShapeRecord  c   -> fromThriftRecord c
  TypeShapeEnum    cs  -> fromThriftEnum cs
  TypeShapeSum     cs  -> fromThriftSum  cs

fromThriftNewtype :: ConInfo -> Q Exp
fromThriftNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [| fmap $(conE (conInfoName c)) . TC.fromThrift |]
  _               -> fail "Thrift.Derive: newtype must have exactly one field"

fromThriftRecord :: ConInfo -> Q Exp
fromThriftRecord c = do
  v   <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs (conInfoFields c) (conInfoName c)
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.Struct [varP kvs])
               (normalB (pure bodyE))
               []
       , match wildP
               (normalB
                  [| Left "Thrift.Derive: expected Struct for record type" |])
               []
       ])

recordParser :: Name -> [FieldInfo] -> Name -> Q Exp
recordParser kvs fields conName = case zip [1 ..] fields of
  []         -> [| Right $(conE conName) |]
  (p0 : ps)  -> do
    e0 <- fieldParser kvs p0
    hd <- [| $(conE conName) <$> $(pure e0) |]
    foldlM
      (\acc fp -> do
          ef <- fieldParser kvs fp
          [| $(pure acc) <*> $(pure ef) |])
      hd
      ps

fieldParser :: Name -> (Int16, FieldInfo) -> Q Exp
fieldParser kvs (defaultId, FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendThrift selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [| Right $(varE defNm) |]
      Nothing    -> [| Left ("Thrift.Derive: missing 'defaults' for skipped field " ++
                              $(litE (stringL (nameBase selName)))) |]
    else do
      let fid = case miTag mi of
            Just t  -> fromIntegral t :: Int16
            Nothing -> defaultId
          isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then [| case lookupThriftField $(litE (integerL (fromIntegral fid))) $(varE kvs) of
                    Nothing -> Right Nothing
                    Just  v -> fmap Just (TC.fromThrift v) |]
          else [| case lookupThriftField $(litE (integerL (fromIntegral fid))) $(varE kvs) of
                    Nothing -> Left ("Thrift.Derive: missing field id "
                                     ++ show ($(litE (integerL (fromIntegral fid))) :: Int16))
                    Just v  -> TC.fromThrift v |]
      case miCoerce mi of
        Nothing -> pure base
        Just _  -> [| fmap coerce $(pure base) |]

lookupThriftField :: Int16 -> Vector.Vector (Int16, TV.Value) -> Maybe TV.Value
lookupThriftField fid kvs = Vector.foldr step Nothing kvs
  where
    step (k, v) acc | k == fid  = Just v
                    | otherwise = acc

fromThriftEnum :: [ConInfo] -> Q Exp
fromThriftEnum cs = do
  v <- newName "v"
  i <- newName "i"
  branches <- mapM (enumMatchFrom i) (zip [0 ..] cs)
  let multi = MultiIfE
        (map (\(g, e) -> (g, AppE (ConE 'Right) e)) branches
         ++ [(NormalG (ConE 'True),
               AppE (ConE 'Left)
                 (AppE (AppE (VarE 'mappend)
                       (LitE (StringL "Thrift.Derive: unknown enum value ")))
                       (AppE (VarE 'show) (VarE i))))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.I32 [varP i])
               (normalB (pure multi))
               []
       , match wildP
               (normalB [| Left "Thrift.Derive: enum expected I32" |])
               []
       ])
  where
    enumMatchFrom :: Name -> (Int32, ConInfo) -> Q (Guard, Exp)
    enumMatchFrom iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendThrift (conInfoName c)
      let n = case miTag mi of
            Just t  -> fromIntegral t :: Int32
            Nothing -> defaultIdx
          guardE = InfixE (Just (VarE iVar)) (VarE '(==))
                         (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))

fromThriftSum :: [ConInfo] -> Q Exp
fromThriftSum cs = do
  v   <- newName "v"
  kvs <- newName "kvs"
  fid <- newName "fid"
  pay <- newName "payload"
  branches <- mapM (sumMatchFrom fid pay) (zip [1 ..] cs)
  let multi = MultiIfE
        (branches
         ++ [(NormalG (ConE 'True),
               AppE (ConE 'Left)
                 (AppE (AppE (VarE 'mappend)
                       (LitE (StringL "Thrift.Derive: unknown sum field id ")))
                       (AppE (VarE 'show) (VarE fid))))])
  lamE [varP v]
    (caseE (varE v)
       [ match (conP 'TV.Struct [varP kvs])
               (normalB
                  [| case Vector.toList $(varE kvs) of
                       [(fidLocal, payloadLocal)] ->
                         let $(varP fid) = fidLocal
                             $(varP pay) = payloadLocal
                         in $(pure multi)
                       _ -> Left "Thrift.Derive: sum struct must have exactly one field"
                  |])
               []
       , match wildP
               (normalB [| Left "Thrift.Derive: sum expected Struct" |])
               []
       ])
  where
    sumMatchFrom :: Name -> Name -> (Int16, ConInfo) -> Q (Guard, Exp)
    sumMatchFrom fidVar payVar (defaultId, c) = do
      mi <- reifyModifierInfoFor backendThrift (conInfoName c)
      let fid = case miTag mi of
            Just t  -> fromIntegral t :: Int16
            Nothing -> defaultId
          guardE = InfixE (Just (VarE fidVar)) (VarE '(==))
                         (Just (LitE (IntegerL (fromIntegral fid))))
      bodyE <- case conInfoFields c of
        []      -> [| Right $(conE (conInfoName c)) |]
        [_one]  -> [| fmap $(conE (conInfoName c)) (TC.fromThrift $(varE payVar)) |]
        many    -> sumNAry payVar (conInfoName c) (length many)
      pure (NormalG guardE, bodyE)

sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry payVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [| TC.fromThrift ($(varE arr) !! $(litE (integerL (fromIntegral i)))) |]
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
  [| do
       $(varP arr) <- TC.fromThrift $(varE payVar) :: Either String [TV.Value]
       if length $(varE arr) /= $(litE (integerL (fromIntegral arity)))
         then Left ("Thrift.Derive: " ++ conNameStr ++ " expected "
                    ++ show ($(litE (integerL (fromIntegral arity))) :: Int)
                    ++ " contents, got " ++ show (length $(varE arr)))
         else $(pure body)
   |]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing  =
  fail "Thrift.Derive: cannot derive Thrift for non-record positional field"

applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT

