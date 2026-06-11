{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Annotation-driven Template Haskell deriver for Microsoft Bond
'ToBond' / 'FromBond' instances.

This module also defines the 'ToBond' and 'FromBond' classes
themselves, since the underlying @wireform-bond@ codecs operate on
the dynamic 'Bond.Value.Value' type and do not (yet) ship a
typeclass-based front-end of their own.

== Encoding shape

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — Bond @Struct@ (no base classes) with one
  entry per record field. Field IDs ('Word16') default to the
  field's positional index starting at @1@; an explicit
  @tag N@ modifier overrides it.
* 'TypeShapeEnum'    — Bond 'Bond.Value.Int32' carrying the
  constructor's zero-based positional index unless overridden by
  @tag N@.
* 'TypeShapeSum'     — Bond @Struct@ with exactly one field whose
  ID identifies the constructor (analogous to Thrift's union
  convention). Constructor field IDs are positional unless
  overridden by @tag N@.

Modifiers honoured: 'tag' (field id / enum value override),
'skip', 'defaults', 'optional', 'coerced'. 'rename' /
'renameStyle' are silently ignored on Bond paths because Bond is
a positional, ID-keyed format.
-}
module Bond.Derive (
  -- * Classes
  ToBond (..),
  FromBond (..),

  -- * Helpers
  bondTypeOf,

  -- * TH entry points
  deriveBond,
  deriveToBond,
  deriveFromBond,
) where

import Bond.Value qualified as BV
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Foldable (foldlM)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Classes
-- ---------------------------------------------------------------------------

{- | Bond serialisation: every type that participates on the wire
must report both a 'BV.BondType' (for the wire-level type tag
baked into struct field headers and container headers) and a
'BV.Value' encoding.
-}
class ToBond a where
  bondType :: Proxy a -> BV.BondType
  toBond :: a -> BV.Value


{- | Bond deserialisation: parse a 'BV.Value' back into the user's
type.
-}
class FromBond a where
  fromBond :: BV.Value -> Either String a


{- | Convenience: extract a value's 'BV.BondType' without manually
constructing a 'Proxy'. The argument is never evaluated, so
@'bondTypeOf' (undefined :: a)@ is safe.
-}
bondTypeOf :: forall a. ToBond a => a -> BV.BondType
bondTypeOf _ = bondType (Proxy :: Proxy a)
{-# INLINE bondTypeOf #-}


-- ---------------------------------------------------------------------------
-- Scalar instances
-- ---------------------------------------------------------------------------

instance ToBond Bool where
  bondType _ = BV.BT_BOOL
  toBond = BV.Bool


instance FromBond Bool where
  fromBond (BV.Bool b) = Right b
  fromBond v = Left ("FromBond Bool: expected Bool, got " ++ show v)


instance ToBond Int8 where
  bondType _ = BV.BT_INT8
  toBond = BV.Int8


instance FromBond Int8 where
  fromBond (BV.Int8 n) = Right n
  fromBond v = Left ("FromBond Int8: expected Int8, got " ++ show v)


instance ToBond Int16 where
  bondType _ = BV.BT_INT16
  toBond = BV.Int16


instance FromBond Int16 where
  fromBond (BV.Int16 n) = Right n
  fromBond v = Left ("FromBond Int16: expected Int16, got " ++ show v)


instance ToBond Int32 where
  bondType _ = BV.BT_INT32
  toBond = BV.Int32


instance FromBond Int32 where
  fromBond (BV.Int32 n) = Right n
  fromBond (BV.Enum n) = Right n
  fromBond v = Left ("FromBond Int32: expected Int32, got " ++ show v)


instance ToBond Int64 where
  bondType _ = BV.BT_INT64
  toBond = BV.Int64


instance FromBond Int64 where
  fromBond (BV.Int64 n) = Right n
  fromBond v = Left ("FromBond Int64: expected Int64, got " ++ show v)


instance ToBond Word8 where
  bondType _ = BV.BT_UINT8
  toBond = BV.UInt8


instance FromBond Word8 where
  fromBond (BV.UInt8 n) = Right n
  fromBond v = Left ("FromBond Word8: expected UInt8, got " ++ show v)


instance ToBond Word16 where
  bondType _ = BV.BT_UINT16
  toBond = BV.UInt16


instance FromBond Word16 where
  fromBond (BV.UInt16 n) = Right n
  fromBond v = Left ("FromBond Word16: expected UInt16, got " ++ show v)


instance ToBond Word32 where
  bondType _ = BV.BT_UINT32
  toBond = BV.UInt32


instance FromBond Word32 where
  fromBond (BV.UInt32 n) = Right n
  fromBond v = Left ("FromBond Word32: expected UInt32, got " ++ show v)


instance ToBond Word64 where
  bondType _ = BV.BT_UINT64
  toBond = BV.UInt64


instance FromBond Word64 where
  fromBond (BV.UInt64 n) = Right n
  fromBond v = Left ("FromBond Word64: expected UInt64, got " ++ show v)


instance ToBond Float where
  bondType _ = BV.BT_FLOAT
  toBond = BV.Float


instance FromBond Float where
  fromBond (BV.Float f) = Right f
  fromBond v = Left ("FromBond Float: expected Float, got " ++ show v)


instance ToBond Double where
  bondType _ = BV.BT_DOUBLE
  toBond = BV.Double


instance FromBond Double where
  fromBond (BV.Double d) = Right d
  fromBond v = Left ("FromBond Double: expected Double, got " ++ show v)


instance ToBond Text where
  bondType _ = BV.BT_STRING
  toBond = BV.String


instance FromBond Text where
  fromBond (BV.String t) = Right t
  fromBond (BV.WString t) = Right t
  fromBond v = Left ("FromBond Text: expected String, got " ++ show v)


{- | 'ByteString' fields use Bond's @BT_LIST<BT_UINT8>@ shape
conceptually; in our 'BV.Value' representation this is the
dedicated 'BV.Blob' constructor. The 'bondType' tag is
'BV.BT_LIST' since 'BV.Blob' has no separate wire-level type id.
-}
instance ToBond ByteString where
  bondType _ = BV.BT_LIST
  toBond = BV.Blob


instance FromBond ByteString where
  fromBond (BV.Blob bs) = Right bs
  fromBond v = Left ("FromBond ByteString: expected Blob, got " ++ show v)


instance ToBond a => ToBond [a] where
  bondType _ = BV.BT_LIST
  toBond = listToBond


instance FromBond a => FromBond [a] where
  fromBond (BV.List _ vs) = traverse fromBond (V.toList vs)
  fromBond (BV.Set _ vs) = traverse fromBond (V.toList vs)
  fromBond v = Left ("FromBond [a]: expected List, got " ++ show v)


instance ToBond a => ToBond (Vector a) where
  bondType _ = BV.BT_LIST
  toBond = vectorToBond


instance FromBond a => FromBond (Vector a) where
  fromBond (BV.List _ vs) = V.mapM fromBond vs
  fromBond (BV.Set _ vs) = V.mapM fromBond vs
  fromBond v = Left ("FromBond Vector: expected List, got " ++ show v)


{- | 'Maybe' uses Bond's 'BV.Nullable'. The reported 'bondType' is
the inner type's tag, since Bond models nullable / optional
transparently at the wire level rather than as a top-level type.
-}
instance ToBond a => ToBond (Maybe a) where
  bondType _ = bondType (Proxy :: Proxy a)
  toBond = maybeToBond


instance FromBond a => FromBond (Maybe a) where
  fromBond (BV.Nullable Nothing) = Right Nothing
  fromBond (BV.Nullable (Just v)) = Just <$> fromBond v
  fromBond v = Left ("FromBond Maybe: expected Nullable, got " ++ show v)


{- | The underlying dynamic value passes through unchanged. Used by
the deriver for n-ary sum constructors, whose payload is encoded
as @[BV.Value]@.
-}
instance ToBond BV.Value where
  bondType _ = BV.BT_STRUCT
  toBond = id


instance FromBond BV.Value where
  fromBond = Right


-- ---------------------------------------------------------------------------
-- Top-level helpers (avoid in-instance ScopedTypeVariables surprises)
-- ---------------------------------------------------------------------------

listToBond :: forall a. ToBond a => [a] -> BV.Value
listToBond xs =
  BV.List (bondType (Proxy :: Proxy a)) (V.fromList (map toBond xs))


vectorToBond :: forall a. ToBond a => Vector a -> BV.Value
vectorToBond xs =
  BV.List (bondType (Proxy :: Proxy a)) (V.map toBond xs)


maybeToBond :: forall a. ToBond a => Maybe a -> BV.Value
maybeToBond Nothing = BV.Nullable Nothing
maybeToBond (Just x) = BV.Nullable (Just (toBond x))


-- ---------------------------------------------------------------------------
-- Public TH entry points
-- ---------------------------------------------------------------------------

deriveBond :: Name -> Q [Dec]
deriveBond nm = (++) <$> deriveToBond nm <*> deriveFromBond nm


deriveToBond :: Name -> Q [Dec]
deriveToBond nm = do
  ti <- reifyTypeInfo nm
  toBondE <- toBondBody ti
  bondTyE <- bondTypeForShape ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''ToBond) typ)
          [ FunD 'bondType [Clause [WildP] (NormalB bondTyE) []]
          , FunD 'toBond [Clause [] (NormalB toBondE) []]
          ]
  pure [decl]


deriveFromBond :: Name -> Q [Dec]
deriveFromBond nm = do
  ti <- reifyTypeInfo nm
  body <- fromBondBody ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FromBond) typ)
          [FunD 'fromBond [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- bondType for the type itself
-- ---------------------------------------------------------------------------

bondTypeForShape :: TypeInfo -> Q Exp
bondTypeForShape ti = case typeInfoShape ti of
  TypeShapeNewtype c -> case conInfoFields c of
    [FieldInfo _ fldType] ->
      [|bondType (Proxy :: Proxy $(pure fldType))|]
    _ -> fail "Bond.Derive: newtype must have exactly one field"
  TypeShapeRecord _ -> [|BV.BT_STRUCT|]
  TypeShapeEnum _ -> [|BV.BT_INT32|]
  TypeShapeSum _ -> [|BV.BT_STRUCT|]


-- ---------------------------------------------------------------------------
-- ToBond
-- ---------------------------------------------------------------------------

toBondBody :: TypeInfo -> Q Exp
toBondBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toBondNewtype c
  TypeShapeRecord c -> toBondRecord c
  TypeShapeEnum cs -> toBondEnum cs
  TypeShapeSum cs -> toBondSum cs


toBondNewtype :: ConInfo -> Q Exp
toBondNewtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|toBond ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|toBond $(varE x)|]
  _ -> fail "Bond.Derive: newtype must have exactly one field"


toBondRecord :: ConInfo -> Q Exp
toBondRecord c = do
  x <- newName "x"
  pairs <- recordToBondPairs (varE x) (conInfoFields c)
  lamE
    [varP x]
    [|BV.Struct V.empty (V.fromList $(pure pairs))|]


recordToBondPairs :: Q Exp -> [FieldInfo] -> Q Exp
recordToBondPairs varExp fields = do
  pairExpss <- mapM go (zip [1 ..] fields)
  pure (ListE (concat pairExpss))
  where
    go :: (Word16, FieldInfo) -> Q [Exp]
    go (defaultId, FieldInfo mSel _) = do
      selName <- requireSelector mSel
      mi <- reifyModifierInfoFor backendBond selName
      if miSkip mi
        then pure []
        else do
          let fid = case miTag mi of
                Just n -> fromIntegral n :: Word16
                Nothing -> defaultId
              getter = appE (varE selName) varExp
              encoded = case miCoerce mi of
                Nothing -> [|toBond $getter|]
                Just _ -> [|toBond (coerce $getter)|]
              btE = [|bondTypeOf $getter|]
          pair <-
            [|
              ( $(litE (integerL (fromIntegral fid))) :: Word16
              , $btE
              , $encoded
              )
              |]
          pure [pair]


{- | Enums encoded as Int32. Default values are zero-based positional
indices unless overridden by 'tag'.
-}
toBondEnum :: [ConInfo] -> Q Exp
toBondEnum cs = do
  v <- newName "v"
  matches <- mapM enumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatchTo :: (Int32, ConInfo) -> Q Match
    enumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendBond (conInfoName c)
      let n = case miTag mi of
            Just t -> fromIntegral t :: Int32
            Nothing -> defaultIdx
      bodyE <- [|BV.Int32 $(litE (integerL (fromIntegral n)))|]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])


{- | Sums encoded as a single-field 'BV.Struct'. The field ID
identifies the constructor.
-}
toBondSum :: [ConInfo] -> Q Exp
toBondSum cs = do
  v <- newName "v"
  matches <- mapM sumMatchTo (zip [1 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    sumMatchTo :: (Word16, ConInfo) -> Q Match
    sumMatchTo (defaultId, c) = do
      mi <- reifyModifierInfoFor backendBond (conInfoName c)
      let fid = case miTag mi of
            Just t -> fromIntegral t :: Word16
            Nothing -> defaultId
      fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
      let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      (payloadE, btE) <- case fieldNames of
        [] -> do
          pe <- [|BV.Bool True|]
          bt <- [|BV.BT_BOOL|]
          pure (pe, bt)
        [n] -> do
          pe <- [|toBond $(varE n)|]
          bt <- [|bondTypeOf $(varE n)|]
          pure (pe, bt)
        ns -> do
          let lst = ListE (map (AppE (VarE 'toBond) . VarE) ns)
          pe <- [|toBond ($(pure lst) :: [BV.Value])|]
          bt <- [|BV.BT_LIST|]
          pure (pe, bt)
      bodyE <-
        [|
          BV.Struct
            V.empty
            ( V.singleton
                ( $(litE (integerL (fromIntegral fid))) :: Word16
                , $(pure btE)
                , $(pure payloadE)
                )
            )
          |]
      pure (Match pat (NormalB bodyE) [])


-- ---------------------------------------------------------------------------
-- FromBond
-- ---------------------------------------------------------------------------

fromBondBody :: TypeInfo -> Q Exp
fromBondBody ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromBondNewtype c
  TypeShapeRecord c -> fromBondRecord c
  TypeShapeEnum cs -> fromBondEnum cs
  TypeShapeSum cs -> fromBondSum cs


fromBondNewtype :: ConInfo -> Q Exp
fromBondNewtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . fromBond|]
  _ -> fail "Bond.Derive: newtype must have exactly one field"


fromBondRecord :: ConInfo -> Q Exp
fromBondRecord c = do
  v <- newName "v"
  kvs <- newName "kvs"
  bodyE <- recordParser kvs (conInfoFields c) (conInfoName c)
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'BV.Struct [wildP, varP kvs])
            (normalB (pure bodyE))
            []
        , match
            wildP
            ( normalB
                [|Left "Bond.Derive: expected Struct for record type"|]
            )
            []
        ]
    )


recordParser :: Name -> [FieldInfo] -> Name -> Q Exp
recordParser kvs fields conName = case zip [1 ..] fields of
  [] -> [|Right $(conE conName)|]
  (p0 : ps) -> do
    e0 <- fieldParser kvs p0
    hd <- [|$(conE conName) <$> $(pure e0)|]
    foldlM
      ( \acc fp -> do
          ef <- fieldParser kvs fp
          [|$(pure acc) <*> $(pure ef)|]
      )
      hd
      ps


fieldParser :: Name -> (Word16, FieldInfo) -> Q Exp
fieldParser kvs (defaultId, FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendBond selName
  if miSkip mi
    then case miDefaults mi of
      Just defNm -> [|Right $(varE defNm)|]
      Nothing ->
        [|
          Left
            ( "Bond.Derive: missing 'defaults' for skipped field "
                ++ $(litE (stringL (nameBase selName)))
            )
          |]
    else do
      let fid = case miTag mi of
            Just t -> fromIntegral t :: Word16
            Nothing -> defaultId
          isOptional = miRequired mi == Just False
      base <-
        if isOptional
          then
            [|
              case lookupBondField $(litE (integerL (fromIntegral fid))) $(varE kvs) of
                Nothing -> Right Nothing
                Just v -> fmap Just (fromBond v)
              |]
          else
            [|
              case lookupBondField $(litE (integerL (fromIntegral fid))) $(varE kvs) of
                Nothing ->
                  Left
                    ( "Bond.Derive: missing field id "
                        ++ show ($(litE (integerL (fromIntegral fid))) :: Word16)
                    )
                Just v -> fromBond v
              |]
      case miCoerce mi of
        Nothing -> pure base
        Just _ -> [|fmap coerce $(pure base)|]


-- | Linear scan for a field id in a Bond struct's field vector.
lookupBondField
  :: Word16
  -> V.Vector (Word16, BV.BondType, BV.Value)
  -> Maybe BV.Value
lookupBondField fid kvs = V.foldr step Nothing kvs
  where
    step (k, _, v) acc
      | k == fid = Just v
      | otherwise = acc


fromBondEnum :: [ConInfo] -> Q Exp
fromBondEnum cs = do
  v <- newName "v"
  i <- newName "i"
  branches <- mapM (enumMatchFrom i) (zip [0 ..] cs)
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
                               (LitE (StringL "Bond.Derive: unknown enum value "))
                           )
                           (AppE (VarE 'show) (VarE i))
                       )
                   )
                 ]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'BV.Int32 [varP i])
            (normalB (pure multi))
            []
        , match
            (conP 'BV.Enum [varP i])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "Bond.Derive: enum expected Int32"|])
            []
        ]
    )
  where
    enumMatchFrom :: Name -> (Int32, ConInfo) -> Q (Guard, Exp)
    enumMatchFrom iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendBond (conInfoName c)
      let n = case miTag mi of
            Just t -> fromIntegral t :: Int32
            Nothing -> defaultIdx
          guardE =
            InfixE
              (Just (VarE iVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))


fromBondSum :: [ConInfo] -> Q Exp
fromBondSum cs = do
  v <- newName "v"
  kvs <- newName "kvs"
  fid <- newName "fid"
  pay <- newName "payload"
  branches <- mapM (sumMatchFrom fid pay) (zip [1 ..] cs)
  let multi =
        MultiIfE
          ( branches
              ++ [
                   ( NormalG (ConE 'True)
                   , AppE
                       (ConE 'Left)
                       ( AppE
                           ( AppE
                               (VarE 'mappend)
                               (LitE (StringL "Bond.Derive: unknown sum field id "))
                           )
                           (AppE (VarE 'show) (VarE fid))
                       )
                   )
                 ]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'BV.Struct [wildP, varP kvs])
            ( normalB
                [|
                  case V.toList $(varE kvs) of
                    [(fidLocal, _btLocal, payloadLocal)] ->
                      let $(varP fid) = fidLocal
                          $(varP pay) = payloadLocal
                      in $(pure multi)
                    _ -> Left "Bond.Derive: sum struct must have exactly one field"
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "Bond.Derive: sum expected Struct"|])
            []
        ]
    )
  where
    sumMatchFrom :: Name -> Name -> (Word16, ConInfo) -> Q (Guard, Exp)
    sumMatchFrom fidVar payVar (defaultId, c) = do
      mi <- reifyModifierInfoFor backendBond (conInfoName c)
      let fid = case miTag mi of
            Just t -> fromIntegral t :: Word16
            Nothing -> defaultId
          guardE =
            InfixE
              (Just (VarE fidVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral fid))))
      bodyE <- case conInfoFields c of
        [] -> [|Right $(conE (conInfoName c))|]
        [_one] -> [|fmap $(conE (conInfoName c)) (fromBond $(varE payVar))|]
        many -> sumNAry payVar (conInfoName c) (length many)
      pure (NormalG guardE, bodyE)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry payVar conName arity = do
  arr <- newName "arr"
  let parseI :: Int -> Q Exp
      parseI i = [|fromBond ($(varE arr) !! $(litE (integerL (fromIntegral i))))|]
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
    do
      $(varP arr) <- fromBond $(varE payVar) :: Either String [BV.Value]
      if length $(varE arr) /= $(litE (integerL (fromIntegral arity)))
        then
          Left
            ( "Bond.Derive: "
                ++ conNameStr
                ++ " expected "
                ++ show ($(litE (integerL (fromIntegral arity))) :: Int)
                ++ " contents, got "
                ++ show (length $(varE arr))
            )
        else $(pure body)
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "Bond.Derive: cannot derive Bond for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
