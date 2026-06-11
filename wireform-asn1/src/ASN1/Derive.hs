{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- | Annotation-driven Template Haskell deriver for ASN.1.

Unlike the format packages that ship a reusable @ToFormat@ /
@FromFormat@ class, @wireform-asn1@ historically only exposed the
bare 'ASN1.Value.Value' AST plus encoder\/decoder. This module
introduces local 'ToASN1' \/ 'FromASN1' classes and a Template
Haskell deriver targeting them.

== Encoding shape

* 'TypeShapeNewtype' — pass-through to the inner field's instance.
* 'TypeShapeRecord'  — encoded positionally as a 'AV.Sequence' of
  field values, in declaration order. Skipped fields take no slot.
  A field annotated with the 'Asn1Tag' extension @Implicit n@ or
  @Explicit n@ is wrapped in @Tagged ContextSpecific n inner@.
* 'TypeShapeEnum'    — encoded as 'AV.Integer' carrying the
  constructor's positional ordinal (overridable via the @tag N@
  modifier).
* 'TypeShapeSum'     — encoded as @Tagged ContextSpecific i inner@
  where @i@ is the constructor's zero-based positional index
  (overridable via @tag N@). The @inner@ value is 'AV.Null' for
  nullary constructors, the single field's encoding for unary
  constructors, and a @Sequence@ otherwise.

== Per-backend customisation via extension modifier

This deriver introduces 'Asn1Tag', a backend-specific
'Wireform.Derive.Extension.BackendModifier'. Use 'asn1ImplicitTag'
and 'asn1ExplicitTag' as the smart constructors:

@
{\-\# ANN myField (asn1ImplicitTag 0) \#-\}
@

The 'Universal' constructor exists for explicit \"no tagging\"
overrides; it has no effect on the wire (the field's natural
universal tag is used).
-}
module ASN1.Derive (
  -- * Classes
  ToASN1 (..),
  FromASN1 (..),
  encodeASN1,
  decodeASN1,

  -- * Deriver
  deriveASN1,
  deriveToASN1,
  deriveFromASN1,

  -- * Backend extension vocabulary
  Asn1Tag (..),
  asn1ImplicitTag,
  asn1ExplicitTag,
  asn1Universal,
) where

import ASN1.Decode qualified as AD
import ASN1.Encode qualified as AE
import ASN1.Value qualified as AV
import Data.ByteString (ByteString)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Foldable (foldlM)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics (Generic)
import Language.Haskell.TH
import Wireform.Derive.Backend
import Wireform.Derive.Extension (BackendModifier (..), extension, lookupExtension)
import Wireform.Derive.Modifier (Modifier)
import Wireform.Derive.ModifierInfo
import Wireform.Derive.TypeInfo


-- ---------------------------------------------------------------------------
-- Classes (local to this package)
-- ---------------------------------------------------------------------------

-- | Conversion from a Haskell value to an 'AV.Value'.
class ToASN1 a where
  toASN1 :: a -> AV.Value


-- | Conversion from an 'AV.Value' back to a Haskell value.
class FromASN1 a where
  fromASN1 :: AV.Value -> Either String a


-- | Convenience: @encode . toASN1@.
encodeASN1 :: ToASN1 a => a -> ByteString
encodeASN1 = AE.encode . toASN1


-- | Convenience: @decode >=> fromASN1@.
decodeASN1 :: FromASN1 a => ByteString -> Either String a
decodeASN1 bs = AD.decode bs >>= fromASN1


-- ---------------------------------------------------------------------------
-- Standard scalar instances
-- ---------------------------------------------------------------------------

instance ToASN1 AV.Value where
  toASN1 = id


instance FromASN1 AV.Value where
  fromASN1 = Right


instance ToASN1 Bool where
  toASN1 = AV.Boolean


instance FromASN1 Bool where
  fromASN1 (AV.Boolean b) = Right b
  fromASN1 v = Left ("ASN1.Derive: expected BOOLEAN, got " ++ shapeName v)


instance ToASN1 Integer where
  toASN1 = AV.Integer


instance FromASN1 Integer where
  fromASN1 (AV.Integer n) = Right n
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER, got " ++ shapeName v)


instance ToASN1 Int where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Int where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Int, got " ++ shapeName v)


instance ToASN1 Int8 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Int8 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Int8, got " ++ shapeName v)


instance ToASN1 Int16 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Int16 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Int16, got " ++ shapeName v)


instance ToASN1 Int32 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Int32 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Int32, got " ++ shapeName v)


instance ToASN1 Int64 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Int64 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Int64, got " ++ shapeName v)


instance ToASN1 Word where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Word where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Word, got " ++ shapeName v)


instance ToASN1 Word8 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Word8 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Word8, got " ++ shapeName v)


instance ToASN1 Word16 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Word16 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Word16, got " ++ shapeName v)


instance ToASN1 Word32 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Word32 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Word32, got " ++ shapeName v)


instance ToASN1 Word64 where
  toASN1 = AV.Integer . fromIntegral


instance FromASN1 Word64 where
  fromASN1 (AV.Integer n) = Right (fromIntegral n)
  fromASN1 v = Left ("ASN1.Derive: expected INTEGER for Word64, got " ++ shapeName v)


instance ToASN1 Text where
  toASN1 = AV.UTF8String


instance FromASN1 Text where
  fromASN1 (AV.UTF8String t) = Right t
  fromASN1 (AV.PrintableString t) = Right t
  fromASN1 (AV.IA5String t) = Right t
  fromASN1 v = Left ("ASN1.Derive: expected string, got " ++ shapeName v)


instance ToASN1 ByteString where
  toASN1 = AV.OctetString


instance FromASN1 ByteString where
  fromASN1 (AV.OctetString bs) = Right bs
  fromASN1 v = Left ("ASN1.Derive: expected OCTET STRING, got " ++ shapeName v)


instance ToASN1 () where
  toASN1 () = AV.Null


instance FromASN1 () where
  fromASN1 AV.Null = Right ()
  fromASN1 v = Left ("ASN1.Derive: expected NULL, got " ++ shapeName v)


instance ToASN1 a => ToASN1 [a] where
  toASN1 xs = AV.Sequence (V.fromList (map toASN1 xs))


instance FromASN1 a => FromASN1 [a] where
  fromASN1 (AV.Sequence vs) = traverse fromASN1 (V.toList vs)
  fromASN1 (AV.Set vs) = traverse fromASN1 (V.toList vs)
  fromASN1 v = Left ("ASN1.Derive: expected SEQUENCE, got " ++ shapeName v)


instance ToASN1 a => ToASN1 (V.Vector a) where
  toASN1 xs = AV.Sequence (V.map toASN1 xs)


instance FromASN1 a => FromASN1 (V.Vector a) where
  fromASN1 (AV.Sequence vs) = V.mapM fromASN1 vs
  fromASN1 (AV.Set vs) = V.mapM fromASN1 vs
  fromASN1 v = Left ("ASN1.Derive: expected SEQUENCE, got " ++ shapeName v)


instance ToASN1 a => ToASN1 (Maybe a) where
  toASN1 Nothing = AV.Null
  toASN1 (Just x) = toASN1 x


instance FromASN1 a => FromASN1 (Maybe a) where
  fromASN1 AV.Null = Right Nothing
  fromASN1 v = Just <$> fromASN1 v


{- | Human-readable identification of a value's outermost constructor,
used in error messages.
-}
shapeName :: AV.Value -> String
shapeName = \case
  AV.Boolean _ -> "BOOLEAN"
  AV.Integer _ -> "INTEGER"
  AV.BitString _ _ -> "BIT STRING"
  AV.OctetString _ -> "OCTET STRING"
  AV.Null -> "NULL"
  AV.OID _ -> "OID"
  AV.UTF8String _ -> "UTF8String"
  AV.PrintableString _ -> "PrintableString"
  AV.IA5String _ -> "IA5String"
  AV.UTCTime _ -> "UTCTime"
  AV.GeneralizedTime _ -> "GeneralizedTime"
  AV.Sequence _ -> "SEQUENCE"
  AV.Set _ -> "SET"
  AV.Tagged tc tag _ ->
    "Tagged " ++ show tc ++ " " ++ show tag
  AV.Other tc _ tag _ ->
    "Other " ++ show tc ++ " " ++ show tag


-- ---------------------------------------------------------------------------
-- Backend extension: implicit / explicit context-specific tagging
-- ---------------------------------------------------------------------------

{- | ASN.1-specific per-field tagging directives.

@
{\-\# ANN myField (asn1ImplicitTag 0) \#-\}
@

* @Implicit n@ wraps the field value in
  @Tagged ContextSpecific n inner@. In ASN.1 BER\/DER this would
  replace the inner type's tag; this deriver leaves the encoder
  to decide the exact wire shape.
* @Explicit n@ behaves identically at the 'AV.Value' level — both
  end up as @Tagged ContextSpecific n inner@. The semantic
  distinction shows up only at the wire layer (constructed bit).
* @Universal@ is a no-op marker for explicit overrides of an
  inherited tag.
-}
data Asn1Tag
  = Implicit !Int
  | Explicit !Int
  | Universal
  deriving stock (Eq, Show, Read, Typeable, Data, Generic)


instance BackendModifier Asn1Tag where
  backendModifierTag _ = "wireform-asn1.field-opt"


-- | Smart constructor: implicit context-specific tag.
asn1ImplicitTag :: Int -> Modifier
asn1ImplicitTag = extension . Implicit


-- | Smart constructor: explicit context-specific tag.
asn1ExplicitTag :: Int -> Modifier
asn1ExplicitTag = extension . Explicit


{- | Smart constructor: explicitly mark a field as universally tagged
(the default — provided for symmetry).
-}
asn1Universal :: Modifier
asn1Universal = extension Universal


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

-- | Derive both 'ToASN1' and 'FromASN1' for a type.
deriveASN1 :: Name -> Q [Dec]
deriveASN1 nm = (++) <$> deriveToASN1 nm <*> deriveFromASN1 nm


-- | Derive only 'ToASN1'.
deriveToASN1 :: Name -> Q [Dec]
deriveToASN1 nm = do
  ti <- reifyTypeInfo nm
  body <- toASN1Body ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''ToASN1) typ)
          [FunD 'toASN1 [Clause [] (NormalB body) []]]
  pure [decl]


-- | Derive only 'FromASN1'.
deriveFromASN1 :: Name -> Q [Dec]
deriveFromASN1 nm = do
  ti <- reifyTypeInfo nm
  body <- fromASN1Body ti
  let typ = applyTypeArgs (ConT (typeInfoName ti)) (typeInfoVarTypes ti)
      decl =
        InstanceD
          Nothing
          []
          (AppT (ConT ''FromASN1) typ)
          [FunD 'fromASN1 [Clause [] (NormalB body) []]]
  pure [decl]


-- ---------------------------------------------------------------------------
-- ToASN1 dispatch
-- ---------------------------------------------------------------------------

toASN1Body :: TypeInfo -> Q Exp
toASN1Body ti = case typeInfoShape ti of
  TypeShapeNewtype c -> toASN1Newtype c
  TypeShapeRecord c -> toASN1Record c
  TypeShapeEnum cs -> toASN1Enum cs
  TypeShapeSum cs -> toASN1Sum cs


toASN1Newtype :: ConInfo -> Q Exp
toASN1Newtype c = case conInfoFields c of
  [FieldInfo (Just sel) _] -> do
    x <- newName "x"
    lamE [varP x] [|toASN1 ($(varE sel) $(varE x))|]
  [FieldInfo Nothing _] -> do
    x <- newName "x"
    lamE [conP (conInfoName c) [varP x]] [|toASN1 $(varE x)|]
  _ -> fail "ASN1.Derive: newtype must have exactly one field"


toASN1Record :: ConInfo -> Q Exp
toASN1Record c = do
  x <- newName "x"
  pieces <- mapM (toASN1Field (varE x)) (conInfoFields c)
  lamE
    [varP x]
    [|AV.Sequence (V.fromList $(pure (ListE (concat pieces))))|]


{- | Produce the encoded element list for a single record field. Empty
list when the field is skipped; otherwise a singleton with the
(possibly Tagged-wrapped) value.
-}
toASN1Field :: Q Exp -> FieldInfo -> Q [Exp]
toASN1Field varExp (FieldInfo mSel _) = do
  selName <- requireSelector mSel
  mi <- reifyModifierInfoFor backendASN1 selName
  if miSkip mi
    then pure []
    else do
      let getter = appE (varE selName) varExp
          encoded = case miCoerce mi of
            Nothing -> [|toASN1 $getter|]
            Just _ -> [|toASN1 (coerce $getter)|]
      wrapped <- case lookupExtension @Asn1Tag mi of
        Just (Implicit n) -> wrapTagged n encoded
        Just (Explicit n) -> wrapTagged n encoded
        _ -> encoded
      pure [wrapped]
  where
    wrapTagged :: Int -> Q Exp -> Q Exp
    wrapTagged n inner =
      [|
        AV.Tagged
          AV.ContextSpecific
          $(litE (integerL (fromIntegral n)))
          $inner
        |]


toASN1Enum :: [ConInfo] -> Q Exp
toASN1Enum cs = do
  v <- newName "v"
  matches <- mapM enumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    enumMatchTo :: (Int, ConInfo) -> Q Match
    enumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendASN1 (conInfoName c)
      let n = case miTag mi of
            Just t -> t
            Nothing -> defaultIdx
      bodyE <- [|AV.Integer $(litE (integerL (fromIntegral n)))|]
      pure (Match (ConP (conInfoName c) [] []) (NormalB bodyE) [])


toASN1Sum :: [ConInfo] -> Q Exp
toASN1Sum cs = do
  v <- newName "v"
  matches <- mapM sumMatchTo (zip [0 ..] cs)
  body <- caseE (varE v) (map pure matches)
  lamE [varP v] (pure body)
  where
    sumMatchTo :: (Int, ConInfo) -> Q Match
    sumMatchTo (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendASN1 (conInfoName c)
      let i = case miTag mi of
            Just t -> t
            Nothing -> defaultIdx
      fieldNames <- mapM (\_ -> newName "f") (conInfoFields c)
      let pat = ConP (conInfoName c) [] (map VarP fieldNames)
      inner <- case fieldNames of
        [] -> [|AV.Null|]
        [n] -> [|toASN1 $(varE n)|]
        ns ->
          [|
            AV.Sequence
              ( V.fromList
                  $(pure (ListE (map (AppE (VarE 'toASN1) . VarE) ns)))
              )
            |]
      bodyE <-
        [|
          AV.Tagged
            AV.ContextSpecific
            $(litE (integerL (fromIntegral i)))
            $(pure inner)
          |]
      pure (Match pat (NormalB bodyE) [])


-- ---------------------------------------------------------------------------
-- FromASN1 dispatch
-- ---------------------------------------------------------------------------

fromASN1Body :: TypeInfo -> Q Exp
fromASN1Body ti = case typeInfoShape ti of
  TypeShapeNewtype c -> fromASN1Newtype c
  TypeShapeRecord c -> fromASN1Record c
  TypeShapeEnum cs -> fromASN1Enum cs
  TypeShapeSum cs -> fromASN1Sum cs


fromASN1Newtype :: ConInfo -> Q Exp
fromASN1Newtype c = case conInfoFields c of
  [FieldInfo _ _] -> [|fmap $(conE (conInfoName c)) . fromASN1|]
  _ -> fail "ASN1.Derive: newtype must have exactly one field"


-- | Per-field plan computed once at splice time.
data FieldPlan
  = {- | Field is skipped on the wire. Decoder substitutes either
    the named default value or yields an error.
    -}
    PlanSkip !(Q Exp)
  | {- | Field consumes one slot of the encoded SEQUENCE. Carries the
    active index, the optional implicit\/explicit context tag, a
    coercion flag, and the field's selector name (used in error
    messages).
    -}
    PlanActive
      !Int
      -- ^ active index in the SEQUENCE
      !(Maybe Int)
      -- ^ context-specific tag, if any
      !Bool
      -- ^ apply 'coerce' to the parsed value
      !Name
      -- ^ selector name (for error messages)


fromASN1Record :: ConInfo -> Q Exp
fromASN1Record c = do
  v <- newName "v"
  vec <- newName "vec"
  plans <- planFields (conInfoFields c)
  let activeCount = countActive plans
  bodyE <- buildRecord (conInfoName c) vec plans
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            (conP 'AV.Sequence [varP vec])
            ( normalB
                [|
                  if V.length $(varE vec)
                    == $(litE (integerL (fromIntegral activeCount)))
                    then $(pure bodyE)
                    else
                      Left
                        ( "ASN1.Derive: expected SEQUENCE of "
                            ++ show
                              ( $(litE (integerL (fromIntegral activeCount)))
                                  :: Int
                              )
                            ++ " elements, got "
                            ++ show (V.length $(varE vec))
                        )
                  |]
            )
            []
        , match
            wildP
            (normalB [|Left "ASN1.Derive: expected SEQUENCE for record type"|])
            []
        ]
    )


planFields :: [FieldInfo] -> Q [FieldPlan]
planFields = go 0
  where
    go _ [] = pure []
    go !i (FieldInfo mSel _ : rest) = do
      selName <- requireSelector mSel
      mi <- reifyModifierInfoFor backendASN1 selName
      if miSkip mi
        then do
          let skipExpr = case miDefaults mi of
                Just defNm ->
                  [|Right $(varE defNm)|]
                Nothing ->
                  [|
                    Left
                      ( "ASN1.Derive: missing 'defaults' for skipped field "
                          ++ $(litE (stringL (nameBase selName)))
                      )
                    |]
          rest' <- go i rest
          pure (PlanSkip skipExpr : rest')
        else do
          let mTag = case lookupExtension @Asn1Tag mi of
                Just (Implicit n) -> Just n
                Just (Explicit n) -> Just n
                _ -> Nothing
              doCoerce = case miCoerce mi of
                Just _ -> True
                Nothing -> False
              plan = PlanActive i mTag doCoerce selName
          rest' <- go (i + 1) rest
          pure (plan : rest')


{- | Count the number of 'PlanActive' entries in a list of plans (the
expected SEQUENCE arity).
-}
countActive :: [FieldPlan] -> Int
countActive = foldr step 0
  where
    step (PlanActive {}) acc = acc + 1
    step (PlanSkip _) acc = acc


-- | Build @Right Con \<*\> p1 \<*\> p2 ...@ from the field plans.
buildRecord :: Name -> Name -> [FieldPlan] -> Q Exp
buildRecord conName vec plans = case plans of
  [] -> [|Right $(conE conName)|]
  (p0 : ps) -> do
    e0 <- planExpr vec p0
    hd <- [|$(conE conName) <$> $(pure e0)|]
    foldlM
      ( \acc p -> do
          ep <- planExpr vec p
          [|$(pure acc) <*> $(pure ep)|]
      )
      hd
      ps


-- | Compile a single 'FieldPlan' into an @Either String t@ expression.
planExpr :: Name -> FieldPlan -> Q Exp
planExpr _ (PlanSkip e) = e
planExpr vec (PlanActive idx mTag doCoerce selName) = do
  let idxLit = litE (integerL (fromIntegral idx))
      raw = [|V.unsafeIndex $(varE vec) $idxLit|]
      selStr = litE (stringL (nameBase selName))
  inner <- case mTag of
    Nothing -> [|fromASN1 $raw|]
    Just n ->
      let nLit = litE (integerL (fromIntegral n))
      in [|
           case $raw of
             AV.Tagged AV.ContextSpecific tg innerVal
               | tg == $nLit -> fromASN1 innerVal
               | otherwise ->
                   Left
                     ( "ASN1.Derive: field "
                         ++ $selStr
                         ++ ": tag mismatch, expected "
                         ++ show ($nLit :: Int)
                         ++ ", got "
                         ++ show tg
                     )
             other ->
               Left
                 ( "ASN1.Derive: field "
                     ++ $selStr
                     ++ ": expected ContextSpecific tag, got "
                     ++ shapeName other
                 )
           |]
  if doCoerce
    then [|fmap coerce $(pure inner)|]
    else pure inner


-- ---------------------------------------------------------------------------
-- FromASN1: enums and sums
-- ---------------------------------------------------------------------------

fromASN1Enum :: [ConInfo] -> Q Exp
fromASN1Enum cs = do
  v <- newName "v"
  i <- newName "i"
  branches <- mapM (enumDispatch i) (zip [0 ..] cs)
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
                               (LitE (StringL "ASN1.Derive: unknown enum value "))
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
            (conP 'AV.Integer [varP i])
            (normalB (pure multi))
            []
        , match
            wildP
            (normalB [|Left "ASN1.Derive: enum expected INTEGER"|])
            []
        ]
    )
  where
    enumDispatch :: Name -> (Int, ConInfo) -> Q (Guard, Exp)
    enumDispatch iVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendASN1 (conInfoName c)
      let n = case miTag mi of
            Just t -> t
            Nothing -> defaultIdx
          guardE =
            InfixE
              (Just (VarE iVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral n))))
      pure (NormalG guardE, ConE (conInfoName c))


fromASN1Sum :: [ConInfo] -> Q Exp
fromASN1Sum cs = do
  v <- newName "v"
  tagVar <- newName "tg"
  innerVar <- newName "inner"
  branches <- mapM (sumDispatch tagVar innerVar) (zip [0 ..] cs)
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
                               (LitE (StringL "ASN1.Derive: unknown sum tag "))
                           )
                           (AppE (VarE 'show) (VarE tagVar))
                       )
                   )
                 ]
          )
  lamE
    [varP v]
    ( caseE
        (varE v)
        [ match
            ( conP
                'AV.Tagged
                [conP 'AV.ContextSpecific [], varP tagVar, varP innerVar]
            )
            (normalB (pure multi))
            []
        , match
            wildP
            ( normalB
                [|Left "ASN1.Derive: sum expected Tagged ContextSpecific"|]
            )
            []
        ]
    )
  where
    sumDispatch :: Name -> Name -> (Int, ConInfo) -> Q (Guard, Exp)
    sumDispatch tagVar innerVar (defaultIdx, c) = do
      mi <- reifyModifierInfoFor backendASN1 (conInfoName c)
      let i = case miTag mi of
            Just t -> t
            Nothing -> defaultIdx
          guardE =
            InfixE
              (Just (VarE tagVar))
              (VarE '(==))
              (Just (LitE (IntegerL (fromIntegral i))))
      bodyE <- case conInfoFields c of
        [] -> [|Right $(conE (conInfoName c))|]
        [_one] -> [|fmap $(conE (conInfoName c)) (fromASN1 $(varE innerVar))|]
        many -> sumNAry innerVar (conInfoName c) (length many)
      pure (NormalG guardE, bodyE)


sumNAry :: Name -> Name -> Int -> Q Exp
sumNAry innerVar conName arity = do
  vec <- newName "vec"
  let parseI :: Int -> Q Exp
      parseI i = [|fromASN1 ($(varE vec) V.! $(litE (integerL (fromIntegral i))))|]
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
    case $(varE innerVar) of
      AV.Sequence $(varP vec)
        | V.length $(varE vec) == $(litE (integerL (fromIntegral arity))) ->
            $(pure body)
        | otherwise ->
            Left
              ( "ASN1.Derive: "
                  ++ conNameStr
                  ++ " expected "
                  ++ arityStr
                  ++ " contents, got "
                  ++ show (V.length $(varE vec))
              )
      _ ->
        Left
          ( "ASN1.Derive: "
              ++ conNameStr
              ++ " expected SEQUENCE contents"
          )
    |]


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

requireSelector :: Maybe Name -> Q Name
requireSelector (Just n) = pure n
requireSelector Nothing =
  fail "ASN1.Derive: cannot derive ASN1 for non-record positional field"


applyTypeArgs :: Type -> [Type] -> Type
applyTypeArgs = foldl AppT
