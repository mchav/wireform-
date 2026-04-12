{-# LANGUAGE TemplateHaskell #-}
-- | CDDL code generation — generates Haskell data types and
-- ToCBOR\/FromCBOR stub instances from CDDL schemas (RFC 8610).
-- Map rules become records, array rules become newtypes over Vector.
module CBOR.CDDLCodeGen
  ( generateCDDLTypes
  , deriveCDDL
  ) where

import Data.Char (toLower, toUpper)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Language.Haskell.TH

import CBOR.CDDLSchema

-- ---------------------------------------------------------------------------
-- Text-based code generation
-- ---------------------------------------------------------------------------

generateCDDLTypes :: CDDLSchema -> Text
generateCDDLTypes (CDDLSchema rules) =
  let decls = concatMap genRule (V.toList rules)
  in T.intercalate "\n\n" decls

genRule :: CDDLRule -> [Text]
genRule (CDDLRule name ty) = genRuleType (upperFirst name) ty

genRuleType :: Text -> CDDLType -> [Text]
genRuleType name = \case
  CTMap members ->
    let fields = V.toList members
    in [ genMapRecord name fields
       , genToCBORMap name fields
       , genFromCBORMap name fields
       ]

  CTArray members ->
    let fields = V.toList members
    in [ genArrayNewtype name fields ]

  CTChoice alts ->
    let altList = V.toList alts
    in [ genChoiceType name altList ]

  _ -> [ "type " <> name <> " = CBORValue" ]

-- ---------------------------------------------------------------------------
-- Map -> record
-- ---------------------------------------------------------------------------

genMapRecord :: Text -> [CDDLMember] -> Text
genMapRecord name members = T.unlines $
  [ "data " <> name <> " = " <> name ]
  <> case members of
    [] ->
      [ "  deriving stock (Show, Eq, Generic)" ]
    (m:ms) ->
      [ "  { " <> genMemberField name m ]
      <> map (\mem -> "  , " <> genMemberField name mem) ms
      <> [ "  } deriving stock (Show, Eq, Generic)" ]

genMemberField :: Text -> CDDLMember -> Text
genMemberField recName (CDDLMember fieldName ty occ) =
  let accessor = cddlFieldAccessorName recName fieldName
      hsType = cddlFieldHsType ty occ
  in accessor <> " :: " <> hsType

cddlFieldAccessorName :: Text -> Text -> Text
cddlFieldAccessorName recName fieldName =
  lowerFirst recName <> upperFirst (snakeToCamel fieldName)

cddlFieldHsType :: CDDLType -> Occurrence -> Text
cddlFieldHsType ty occ = case occ of
  Optional    -> "!(Maybe " <> cddlInnerHsType ty <> ")"
  ZeroOrMore  -> "!(Vector " <> cddlInnerHsType ty <> ")"
  OneOrMore   -> "!(Vector " <> cddlInnerHsType ty <> ")"
  Once        -> cddlStrictHsType ty

cddlStrictHsType :: CDDLType -> Text
cddlStrictHsType = \case
  CTUint  -> "{-# UNPACK #-} !Word64"
  CTNint  -> "{-# UNPACK #-} !Int64"
  CTInt   -> "{-# UNPACK #-} !Int64"
  CTTstr  -> "!Text"
  CTBstr  -> "!ByteString"
  CTFloat -> "{-# UNPACK #-} !Double"
  CTBool  -> "!Bool"
  CTNil   -> "()"
  CTAny   -> "!CBORValue"
  CTRef n -> "!" <> upperFirst n
  CTTagged _ inner -> cddlStrictHsType inner
  CTMap _ -> "!CBORValue"
  CTArray _ -> "!CBORValue"
  CTChoice _ -> "!CBORValue"
  CTLiteral _ -> "!Text"

cddlInnerHsType :: CDDLType -> Text
cddlInnerHsType = \case
  CTUint  -> "Word64"
  CTNint  -> "Int64"
  CTInt   -> "Int64"
  CTTstr  -> "Text"
  CTBstr  -> "ByteString"
  CTFloat -> "Double"
  CTBool  -> "Bool"
  CTNil   -> "()"
  CTAny   -> "CBORValue"
  CTRef n -> upperFirst n
  CTTagged _ inner -> cddlInnerHsType inner
  CTMap _ -> "CBORValue"
  CTArray _ -> "CBORValue"
  CTChoice _ -> "CBORValue"
  CTLiteral _ -> "Text"

-- ---------------------------------------------------------------------------
-- ToCBOR / FromCBOR stub instances (text)
-- ---------------------------------------------------------------------------

genToCBORMap :: Text -> [CDDLMember] -> Text
genToCBORMap name _ = T.unlines
  [ "instance ToCBOR " <> name <> " where"
  , "  toCBOR _ = error \"ToCBOR " <> name <> ": stub\""
  ]

genFromCBORMap :: Text -> [CDDLMember] -> Text
genFromCBORMap name _ = T.unlines
  [ "instance FromCBOR " <> name <> " where"
  , "  fromCBOR _ = Left \"FromCBOR " <> name <> ": stub\""
  ]

-- ---------------------------------------------------------------------------
-- Array -> newtype over Vector
-- ---------------------------------------------------------------------------

genArrayNewtype :: Text -> [CDDLMember] -> Text
genArrayNewtype name members = case members of
  [CDDLMember _ inner _] ->
    T.unlines
      [ "newtype " <> name <> " = " <> name
      , "  { un" <> name <> " :: Vector " <> cddlInnerHsType inner
      , "  } deriving stock (Show, Eq, Generic)"
      ]
  _ ->
    T.unlines
      [ "newtype " <> name <> " = " <> name
      , "  { un" <> name <> " :: Vector CBORValue"
      , "  } deriving stock (Show, Eq, Generic)"
      ]

-- ---------------------------------------------------------------------------
-- Choice -> sum type
-- ---------------------------------------------------------------------------

genChoiceType :: Text -> [CDDLType] -> Text
genChoiceType name alts = T.unlines $
  [ "data " <> name ]
  <> case zip [0 :: Int ..] alts of
    [] -> [ "  deriving stock (Show, Eq, Generic)" ]
    ((i, a):rest) ->
      [ "  = " <> name <> "Alt" <> T.pack (show i) <> " " <> cddlStrictHsType a ]
      <> map (\(j, alt) -> "  | " <> name <> "Alt" <> T.pack (show j) <> " " <> cddlStrictHsType alt) rest
      <> [ "  deriving stock (Show, Eq, Generic)" ]

-- ---------------------------------------------------------------------------
-- Template Haskell
-- ---------------------------------------------------------------------------

deriveCDDL :: CDDLSchema -> Q [Dec]
deriveCDDL (CDDLSchema rules) = do
  concat <$> mapM deriveCDDLRule (V.toList rules)

deriveCDDLRule :: CDDLRule -> Q [Dec]
deriveCDDLRule (CDDLRule name ty) = deriveCDDLRuleType (upperFirst name) ty

deriveCDDLRuleType :: Text -> CDDLType -> Q [Dec]
deriveCDDLRuleType name = \case
  CTMap members -> deriveCDDLMapTH name (V.toList members)
  CTArray members -> deriveCDDLArrayTH name (V.toList members)
  CTChoice alts -> deriveCDDLChoiceTH name (V.toList alts)
  _ -> pure []

-- ---------------------------------------------------------------------------
-- TH: Map -> record
-- ---------------------------------------------------------------------------

deriveCDDLMapTH :: Text -> [CDDLMember] -> Q [Dec]
deriveCDDLMapTH name members = do
  let tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
  fieldDecs <- mapM (mkCDDLRecordField name) members
  let dataDec = DataD [] tyName [] Nothing
        [RecC conName fieldDecs]
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  pure [dataDec]

mkCDDLRecordField :: Text -> CDDLMember -> Q VarBangType
mkCDDLRecordField recName (CDDLMember fieldName ty occ) = do
  let accessor = cddlFieldAccessorName recName fieldName
      accName = mkName (T.unpack accessor)
  hsTy <- case occ of
    Optional   -> AppT (ConT ''Maybe) <$> cddlTypeToTH ty
    ZeroOrMore -> AppT (ConT ''V.Vector) <$> cddlTypeToTH ty
    OneOrMore  -> AppT (ConT ''V.Vector) <$> cddlTypeToTH ty
    Once       -> cddlTypeToTH ty
  let bangTy = Bang NoSourceUnpackedness SourceStrict
  pure (accName, bangTy, hsTy)

-- ---------------------------------------------------------------------------
-- TH: Array -> newtype
-- ---------------------------------------------------------------------------

deriveCDDLArrayTH :: Text -> [CDDLMember] -> Q [Dec]
deriveCDDLArrayTH name members = do
  let tyName = mkName (T.unpack name)
      conName = mkName (T.unpack name)
      accName = mkName (T.unpack ("un" <> name))
  innerTy <- case members of
    [CDDLMember _ inner _] -> cddlTypeToTH inner
    _ -> [t| () |]
  let vecTy = AppT (ConT ''V.Vector) innerTy
      bangTy = Bang NoSourceUnpackedness SourceStrict
      fieldDec = (accName, bangTy, vecTy)
      dataDec = NewtypeD [] tyName [] Nothing
        (RecC conName [fieldDec])
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  pure [dataDec]

-- ---------------------------------------------------------------------------
-- TH: Choice -> sum type
-- ---------------------------------------------------------------------------

deriveCDDLChoiceTH :: Text -> [CDDLType] -> Q [Dec]
deriveCDDLChoiceTH name alts = do
  let tyName = mkName (T.unpack name)
  cons <- mapM (\(i, alt) -> do
    argTy <- cddlTypeToTH alt
    let conNm = mkName (T.unpack (name <> "Alt" <> T.pack (show (i :: Int))))
        bangTy = (Bang NoSourceUnpackedness SourceStrict, argTy)
    pure (NormalC conNm [bangTy])
    ) (zip [0..] alts)
  let dataDec = DataD [] tyName [] Nothing cons
        [ DerivClause (Just StockStrategy) [ConT ''Show, ConT ''Eq] ]
  pure [dataDec]

-- ---------------------------------------------------------------------------
-- TH type mapping
-- ---------------------------------------------------------------------------

cddlTypeToTH :: CDDLType -> Q Type
cddlTypeToTH = \case
  CTUint  -> [t| Word64 |]
  CTNint  -> [t| Int64 |]
  CTInt   -> [t| Int64 |]
  CTTstr  -> [t| Text |]
  CTBstr  -> [t| ByteString |]
  CTFloat -> [t| Double |]
  CTBool  -> [t| Bool |]
  CTNil   -> [t| () |]
  CTAny   -> [t| () |]
  CTRef n -> pure (ConT (mkName (T.unpack (upperFirst n))))
  CTTagged _ inner -> cddlTypeToTH inner
  _       -> [t| () |]

-- ---------------------------------------------------------------------------
-- Name helpers
-- ---------------------------------------------------------------------------

lowerFirst :: Text -> Text
lowerFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toLower c) rest
  Nothing -> s

upperFirst :: Text -> Text
upperFirst s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) rest
  Nothing -> s

snakeToCamel :: Text -> Text
snakeToCamel t =
  let parts = T.splitOn "_" t
  in case parts of
    [] -> t
    (p:ps) -> T.concat (lowerFirst p : map titleCase ps)

titleCase :: Text -> Text
titleCase s = case T.uncons s of
  Just (c, rest) -> T.cons (toUpper c) (T.toLower rest)
  Nothing -> s
