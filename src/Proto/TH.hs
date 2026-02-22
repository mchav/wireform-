{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- | Template Haskell support for generating protobuf types at compile time.
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Proto.TH
--
-- -- Default (strict Text, strict ByteString, Vector):
-- \$(loadProto "path/to/message.proto")
--
-- -- Custom representation:
-- \$(loadProtoWith (defaultLoadOpts
--       { loRepConfig = defaultRepConfig
--           { rcFieldOverrides = Map.fromList
--               [ (("Person","name"), defaultFieldRep { frString = ShortTextRep })
--               , (("Blob","data"), defaultFieldRep { frBytes = LazyBytesRep })
--               , (("Config","tags"), defaultFieldRep { frRepeated = ListRep })
--               ]
--           }
--       })
--     "path/to/file.proto")
-- @
module Proto.TH
  ( loadProto
  , loadProtoWith
  , LoadOpts (..)
  , defaultLoadOpts
  , protoFileToDecls
  , messageToDecls
  , enumToDecls
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, addModFinalizer)

import Proto.AST
import Proto.Parser (parseProtoFile)
import Proto.CodeGen (hsTypeName, snakeToCamel, snakeToPascal, lowerFirst, escapeReserved)
import qualified Proto.Encode as Encode
import qualified Proto.Decode as Decode
import Proto.Wire (Tag(..))
import qualified Proto.Wire.Encode as WE
import Proto.Repr

hsFieldName :: Text -> Text
hsFieldName = snakeToCamel

hsEnumCon :: Text -> Text -> Text
hsEnumCon _enumName = snakeToPascal

-- | Options for compile-time proto loading.
data LoadOpts = LoadOpts
  { loIncludeDirs :: [FilePath]
  , loRepConfig   :: RepConfig
  } deriving stock (Show, Eq)

defaultLoadOpts :: LoadOpts
defaultLoadOpts = LoadOpts
  { loIncludeDirs = ["proto/", "."]
  , loRepConfig   = defaultRepConfig
  }

loadProto :: FilePath -> Q [Dec]
loadProto = loadProtoWith defaultLoadOpts

loadProtoWith :: LoadOpts -> FilePath -> Q [Dec]
loadProtoWith opts path = do
  addDependentFile path
  contents <- runIO (TIO.readFile path)
  case parseProtoFile path contents of
    Left err -> fail ("Proto parse error in " <> path <> ": " <> show err)
    Right pf -> protoFileToDecls' (loRepConfig opts) pf

protoFileToDecls :: ProtoFile -> Q [Dec]
protoFileToDecls = protoFileToDecls' defaultRepConfig

protoFileToDecls' :: RepConfig -> ProtoFile -> Q [Dec]
protoFileToDecls' cfg pf = concat <$> mapM (topLevelToDecls cfg) (protoTopLevels pf)

topLevelToDecls :: RepConfig -> TopLevel -> Q [Dec]
topLevelToDecls cfg = \case
  TLMessage msg -> messageToDecls' cfg msg
  TLEnum ed     -> enumToDecls ed
  _             -> pure []

messageToDecls :: MessageDef -> Q [Dec]
messageToDecls = messageToDecls' defaultRepConfig

messageToDecls' :: RepConfig -> MessageDef -> Q [Dec]
messageToDecls' cfg msg = do
  let tyName = mkName (T.unpack (hsTypeName (msgName msg)))
      fields = extractMessageFields cfg (msgName msg) (msgElements msg)

  nestedDecls <- concat <$> mapM (\case
    MEMessage inner -> messageToDecls' cfg inner
    MEEnum ed       -> enumToDecls ed
    _               -> pure []) (msgElements msg)

  dataDec <- mkDataDec tyName fields
  defaultDec <- mkDefaultDec tyName fields
  encodeDec <- mkEncodeInstance tyName fields
  decodeDec <- mkDecodeInstance tyName fields
  sizeDec <- mkSizeInstance tyName fields

  -- Haddock documentation via TH putDoc (deferred via addModFinalizer)
  addModFinalizer (putDoc (DeclDoc tyName) (messageHaddock msg fields))
  let defName = mkName ("default" <> nameBase tyName)
  addModFinalizer (putDoc (DeclDoc defName)
    ("Default value for @" <> T.unpack (msgName msg)
    <> "@ with all fields at their proto default values."))

  pure (nestedDecls <> [dataDec] <> defaultDec <> encodeDec <> decodeDec <> sizeDec)

messageHaddock :: MessageDef -> [FieldSpec] -> String
messageHaddock msg fields =
  "Protobuf message @" <> T.unpack (msgName msg) <> "@.\n\n"
  <> "Fields:\n\n"
  <> concatMap fieldHaddock fields

fieldHaddock :: FieldSpec -> String
fieldHaddock (FSField name num lbl ft _) =
  "* @" <> T.unpack name <> "@ ("
  <> labelStr lbl
  <> fieldTypeStr ft <> ", field "
  <> show num <> ")\n"
fieldHaddock (FSMap name num kt vt) =
  "* @" <> T.unpack name <> "@ (map<"
  <> scalarStr kt <> ", " <> fieldTypeStr vt <> ">, field "
  <> show num <> ")\n"
fieldHaddock (FSOneof name ofs) =
  "* @" <> T.unpack name <> "@ (oneof, "
  <> show (length ofs) <> " variants)\n"

labelStr :: Maybe FieldLabel -> String
labelStr Nothing = ""
labelStr (Just Optional) = "optional "
labelStr (Just Required) = "required "
labelStr (Just Repeated) = "repeated "

fieldTypeStr :: FieldType -> String
fieldTypeStr (FTScalar s) = scalarStr s
fieldTypeStr (FTNamed n) = T.unpack n

scalarStr :: ScalarType -> String
scalarStr SDouble = "double"; scalarStr SFloat = "float"
scalarStr SInt32 = "int32"; scalarStr SInt64 = "int64"
scalarStr SUInt32 = "uint32"; scalarStr SUInt64 = "uint64"
scalarStr SSInt32 = "sint32"; scalarStr SSInt64 = "sint64"
scalarStr SFixed32 = "fixed32"; scalarStr SFixed64 = "fixed64"
scalarStr SSFixed32 = "sfixed32"; scalarStr SSFixed64 = "sfixed64"
scalarStr SBool = "bool"; scalarStr SString = "string"; scalarStr SBytes = "bytes"

-- A resolved field spec carrying the concrete representation choices.
data FieldSpec
  = FSField
    { fsName    :: Text
    , fsNum     :: Int
    , fsLabel   :: Maybe FieldLabel
    , fsType    :: FieldType
    , fsRep     :: FieldRep
    }
  | FSMap
    { fsName    :: Text
    , fsNum     :: Int
    , fsMapKey  :: ScalarType
    , fsMapVal  :: FieldType
    }
  | FSOneof
    { fsName    :: Text
    , fsOneofFields :: [OneofField]
    }

fsFieldName :: FieldSpec -> Text
fsFieldName (FSField n _ _ _ _) = n
fsFieldName (FSMap n _ _ _) = n
fsFieldName (FSOneof n _) = n

extractMessageFields :: RepConfig -> Text -> [MessageElement] -> [FieldSpec]
extractMessageFields cfg msgN = concatMap go
  where
    go (MEField fd) = [FSField
      { fsName  = fieldName fd
      , fsNum   = unFieldNumber (fieldNumber fd)
      , fsLabel = fieldLabel fd
      , fsType  = fieldType fd
      , fsRep   = lookupFieldRep msgN (fieldName fd) cfg
      }]
    go (MEMapField mf) = [FSMap
      { fsName   = mapFieldName mf
      , fsNum    = unFieldNumber (mapFieldNum mf)
      , fsMapKey = mapKeyType mf
      , fsMapVal = mapValueType mf
      }]
    go (MEOneof od) = [FSOneof
      { fsName        = oneofName od
      , fsOneofFields = oneofFields od
      }]
    go _ = []

-- Data type generation: uses fsRep to pick the Haskell type.

mkDataDec :: Name -> [FieldSpec] -> Q Dec
mkDataDec tyName fields = do
  recFields <- fmap concat (mapM mkField fields)
  let con = recC tyName (fmap pure recFields)
  dataD (pure []) tyName [] Nothing [con]
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]]
  where
    mkField :: FieldSpec -> Q [VarBangType]
    mkField (FSField name _ lbl ft rep) = do
      let fname = mkName (T.unpack (hsFieldName name))
      ty <- fieldTypeToTH lbl ft rep
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]
    mkField (FSMap name _ kt vt) = do
      let fname = mkName (T.unpack (hsFieldName name))
      kty <- scalarToTH kt
      vty <- fieldTypeInnerQ vt
      t <- appT (appT (conT ''Map) (pure kty)) (pure vty)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, t)]
    mkField (FSOneof name _ofs) = do
      let fname = mkName (T.unpack (hsFieldName name))
          oneofTyName = mkName (T.unpack (hsTypeName name))
      ty <- appT (conT ''Maybe) (conT oneofTyName)
      pure [(fname, Bang NoSourceUnpackedness SourceStrict, ty)]

fieldTypeToTH :: Maybe FieldLabel -> FieldType -> FieldRep -> Q Type
fieldTypeToTH lbl ft rep = case lbl of
  Just Repeated -> repeatedTypeQ (frRepeated rep) (fieldTypeInnerQ ft)
  Just Optional -> optionalTypeQ (frOptional rep) (fieldTypeInnerQ ft)
  _             -> fieldTypeInnerQ ft

fieldTypeInnerQ :: FieldType -> Q Type
fieldTypeInnerQ = \case
  FTScalar SString -> conT ''Text
  FTScalar SBytes  -> conT ''ByteString
  FTScalar s       -> scalarToTH s
  FTNamed n        -> conT (mkName (T.unpack (hsTypeName n)))

stringTypeQ :: StringRep -> Q Type
stringTypeQ = \case
  StrictTextRep -> conT ''Text
  LazyTextRep   -> conT ''TL.Text
  ShortTextRep  -> conT ''SBS.ShortByteString
  HsStringRep   -> conT ''String

bytesTypeQ :: BytesRep -> Q Type
bytesTypeQ = \case
  StrictBytesRep -> conT ''ByteString
  LazyBytesRep   -> conT ''BL.ByteString
  ShortBytesRep  -> conT ''SBS.ShortByteString

repeatedTypeQ :: RepeatedRep -> Q Type -> Q Type
repeatedTypeQ = \case
  VectorRep -> appT (conT ''V.Vector)
  ListRep   -> appT listT
  SeqRep    -> appT (conT ''Seq)

optionalTypeQ :: OptionalRep -> Q Type -> Q Type
optionalTypeQ = \case
  MaybeRep         -> appT (conT ''Maybe)
  FieldPresenceRep -> appT (conT ''Maybe)

scalarToTH :: ScalarType -> Q Type
scalarToTH = \case
  SDouble   -> conT ''Double
  SFloat    -> conT ''Float
  SInt32    -> conT ''Int32
  SInt64    -> conT ''Int64
  SUInt32   -> conT ''Word32
  SUInt64   -> conT ''Word64
  SSInt32   -> conT ''Int32
  SSInt64   -> conT ''Int64
  SFixed32  -> conT ''Word32
  SFixed64  -> conT ''Word64
  SSFixed32 -> conT ''Int32
  SSFixed64 -> conT ''Int64
  SBool     -> conT ''Bool
  SString   -> conT ''Text
  SBytes    -> conT ''ByteString

-- Default values depend on the representation.

mkDefaultDec :: Name -> [FieldSpec] -> Q [Dec]
mkDefaultDec tyName fields = do
  let defName = mkName ("default" <> nameBase tyName)
  sig <- sigD defName (conT tyName)
  defFields <- mapM (\fs -> do
    val <- defaultValueExpr fs
    pure (mkName (T.unpack (hsFieldName (fsFieldName fs))), val)) fields
  body <- valD (varP defName)
    (normalB (recConE tyName (fmap pure defFields))) []
  pure [sig, body]

defaultValueExpr :: FieldSpec -> Q Exp
defaultValueExpr (FSField _ _ lbl ft rep) = case lbl of
  Just Repeated -> emptyRepeatedQ (frRepeated rep)
  Just Optional -> conE 'Nothing
  _ -> case ft of
    FTScalar SBool   -> conE 'False
    FTScalar SString -> emptyStringQ (frString rep)
    FTScalar SBytes  -> emptyBytesQ (frBytes rep)
    FTScalar _       -> litE (integerL 0)
    FTNamed _        -> conE 'Nothing
defaultValueExpr (FSMap {}) = [| Map.empty |]
defaultValueExpr (FSOneof _ _) = conE 'Nothing

emptyRepeatedQ :: RepeatedRep -> Q Exp
emptyRepeatedQ = \case
  VectorRep -> [| V.empty |]
  ListRep   -> [| [] |]
  SeqRep    -> [| Seq.empty |]

emptyStringQ :: StringRep -> Q Exp
emptyStringQ = \case
  StrictTextRep -> [| T.empty |]
  LazyTextRep   -> [| TL.empty |]
  ShortTextRep  -> [| SBS.empty |]
  HsStringRep   -> [| "" :: String |]

emptyBytesQ :: BytesRep -> Q Exp
emptyBytesQ = \case
  StrictBytesRep -> [| BS.empty |]
  LazyBytesRep   -> [| BL.empty |]
  ShortBytesRep  -> [| SBS.empty |]

-- Encode: the generated buildMessage dispatches based on the field's rep.

mkEncodeInstance :: Name -> [FieldSpec] -> Q [Dec]
mkEncodeInstance tyName fields = do
  msgVar <- newName "msg"
  let body = case fields of
        [] -> varE 'mempty
        _  -> foldl1 (\a b -> [| $a <> $b |]) (fmap (mkFieldEncode msgVar) fields)
  inst <- instanceD (pure [])
    [t| Encode.MessageEncode $(conT tyName) |]
    [funD 'Encode.buildMessage [clause [varP msgVar] (normalB body) []]]
  pure [inst]

mkFieldEncode :: Name -> FieldSpec -> Q Exp
mkFieldEncode msgVar (FSField name num lbl ft rep) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
      fn = litE (integerL (fromIntegral num))
  case lbl of
    Just Repeated -> [| $(foldRepeatedQ (frRepeated rep)) ($(encodeFnQ ft rep) $fn) $accessor |]
    Just Optional -> [| maybe mempty ($(encodeFnQ ft rep) $fn) $accessor |]
    _ -> [| if $(defaultCheckQ ft rep accessor)
            then mempty
            else $(encodeFnQ ft rep) $fn $accessor |]
mkFieldEncode msgVar (FSMap name num kt vt) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
      fn = litE (integerL (fromIntegral num))
  [| Map.foldlWithKey' (\acc k v ->
       acc <> Encode.encodeMapField $fn
         ($(encodeScalarFnQ kt) 1 k) ($(encodeFnQ vt defaultFieldRep) 2 v)) mempty $accessor |]
mkFieldEncode msgVar (FSOneof name ofs) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
  [| maybe mempty Encode.buildMessage $accessor |]

foldRepeatedQ :: RepeatedRep -> Q Exp
foldRepeatedQ = \case
  VectorRep -> [| \f -> V.foldl' (\acc v -> acc <> f v) mempty |]
  ListRep   -> [| \f -> foldl (\acc v -> acc <> f v) mempty |]
  SeqRep    -> [| \f -> foldl (\acc v -> acc <> f v) mempty |]

encodeFnQ :: FieldType -> FieldRep -> Q Exp
encodeFnQ ft rep = case ft of
  FTScalar s -> encodeScalarFnQ s
  FTNamed _  -> varE 'Encode.encodeFieldMessage

encodeScalarFnQ :: ScalarType -> Q Exp
encodeScalarFnQ = \case
  SDouble  -> varE 'Encode.encodeFieldDouble
  SFloat   -> varE 'Encode.encodeFieldFloat
  SBool    -> varE 'Encode.encodeFieldBool
  SString  -> varE 'Encode.encodeFieldString
  SBytes   -> varE 'Encode.encodeFieldBytes
  SUInt64  -> varE 'Encode.encodeFieldVarint
  _        -> [| \fieldNum val -> Encode.encodeFieldVarint fieldNum (fromIntegral val) |]

encodeStringFnQ :: StringRep -> Q Exp
encodeStringFnQ = \case
  StrictTextRep -> varE 'Encode.encodeFieldString
  LazyTextRep   -> varE 'encodeLazyText
  ShortTextRep  -> varE 'encodeShortByteString
  HsStringRep   -> varE 'encodeHsString

encodeBytesFnQ :: BytesRep -> Q Exp
encodeBytesFnQ = \case
  StrictBytesRep -> varE 'Encode.encodeFieldBytes
  LazyBytesRep   -> varE 'encodeLazyBytes
  ShortBytesRep  -> varE 'encodeShortBytes

defaultCheckQ :: FieldType -> FieldRep -> Q Exp -> Q Exp
defaultCheckQ ft rep accessor = case ft of
  FTScalar SBool   -> [| not $accessor |]
  FTScalar SString -> defaultCheckStringQ (frString rep) accessor
  FTScalar SBytes  -> defaultCheckBytesQ (frBytes rep) accessor
  FTScalar _       -> [| $accessor == 0 |]
  FTNamed _        -> [| isNothing $accessor |]

defaultCheckStringQ :: StringRep -> Q Exp -> Q Exp
defaultCheckStringQ rep accessor = case rep of
  StrictTextRep -> [| $accessor == T.empty |]
  LazyTextRep   -> [| $accessor == TL.empty |]
  ShortTextRep  -> [| $accessor == SBS.empty |]
  HsStringRep   -> [| null $accessor |]

defaultCheckBytesQ :: BytesRep -> Q Exp -> Q Exp
defaultCheckBytesQ rep accessor = case rep of
  StrictBytesRep -> [| BS.null $accessor |]
  LazyBytesRep   -> [| BL.null $accessor |]
  ShortBytesRep  -> [| SBS.null $accessor |]

-- Decode: convert from wire format to the chosen representation.

mkDecodeInstance :: Name -> [FieldSpec] -> Q [Dec]
mkDecodeInstance tyName fields = do
  let flatFields = concatMap flattenFieldSpec fields
      accNames = fmap (\(i, _) -> mkName ("acc_" <> show i)) (zip [(0::Int)..] fields)
      loopName = mkName "loop"
      recExpr = recConE tyName
        (zipWith (\fs accN ->
          pure (mkName (T.unpack (hsFieldName (fsFieldName fs))), VarE accN))
          fields accNames)

  let mkLoopCall :: Int -> Q Exp -> Q Exp
      mkLoopCall i valExpr = appsE (varE loopName :
            fmap (\(j, n) -> if j == i then valExpr else varE n)
              (zip [(0::Int)..] accNames))
      passThruLoop :: Q Exp
      passThruLoop = appsE (varE loopName : fmap varE accNames)

  let fieldCases = concatMap (\(i, fs) ->
        case fs of
          FSField _ num lbl ft rep ->
            let loopCall = mkLoopCall i $ case lbl of
                  Just Repeated -> snocRepeatedQ (frRepeated rep) (varE (accNames !! i))
                  _             -> varE (mkName "v")
            in [match (litP (integerL (fromIntegral num)))
                 (normalB [| $(decodeFnQ ft rep) >>= \v -> $loopCall |]) []]
          FSMap _ num _kt _vt ->
            [match (litP (integerL (fromIntegral num)))
              (normalB [| Decode.decodeFieldBytes >> $passThruLoop |]) []]
          FSOneof _ ofs ->
            fmap (\of' ->
              let ofNum = unFieldNumber (oneofFieldNumber of')
                  loopCall = mkLoopCall i (varE (mkName "v"))
              in match (litP (integerL (fromIntegral ofNum)))
                   (normalB [| $(decodeFnQ (oneofFieldType of') defaultFieldRep) >>= \v -> $loopCall |]) []
            ) ofs
        ) (zip [(0::Int)..] fields)

  let skipCase = match wildP (normalB
        [| Decode.skipField $(varE (mkName "wt")) >> $(appsE (varE loopName : fmap varE accNames)) |]
        ) []

  let loopBody =
        [| Decode.getTagOr >>= \mt -> $(caseE (varE (mkName "mt"))
            [ match (conP 'Nothing []) (normalB [| pure $recExpr |]) []
            , match (conP 'Just [conP 'Tag [varP (mkName "fn"), varP (mkName "wt")]])
                (normalB (caseE (varE (mkName "fn")) (fieldCases <> [skipCase]))) []
            ]) |]

  let defaults = fmap defaultValueExpr fields
      loopDef = funD loopName [clause (fmap varP accNames) (normalB loopBody) []]
      decoderBody = letE [loopDef] (appsE (varE loopName : defaults))

  inst <- instanceD (pure [])
    [t| Decode.MessageDecode $(conT tyName) |]
    [funD 'Decode.messageDecoder [clause [] (normalB decoderBody) []]]
  pure [inst]

flattenFieldSpec :: FieldSpec -> [(Int, FieldType)]
flattenFieldSpec (FSField _ num _ ft _) = [(num, ft)]
flattenFieldSpec (FSMap _ num _ _) = [(num, FTScalar SBytes)]
flattenFieldSpec (FSOneof _ ofs) = fmap (\of' -> (unFieldNumber (oneofFieldNumber of'), oneofFieldType of')) ofs

snocRepeatedQ :: RepeatedRep -> Q Exp -> Q Exp
snocRepeatedQ rep accQ = case rep of
  VectorRep -> [| V.snoc $accQ v |]
  ListRep   -> [| $accQ <> [v] |]
  SeqRep    -> [| $accQ Seq.|> v |]

decodeFnQ :: FieldType -> FieldRep -> Q Exp
decodeFnQ ft _rep = case ft of
  FTScalar SDouble -> varE 'Decode.decodeFieldDouble
  FTScalar SFloat  -> varE 'Decode.decodeFieldFloat
  FTScalar SBool   -> varE 'Decode.decodeFieldBool
  FTScalar SString -> varE 'Decode.decodeFieldString
  FTScalar SBytes  -> varE 'Decode.decodeFieldBytes
  FTScalar SUInt64 -> varE 'Decode.decodeFieldVarint
  FTScalar _       -> [| fromIntegral <$> Decode.decodeFieldVarint |]
  FTNamed _        -> varE 'Decode.decodeFieldMessage

decodeStringFnQ :: StringRep -> Q Exp
decodeStringFnQ = \case
  StrictTextRep -> varE 'Decode.decodeFieldString
  LazyTextRep   -> [| fmap TL.fromStrict Decode.decodeFieldString |]
  ShortTextRep  -> [| fmap (SBS.toShort . TE.encodeUtf8) Decode.decodeFieldString |]
  HsStringRep   -> [| fmap T.unpack Decode.decodeFieldString |]

decodeBytesFnQ :: BytesRep -> Q Exp
decodeBytesFnQ = \case
  StrictBytesRep -> varE 'Decode.decodeFieldBytes
  LazyBytesRep   -> [| fmap BL.fromStrict Decode.decodeFieldBytes |]
  ShortBytesRep  -> [| fmap SBS.toShort Decode.decodeFieldBytes |]

-- Size calculation.

mkSizeInstance :: Name -> [FieldSpec] -> Q [Dec]
mkSizeInstance tyName fields = do
  msgVar <- newName "msg"
  let body = case fields of
        [] -> litE (integerL 0)
        _  -> foldl1 (\a b -> [| $a + $b |]) (fmap (mkFieldSize msgVar) fields)
  inst <- instanceD (pure [])
    [t| Encode.MessageSize $(conT tyName) |]
    [funD 'Encode.messageSize [clause [varP msgVar] (normalB body) []]]
  pure [inst]

mkFieldSize :: Name -> FieldSpec -> Q Exp
mkFieldSize msgVar (FSField name num lbl ft rep) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
      fn = litE (integerL (fromIntegral num))
  case lbl of
    Just Repeated -> [| $(foldRepeatedSizeQ (frRepeated rep)) ($(sizeFnQ ft) $fn) $accessor |]
    Just Optional -> [| maybe 0 ($(sizeFnQ ft) $fn) $accessor |]
    _ -> [| if $(defaultCheckQ ft rep accessor)
            then 0
            else $(sizeFnQ ft) $fn $accessor |]
mkFieldSize msgVar (FSMap name num _ _) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
  [| if Map.null $accessor then 0 else Map.size $accessor * 10 |]
mkFieldSize msgVar (FSOneof name _) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
  [| maybe 0 Encode.messageSize $accessor |]

sizeFnQ :: FieldType -> Q Exp
sizeFnQ = \case
  FTScalar SDouble -> [| \fn _ -> WE.fieldDoubleSize fn |]
  FTScalar SFloat  -> [| \fn _ -> WE.fieldFloatSize fn |]
  FTScalar SBool   -> [| \fn _ -> WE.fieldBoolSize fn |]
  FTScalar SString -> varE 'WE.fieldTextSize
  FTScalar SBytes  -> varE 'WE.fieldBytesSize
  FTScalar SUInt64 -> varE 'WE.fieldVarintSize
  FTScalar _       -> [| \fn val -> WE.fieldVarintSize fn (fromIntegral val) |]
  FTNamed _        -> [| \fn val -> WE.fieldMessageSize fn (Encode.messageSize val) |]

foldRepeatedSizeQ :: RepeatedRep -> Q Exp
foldRepeatedSizeQ = \case
  VectorRep -> [| \f -> V.foldl' (\acc v -> acc + f v) (0 :: Int) |]
  ListRep   -> [| \f -> foldl (\acc v -> acc + f v) (0 :: Int) |]
  SeqRep    -> [| \f -> foldl (\acc v -> acc + f v) (0 :: Int) |]

sizeStringFnQ :: StringRep -> Q Exp
sizeStringFnQ = \case
  StrictTextRep -> varE 'WE.fieldTextSize
  LazyTextRep   -> [| \fn t -> WE.fieldTextSize fn (TL.toStrict t) |]
  ShortTextRep  -> [| \fn sbs -> WE.fieldBytesSize fn (SBS.fromShort sbs) |]
  HsStringRep   -> [| \fn s -> WE.fieldTextSize fn (T.pack s) |]

sizeBytesFnQ :: BytesRep -> Q Exp
sizeBytesFnQ = \case
  StrictBytesRep -> varE 'WE.fieldBytesSize
  LazyBytesRep   -> [| \fn lbs -> WE.fieldBytesSize fn (BL.toStrict lbs) |]
  ShortBytesRep  -> [| \fn sbs -> WE.fieldBytesSize fn (SBS.fromShort sbs) |]

enumToDecls :: EnumDef -> Q [Dec]
enumToDecls ed = do
  let tyName = mkName (T.unpack (hsTypeName (enumName ed)))
      cons = fmap (\ev ->
        normalC (mkName (T.unpack (hsEnumCon (enumName ed) (evName ev)))) []
        ) (enumValues ed)
  dataDec <- dataD (pure []) tyName [] Nothing cons
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Ord, conT ''Generic]]

  addModFinalizer $ putDoc (DeclDoc tyName) (enumHaddock ed)

  pure [dataDec]

enumHaddock :: EnumDef -> String
enumHaddock ed =
  "Protobuf enum @" <> T.unpack (enumName ed) <> "@.\n\n"
  <> "Values:\n\n"
  <> concatMap (\ev ->
      "* @" <> T.unpack (evName ev) <> "@ = " <> show (evNumber ev) <> "\n"
    ) (enumValues ed)
