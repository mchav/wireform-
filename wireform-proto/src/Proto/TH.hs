{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
-- | Template Haskell support for generating protobuf types at compile time.
--
-- == Basic usage
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Proto.TH
--
-- \$(loadProto "path/to/message.proto")
-- @
--
-- == Custom representations
--
-- @
-- \$(loadProtoWith (defaultLoadOpts
--       { loRepConfig = defaultRepConfig
--           { rcFieldOverrides = Map.fromList
--               [ (("Person","name"), defaultFieldRep { frString = ShortTextRep })
--               ]
--           }
--       })
--     "path/to/file.proto")
-- @
--
-- == Codegen hooks
--
-- Use 'loTHHooks' to register 'Proto.CodeGen.Hooks.THHooks' that produce
-- extra declarations based on proto attributes:
--
-- @
-- import Proto.TH
-- import Proto.CodeGen.Hooks
-- import Language.Haskell.TH
--
-- -- Generate a @describeX :: String@ function for every message
-- descrHook :: THHooks
-- descrHook = mempty
--   { thOnMessage = \\ctx -> do
--       let name = mkName ("describe" <> T.unpack (mhcHsTypeName ctx))
--       sig  \<- sigD name [t| String |]
--       body \<- valD (varP name)
--                (normalB (litE (stringL (T.unpack (mhcFqProtoName ctx))))) []
--       pure [sig, body]
--   }
--
-- \$(loadProtoWith defaultLoadOpts { loTHHooks = descrHook } "my.proto")
-- -- Now @describeMyMessage :: String@ is in scope.
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
import qualified Data.Char
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
import Proto.Parser (parseProtoFile, renderParseError)
import Proto.CodeGen (hsTypeName, snakeToCamel, snakeToPascal, lowerFirst, escapeReserved)
import Proto.CodeGen.Hooks
import qualified Proto.Encode as Encode
import qualified Proto.Decode as Decode
import qualified Proto.Extension as Ext
import Proto.Wire (Tag(..))
import qualified Proto.Wire.Encode as WE
import Proto.Repr

-- | Produce a Haskell-valid record-field name from a proto field.
-- The proto-side name is snake_cased (@file_path@, @num_rows@); we
-- convert to camelCase and escape reserved keywords with a
-- trailing prime (@data@ → @data'@, @type@ → @type'@, …). Without
-- the escape, TH splices produce @data :: Foo -> Bar@ which is a
-- parse error.
hsFieldName :: Text -> Text
hsFieldName = escapeReserved . snakeToCamel

hsEnumCon :: Text -> Text -> Text
hsEnumCon _enumName = snakeToPascal

-- | Options for compile-time proto loading.
--
-- Use 'loTHHooks' to register hooks that produce extra TH declarations
-- based on proto attributes:
--
-- @
-- \$(loadProtoWith defaultLoadOpts
--       { loTHHooks = myTHHooks }
--     \"path/to/file.proto\")
-- @
data LoadOpts = LoadOpts
  { loIncludeDirs :: [FilePath]
  , loRepConfig   :: RepConfig
  , loTHHooks     :: THHooks
  }

defaultLoadOpts :: LoadOpts
defaultLoadOpts = LoadOpts
  { loIncludeDirs = ["proto/", "."]
  , loRepConfig   = defaultRepConfig
  , loTHHooks     = defaultTHHooks
  }

loadProto :: FilePath -> Q [Dec]
loadProto = loadProtoWith defaultLoadOpts

loadProtoWith :: LoadOpts -> FilePath -> Q [Dec]
loadProtoWith opts path = do
  addDependentFile path
  contents <- runIO (TIO.readFile path)
  case parseProtoFile path contents of
    Left err -> fail (renderParseError err)
    Right pf -> do
      let hooks = loTHHooks opts
          fileCtx = FileHookCtx
            { fhcProtoFile   = pf
            , fhcModuleName  = T.pack path
            , fhcFileOptions = protoOptions pf
            }
      decls <- protoFileToDecls' (loRepConfig opts) hooks pf
      hookDecls <- thOnFile hooks fileCtx
      pure (decls <> hookDecls)

protoFileToDecls :: ProtoFile -> Q [Dec]
protoFileToDecls = protoFileToDecls' defaultRepConfig defaultTHHooks

protoFileToDecls' :: RepConfig -> THHooks -> ProtoFile -> Q [Dec]
protoFileToDecls' cfg hooks pf = concat <$> mapM (topLevelToDecls cfg hooks) (protoTopLevels pf)

topLevelToDecls :: RepConfig -> THHooks -> TopLevel -> Q [Dec]
topLevelToDecls cfg hooks = \case
  TLMessage msg -> messageToDecls' cfg hooks msg
  TLEnum ed     -> enumToDecls' hooks ed
  TLExtend owner fields -> extendToDecls owner fields
  _             -> pure []

messageToDecls :: MessageDef -> Q [Dec]
messageToDecls = messageToDecls' defaultRepConfig defaultTHHooks

messageToDecls' :: RepConfig -> THHooks -> MessageDef -> Q [Dec]
messageToDecls' cfg hooks msg = do
  let tyName = mkName (T.unpack (hsTypeName (msgName msg)))
      fields = extractMessageFields cfg (msgName msg) (msgElements msg)
      scope = [msgName msg]
      hookCtx = MessageHookCtx
        { mhcMessageDef  = msg
        , mhcScope       = scope
        , mhcHsTypeName  = hsTypeName (msgName msg)
        , mhcFqProtoName = msgName msg
        , mhcOptions     = messageOptions msg
        }

  nestedDecls <- concat <$> mapM (\case
    MEMessage inner -> messageToDecls' cfg hooks inner
    MEEnum ed       -> enumToDecls' hooks ed
    _               -> pure []) (msgElements msg)

  dataDec <- mkDataDec tyName fields
  defaultDec <- mkDefaultDec tyName fields
  encodeDec <- mkEncodeInstance tyName fields
  decodeDec <- mkDecodeInstance tyName fields
  sizeDec <- mkSizeInstance tyName fields
  hasExtDec <- mkHasExtensionsInstance tyName (msgName msg)
  hookDecls <- thOnMessage hooks hookCtx

  addModFinalizer (putDoc (DeclDoc tyName) (messageHaddock msg fields))
  let defName = mkName ("default" <> nameBase tyName)
  addModFinalizer (putDoc (DeclDoc defName)
    ("Default value for @" <> T.unpack (msgName msg)
    <> "@ with all fields at their proto default values."))

  pure (nestedDecls <> [dataDec] <> defaultDec <> encodeDec <> decodeDec
         <> sizeDec <> hasExtDec <> hookDecls)

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
  let unknownFieldEntry = mkUnknownFieldsField tyName
  let con = recC tyName (fmap pure (recFields <> [unknownFieldEntry]))
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

-- | The field name used by TH-generated records for their
-- unknown-fields list. Derived from the type name by
-- lower-casing the leading character and appending
-- @UnknownFields@ (matching the convention the pure-Doc
-- codegen in "Proto.CodeGen" follows).
unknownFieldsName :: Name -> Name
unknownFieldsName tyName =
  mkName (lowerFirstStr (nameBase tyName) <> "UnknownFields")

lowerFirstStr :: String -> String
lowerFirstStr (c:rest) = Data.Char.toLower c : rest
lowerFirstStr []       = []

-- | The VarBangType entry for a TH record's unknown-fields slot.
mkUnknownFieldsField :: Name -> VarBangType
mkUnknownFieldsField tyName =
  ( unknownFieldsName tyName
  , Bang NoSourceUnpackedness SourceStrict
  , AppT ListT (ConT ''Decode.UnknownField)
  )

fieldTypeToTH :: Maybe FieldLabel -> FieldType -> FieldRep -> Q Type
fieldTypeToTH lbl ft rep = case lbl of
  Just Repeated -> repeatedTypeQ (frRepeated rep) (fieldTypeInnerQWithRep rep ft)
  Just Optional -> optionalTypeQ (frOptional rep) (fieldTypeInnerQWithRep rep ft)
  _             -> fieldTypeInnerQWithRep rep ft

-- | 'fieldTypeInnerQ' ignores the per-field 'FieldRep'; used for map
-- keys/values where we haven't threaded a rep config through yet.
-- Prefer 'fieldTypeInnerQWithRep' for message fields so that
-- custom bytes/string representations (@frBytes@ / @frString@)
-- actually materialize in the generated Haskell type.
fieldTypeInnerQ :: FieldType -> Q Type
fieldTypeInnerQ = fieldTypeInnerQWithRep defaultFieldRep

fieldTypeInnerQWithRep :: FieldRep -> FieldType -> Q Type
fieldTypeInnerQWithRep rep = \case
  FTScalar SString -> stringTypeQ (frString rep)
  FTScalar SBytes  -> bytesTypeQ (frBytes rep)
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
  -- Every TH-generated record now carries an empty unknown-fields
  -- list. Proto2 extensions travel through this field; see
  -- "Proto.Extension" for the typed accessors.
  let ufDefault = (unknownFieldsName tyName, ListE [])
  body <- valD (varP defName)
    (normalB (recConE tyName (fmap pure (defFields <> [ufDefault])))) []
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
  let ufName = unknownFieldsName tyName
      ufExpr = [| Decode.encodeUnknownFields ($(varE ufName) $(varE msgVar)) |]
      body = case fields of
        [] -> ufExpr
        _  ->
          let fieldBody =
                foldl1 (\a b -> [| $a <> $b |]) (fmap (mkFieldEncode msgVar) fields)
          in [| $fieldBody <> $ufExpr |]
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
  FTScalar SString -> encodeStringFnQ (frString rep)
  FTScalar SBytes  -> encodeBytesFnQ  (frBytes  rep)
  FTScalar s       -> encodeScalarFnQ s
  FTNamed _        -> varE 'Encode.encodeFieldMessage

-- | Encoder dispatch for non-string / non-bytes scalars. String and
-- bytes dispatch through 'encodeStringFnQ' / 'encodeBytesFnQ' in
-- 'encodeFnQ' because their Haskell-side representation is
-- configurable per-field ('frString' / 'frBytes').
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
  -- Accumulator names: one per declared field, plus a dedicated
  -- accumulator for unknown / extension fields at the tail.
  let accNames = fmap (\(i, _) -> mkName ("acc_" <> show i))
                      (zip [(0::Int)..] fields)
      ufAcc   = mkName "acc_unknown_"
      allAccs = accNames <> [ufAcc]
      loopName = mkName "loop"
      ufField  = (unknownFieldsName tyName, AppE (VarE 'reverse) (VarE ufAcc))
      declaredFields = zipWith (\fs accN ->
        (mkName (T.unpack (hsFieldName (fsFieldName fs))), VarE accN))
        fields accNames
      recExpr = recConE tyName (fmap pure (declaredFields <> [ufField]))

  let mkLoopCall :: Int -> Q Exp -> Q Exp
      mkLoopCall i valExpr = appsE (varE loopName :
            fmap (\(j, n) -> if j == i then valExpr else varE n)
              (zip [(0::Int)..] accNames)
        <> [varE ufAcc])
      passThruLoop :: Q Exp
      passThruLoop = appsE (varE loopName : fmap varE allAccs)

  let fieldCases = concatMap (\(i, fs) ->
        case fs of
          FSField _ num lbl ft rep ->
            let loopCall = mkLoopCall i $ case lbl of
                  Just Repeated -> snocRepeatedQ (frRepeated rep) (varE (accNames !! i))
                  Just Optional -> [| Just v |]
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

  -- Skip case: capture the field as an unknown-field entry rather
  -- than dropping it, so proto2 extensions and forward-compatible
  -- unknown tags both round-trip through this decoder.
  let captureCall =
        appsE (varE loopName :
               fmap varE accNames
            <> [[| (uf : $(varE ufAcc)) |]])
      skipCase = match wildP
        (normalB
          [| Decode.captureUnknownField
               $(varE (mkName "fn"))
               $(varE (mkName "wt"))
             >>= \uf -> $captureCall |])
        []

  let loopBody =
        [| Decode.getTagOr >>= \mt -> $(caseE (varE (mkName "mt"))
            [ match (conP 'Nothing []) (normalB [| pure $recExpr |]) []
            , match (conP 'Just [conP 'Tag [varP (mkName "fn"), varP (mkName "wt")]])
                (normalB (caseE (varE (mkName "fn")) (fieldCases <> [skipCase]))) []
            ]) |]

  let defaults = fmap defaultValueExpr fields
      loopDef = funD loopName
        [clause (fmap varP allAccs) (normalB loopBody) []]
      decoderBody = letE [loopDef]
        (appsE (varE loopName : defaults <> [[| [] |]]))

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
decodeFnQ ft rep = case ft of
  FTScalar SDouble -> varE 'Decode.decodeFieldDouble
  FTScalar SFloat  -> varE 'Decode.decodeFieldFloat
  FTScalar SBool   -> varE 'Decode.decodeFieldBool
  FTScalar SString -> decodeStringFnQ (frString rep)
  FTScalar SBytes  -> decodeBytesFnQ  (frBytes  rep)
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
  let ufName = unknownFieldsName tyName
      ufSizeExpr = [| Decode.unknownFieldsSize ($(varE ufName) $(varE msgVar)) |]
      body = case fields of
        [] -> ufSizeExpr
        _  ->
          let fieldBody = foldl1 (\a b -> [| $a + $b |])
                                 (fmap (mkFieldSize msgVar) fields)
          in [| $fieldBody + $ufSizeExpr |]
  inst <- instanceD (pure [])
    [t| Encode.MessageSize $(conT tyName) |]
    [funD 'Encode.messageSize [clause [varP msgVar] (normalB body) []]]
  pure [inst]

mkFieldSize :: Name -> FieldSpec -> Q Exp
mkFieldSize msgVar (FSField name num lbl ft rep) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
      fn = litE (integerL (fromIntegral num))
  case lbl of
    Just Repeated -> [| $(foldRepeatedSizeQ (frRepeated rep)) ($(sizeFnQ ft rep) $fn) $accessor |]
    Just Optional -> [| maybe 0 ($(sizeFnQ ft rep) $fn) $accessor |]
    _ -> [| if $(defaultCheckQ ft rep accessor)
            then 0
            else $(sizeFnQ ft rep) $fn $accessor |]
mkFieldSize msgVar (FSMap name num _ _) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
  [| if Map.null $accessor then 0 else Map.size $accessor * 10 |]
mkFieldSize msgVar (FSOneof name _) = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName name)))) (varE msgVar)
  [| maybe 0 Encode.messageSize $accessor |]

sizeFnQ :: FieldType -> FieldRep -> Q Exp
sizeFnQ ft rep = case ft of
  FTScalar SDouble -> [| \fn _ -> WE.fieldDoubleSize fn |]
  FTScalar SFloat  -> [| \fn _ -> WE.fieldFloatSize fn |]
  FTScalar SBool   -> [| \fn _ -> WE.fieldBoolSize fn |]
  FTScalar SString -> sizeStringFnQ (frString rep)
  FTScalar SBytes  -> sizeBytesFnQ  (frBytes  rep)
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
enumToDecls = enumToDecls' defaultTHHooks

enumToDecls' :: THHooks -> EnumDef -> Q [Dec]
enumToDecls' hooks ed = do
  let tyName = mkName (T.unpack (hsTypeName (enumName ed)))
      cons = fmap (\ev ->
        normalC (mkName (T.unpack (hsEnumCon (enumName ed) (evName ev)))) []
        ) (enumValues ed)
      hookCtx = EnumHookCtx
        { ehcEnumDef    = ed
        , ehcScope      = [enumName ed]
        , ehcHsTypeName = hsTypeName (enumName ed)
        , ehcOptions    = enumOptions ed
        }
  dataDec <- dataD (pure []) tyName [] Nothing cons
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Ord, conT ''Generic]]
  hookDecls <- thOnEnum hooks hookCtx

  addModFinalizer $ putDoc (DeclDoc tyName) (enumHaddock ed)

  pure ([dataDec] <> hookDecls)

enumHaddock :: EnumDef -> String
enumHaddock ed =
  "Protobuf enum @" <> T.unpack (enumName ed) <> "@.\n\n"
  <> "Values:\n\n"
  <> concatMap (\ev ->
      "* @" <> T.unpack (evName ev) <> "@ = " <> show (evNumber ev) <> "\n"
    ) (enumValues ed)

-- ============================================================
-- Proto2 extensions
-- ============================================================

-- | Emit the @HasExtensions@ instance for a generated record. The
-- instance's two methods read and write the record's
-- unknown-fields slot, which is where extension payloads (and any
-- other forward-compatible unknown tags) live.
--
-- The class and method names are referenced via their bound 'Name's
-- ('Ext.messageUnknownFields' / 'Ext.setMessageUnknownFields') so
-- the generated splice doesn't require the user's module to
-- already have @import qualified Proto.Extension@ in scope.
mkHasExtensionsInstance :: Name -> Text -> Q [Dec]
mkHasExtensionsInstance tyName _protoName = do
  let ufName = unknownFieldsName tyName
  msgVar <- newName "msg"
  ufsVar <- newName "ufs"
  inst <- instanceD (pure [])
    [t| Ext.HasExtensions $(conT tyName) |]
    [ funD 'Ext.messageUnknownFields
        [clause [] (normalB (varE ufName)) []]
    , funD 'Ext.setMessageUnknownFields
        [clause [varP ufsVar, varP msgVar]
          (normalB (recUpdE (varE msgVar)
             [pure (ufName, VarE ufsVar)])) []]
    ]
  pure [inst]

-- | Handle a @TLExtend@ top-level declaration: emit one
-- 'Proto.Extension.Extension' binding per extension field in the
-- block. The owning message's 'HasExtensions' instance is emitted
-- separately by 'messageToDecls''.
extendToDecls :: Text -> [FieldDef] -> Q [Dec]
extendToDecls ownerProtoName fields =
  concat <$> mapM (oneExtensionDec ownerHsName ownerPrefix) fields
  where
    ownerHsName = mkName (T.unpack (hsTypeName (lastProtoSegment ownerProtoName)))
    ownerPrefix = lowerFirst (hsTypeName (lastProtoSegment ownerProtoName))

lastProtoSegment :: Text -> Text
lastProtoSegment t = case T.splitOn "." t of
  []    -> t
  parts -> last parts

-- | Generate the two declarations for one extension field: a
-- type signature plus a value binding.
oneExtensionDec :: Name -> Text -> FieldDef -> Q [Dec]
oneExtensionDec ownerHs ownerPrefix fd =
  case thExtensionPayload (fieldLabel fd) (fieldType fd) of
    Nothing ->
      -- Repeated / group / unsupported shape: skip silently at the
      -- TH level (the non-TH 'Proto.CodeGen' path emits a warning
      -- comment; in TH we don't have a comment channel).
      pure []
    Just (hsTy, extConName) -> do
      let extName = mkName
            (T.unpack
               (escapeReserved
                  (ownerPrefix <> upperFirst (snakeToCamel (fieldName fd)))))
          num = unFieldNumber (fieldNumber fd)
          extConE = conE (mkName ("Ext." <> T.unpack extConName))
      sig <- sigD extName
        [t| Ext.Extension $(conT ownerHs) $(pure hsTy) |]
      body <- valD (varP extName)
        (normalB [| Ext.Extension
                      { Ext.extNumber = $(litE (IntegerL (fromIntegral num)))
                      , Ext.extType   = $extConE
                      } |]) []
      pure [sig, body]

-- | Map a proto @(label, type)@ to the Haskell type + the
-- corresponding 'Proto.Extension.ExtensionType' constructor name.
-- 'Nothing' for unsupported shapes.
thExtensionPayload :: Maybe FieldLabel -> FieldType -> Maybe (Type, Text)
thExtensionPayload (Just Repeated) _ = Nothing
thExtensionPayload _ (FTScalar s)    = Just $ case s of
  SDouble   -> (ConT ''Double,   "ExtDouble")
  SFloat    -> (ConT ''Float,    "ExtFloat")
  SInt32    -> (ConT ''Int32,    "ExtInt32")
  SInt64    -> (ConT ''Int64,    "ExtInt64")
  SUInt32   -> (ConT ''Word32,   "ExtUInt32")
  SUInt64   -> (ConT ''Word64,   "ExtUInt64")
  SSInt32   -> (ConT ''Int32,    "ExtSInt32")
  SSInt64   -> (ConT ''Int64,    "ExtSInt64")
  SFixed32  -> (ConT ''Word32,   "ExtFixed32")
  SFixed64  -> (ConT ''Word64,   "ExtFixed64")
  SSFixed32 -> (ConT ''Int32,    "ExtSFixed32")
  SSFixed64 -> (ConT ''Int64,    "ExtSFixed64")
  SBool     -> (ConT ''Bool,     "ExtBool")
  SString   -> (ConT ''Text,     "ExtString")
  SBytes    -> (ConT ''ByteString, "ExtBytes")
thExtensionPayload _ (FTNamed _) =
  Just (ConT ''ByteString, "ExtMessage")

upperFirst :: Text -> Text
upperFirst t = case T.uncons t of
  Just (c, rest) -> T.cons (Data.Char.toUpper c) rest
  Nothing        -> t
