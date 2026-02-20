{-# LANGUAGE TemplateHaskellQuotes #-}
-- | Template Haskell support for generating protobuf types at compile time.
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- {-\# LANGUAGE StrictData \#-}
-- {-\# LANGUAGE DeriveGeneric \#-}
-- {-\# LANGUAGE DerivingStrategies \#-}
-- import Proto.TH
--
-- \$(loadProto "path/to/message.proto")
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
import Data.Int (Int32, Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Word (Word32, Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)

import Proto.AST
import Proto.Parser (parseProtoFile)
import Proto.CodeGen.Types (hsTypeName, hsFieldName, hsEnumCon)
import qualified Proto.Encode as Encode
import qualified Proto.Decode as Decode
import Proto.Wire (Tag(..))
import qualified Proto.Wire.Encode as WE

data LoadOpts = LoadOpts
  { loIncludeDirs :: [FilePath]
  } deriving stock (Show, Eq)

defaultLoadOpts :: LoadOpts
defaultLoadOpts = LoadOpts { loIncludeDirs = ["proto/", "."] }

loadProto :: FilePath -> Q [Dec]
loadProto = loadProtoWith defaultLoadOpts

loadProtoWith :: LoadOpts -> FilePath -> Q [Dec]
loadProtoWith _opts path = do
  addDependentFile path
  contents <- runIO (TIO.readFile path)
  case parseProtoFile path contents of
    Left err -> fail ("Proto parse error in " <> path <> ": " <> show err)
    Right pf -> protoFileToDecls pf

protoFileToDecls :: ProtoFile -> Q [Dec]
protoFileToDecls pf = concat <$> mapM topLevelToDecls (protoTopLevels pf)

topLevelToDecls :: TopLevel -> Q [Dec]
topLevelToDecls = \case
  TLMessage msg -> messageToDecls msg
  TLEnum ed     -> enumToDecls ed
  _             -> pure []

messageToDecls :: MessageDef -> Q [Dec]
messageToDecls msg = do
  let tyName = mkName (T.unpack (hsTypeName (msgName msg)))
      fields = extractMessageFields (msgElements msg)

  nestedDecls <- fmap concat $ mapM (\case
    MEMessage inner -> messageToDecls inner
    MEEnum ed       -> enumToDecls ed
    _               -> pure []) (msgElements msg)

  dataDec <- mkDataDec tyName fields
  defaultDec <- mkDefaultDec tyName fields
  encodeDec <- mkEncodeInstance tyName fields
  decodeDec <- mkDecodeInstance tyName fields
  sizeDec <- mkSizeInstance tyName fields
  pure (nestedDecls <> [dataDec] <> defaultDec <> encodeDec <> decodeDec <> sizeDec)

data FieldSpec = FieldSpec
  { fsName :: Text, fsNum :: Int
  , fsLabel :: Maybe FieldLabel, fsType :: FieldType }

extractMessageFields :: [MessageElement] -> [FieldSpec]
extractMessageFields = mapMaybe go
  where
    go (MEField fd) = Just FieldSpec
      { fsName = fieldName fd, fsNum = unFieldNumber (fieldNumber fd)
      , fsLabel = fieldLabel fd, fsType = fieldType fd }
    go _ = Nothing

mkDataDec :: Name -> [FieldSpec] -> Q Dec
mkDataDec tyName fields = do
  let con = recC tyName (fmap mkField fields)
  dataD (pure []) tyName [] Nothing [con]
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Generic]]
  where
    mkField fs = do
      ty <- fieldTypeToTH (fsLabel fs) (fsType fs)
      let fname = mkName (T.unpack (hsFieldName (fsName fs)))
      pure (fname, Bang NoSourceUnpackedness SourceStrict, ty)

fieldTypeToTH :: Maybe FieldLabel -> FieldType -> Q Type
fieldTypeToTH lbl ft = case lbl of
  Just Repeated -> appT (conT ''V.Vector) (innerType ft)
  Just Optional -> appT (conT ''Maybe) (innerType ft)
  _ -> innerType ft
  where
    innerType (FTScalar s) = scalarToTH s
    innerType (FTNamed n)  = conT (mkName (T.unpack (hsTypeName n)))

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

mkDefaultDec :: Name -> [FieldSpec] -> Q [Dec]
mkDefaultDec tyName fields = do
  let defName = mkName ("default" <> nameBase tyName)
  sig <- sigD defName (conT tyName)
  body <- valD (varP defName)
    (normalB (recConE tyName (fmap mkDefaultField fields))) []
  pure [sig, body]
  where
    mkDefaultField fs = do
      val <- defaultValueExpr (fsLabel fs) (fsType fs)
      pure (mkName (T.unpack (hsFieldName (fsName fs))), val)

defaultValueExpr :: Maybe FieldLabel -> FieldType -> Q Exp
defaultValueExpr lbl ft = case lbl of
  Just Repeated -> [| V.empty |]
  Just Optional -> conE 'Nothing
  _ -> case ft of
    FTScalar SBool   -> conE 'False
    FTScalar SString -> litE (stringL "")
    FTScalar SBytes  -> litE (stringL "")
    FTScalar _       -> litE (integerL 0)
    FTNamed _        -> conE 'Nothing

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
mkFieldEncode msgVar fs = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName (fsName fs))))) (varE msgVar)
      fn = litE (integerL (fromIntegral (fsNum fs)))
  case fsLabel fs of
    Just Repeated -> [| V.foldl' (\acc v -> acc <> $(encodeFnQ (fsType fs)) $fn v) mempty $accessor |]
    Just Optional -> [| maybe mempty (\v -> $(encodeFnQ (fsType fs)) $fn v) $accessor |]
    _ -> [| if $(defaultCheckQ accessor (fsType fs))
            then mempty
            else $(encodeFnQ (fsType fs)) $fn $accessor |]

encodeFnQ :: FieldType -> Q Exp
encodeFnQ = \case
  FTScalar SDouble -> varE 'Encode.encodeFieldDouble
  FTScalar SFloat  -> varE 'Encode.encodeFieldFloat
  FTScalar SBool   -> varE 'Encode.encodeFieldBool
  FTScalar SString -> varE 'Encode.encodeFieldString
  FTScalar SBytes  -> varE 'Encode.encodeFieldBytes
  FTScalar SUInt64 -> varE 'Encode.encodeFieldVarint
  FTScalar _       -> [| \fieldNum val -> Encode.encodeFieldVarint fieldNum (fromIntegral val) |]
  FTNamed _        -> varE 'Encode.encodeFieldMessage

defaultCheckQ :: Q Exp -> FieldType -> Q Exp
defaultCheckQ accessor = \case
  FTScalar SBool   -> [| not $accessor |]
  FTScalar SString -> [| $accessor == ("" :: Text) |]
  FTScalar SBytes  -> [| BS.null $accessor |]
  FTScalar _       -> [| $accessor == 0 |]
  FTNamed _        -> [| $accessor == Nothing |]

mkDecodeInstance :: Name -> [FieldSpec] -> Q [Dec]
mkDecodeInstance tyName fields = do
  let accNames = fmap (\(i, _) -> mkName ("acc_" <> show i)) (zip [(0::Int)..] fields)
      loopName = mkName "loop"
      recExpr = recConE tyName
        (zipWith (\fs accN ->
          pure (mkName (T.unpack (hsFieldName (fsName fs))), VarE accN))
          fields accNames)

  let fieldCases = fmap (\(i, fs) ->
        let loopCall lbl = appsE (varE loopName :
              fmap (\(j, n) -> if j == i
                then case lbl of
                  Just Repeated -> [| V.snoc $(varE n) v |]
                  _             -> varE (mkName "v")
                else varE n)
                (zip [(0::Int)..] accNames))
        in match (litP (integerL (fromIntegral (fsNum fs))))
          (normalB [| $(decodeFnQ (fsType fs)) >>= \v -> $(loopCall (fsLabel fs)) |]) []
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

  let defaults = fmap (\fs -> defaultValueExpr (fsLabel fs) (fsType fs)) fields
      loopDef = funD loopName [clause (fmap varP accNames) (normalB loopBody) []]
      decoderBody = letE [loopDef] (appsE (varE loopName : defaults))

  inst <- instanceD (pure [])
    [t| Decode.MessageDecode $(conT tyName) |]
    [funD 'Decode.messageDecoder [clause [] (normalB decoderBody) []]]
  pure [inst]

decodeFnQ :: FieldType -> Q Exp
decodeFnQ = \case
  FTScalar SDouble -> varE 'Decode.decodeFieldDouble
  FTScalar SFloat  -> varE 'Decode.decodeFieldFloat
  FTScalar SBool   -> varE 'Decode.decodeFieldBool
  FTScalar SString -> varE 'Decode.decodeFieldString
  FTScalar SBytes  -> varE 'Decode.decodeFieldBytes
  FTScalar SUInt64 -> varE 'Decode.decodeFieldVarint
  FTScalar _       -> [| fromIntegral <$> Decode.decodeFieldVarint |]
  FTNamed _        -> varE 'Decode.decodeFieldMessage

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
mkFieldSize msgVar fs = do
  let accessor = appE (varE (mkName (T.unpack (hsFieldName (fsName fs))))) (varE msgVar)
      fn = litE (integerL (fromIntegral (fsNum fs)))
  case fsLabel fs of
    Just Repeated -> [| V.foldl' (\acc v -> acc + $(sizeFnQ (fsType fs)) $fn v) 0 $accessor |]
    Just Optional -> [| maybe 0 (\v -> $(sizeFnQ (fsType fs)) $fn v) $accessor |]
    _ -> [| if $(defaultCheckQ accessor (fsType fs))
            then 0
            else $(sizeFnQ (fsType fs)) $fn $accessor |]

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

enumToDecls :: EnumDef -> Q [Dec]
enumToDecls ed = do
  let tyName = mkName (T.unpack (hsTypeName (enumName ed)))
      cons = fmap (\ev ->
        normalC (mkName (T.unpack (hsEnumCon (enumName ed) (evName ev)))) []
        ) (enumValues ed)
  dataDec <- dataD (pure []) tyName [] Nothing cons
    [derivClause (Just StockStrategy) [conT ''Show, conT ''Eq, conT ''Ord, conT ''Generic]]
  pure [dataDec]
