-- | Exact printer for protobuf ASTs.
--
-- Renders a 'ProtoFile' AST back to valid @.proto@ source text.
-- The output is a faithful representation: @parse . print ≡ id@ up
-- to whitespace normalization.
--
-- This enables:
--
-- * Round-tripping: parse a .proto, transform the AST, print it back
-- * Code formatting / normalization
-- * Proto file generation from Haskell (e.g. schema-first design)
-- * Debugging: inspect what the parser produced
module Proto.Print
  ( printProtoFile
  , printTopLevel
  , printMessage
  , printEnum
  , printService
  , printField
  , printOption
  , printConstant
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Proto.AST

-- | Render a complete proto file to source text.
printProtoFile :: ProtoFile -> Text
printProtoFile pf = T.intercalate "\n" $ filter (not . T.null) $
  [ printSyntax (protoSyntax pf) ]
  <> maybe [] (\pkg -> ["", "package " <> pkg <> ";"]) (protoPackage pf)
  <> (if null (protoImports pf) then [] else "" : fmap printImport (protoImports pf))
  <> (if null (protoOptions pf) then [] else "" : fmap printTopLevelOption (protoOptions pf))
  <> concatMap (\tl -> ["", printTopLevel tl]) (protoTopLevels pf)
  <> [""]

printSyntax :: Syntax -> Text
printSyntax Proto2 = "syntax = \"proto2\";"
printSyntax Proto3 = "syntax = \"proto3\";"
printSyntax (Editions ed) = "edition = \"" <> editionName ed <> "\";"

printImport :: ImportDef -> Text
printImport (ImportDef modifier path) =
  "import " <> maybe "" printImportMod modifier <> "\"" <> path <> "\";"

printImportMod :: ImportModifier -> Text
printImportMod ImportPublic = "public "
printImportMod ImportWeak   = "weak "

printTopLevelOption :: OptionDef -> Text
printTopLevelOption opt = "option " <> printOptionAssignment opt <> ";"

-- | Render a top-level definition.
printTopLevel :: TopLevel -> Text
printTopLevel = \case
  TLMessage msg   -> printMessage 0 msg
  TLEnum ed       -> printEnum 0 ed
  TLService svc   -> printService svc
  TLExtend name fields ->
    "extend " <> name <> " {\n" <>
    T.concat (fmap (\f -> indent 1 <> printField f <> "\n") fields) <>
    "}"
  TLOption opt -> "option " <> printOptionAssignment opt <> ";"

-- | Render a message definition at a given indentation depth.
printMessage :: Int -> MessageDef -> Text
printMessage depth msg =
  indent depth <> "message " <> msgName msg <> " {\n" <>
  T.concat (fmap (printMessageElement (depth + 1)) (msgElements msg)) <>
  indent depth <> "}"

printMessageElement :: Int -> MessageElement -> Text
printMessageElement depth = \case
  MEField fd       -> indent depth <> printField fd <> "\n"
  MEEnum ed        -> printEnum depth ed <> "\n"
  MEMessage msg    -> printMessage depth msg <> "\n"
  MEOneof od       -> printOneof depth od <> "\n"
  MEMapField mf    -> indent depth <> printMapField mf <> "\n"
  MEReserved rd    -> indent depth <> printReserved rd <> "\n"
  MEExtensions exs -> indent depth <> "extensions " <> printExtensionRanges exs <> ";\n"
  MEOption opt     -> indent depth <> "option " <> printOptionAssignment opt <> ";\n"

-- | Render a field definition.
printField :: FieldDef -> Text
printField fd =
  maybe "" (\l -> printLabel l <> " ") (fieldLabel fd)
  <> printFieldType (fieldType fd) <> " "
  <> fieldName fd <> " = "
  <> intToText (unFieldNumber (fieldNumber fd))
  <> printFieldOptions (fieldOptions fd)
  <> ";"

printLabel :: FieldLabel -> Text
printLabel Optional = "optional"
printLabel Required = "required"
printLabel Repeated = "repeated"

printFieldType :: FieldType -> Text
printFieldType = \case
  FTScalar s -> printScalarType s
  FTNamed n  -> n

printScalarType :: ScalarType -> Text
printScalarType = \case
  SDouble   -> "double"
  SFloat    -> "float"
  SInt32    -> "int32"
  SInt64    -> "int64"
  SUInt32   -> "uint32"
  SUInt64   -> "uint64"
  SSInt32   -> "sint32"
  SSInt64   -> "sint64"
  SFixed32  -> "fixed32"
  SFixed64  -> "fixed64"
  SSFixed32 -> "sfixed32"
  SSFixed64 -> "sfixed64"
  SBool     -> "bool"
  SString   -> "string"
  SBytes    -> "bytes"

printMapField :: MapField -> Text
printMapField mf =
  "map<" <> printScalarType (mapKeyType mf) <> ", "
  <> printFieldType (mapValueType mf) <> "> "
  <> mapFieldName mf <> " = "
  <> intToText (unFieldNumber (mapFieldNum mf))
  <> printFieldOptions (mapOptions mf)
  <> ";"

printOneof :: Int -> OneofDef -> Text
printOneof depth od =
  indent depth <> "oneof " <> oneofName od <> " {\n"
  <> T.concat (fmap (\opt -> indent (depth + 1) <> "option " <> printOptionAssignment opt <> ";\n") (oneofOptions od))
  <> T.concat (fmap (\f -> indent (depth + 1) <> printOneofField f <> "\n") (oneofFields od))
  <> indent depth <> "}"

printOneofField :: OneofField -> Text
printOneofField f =
  printFieldType (oneofFieldType f) <> " "
  <> oneofFieldName f <> " = "
  <> intToText (unFieldNumber (oneofFieldNumber f))
  <> printFieldOptions (oneofFieldOptions f)
  <> ";"

printFieldOptions :: [OptionDef] -> Text
printFieldOptions [] = ""
printFieldOptions opts =
  " [" <> T.intercalate ", " (fmap printOptionAssignment opts) <> "]"

-- | Render an enum definition.
printEnum :: Int -> EnumDef -> Text
printEnum depth ed =
  indent depth <> "enum " <> enumName ed <> " {\n"
  <> T.concat (fmap (\opt -> indent (depth + 1) <> "option " <> printOptionAssignment opt <> ";\n") (enumOptions ed))
  <> T.concat (fmap (\v -> indent (depth + 1) <> printEnumValue v <> "\n") (enumValues ed))
  <> indent depth <> "}"

printEnumValue :: EnumValue -> Text
printEnumValue ev =
  evName ev <> " = " <> intToText (evNumber ev)
  <> printFieldOptions (evOptions ev) <> ";"

-- | Render a service definition.
printService :: ServiceDef -> Text
printService svc =
  "service " <> svcName svc <> " {\n"
  <> T.concat (fmap (\opt -> indent 1 <> "option " <> printOptionAssignment opt <> ";\n") (svcOptions svc))
  <> T.concat (fmap (\rpc -> indent 1 <> printRpc rpc <> "\n") (svcRpcs svc))
  <> "}"

printRpc :: RpcDef -> Text
printRpc rpc =
  "rpc " <> rpcName rpc
  <> " (" <> printStream (rpcInputStr rpc) <> rpcInput rpc <> ")"
  <> " returns (" <> printStream (rpcOutputStr rpc) <> rpcOutput rpc <> ")"
  <> case rpcOptions rpc of
       [] -> ";"
       opts -> " {\n"
         <> T.concat (fmap (\opt -> indent 2 <> "option " <> printOptionAssignment opt <> ";\n") opts)
         <> indent 1 <> "}"

printStream :: StreamQualifier -> Text
printStream NoStream  = ""
printStream Streaming = "stream "

-- | Render an option assignment (name = value).
printOption :: OptionDef -> Text
printOption = printOptionAssignment

printOptionAssignment :: OptionDef -> Text
printOptionAssignment opt =
  printOptionName (optName opt) <> " = " <> printConstant (optValue opt)

printOptionName :: OptionName -> Text
printOptionName (OptionName parts) = T.intercalate "." (fmap printOptionNamePart parts)

printOptionNamePart :: OptionNamePart -> Text
printOptionNamePart = \case
  SimpleOption name    -> name
  ExtensionOption name -> "(" <> name <> ")"

-- | Render a constant value.
printConstant :: Constant -> Text
printConstant = \case
  CIdent t     -> t
  CInt n       -> integerToText n
  CFloat d     -> T.pack (show d)
  CString s    -> "\"" <> escapeProtoString s <> "\""
  CBool True   -> "true"
  CBool False  -> "false"
  CAggregate kvs -> "{ " <> T.intercalate " " (fmap printAggField kvs) <> " }"
  where
    printAggField (k, v) = k <> ": " <> printConstant v

escapeProtoString :: Text -> Text
escapeProtoString = T.concatMap escChar
  where
    escChar '"'  = "\\\""
    escChar '\\' = "\\\\"
    escChar '\n' = "\\n"
    escChar '\r' = "\\r"
    escChar '\t' = "\\t"
    escChar c    = T.singleton c

printReserved :: ReservedDef -> Text
printReserved = \case
  ReservedNumbers ranges ->
    "reserved " <> T.intercalate ", " (fmap printReservedRange ranges) <> ";"
  ReservedNames names ->
    "reserved " <> T.intercalate ", " (fmap (\n -> "\"" <> n <> "\"") names) <> ";"

printReservedRange :: ReservedRange -> Text
printReservedRange = \case
  ReservedSingle n    -> intToText n
  ReservedRange lo hi -> intToText lo <> " to " <> intToText hi

printExtensionRanges :: [ExtensionRange] -> Text
printExtensionRanges = T.intercalate ", " . fmap printExtRange
  where
    printExtRange er =
      intToText (extStart er) <>
      case extEnd er of
        ExtBoundNum n | n == extStart er -> ""
        ExtBoundNum n -> " to " <> intToText n
        ExtBoundMax   -> " to max"

indent :: Int -> Text
indent n = T.replicate (n * 2) " "

intToText :: Int -> Text
intToText n
  | n < 0     = "-" <> intToText (negate n)
  | n < 10    = T.singleton (toEnum (n + 48))
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v = let (!q, !r) = v `quotRem` 10
                in go (T.cons (toEnum (r + 48)) acc) q

integerToText :: Integer -> Text
integerToText n
  | n < 0     = "-" <> integerToText (negate n)
  | n < 10    = T.singleton (toEnum (fromIntegral n + 48))
  | otherwise = go T.empty n
  where
    go !acc 0 = acc
    go !acc v = let (!q, !r) = v `quotRem` 10
                in go (T.cons (toEnum (fromIntegral r + 48)) acc) q
