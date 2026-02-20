-- | Parser for the Protocol Buffers IDL (.proto files).
--
-- Supports proto2 and proto3 syntax including messages, enums, services,
-- oneofs, map fields, extensions, imports, packages, and custom options.
module Proto.Parser
  ( parseProtoFile
  , parseProto
  , Parser
  ) where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char ()

import Data.Void (Void)

import Proto.AST
import Proto.Parser.Lexer

-- | Parse a .proto file from its filename and contents.
parseProtoFile :: FilePath -> Text -> Either (ParseErrorBundle Text Void) ProtoFile
parseProtoFile = parse parseProto

parseProto :: Parser ProtoFile
parseProto = do
  sc
  syn <- option Proto3 syntaxDecl
  stmts <- many topLevelStmt
  eof
  let pkg     = firstJust (\case TLStmtPackage p -> Just p; _ -> Nothing) stmts
  let imps    = concatMap (\case TLStmtImport i -> [i]; _ -> []) stmts
  let opts    = concatMap (\case TLStmtOption o -> [o]; _ -> []) stmts
  let topDefs = concatMap (\case TLStmtTopLevel t -> [t]; _ -> []) stmts
  pure ProtoFile
    { protoSyntax    = syn
    , protoPackage   = pkg
    , protoImports   = imps
    , protoOptions   = opts
    , protoTopLevels = topDefs
    }

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just v  -> Just v
  Nothing -> firstJust f xs

data TLStmt
  = TLStmtPackage Text
  | TLStmtImport ImportDef
  | TLStmtOption OptionDef
  | TLStmtTopLevel TopLevel

syntaxDecl :: Parser Syntax
syntaxDecl = do
  reserved "syntax"
  equals
  s <- stringLiteral
  semi
  case s of
    "proto2" -> pure Proto2
    "proto3" -> pure Proto3
    _        -> fail ("Unknown syntax: " <> T.unpack s)

topLevelStmt :: Parser TLStmt
topLevelStmt = choice
  [ TLStmtPackage  <$> packageDecl
  , TLStmtImport   <$> importDecl
  , TLStmtOption   <$> optionDecl
  , TLStmtTopLevel <$> topLevelDef
  ]

packageDecl :: Parser Text
packageDecl = do
  reserved "package"
  pkg <- fullIdent
  semi
  pure pkg

importDecl :: Parser ImportDef
importDecl = do
  reserved "import"
  modifier <- optional $ choice
    [ ImportPublic <$ reserved "public"
    , ImportWeak   <$ reserved "weak"
    ]
  path <- stringLiteral
  semi
  pure ImportDef { importModifier = modifier, importPath = path }

optionDecl :: Parser OptionDef
optionDecl = do
  reserved "option"
  opt <- optionAssignment
  semi
  pure opt

optionAssignment :: Parser OptionDef
optionAssignment = do
  name <- optionName
  equals
  val <- constant
  pure OptionDef { optName = name, optValue = val }

optionName :: Parser OptionName
optionName = do
  first <- optionNamePart
  rest <- many (symbol "." *> simpleOptionPart)
  pure OptionName { optNameParts = first : rest }
  where
    simpleOptionPart = SimpleOption <$> identifier

optionNamePart :: Parser OptionNamePart
optionNamePart = choice
  [ ExtensionOption <$> parens fullIdent
  , SimpleOption <$> identifier
  ]

constant :: Parser Constant
constant = choice
  [ CBool <$> try boolLiteral
  , CString <$> try stringLiteral
  , try (CFloat <$> floatLiteral)
  , CInt <$> try intLiteral
  , CAggregate <$> aggregateLiteral
  , CIdent <$> fullIdent
  ]

aggregateLiteral :: Parser [(Text, Constant)]
aggregateLiteral = braces (many aggregateField)
  where
    aggregateField = do
      key <- identifier
      -- Both "key: value" and "key { ... }" are valid
      val <- (symbol ":" *> constant) <|> (CAggregate <$> aggregateLiteral)
      _ <- optional (comma <|> semi)
      pure (key, val)

topLevelDef :: Parser TopLevel
topLevelDef = choice
  [ TLMessage <$> messageDef
  , TLEnum    <$> enumDef
  , TLService <$> serviceDef
  , extendDef
  ]
  where
    extendDef = do
      reserved "extend"
      name <- fullIdent
      fields <- braces (many fieldDef)
      pure (TLExtend name fields)

messageDef :: Parser MessageDef
messageDef = do
  reserved "message"
  name <- identifier
  elems <- braces (many messageElement)
  pure MessageDef { msgName = name, msgElements = elems }

messageElement :: Parser MessageElement
messageElement = choice
  [ MEReserved   <$> reservedDecl
  , MEExtensions <$> extensionsDecl
  , MEOption     <$> optionDecl
  , MEEnum       <$> enumDef
  , MEMessage    <$> messageDef
  , MEOneof      <$> oneofDef
  , try (MEMapField <$> mapFieldDef)
  , MEField      <$> fieldDef
  ]

fieldDef :: Parser FieldDef
fieldDef = do
  lbl <- optional $ choice
    [ Optional <$ reserved "optional"
    , Required <$ reserved "required"
    , Repeated <$ reserved "repeated"
    ]
  ft <- parseFieldType
  name <- identifier
  equals
  num <- FieldNumber . fromIntegral <$> intLiteral
  opts <- fieldOptionList
  semi
  pure FieldDef
    { fieldLabel   = lbl
    , fieldType    = ft
    , fieldName    = name
    , fieldNumber  = num
    , fieldOptions = opts
    }

parseFieldType :: Parser FieldType
parseFieldType = choice
  [ FTScalar SDouble   <$ reserved "double"
  , FTScalar SFloat    <$ reserved "float"
  , FTScalar SInt32    <$ reserved "int32"
  , FTScalar SInt64    <$ reserved "int64"
  , FTScalar SUInt32   <$ reserved "uint32"
  , FTScalar SUInt64   <$ reserved "uint64"
  , FTScalar SSInt32   <$ reserved "sint32"
  , FTScalar SSInt64   <$ reserved "sint64"
  , FTScalar SFixed32  <$ reserved "fixed32"
  , FTScalar SFixed64  <$ reserved "fixed64"
  , FTScalar SSFixed32 <$ reserved "sfixed32"
  , FTScalar SSFixed64 <$ reserved "sfixed64"
  , FTScalar SBool     <$ reserved "bool"
  , FTScalar SString   <$ reserved "string"
  , FTScalar SBytes    <$ reserved "bytes"
  , FTNamed <$> fullIdent
  ]

mapFieldDef :: Parser MapField
mapFieldDef = do
  reserved "map"
  (kt, vt) <- angles $ do
    k <- scalarType
    comma
    v <- parseFieldType
    pure (k, v)
  name <- identifier
  equals
  num <- FieldNumber . fromIntegral <$> intLiteral
  opts <- fieldOptionList
  semi
  pure MapField
    { mapKeyType   = kt
    , mapValueType = vt
    , mapFieldName = name
    , mapFieldNum  = num
    , mapOptions   = opts
    }

scalarType :: Parser ScalarType
scalarType = choice
  [ SDouble   <$ reserved "double"
  , SFloat    <$ reserved "float"
  , SInt32    <$ reserved "int32"
  , SInt64    <$ reserved "int64"
  , SUInt32   <$ reserved "uint32"
  , SUInt64   <$ reserved "uint64"
  , SSInt32   <$ reserved "sint32"
  , SSInt64   <$ reserved "sint64"
  , SFixed32  <$ reserved "fixed32"
  , SFixed64  <$ reserved "fixed64"
  , SSFixed32 <$ reserved "sfixed32"
  , SSFixed64 <$ reserved "sfixed64"
  , SBool     <$ reserved "bool"
  , SString   <$ reserved "string"
  , SBytes    <$ reserved "bytes"
  ]

oneofDef :: Parser OneofDef
oneofDef = do
  reserved "oneof"
  name <- identifier
  (fields, opts) <- braces $ do
    items <- many (Left <$> try oneofOption <|> Right <$> oneofField)
    let os = concatMap (either (:[]) (const [])) items
    let fs = concatMap (either (const []) (:[])) items
    pure (fs, os)
  pure OneofDef
    { oneofName    = name
    , oneofFields  = fields
    , oneofOptions = opts
    }
  where
    oneofOption = optionDecl
    oneofField = do
      ft <- parseFieldType
      name <- identifier
      equals
      num <- FieldNumber . fromIntegral <$> intLiteral
      opts <- fieldOptionList
      semi
      pure OneofField
        { oneofFieldType    = ft
        , oneofFieldName    = name
        , oneofFieldNumber  = num
        , oneofFieldOptions = opts
        }

fieldOptionList :: Parser [OptionDef]
fieldOptionList = fromMaybe [] <$> optional (brackets (optionAssignment `sepBy1` comma))

reservedDecl :: Parser ReservedDef
reservedDecl = do
  reserved "reserved"
  res <- try reservedNames <|> reservedNumbers
  semi
  pure res
  where
    reservedNames = ReservedNames <$> (stringLiteral `sepBy1` comma)
    reservedNumbers = ReservedNumbers <$> (reservedRange `sepBy1` comma)
    reservedRange = do
      start <- fromIntegral <$> intLiteral
      end' <- optional (reserved "to" *> (Nothing <$ reserved "max" <|> Just . fromIntegral <$> intLiteral))
      case end' of
        Nothing       -> pure (ReservedSingle start)
        Just Nothing  -> pure (ReservedRange start 536870911)  -- max field number
        Just (Just e) -> pure (ReservedRange start e)

extensionsDecl :: Parser [ExtensionRange]
extensionsDecl = do
  reserved "extensions"
  ranges <- extensionRange `sepBy1` comma
  semi
  pure ranges
  where
    extensionRange = do
      start <- fromIntegral <$> intLiteral
      end' <- optional (reserved "to" *> (ExtBoundMax <$ reserved "max" <|> ExtBoundNum . fromIntegral <$> intLiteral))
      pure ExtensionRange
        { extStart = start
        , extEnd   = fromMaybe (ExtBoundNum start) end'
        }

enumDef :: Parser EnumDef
enumDef = do
  reserved "enum"
  name <- identifier
  (vals, opts) <- braces $ do
    items <- many (Left <$> try optionDecl <|> Right <$> enumValueDef)
    pure (concatMap (either (const []) (:[])) items
         ,concatMap (either (:[]) (const [])) items)
  pure EnumDef
    { enumName    = name
    , enumValues  = vals
    , enumOptions = opts
    }

enumValueDef :: Parser EnumValue
enumValueDef = do
  name <- identifier
  equals
  num <- fromIntegral <$> intLiteral
  opts <- fieldOptionList
  semi
  pure EnumValue
    { evName    = name
    , evNumber  = num
    , evOptions = opts
    }

serviceDef :: Parser ServiceDef
serviceDef = do
  reserved "service"
  name <- identifier
  (rpcs, opts) <- braces $ do
    items <- many (Left <$> try optionDecl <|> Right <$> rpcDef)
    pure (concatMap (either (const []) (:[])) items
         ,concatMap (either (:[]) (const [])) items)
  pure ServiceDef
    { svcName    = name
    , svcRpcs    = rpcs
    , svcOptions = opts
    }

rpcDef :: Parser RpcDef
rpcDef = do
  reserved "rpc"
  name <- identifier
  void (symbol "(")
  inStream <- option NoStream (Streaming <$ reserved "stream")
  inType <- fullIdent
  void (symbol ")")
  reserved "returns"
  void (symbol "(")
  outStream <- option NoStream (Streaming <$ reserved "stream")
  outType <- fullIdent
  void (symbol ")")
  opts <- rpcBody <|> ([] <$ semi)
  pure RpcDef
    { rpcName      = name
    , rpcInput     = inType
    , rpcInputStr  = inStream
    , rpcOutput    = outType
    , rpcOutputStr = outStream
    , rpcOptions   = opts
    }
  where
    rpcBody = braces (many (optionDecl <* optional semi))

