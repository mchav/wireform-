module CapnProto.Parser
  ( parseCapnProto
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isAlpha)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word16, Word64)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import CapnProto.Schema

type Parser = Parsec Void Text

parseCapnProto :: Text -> Either String CapnProtoSchema
parseCapnProto input =
  case parse (sc *> schemaP <* eof) "<capnp>" input of
    Left err -> Left (errorBundlePretty err)
    Right s  -> Right s

--------------------------------------------------------------------------------
-- Whitespace / lexer helpers
--------------------------------------------------------------------------------

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '.')
  pure (T.cons c rest)

stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  void (char '"')
  content <- takeWhileP Nothing (/= '"')
  void (char '"')
  pure content

--------------------------------------------------------------------------------
-- Top-level schema
--------------------------------------------------------------------------------

schemaP :: Parser CapnProtoSchema
schemaP = do
  items <- many topLevelP
  let fileId  = listToMaybe' [fid | TLFileId fid <- items]
      imports = V.fromList [i | TLImport i <- items]
      decls   = V.fromList [d | TLDecl d <- items]
  pure CapnProtoSchema
    { csFileId  = fileId
    , csImports = imports
    , csDecls   = decls
    }

listToMaybe' :: [a] -> Maybe a
listToMaybe' []    = Nothing
listToMaybe' (x:_) = Just x

data TopLevel
  = TLFileId !Word64
  | TLImport !Text
  | TLDecl !Declaration
  | TLUsing

topLevelP :: Parser TopLevel
topLevelP = choice
  [ TLFileId <$> fileIdP
  , TLImport <$> importP
  , TLUsing <$ usingP
  , TLDecl <$> declarationP
  ]

--------------------------------------------------------------------------------
-- File ID: @0xHEXDIGITS;
--------------------------------------------------------------------------------

fileIdP :: Parser Word64
fileIdP = lexeme $ do
  void (char '@')
  void (string "0x")
  n <- L.hexadecimal
  void (symbol ";")
  pure n

--------------------------------------------------------------------------------
-- Import: using import "file.capnp".Name;
--------------------------------------------------------------------------------

importP :: Parser Text
importP = try $ do
  void (symbol "using")
  void (symbol "import")
  path <- stringLiteral
  _ <- optional (symbol "." *> identifier)
  void (symbol ";")
  pure path

usingP :: Parser ()
usingP = do
  void (symbol "using")
  _ <- identifier
  void (symbol "=")
  void (symbol "import")
  _ <- stringLiteral
  _ <- optional (symbol "." *> identifier)
  void (symbol ";")

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

declarationP :: Parser Declaration
declarationP = choice
  [ DStruct <$> structP
  , DEnum <$> enumP
  , DInterface <$> interfaceP
  , try constDeclP
  , try annotationP'
  ]

constDeclP :: Parser Declaration
constDeclP = do
  (name, ty, val) <- constP
  pure (DConst name ty val)

annotationP' :: Parser Declaration
annotationP' = do
  name <- annotationDeclP
  ty <- annotationTypeP
  pure (DAnnotation name ty)

--------------------------------------------------------------------------------
-- Struct
--------------------------------------------------------------------------------

structP :: Parser StructDef
structP = do
  void (symbol "struct")
  name <- identifier
  members <- braces (many structMemberP)
  let fields = V.fromList [f | SMField f <- members]
      nested = V.fromList [d | SMNested d <- members]
      unions = V.fromList [u | SMUnion u <- members]
  pure StructDef
    { sdName   = name
    , sdFields = fields
    , sdNested = nested
    , sdUnions = unions
    }

data StructMember
  = SMField !FieldDef
  | SMNested !Declaration
  | SMUnion !UnionDef

structMemberP :: Parser StructMember
structMemberP = choice
  [ SMUnion <$> unionP
  , SMNested <$> try (DStruct <$> structP)
  , SMNested <$> try (DEnum <$> enumP)
  , SMField <$> fieldDefP
  ]

fieldAnnotationP :: Parser (Text, Maybe Text)
fieldAnnotationP = do
  void (symbol "$")
  name <- identifier
  val <- optional (parens (stringLiteral <|> lexeme (takeWhile1P Nothing (\c -> c /= ')' && c /= '\n'))))
  pure (name, val)

fieldAnnotationsP :: Parser (V.Vector (Text, Maybe Text))
fieldAnnotationsP = V.fromList <$> many (try fieldAnnotationP)

fieldDefP :: Parser FieldDef
fieldDefP = do
  name <- identifier
  void (symbol "@")
  ordinal <- lexeme L.decimal
  void (symbol ":")
  ty <- capnTypeP
  dflt <- optional (symbol "=" *> defaultValueP)
  anns <- fieldAnnotationsP
  void (symbol ";")
  pure FieldDef
    { fdName        = name
    , fdOrdinal     = ordinal
    , fdType        = ty
    , fdDefault     = dflt
    , fdAnnotations = anns
    }

defaultValueP :: Parser Text
defaultValueP = lexeme $ takeWhile1P Nothing (\c -> c /= ';' && c /= '\n' && c /= '$')

--------------------------------------------------------------------------------
-- Union
--------------------------------------------------------------------------------

unionP :: Parser UnionDef
unionP = do
  void (symbol "union")
  fields <- braces (many fieldDefP)
  pure UnionDef { udFields = V.fromList fields }

--------------------------------------------------------------------------------
-- Enum
--------------------------------------------------------------------------------

enumP :: Parser EnumDef
enumP = do
  void (symbol "enum")
  name <- identifier
  vals <- braces (many enumValueP)
  pure EnumDef
    { edName   = name
    , edValues = V.fromList vals
    }

enumValueP :: Parser (Text, Word16)
enumValueP = do
  name <- identifier
  void (symbol "@")
  ordinal <- lexeme L.decimal
  void (symbol ";")
  pure (name, ordinal)

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------

interfaceP :: Parser InterfaceDef
interfaceP = do
  void (symbol "interface")
  name <- identifier
  methods <- braces (many methodP)
  pure InterfaceDef
    { idName    = name
    , idMethods = V.fromList methods
    }

methodP :: Parser MethodDef
methodP = do
  name <- identifier
  void (symbol "@")
  _ <- lexeme (L.decimal :: Parser Word16)
  params <- parens (paramP `sepBy` symbol ",")
  void (symbol "->")
  ret <- parens returnTypeP
  void (symbol ";")
  pure MethodDef
    { mdName   = name
    , mdParams = V.fromList params
    , mdReturn = ret
    }

paramP :: Parser (Text, CapnType)
paramP = do
  name <- identifier
  void (symbol ":")
  ty <- capnTypeP
  pure (name, ty)

returnTypeP :: Parser CapnType
returnTypeP = do
  _ <- optional identifier
  _ <- optional (symbol ":")
  capnTypeP

--------------------------------------------------------------------------------
-- Const
--------------------------------------------------------------------------------

constP :: Parser (Text, CapnType, Text)
constP = do
  void (symbol "const")
  name <- identifier
  void (symbol ":")
  ty <- capnTypeP
  void (symbol "=")
  val <- lexeme $ takeWhile1P Nothing (\c -> c /= ';')
  void (symbol ";")
  pure (name, ty, T.strip val)

--------------------------------------------------------------------------------
-- Annotation declaration
--------------------------------------------------------------------------------

annotationDeclP :: Parser Text
annotationDeclP = do
  void (symbol "annotation")
  identifier

annotationTypeP :: Parser CapnType
annotationTypeP = do
  void (symbol ":")
  ty <- capnTypeP
  void (symbol ";")
  pure ty

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

capnTypeP :: Parser CapnType
capnTypeP = choice
  [ CTVoid    <$ symbol "Void"
  , CTBool    <$ symbol "Bool"
  , CTInt8    <$ symbol "Int8"
  , CTInt16   <$ symbol "Int16"
  , CTInt32   <$ symbol "Int32"
  , CTInt64   <$ symbol "Int64"
  , CTUInt8   <$ symbol "UInt8"
  , CTUInt16  <$ symbol "UInt16"
  , CTUInt32  <$ symbol "UInt32"
  , CTUInt64  <$ symbol "UInt64"
  , CTFloat32 <$ symbol "Float32"
  , CTFloat64 <$ symbol "Float64"
  , CTText    <$ symbol "Text"
  , CTData    <$ symbol "Data"
  , listTypeP
  , CTNamed <$> identifier
  ]

listTypeP :: Parser CapnType
listTypeP = do
  void (symbol "List")
  void (symbol "(")
  ty <- capnTypeP
  void (symbol ")")
  pure (CTList ty)
