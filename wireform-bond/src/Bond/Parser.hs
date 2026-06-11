{- | Microsoft Bond IDL parser.

Parses Bond schema definition files (@.bond@) into a 'Bond.Schema.BondSchema'
AST using Megaparsec. Supports structs, enums, field modifiers
(required\/optional), generics, and default values.
-}
module Bond.Parser (
  parseBond,
) where

import Bond.Schema
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


-- | Parse Bond IDL text into a 'BondSchema'.
parseBond :: Text -> Either String BondSchema
parseBond input =
  case parse (sc *> schemaP <* eof) "<bond>" input of
    Left err -> Left (errorBundlePretty err)
    Right s -> Right s


sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")


lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


symbol :: Text -> Parser Text
symbol = L.symbol sc


reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)


braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")


angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")


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


integerLiteral :: Parser Int64
integerLiteral = lexeme $ do
  sign <- optional (char '-')
  n <- try (string "0x" *> L.hexadecimal) <|> L.decimal
  let val = case sign of
        Just _ -> negate n
        Nothing -> n
  pure val


schemaP :: Parser BondSchema
schemaP = do
  ns <- optional (try namespaceP)
  imports <- many (try importP)
  decls <- many declarationP
  pure
    BondSchema
      { bondNamespace = ns
      , bondImports = imports
      , bondDecls = decls
      }


namespaceP :: Parser Text
namespaceP = do
  reserved "namespace"
  identifier


importP :: Parser Text
importP = do
  reserved "import"
  stringLiteral


declarationP :: Parser BondDecl
declarationP =
  choice
    [ BondDeclStruct <$> structP
    , BondDeclEnum <$> enumP
    ]


bondAttributeP :: Parser (Text, Maybe Text)
bondAttributeP = do
  void (symbol "[")
  name <- identifier
  val <-
    optional
      ( do
          void (symbol "(")
          v <- stringLiteral
          void (symbol ")")
          pure v
      )
  void (symbol "]")
  pure (name, val)


bondAttributesP :: Parser (V.Vector (Text, Maybe Text))
bondAttributesP = V.fromList <$> many (try bondAttributeP)


structP :: Parser BondStruct
structP = do
  attrs <- bondAttributesP
  reserved "struct"
  name <- identifier
  tp <- optional (angles identifier)
  optional (symbol ":" *> identifier)
  fields <- braces (many fieldP)
  pure
    BondStruct
      { bsName = name
      , bsTypeParam = tp
      , bsFields = fields
      , bsAttributes = attrs
      }


fieldP :: Parser BondField
fieldP = do
  attrs <- bondAttributesP
  fid <- integerLiteral
  void (symbol ":")
  mods <- optional modifierP
  ftype <- fieldTypeP
  fname <- identifier
  dflt <- optional (symbol "=" *> defaultValueP)
  void (symbol ";")
  pure
    BondField
      { bfFieldId = fromIntegral fid
      , bfModifier = fromMaybe BondOptional mods
      , bfType = ftype
      , bfName = fname
      , bfDefault = dflt
      , bfAttributes = attrs
      }


modifierP :: Parser BondFieldModifier
modifierP =
  choice
    [ BondRequiredOptional <$ try (reserved "required_optional")
    , BondRequired <$ reserved "required"
    , BondOptional <$ reserved "optional"
    ]


fieldTypeP :: Parser BondFieldType
fieldTypeP =
  choice
    [ BFTBool <$ reserved "bool"
    , BFTInt8 <$ reserved "int8"
    , BFTInt16 <$ reserved "int16"
    , BFTInt32 <$ reserved "int32"
    , BFTInt64 <$ reserved "int64"
    , BFTUInt8 <$ reserved "uint8"
    , BFTUInt16 <$ reserved "uint16"
    , BFTUInt32 <$ reserved "uint32"
    , BFTUInt64 <$ reserved "uint64"
    , BFTFloat <$ reserved "float"
    , BFTDouble <$ reserved "double"
    , BFTString <$ reserved "string"
    , BFTWString <$ reserved "wstring"
    , BFTBlob <$ reserved "blob"
    , try nullableTypeP
    , try listTypeP
    , try setTypeP
    , try mapTypeP
    , BFTNamed <$> identifier
    ]


listTypeP :: Parser BondFieldType
listTypeP = do
  reserved "list"
  BFTList <$> angles fieldTypeP


setTypeP :: Parser BondFieldType
setTypeP = do
  reserved "set"
  BFTSet <$> angles fieldTypeP


mapTypeP :: Parser BondFieldType
mapTypeP = do
  reserved "map"
  angles $ do
    kt <- fieldTypeP
    void (symbol ",")
    vt <- fieldTypeP
    pure (BFTMap kt vt)


nullableTypeP :: Parser BondFieldType
nullableTypeP = do
  reserved "nullable"
  BFTNullable <$> angles fieldTypeP


enumP :: Parser BondEnum
enumP = do
  reserved "enum"
  name <- identifier
  vals <- braces (many enumValueP)
  pure BondEnum {beName = name, beValues = vals}


enumValueP :: Parser BondEnumValue
enumValueP = do
  name <- identifier
  val <- optional (symbol "=" *> (fromIntegral <$> integerLiteral))
  optional (void (symbol ",") <|> void (symbol ";"))
  pure BondEnumValue {bevName = name, bevValue = val}


defaultValueP :: Parser Text
defaultValueP = lexeme $ takeWhileP Nothing (\c -> c /= ';' && c /= '\n')
