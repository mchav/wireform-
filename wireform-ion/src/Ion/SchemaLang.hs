{- | Ion Schema Language (ISL) parser.

Parses Ion Schema documents into an 'Ion.ISLSchema.ISLSchema' AST
using Megaparsec. ISL is written in Ion text format with annotations.
Parses schema_header, type declarations, schema_footer, field
constraints, valid_values ranges, and imports.
-}
module Ion.SchemaLang (
  parseISL,
) where

import Data.Char (isAlpha, isAlphaNum)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void)
import Ion.ISLSchema
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


type Parser = Parsec Void Text


-- | Parse Ion Schema Language text into an 'ISLSchema'.
parseISL :: Text -> Either String ISLSchema
parseISL input =
  case parse (sc *> islDocP <* eof) "<isl>" input of
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


identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_')
  pure (T.cons c rest)


signedInteger :: Parser Int64
signedInteger = lexeme (L.signed sc L.decimal)


braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")


brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")


islDocP :: Parser ISLSchema
islDocP = do
  imports <- headerImportsP
  types <- many (try typeDefP)
  _ <- optional footerP
  pure
    ISLSchema
      { islTypes = V.fromList types
      , islImports = V.fromList imports
      }


headerImportsP :: Parser [ISLImport]
headerImportsP = do
  mHeader <- optional (try headerP)
  case mHeader of
    Nothing -> pure []
    Just imports -> pure imports


headerP :: Parser [ISLImport]
headerP = do
  _ <- try (identifier >>= \i -> if i == "schema_header" then pure () else fail "expected schema_header")
  _ <- symbol "::"
  braces $ do
    imports <- optional (try importsBlockP)
    pure (maybe [] id imports)


importsBlockP :: Parser [ISLImport]
importsBlockP = do
  _ <- identifier >>= \i -> if i == "imports" then pure () else fail "expected imports"
  _ <- symbol ":"
  brackets (importP `sepEndBy` symbol ",")


importP :: Parser ISLImport
importP = braces $ do
  _ <- identifier >>= \i -> if i == "id" then pure () else fail "expected id"
  _ <- symbol ":"
  schemaId <- ionStringOrSymbol
  _ <- symbol ","
  typeName <- optional $ try $ do
    _ <- identifier >>= \i -> if i == "type" then pure () else fail "expected type"
    _ <- symbol ":"
    ionStringOrSymbol
  pure (ISLImport schemaId typeName)


footerP :: Parser ()
footerP = do
  _ <- try (identifier >>= \i -> if i == "schema_footer" then pure () else fail "expected schema_footer")
  _ <- symbol "::"
  _ <- braces (pure ())
  pure ()


typeDefP :: Parser ISLType
typeDefP = do
  _ <- try (typeAnnotation)
  _ <- symbol "::"
  braces typeBodyP


typeAnnotation :: Parser ()
typeAnnotation = do
  w <- identifier
  if w == "type"
    then pure ()
    else fail "expected type annotation"


typeBodyP :: Parser ISLType
typeBodyP = do
  entries <- entryP `sepEndBy` symbol ","
  let name = lookupText "name" entries
      baseType = lookupText "type" entries
      fields = lookupFields entries
      validV = lookupValidValues entries
      occ = lookupOccurs entries
  pure
    ISLType
      { islTypeName = maybe "" id name
      , islBaseType = baseType
      , islFields = fields
      , islValidValues = validV
      , islOccurs = occ
      }


data Entry
  = EText !Text !Text
  | EFields !(Vector ISLField)
  | EValidValues !ISLConstraint
  | EOccurs !Occurs


lookupText :: Text -> [Entry] -> Maybe Text
lookupText _ [] = Nothing
lookupText key (EText k v : rest)
  | k == key = Just v
  | otherwise = lookupText key rest
lookupText key (_ : rest) = lookupText key rest


lookupFields :: [Entry] -> Maybe (Vector ISLField)
lookupFields [] = Nothing
lookupFields (EFields fs : _) = Just fs
lookupFields (_ : rest) = lookupFields rest


lookupValidValues :: [Entry] -> Maybe ISLConstraint
lookupValidValues [] = Nothing
lookupValidValues (EValidValues c : _) = Just c
lookupValidValues (_ : rest) = lookupValidValues rest


lookupOccurs :: [Entry] -> Maybe Occurs
lookupOccurs [] = Nothing
lookupOccurs (EOccurs o : _) = Just o
lookupOccurs (_ : rest) = lookupOccurs rest


entryP :: Parser Entry
entryP =
  choice
    [ try fieldsEntryP
    , try validValuesEntryP
    , try occursEntryP
    , textEntryP
    ]


textEntryP :: Parser Entry
textEntryP = do
  key <- identifier
  _ <- symbol ":"
  val <- ionStringOrSymbol
  pure (EText key val)


fieldsEntryP :: Parser Entry
fieldsEntryP = do
  _ <- identifier >>= \i -> if i == "fields" then pure () else fail "expected fields"
  _ <- symbol ":"
  fields <- braces (fieldP `sepEndBy` symbol ",")
  pure (EFields (V.fromList fields))


fieldP :: Parser ISLField
fieldP = do
  name <- identifier
  _ <- symbol ":"
  ty <- fieldTypeP
  pure (ISLField name ty)


fieldTypeP :: Parser ISLType
fieldTypeP = braces fieldTypeBodyP <|> simpleFieldType


simpleFieldType :: Parser ISLType
simpleFieldType = do
  ty <- ionStringOrSymbol
  pure
    ISLType
      { islTypeName = ""
      , islBaseType = Just ty
      , islFields = Nothing
      , islValidValues = Nothing
      , islOccurs = Nothing
      }


fieldTypeBodyP :: Parser ISLType
fieldTypeBodyP = do
  entries <- entryP `sepEndBy` symbol ","
  let baseType = lookupText "type" entries
      validV = lookupValidValues entries
      occ = lookupOccurs entries
  pure
    ISLType
      { islTypeName = ""
      , islBaseType = baseType
      , islFields = Nothing
      , islValidValues = validV
      , islOccurs = occ
      }


validValuesEntryP :: Parser Entry
validValuesEntryP = do
  _ <- identifier >>= \i -> if i == "valid_values" then pure () else fail "expected valid_values"
  _ <- symbol ":"
  constraint <-
    choice
      [ try rangeValP
      , enumValP
      ]
  pure (EValidValues constraint)


rangeValP :: Parser ISLConstraint
rangeValP = do
  _ <- identifier >>= \i -> if i == "range" then pure () else fail "expected range"
  _ <- symbol "::"
  brackets $ do
    lo <- optional (try signedInteger)
    _ <- symbol ","
    hi <- optional signedInteger
    pure (RangeVal lo hi)


enumValP :: Parser ISLConstraint
enumValP = do
  vals <- brackets (ionStringOrSymbol `sepBy1` symbol ",")
  pure (EnumVal (V.fromList vals))


occursEntryP :: Parser Entry
occursEntryP = do
  _ <- identifier >>= \i -> if i == "occurs" then pure () else fail "expected occurs"
  _ <- symbol ":"
  occ <-
    choice
      [ ORequired <$ try (identifier >>= \i -> if i == "required" then pure () else fail "")
      , OOptional <$ try (identifier >>= \i -> if i == "optional" then pure () else fail "")
      , try rangeOccursP
      ]
  pure (EOccurs occ)


rangeOccursP :: Parser Occurs
rangeOccursP = do
  _ <- identifier >>= \i -> if i == "range" then pure () else fail "expected range"
  _ <- symbol "::"
  brackets $ do
    lo <- lexeme L.decimal
    _ <- symbol ","
    hi <- lexeme L.decimal
    pure (ORange lo hi)


ionStringOrSymbol :: Parser Text
ionStringOrSymbol =
  choice
    [ try ionString
    , identifier
    ]


ionString :: Parser Text
ionString = lexeme $ do
  _ <- char '"'
  content <- takeWhileP Nothing (/= '"')
  _ <- char '"'
  pure content
