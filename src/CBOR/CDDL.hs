-- | CDDL (RFC 8610) parser.
--
-- Parses CDDL schema definitions into a 'CBOR.CDDLSchema.CDDLSchema'
-- AST using Megaparsec. Supports rule assignments, map/array groups,
-- choices, optional members, occurrence indicators, enum groups,
-- tagged types, and built-in CBOR types.
module CBOR.CDDL
  ( parseCDDL
  ) where

import Data.Char (isAlphaNum, isAlpha, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import CBOR.CDDLSchema

type Parser = Parsec Void Text

-- | Parse CDDL text into a 'CDDLSchema'.
parseCDDL :: Text -> Either String CDDLSchema
parseCDDL input =
  case parse (sc *> schemaP <* eof) "<cddl>" input of
    Left err -> Left (errorBundlePretty err)
    Right s  -> Right s

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment ";")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_' || ch == '-')
  pure (T.cons c rest)

schemaP :: Parser CDDLSchema
schemaP = do
  rules <- many (try ruleP)
  pure (CDDLSchema (V.fromList rules))

ruleP :: Parser CDDLRule
ruleP = do
  name <- identifier
  _ <- symbol "="
  ty <- typeExprP
  pure (CDDLRule name ty)

typeExprP :: Parser CDDLType
typeExprP = do
  t <- singleTypeP
  alts <- many (try (symbol "/" *> singleTypeP))
  case alts of
    [] -> pure t
    _  -> pure (CTChoice (V.fromList (t : alts)))

singleTypeP :: Parser CDDLType
singleTypeP = choice
  [ try taggedP
  , try enumGroupP
  , try mapP
  , try arrayP
  , try literalP
  , try builtinP
  , refP
  ]

builtinP :: Parser CDDLType
builtinP = choice
  [ CTUint  <$ reserved "uint"
  , CTNint  <$ reserved "nint"
  , CTInt   <$ reserved "int"
  , CTTstr  <$ reserved "tstr"
  , CTBstr  <$ reserved "bstr"
  , CTFloat <$ try (reserved "float")
  , CTBool  <$ reserved "bool"
  , CTNil   <$ reserved "nil"
  , CTAny   <$ reserved "any"
  ]

reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy (satisfy isIdentContinue))

isIdentContinue :: Char -> Bool
isIdentContinue c = isAlphaNum c || c == '_' || c == '-'

refP :: Parser CDDLType
refP = CTRef <$> identifier

mapP :: Parser CDDLType
mapP = do
  _ <- symbol "{"
  members <- memberP `sepEndBy` symbol ","
  _ <- symbol "}"
  pure (CTMap (V.fromList members))

arrayP :: Parser CDDLType
arrayP = do
  _ <- symbol "["
  members <- arrayMemberP `sepEndBy` symbol ","
  _ <- symbol "]"
  pure (CTArray (V.fromList members))

memberP :: Parser CDDLMember
memberP = do
  occ <- occurrenceP
  name <- try (identifier <* symbol ":")
  ty <- typeExprP
  pure (CDDLMember name ty occ)

arrayMemberP :: Parser CDDLMember
arrayMemberP = try namedArrayMember <|> unnamedArrayMember

namedArrayMember :: Parser CDDLMember
namedArrayMember = do
  occ <- occurrenceP
  name <- try (identifier <* symbol ":")
  ty <- typeExprP
  pure (CDDLMember name ty occ)

unnamedArrayMember :: Parser CDDLMember
unnamedArrayMember = do
  occ <- occurrenceP
  ty <- typeExprP
  pure (CDDLMember "" ty occ)

occurrenceP :: Parser Occurrence
occurrenceP = choice
  [ Optional   <$ try (symbol "?")
  , ZeroOrMore <$ try (symbol "*")
  , OneOrMore  <$ try (symbol "+")
  , pure Once
  ]

enumGroupP :: Parser CDDLType
enumGroupP = do
  _ <- symbol "&"
  _ <- symbol "("
  members <- enumMemberP `sepBy1` symbol ","
  _ <- symbol ")"
  pure (CTChoice (V.fromList members))

enumMemberP :: Parser CDDLType
enumMemberP = do
  _ <- optional (try (identifier <* symbol ":"))
  choice
    [ try literalP
    , CTRef <$> identifier
    ]

taggedP :: Parser CDDLType
taggedP = do
  _ <- symbol "#"
  _ <- string "6"
  _ <- char '.'
  tagNum <- lexeme L.decimal
  ty <- parens typeExprP
  pure (CTTagged tagNum ty)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

literalP :: Parser CDDLType
literalP = choice
  [ try stringLiteralP
  , try numLiteralP
  ]

stringLiteralP :: Parser CDDLType
stringLiteralP = lexeme $ do
  _ <- char '"'
  content <- takeWhileP Nothing (/= '"')
  _ <- char '"'
  pure (CTLiteral (T.concat ["\"", content, "\""]))

numLiteralP :: Parser CDDLType
numLiteralP = lexeme $ do
  sign <- optional (char '-')
  digits <- takeWhile1P Nothing isDigit
  let txt = case sign of
              Just _  -> T.cons '-' digits
              Nothing -> digits
  notFollowedBy (satisfy isIdentContinue)
  pure (CTLiteral txt)
