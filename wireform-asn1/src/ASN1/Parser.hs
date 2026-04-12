-- | ASN.1 Module Definition Language parser.
--
-- Parses ASN.1 module definitions into an 'ASN1.Schema.ASN1Module'
-- AST using Megaparsec. Supports SEQUENCE, CHOICE, ENUMERATED,
-- INTEGER with constraints, OPTIONAL, DEFAULT, SIZE constraints,
-- and basic built-in types.
module ASN1.Parser
  ( parseASN1Module
  ) where

import Data.Char (isAlphaNum, isAlpha, isUpper)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import ASN1.Schema

type Parser = Parsec Void Text

-- | Parse ASN.1 module definition text into an 'ASN1Module'.
parseASN1Module :: Text -> Either String ASN1Module
parseASN1Module input =
  case parse (sc *> moduleP <* eof) "<asn1>" input of
    Left err -> Left (errorBundlePretty err)
    Right m  -> Right m

sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "--")
  empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy (satisfy isIdentChar))

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-'

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  rest <- takeWhileP Nothing isIdentChar
  pure (T.cons c rest)

typeReference :: Parser Text
typeReference = lexeme $ do
  c <- satisfy isUpper
  rest <- takeWhileP Nothing isIdentChar
  pure (T.cons c rest)

signedInteger :: Parser Int64
signedInteger = lexeme (L.signed sc L.decimal)

moduleP :: Parser ASN1Module
moduleP = do
  name <- identifier
  reserved "DEFINITIONS"
  tagMode <- tagModeP
  reserved "TAGS"
  _ <- symbol "::="
  reserved "BEGIN"
  assignments <- many (try typeAssignmentP)
  reserved "END"
  pure ASN1Module
    { asnModuleName  = name
    , asnTagMode     = tagMode
    , asnAssignments = V.fromList assignments
    }

tagModeP :: Parser TagMode
tagModeP = choice
  [ AutomaticTags <$ reserved "AUTOMATIC"
  , ImplicitTags  <$ reserved "IMPLICIT"
  , ExplicitTags  <$ reserved "EXPLICIT"
  , pure DefaultTags
  ]

typeAssignmentP :: Parser TypeAssignment
typeAssignmentP = do
  name <- typeReference
  _ <- symbol "::="
  td <- typeDefP
  pure (TypeAssignment name td)

typeDefP :: Parser ASN1TypeDef
typeDefP = choice
  [ try sequenceOfP
  , try setOfP
  , try sequenceP
  , try choiceP
  , try enumeratedP
  , try integerP
  , try octetStringP
  , try bitStringP
  , try booleanP
  , try nullP
  , try builtinStringTypeP
  , namedTypeP
  ]

sequenceP :: Parser ASN1TypeDef
sequenceP = do
  reserved "SEQUENCE"
  components <- braces (componentP `sepBy1` symbol ",")
  pure (TDSequence (V.fromList components))

choiceP :: Parser ASN1TypeDef
choiceP = do
  reserved "CHOICE"
  components <- braces (componentP `sepBy1` symbol ",")
  pure (TDChoice (V.fromList components))

componentP :: Parser ComponentType
componentP = do
  name <- identifier
  td <- typeDefP
  (opt, td') <- optionalOrDefault td
  pure (ComponentType name td' opt)

optionalOrDefault :: ASN1TypeDef -> Parser (Bool, ASN1TypeDef)
optionalOrDefault td = choice
  [ do reserved "OPTIONAL"
       pure (True, td)
  , do reserved "DEFAULT"
       val <- defaultValueP
       pure (False, TDDefault td val)
  , pure (False, td)
  ]

defaultValueP :: Parser Text
defaultValueP = lexeme $ takeWhile1P Nothing (\c -> c /= ',' && c /= '}' && c /= '\n')

enumeratedP :: Parser ASN1TypeDef
enumeratedP = do
  reserved "ENUMERATED"
  vals <- braces (enumValueP `sepBy1` symbol ",")
  pure (TDEnumerated (V.fromList vals))

enumValueP :: Parser (Text, Maybe Int)
enumValueP = do
  name <- identifier
  val <- optional (parens (fromIntegral <$> signedInteger))
  pure (name, val)

integerP :: Parser ASN1TypeDef
integerP = do
  reserved "INTEGER"
  constraint <- optional (try constraintP)
  pure (TDInteger constraint)

constraintP :: Parser Constraint
constraintP = parens rangeConstraintP

rangeConstraintP :: Parser Constraint
rangeConstraintP = do
  lo <- optional (try signedInteger)
  _ <- symbol ".."
  hi <- optional signedInteger
  pure (RangeConstraint lo hi)

octetStringP :: Parser ASN1TypeDef
octetStringP = do
  reserved "OCTET"
  reserved "STRING"
  constraint <- optional (try sizeConstraintP)
  pure (TDOctetString constraint)

sizeConstraintP :: Parser Constraint
sizeConstraintP = parens $ do
  reserved "SIZE"
  parens $ do
    lo <- optional (try signedInteger)
    _ <- symbol ".."
    hi <- optional signedInteger
    pure (SizeConstraint lo hi)

bitStringP :: Parser ASN1TypeDef
bitStringP = TDBitString <$ (reserved "BIT" *> reserved "STRING")

booleanP :: Parser ASN1TypeDef
booleanP = TDBoolean <$ reserved "BOOLEAN"

nullP :: Parser ASN1TypeDef
nullP = TDNULL <$ reserved "NULL"

builtinStringTypeP :: Parser ASN1TypeDef
builtinStringTypeP = choice
  [ try utf8WithConstraint
  , try printableWithConstraint
  , try ia5WithConstraint
  , try visibleWithConstraint
  ]

utf8WithConstraint :: Parser ASN1TypeDef
utf8WithConstraint = do
  reserved "UTF8String"
  _ <- optional (try sizeConstraintP)
  pure TDUTF8String

printableWithConstraint :: Parser ASN1TypeDef
printableWithConstraint = do
  reserved "PrintableString"
  _ <- optional (try sizeConstraintP)
  pure TDPrintableString

ia5WithConstraint :: Parser ASN1TypeDef
ia5WithConstraint = do
  reserved "IA5String"
  _ <- optional (try sizeConstraintP)
  pure TDIA5String

visibleWithConstraint :: Parser ASN1TypeDef
visibleWithConstraint = do
  reserved "VisibleString"
  _ <- optional (try sizeConstraintP)
  pure TDVisibleString

sequenceOfP :: Parser ASN1TypeDef
sequenceOfP = do
  reserved "SEQUENCE"
  reserved "OF"
  TDSequenceOf <$> typeDefP

setOfP :: Parser ASN1TypeDef
setOfP = do
  reserved "SET"
  reserved "OF"
  TDSetOf <$> typeDefP

namedTypeP :: Parser ASN1TypeDef
namedTypeP = TDNamedType <$> typeReference
