-- | Lexer primitives for the proto IDL parser.
module Proto.Parser.Lexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , braces
  , brackets
  , parens
  , angles
  , semi
  , comma
  , equals
  , identifier
  , fullIdent
  , intLiteral
  , floatLiteral
  , stringLiteral
  , boolLiteral
  , reserved
  , option
  ) where

import Control.Monad (void)
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isLetter, isOctDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (option)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

option :: a -> Parser a -> Parser a
option = MP.option

-- | Space consumer: line comments (//) and block comments (/* ... */)
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

braces :: Parser a -> Parser a
braces p = do
  _ <- symbol "{" <?> "'{'"
  x <- p
  _ <- symbol "}" <?> "closing '}'"
  pure x

brackets :: Parser a -> Parser a
brackets p = do
  _ <- symbol "[" <?> "'['"
  x <- p
  _ <- symbol "]" <?> "closing ']'"
  pure x

parens :: Parser a -> Parser a
parens p = do
  _ <- symbol "(" <?> "'('"
  x <- p
  _ <- symbol ")" <?> "closing ')'"
  pure x

angles :: Parser a -> Parser a
angles p = do
  _ <- symbol "<" <?> "'<'"
  x <- p
  _ <- symbol ">" <?> "closing '>'"
  pure x

semi :: Parser ()
semi = void (symbol ";" <?> "';'")

comma :: Parser ()
comma = void (symbol "," <?> "','")

equals :: Parser ()
equals = void (symbol "=" <?> "'='")

-- | An identifier: letter or underscore followed by alphanums/underscores.
identifier :: Parser Text
identifier = lexeme (do
  c <- satisfy (\ch -> isLetter ch || ch == '_')
  rest <- takeWhileP (Just "identifier character") (\ch -> isAlphaNum ch || ch == '_')
  pure (T.cons c rest)) <?> "identifier"

-- | A fully-qualified identifier: ident (.ident)*
-- Also handles leading dot for fully qualified names.
fullIdent :: Parser Text
fullIdent = (lexeme $ do
  leading <- option "" (T.singleton <$> char '.')
  first <- identRaw
  rest <- many (T.cons <$> char '.' <*> identRaw)
  pure (T.concat (leading : first : rest))) <?> "type name"
  where
    identRaw :: Parser Text
    identRaw = do
      c <- satisfy (\ch -> isLetter ch || ch == '_')
      rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_')
      pure (T.cons c rest)

intLiteral :: Parser Integer
intLiteral = (lexeme $ do
  sign <- option id (negate <$ char '-')
  n <- choice
    [ try (char '0' *> char' 'x') *> L.hexadecimal
    , try (char '0' *> octalNum)
    , L.decimal
    ]
  pure (sign n)) <?> "integer literal"
  where
    octalNum = do
      digits <- takeWhile1P (Just "octal digit") isOctDigit
      pure (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 digits)

floatLiteral :: Parser Double
floatLiteral = (lexeme $ do
  sign <- option id (negate <$ char '-')
  n <- choice
    [ try $ do
        whole <- takeWhile1P Nothing isDigit
        void (char '.')
        frac <- takeWhileP Nothing isDigit
        ex <- option "" exponentPart
        pure (read (T.unpack whole <> "." <> T.unpack frac <> T.unpack ex))
    , try $ do
        whole <- takeWhile1P Nothing isDigit
        ex <- exponentPart
        pure (read (T.unpack whole <> T.unpack ex))
    , 1/0 <$ (string "inf" <|> string "infinity")
    , (0/0) <$ string "nan"
    ]
  pure (sign n)) <?> "float literal"
  where
    exponentPart = do
      e <- T.singleton <$> char' 'e'
      s <- option "" (T.singleton <$> (char '+' <|> char '-'))
      digits <- takeWhile1P Nothing isDigit
      pure (e <> s <> digits)

-- | Parse a string literal (double-quoted or single-quoted), with escape support.
-- Adjacent string literals are concatenated per the proto spec.
stringLiteral :: Parser Text
stringLiteral = (lexeme $ do
  parts <- some singleString
  pure (T.concat parts)) <?> "string literal"
  where
    singleString = do
      q <- char '"' <|> char '\''
      content <- many (escapedChar <|> satisfy (\c -> c /= q && c /= '\\' && c /= '\n'))
      void (char q)
      sc
      pure (T.pack content)

    escapedChar = char '\\' *> choice
      [ '\a' <$ char 'a'
      , '\b' <$ char 'b'
      , '\f' <$ char 'f'
      , '\n' <$ char 'n'
      , '\r' <$ char 'r'
      , '\t' <$ char 't'
      , '\v' <$ char 'v'
      , '\\' <$ char '\\'
      , '\'' <$ char '\''
      , '"'  <$ char '"'
      , hexEscape
      , octEscape
      ]

    hexEscape = do
      void (char 'x' <|> char 'X')
      d1 <- hexDigitChar
      d2 <- hexDigitChar
      pure (chr (digitToInt d1 * 16 + digitToInt d2))

    octEscape = do
      d1 <- octDigitChar
      d2 <- optional octDigitChar
      d3 <- optional octDigitChar
      let val = case (d2, d3) of
            (Nothing, _)        -> digitToInt d1
            (Just d2', Nothing) -> digitToInt d1 * 8 + digitToInt d2'
            (Just d2', Just d3') -> digitToInt d1 * 64 + digitToInt d2' * 8 + digitToInt d3'
      pure (chr val)

boolLiteral :: Parser Bool
boolLiteral = (lexeme $ choice
  [ True  <$ string "true"
  , False <$ string "false"
  ]) <?> "boolean (true or false)"

reserved :: Text -> Parser ()
reserved w = lexeme $ do
  void (string w)
  notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_'))
