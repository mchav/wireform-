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
  ) where

import Control.Monad (void)
import Data.Char (chr, digitToInt, isAlphaNum, isLetter)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

semi :: Parser ()
semi = void (symbol ";")

comma :: Parser ()
comma = void (symbol ",")

equals :: Parser ()
equals = void (symbol "=")

-- | An identifier: letter or underscore followed by alphanums/underscores.
identifier :: Parser Text
identifier = lexeme $ do
  c <- satisfy (\ch -> isLetter ch || ch == '_')
  rest <- takeWhileP (Just "identifier character") (\ch -> isAlphaNum ch || ch == '_')
  pure (T.cons c rest)

-- | A fully-qualified identifier: ident (.ident)*
-- Also handles leading dot for fully qualified names.
fullIdent :: Parser Text
fullIdent = lexeme $ do
  leading <- option "" (T.singleton <$> char '.')
  first <- identRaw
  rest <- many (T.cons <$> char '.' <*> identRaw)
  pure (T.concat (leading : first : rest))
  where
    identRaw :: Parser Text
    identRaw = do
      c <- satisfy (\ch -> isLetter ch || ch == '_')
      rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_')
      pure (T.cons c rest)

intLiteral :: Parser Integer
intLiteral = lexeme $ do
  sign <- option id (negate <$ char '-')
  n <- choice
    [ char '0' *> char' 'x' *> L.hexadecimal
    , char '0' *> octalNum
    , L.decimal
    ]
  pure (sign n)
  where
    octalNum = do
      digits <- takeWhile1P (Just "octal digit") (\c -> c >= '0' && c <= '7')
      pure (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 digits)

floatLiteral :: Parser Double
floatLiteral = lexeme $ do
  sign <- option id (negate <$ char '-')
  n <- choice
    [ try $ do
        whole <- takeWhile1P Nothing (\c -> c >= '0' && c <= '9')
        void (char '.')
        frac <- takeWhileP Nothing (\c -> c >= '0' && c <= '9')
        ex <- option "" exponentPart
        pure (read (T.unpack whole <> "." <> T.unpack frac <> T.unpack ex))
    , try $ do
        whole <- takeWhile1P Nothing (\c -> c >= '0' && c <= '9')
        ex <- exponentPart
        pure (read (T.unpack whole <> T.unpack ex))
    , 1/0 <$ (string "inf" <|> string "infinity")
    , (0/0) <$ string "nan"
    ]
  pure (sign n)
  where
    exponentPart = do
      e <- T.singleton <$> char' 'e'
      s <- option "" (T.singleton <$> (char '+' <|> char '-'))
      digits <- takeWhile1P Nothing (\c -> c >= '0' && c <= '9')
      pure (e <> s <> digits)

-- | Parse a string literal (double-quoted or single-quoted), with escape support.
-- Adjacent string literals are concatenated per the proto spec.
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  parts <- some singleString
  pure (T.concat parts)
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
boolLiteral = lexeme $ choice
  [ True  <$ string "true"
  , False <$ string "false"
  ]

reserved :: Text -> Parser ()
reserved w = lexeme $ do
  void (string w)
  notFollowedBy (satisfy (\c -> isAlphaNum c || c == '_'))
