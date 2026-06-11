-- | Lexer primitives for the proto IDL parser.
module Proto.IDL.Parser.Lexer (
  Parser,
  sc,
  lexeme,
  symbol,
  braces,
  brackets,
  parens,
  angles,
  semi,
  comma,
  equals,
  identifier,
  fullIdent,
  intLiteral,
  floatLiteral,
  stringLiteral,
  boolLiteral,
  reserved,
  option,

  -- * Span support
  withSpan,

  -- * Doc comment support
  CommentMap,
  buildCommentMap,
  lookupDoc,

  -- * Full comment collection
  LocComment (..),
  buildAllComments,
  collectComments,
) where

import Control.Monad (void)
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isLetter, isOctDigit)
import Data.Functor qualified
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Proto.IDL.AST (Comment (..))
import Proto.IDL.AST.Span (Span, mkSpan)
import Text.Megaparsec hiding (option)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L


-- | The parser monad used by the proto IDL parser.
type Parser = Parsec Void Text


-- | Try a parser, returning the given default if it does not match.
option :: a -> Parser a -> Parser a
option = MP.option


-- | Space consumer: line comments (//) and block comments (/* ... */)
sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")


-- | Wrap a parser to consume trailing whitespace and comments.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc


-- | Parse a fixed symbol and consume trailing whitespace.
symbol :: Text -> Parser Text
symbol = L.symbol sc


-- | Parse content enclosed in curly braces.
braces :: Parser a -> Parser a
braces p = do
  _ <- symbol "{" <?> "'{'"
  x <- p
  _ <- symbol "}" <?> "closing '}'"
  pure x


-- | Parse content enclosed in square brackets.
brackets :: Parser a -> Parser a
brackets p = do
  _ <- symbol "[" <?> "'['"
  x <- p
  _ <- symbol "]" <?> "closing ']'"
  pure x


-- | Parse content enclosed in parentheses.
parens :: Parser a -> Parser a
parens p = do
  _ <- symbol "(" <?> "'('"
  x <- p
  _ <- symbol ")" <?> "closing ')'"
  pure x


-- | Parse content enclosed in angle brackets.
angles :: Parser a -> Parser a
angles p = do
  _ <- symbol "<" <?> "'<'"
  x <- p
  _ <- symbol ">" <?> "closing '>'"
  pure x


-- | Parse a semicolon.
semi :: Parser ()
semi = void (symbol ";" <?> "';'")


-- | Parse a comma.
comma :: Parser ()
comma = void (symbol "," <?> "','")


-- | Parse an equals sign.
equals :: Parser ()
equals = void (symbol "=" <?> "'='")


-- | An identifier: letter or underscore followed by alphanums/underscores.
identifier :: Parser Text
identifier =
  lexeme
    ( do
        c <- satisfy (\ch -> isLetter ch || ch == '_')
        rest <- takeWhileP (Just "identifier character") (\ch -> isAlphaNum ch || ch == '_')
        pure (T.cons c rest)
    )
    <?> "identifier"


{- | A fully-qualified identifier: ident (.ident)*
Also handles leading dot for fully qualified names.
-}
fullIdent :: Parser Text
fullIdent =
  lexeme
    ( do
        leading <- option "" (T.singleton <$> char '.')
        first <- identRaw
        rest <- many (T.cons <$> char '.' <*> identRaw)
        pure (T.concat (leading : first : rest))
    )
    <?> "type name"
  where
    identRaw :: Parser Text
    identRaw = do
      c <- satisfy (\ch -> isLetter ch || ch == '_')
      rest <- takeWhileP Nothing (\ch -> isAlphaNum ch || ch == '_')
      pure (T.cons c rest)


-- | Parse an integer literal (decimal, hex, or octal), with optional sign.
intLiteral :: Parser Integer
intLiteral =
  lexeme
    ( do
        sign <- option id (negate <$ char '-')
        n <-
          choice
            [ try (char '0' *> char' 'x') *> L.hexadecimal
            , try (char '0' *> octalNum)
            , L.decimal
            ]
        pure (sign n)
    )
    <?> "integer literal"
  where
    octalNum = do
      digits <- takeWhile1P (Just "octal digit") isOctDigit
      pure (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 digits)


-- | Parse a floating-point literal, including @inf@ and @nan@.
floatLiteral :: Parser Double
floatLiteral =
  lexeme
    ( do
        sign <- option id (negate <$ char '-')
        n <-
          choice
            [ try $ do
                whole <- takeWhile1P Nothing isDigit
                void (char '.')
                frac <- takeWhileP Nothing isDigit
                ex <- option "" exponentPart
                -- @read@ rejects a bare trailing dot (e.g. "1."), so
                -- normalise an empty fractional part to a single zero.
                let fracStr = if T.null frac then "0" else T.unpack frac
                pure (read (T.unpack whole <> "." <> fracStr <> T.unpack ex))
            , try $ do
                void (char '.')
                frac <- takeWhile1P Nothing isDigit
                ex <- option "" exponentPart
                pure (read ("0." <> T.unpack frac <> T.unpack ex))
            , try $ do
                whole <- takeWhile1P Nothing isDigit
                ex <- exponentPart
                pure (read (T.unpack whole <> T.unpack ex))
            , 1 / 0 <$ try (keywordTok "infinity" <|> keywordTok "inf")
            , (0 / 0) <$ try (keywordTok "nan")
            ]
        pure (sign n)
    )
    <?> "float literal"
  where
    exponentPart = do
      e <- T.singleton <$> char' 'e'
      s <- option "" (T.singleton <$> (char '+' <|> char '-'))
      digits <- takeWhile1P Nothing isDigit
      pure (e <> s <> digits)


{- | Parse a string literal (double-quoted or single-quoted), with escape support.
Adjacent string literals are concatenated per the proto spec.
-}
stringLiteral :: Parser Text
stringLiteral =
  lexeme
    ( do
        parts <- some singleString
        pure (T.concat parts)
    )
    <?> "string literal"
  where
    singleString = do
      q <- char '"' <|> char '\''
      content <- many (escapedChar <|> satisfy (\c -> c /= q && c /= '\\' && c /= '\n'))
      void (char q)
      sc
      pure (T.pack content)

    escapedChar =
      char '\\'
        *> choice
          [ '\a' <$ char 'a'
          , '\b' <$ char 'b'
          , '\f' <$ char 'f'
          , '\n' <$ char 'n'
          , '\r' <$ char 'r'
          , '\t' <$ char 't'
          , '\v' <$ char 'v'
          , '\\' <$ char '\\'
          , '\'' <$ char '\''
          , '"' <$ char '"'
          , '?' <$ char '?'
          , hexEscape
          , unicode4Escape
          , unicode8Escape
          , octEscape
          ]

    hexValue :: [Char] -> Int
    hexValue = foldl (\acc c -> acc * 16 + digitToInt c) 0

    -- @\xH@ or @\xHH@: one or two hex digits (protoc allows a single digit).
    hexEscape = do
      void (char 'x' <|> char 'X')
      d1 <- hexDigitChar
      md2 <- optional hexDigitChar
      pure (chr (hexValue (d1 : maybe [] (: []) md2)))

    -- @\uHHHH@: a Basic-Multilingual-Plane code point (always valid).
    unicode4Escape = do
      void (char 'u')
      ds <- count 4 hexDigitChar
      pure (chr (hexValue ds))

    -- @\UHHHHHHHH@: a full code point; reject values above U+10FFFF.
    unicode8Escape = do
      void (char 'U')
      ds <- count 8 hexDigitChar
      let cp = hexValue ds
      if cp <= 0x10FFFF
        then pure (chr cp)
        else fail "invalid Unicode code point in \\U escape (above U+10FFFF)"

    octEscape = do
      d1 <- octDigitChar
      d2 <- optional octDigitChar
      d3 <- optional octDigitChar
      let val = case (d2, d3) of
            (Nothing, _) -> digitToInt d1
            (Just d2', Nothing) -> digitToInt d1 * 8 + digitToInt d2'
            (Just d2', Just d3') -> digitToInt d1 * 64 + digitToInt d2' * 8 + digitToInt d3'
      pure (chr val)


-- | Parse a boolean literal (@true@ or @false@).
boolLiteral :: Parser Bool
boolLiteral =
  lexeme
    ( choice
        [ True <$ try (keywordTok "true")
        , False <$ try (keywordTok "false")
        ]
    )
    <?> "boolean (true or false)"


-- | Characters that may continue an identifier.
isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'


-- | Parse a reserved keyword, ensuring it is not a prefix of an identifier.
reserved :: Text -> Parser ()
reserved w = lexeme $ do
  void (string w)
  notFollowedBy (satisfy isIdentChar)


{- | Match a bare keyword token (no trailing-whitespace handling) that must
not be immediately followed by an identifier character. Used for value
keywords like @true@ / @inf@ so identifiers that merely start with them
(e.g. @trueish@, @information@) are not mis-tokenised as the keyword.
-}
keywordTok :: Text -> Parser Text
keywordTok w = string w <* notFollowedBy (satisfy isIdentChar)


-- ---------------------------------------------------------------------------
-- Span support
-- ---------------------------------------------------------------------------

-- | Run a parser and record the byte-offset span of what it consumed.
withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
  start <- getOffset
  result <- p
  end <- getOffset
  pure (mkSpan start end, result)


-- ---------------------------------------------------------------------------
-- Doc comment support (two-pass approach)
-- ---------------------------------------------------------------------------

{- | A map from 1-based line numbers to the doc comment block that applies to
definitions starting on that line.  Built by 'buildCommentMap' in a
pre-scan of the source text before parsing begins.
-}
type CommentMap = IntMap Text


{- | Pre-scan source text to collect doc comment blocks.

Consecutive @\/\/@ comment lines form a single doc block.  The block is
associated with the first non-blank, non-comment line that follows it
(i.e. the definition line).  Each @\/\/@ prefix (plus one optional
leading space) is stripped.
-}
buildCommentMap :: Text -> CommentMap
buildCommentMap src = go 1 [] (T.lines src)
  where
    go :: Int -> [(Int, Text)] -> [Text] -> IntMap Text
    go !_ [] [] = IntMap.empty
    go !_ acc [] =
      -- trailing comments with no following definition: drop them
      case acc of
        [] -> IntMap.empty
        _ -> IntMap.empty
    go !lineNum acc (ln : rest)
      | isCommentLine ln =
          go (lineNum + 1) (acc <> [(lineNum, stripComment ln)]) rest
      | isBlankLine ln =
          if null acc
            then go (lineNum + 1) [] rest
            else -- blank line after comments but before definition:
            -- keep accumulating, the blank line is just spacing
              go (lineNum + 1) acc rest
      | otherwise =
          -- This is a definition (or other non-comment) line.
          let rest' = go (lineNum + 1) [] rest
          in case acc of
               [] -> rest'
               _ -> IntMap.insert lineNum (T.intercalate "\n" (fmap snd acc)) rest'

    isCommentLine t =
      let stripped = T.stripStart t
      in "//" `T.isPrefixOf` stripped
    isBlankLine t = T.null (T.strip t)

    stripComment :: Text -> Text
    stripComment t =
      let afterSlashes = T.drop 2 (T.stripStart t)
      in case T.uncons afterSlashes of
           Just (' ', rest) -> rest
           _ -> afterSlashes


-- | Look up the doc comment for a definition at the given 1-based line number.
lookupDoc :: CommentMap -> Int -> Maybe Text
lookupDoc = flip IntMap.lookup


-- ---------------------------------------------------------------------------
-- Full comment collection (for comment-preserving round-trip)
-- ---------------------------------------------------------------------------

-- | A located comment from the source.
data LocComment = LocComment
  { lcLine :: !Int
  -- ^ 1-based line number
  , lcComment :: !Comment
  -- ^ The comment itself
  }
  deriving stock (Show, Eq)


{- | Pre-scan source text to collect ALL comments with their line numbers.
This captures every @\/\/@ and @/* ... *\/@ comment in the file.
-}
buildAllComments :: Text -> [LocComment]
buildAllComments src = go 1 (T.unpack src)
  where
    go :: Int -> String -> [LocComment]
    go !_ [] = []
    go !ln ('/' : '/' : rest) =
      let (content, after) = span (/= '\n') rest
          lc = LocComment ln (LineComment (T.pack content))
      in lc : case after of
           ('\n' : after') -> go (ln + 1) after'
           _ -> []
    go !ln ('/' : '*' : rest) =
      let (content, after, endLn) = scanBlock ln rest
          lc = LocComment ln (BlockComment (T.pack content))
      in lc : go endLn after
    go !ln ('"' : rest) = go ln (skipString '"' rest)
    go !ln ('\'' : rest) = go ln (skipString '\'' rest)
    go !ln ('\n' : rest) = go (ln + 1) rest
    go !ln (_ : rest) = go ln rest

    scanBlock :: Int -> String -> (String, String, Int)
    scanBlock !ln [] = ([], [], ln)
    scanBlock !ln ('*' : '/' : rest) = ([], rest, ln)
    scanBlock !ln ('\n' : rest) =
      let (content, after, endLn) = scanBlock (ln + 1) rest
      in ('\n' : content, after, endLn)
    scanBlock !ln (c : rest) =
      let (content, after, endLn) = scanBlock ln rest
      in (c : content, after, endLn)

    skipString :: Char -> String -> String
    skipString _ [] = []
    skipString q ('\\' : _ : rest) = skipString q rest
    skipString q (c : rest)
      | c == q = rest
      | otherwise = skipString q rest


{- | Collect any comments and whitespace at the current position,
returning the comments found. This is used by the parser at
definition boundaries to capture standalone comments.
-}
collectComments :: Parser [Comment]
collectComments = do
  cs <- many (try lineCommentP <|> try blockCommentP <|> (space1 Data.Functor.$> Nothing))
  pure (catMaybes cs)
  where
    lineCommentP :: Parser (Maybe Comment)
    lineCommentP = do
      _ <- string "//"
      content <- takeWhileP Nothing (/= '\n')
      _ <- optional newline
      pure (Just (LineComment content))
    blockCommentP :: Parser (Maybe Comment)
    blockCommentP = do
      _ <- string "/*"
      content <- manyTill anySingle (string "*/")
      pure (Just (BlockComment (T.pack content)))
