{-# LANGUAGE BangPatterns #-}

-- | Lexer and recursive-descent parser for CEL source text.
--
-- The parser implements the grammar from the CEL language definition,
-- including the precedence / associativity table, the full lexical syntax for
-- numeric, string, and bytes literals (raw and triple-quoted forms, the
-- complete escape-sequence set), reserved-word rejection, and the
-- message/struct construction form @Name{...}@.
--
-- Negative integer literals are folded at parse time so that
-- @-9223372036854775808@ (whose magnitude does not fit in a signed 64-bit
-- integer) parses to the minimum @int@ value, matching reference CEL behavior.
module CEL.Parser
  ( parse
  , parseExpr
  ) where

import Control.Applicative (Alternative (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (chr, digitToInt, isDigit, ord)
import Data.Int (Int64)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import qualified Data.Set as Set

import CEL.Syntax

-- | Parse CEL source text into an 'Expr', or return a human-readable error.
parse :: Text -> Either String Expr
parse src = do
  toks <- lexer (T.unpack src)
  case runP (parseExpr <* expectTok TkEOF) toks of
    Left err -> Left err
    Right (e, _) -> Right e

----------------------------------------------------------------------
-- Tokens
----------------------------------------------------------------------

data Tok
  = TkIntLit !Integer
  | TkUIntLit !Integer
  | TkDoubleLit !Double
  | TkStringLit !Text
  | TkBytesLit !ByteString
  | TkBool !Bool
  | TkNull
  | TkIdent !Text
  | TkEscIdent !Text
  | TkLParen
  | TkRParen
  | TkLBracket
  | TkRBracket
  | TkLBrace
  | TkRBrace
  | TkComma
  | TkDot
  | TkColon
  | TkQuestion
  | TkOr
  | TkAnd
  | TkNot
  | TkMinus
  | TkPlus
  | TkStar
  | TkSlash
  | TkPercent
  | TkEq
  | TkNe
  | TkLt
  | TkLe
  | TkGt
  | TkGe
  | TkIn
  | TkEOF
  deriving stock (Eq, Show)

-- | Reserved identifiers that may not be used as variable / function /
-- selector / field names.
reservedWords :: Set.Set Text
reservedWords =
  Set.fromList
    [ "as", "break", "const", "continue", "else", "for", "function"
    , "if", "import", "let", "loop", "package", "namespace", "return"
    , "var", "void", "while"
    ]

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

lexer :: String -> Either String [Tok]
lexer = go
  where
    go [] = Right [TkEOF]
    go (c : cs)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' = go cs
      | c == '/' , ('/' : rest) <- cs = go (dropWhile (/= '\n') rest)
      | c == '(' = (TkLParen :) <$> go cs
      | c == ')' = (TkRParen :) <$> go cs
      | c == '[' = (TkLBracket :) <$> go cs
      | c == ']' = (TkRBracket :) <$> go cs
      | c == '{' = (TkLBrace :) <$> go cs
      | c == '}' = (TkRBrace :) <$> go cs
      | c == ',' = (TkComma :) <$> go cs
      | c == ':' = (TkColon :) <$> go cs
      | c == '?' = (TkQuestion :) <$> go cs
      | c == '+' = (TkPlus :) <$> go cs
      | c == '-' = (TkMinus :) <$> go cs
      | c == '*' = (TkStar :) <$> go cs
      | c == '/' = (TkSlash :) <$> go cs
      | c == '%' = (TkPercent :) <$> go cs
      | c == '.' , (d : _) <- cs, isDigit d = lexNumber (c : cs)
      | c == '.' = (TkDot :) <$> go cs
      | c == '|' , ('|' : rest) <- cs = (TkOr :) <$> go rest
      | c == '&' , ('&' : rest) <- cs = (TkAnd :) <$> go rest
      | c == '=' , ('=' : rest) <- cs = (TkEq :) <$> go rest
      | c == '!' , ('=' : rest) <- cs = (TkNe :) <$> go rest
      | c == '!' = (TkNot :) <$> go cs
      | c == '<' , ('=' : rest) <- cs = (TkLe :) <$> go rest
      | c == '<' = (TkLt :) <$> go cs
      | c == '>' , ('=' : rest) <- cs = (TkGe :) <$> go rest
      | c == '>' = (TkGt :) <$> go cs
      | c == '|' = Left "unexpected '|' (did you mean '||'?)"
      | c == '&' = Left "unexpected '&' (did you mean '&&'?)"
      | c == '"' || c == '\'' = lexStringWith False False (c : cs)
      | c == '\x60' = lexBacktick cs
      | isStrPrefix c = lexPrefixed (c : cs)
      | isIdentStart c =
          let (name, rest) = span isIdentChar (c : cs)
           in (identTok (T.pack name) :) <$> go rest
      | isDigit c = lexNumber (c : cs)
      | otherwise = Left ("unexpected character: " ++ show c)

    -- A string/bytes prefix letter followed eventually by a quote.
    lexPrefixed input =
      case scanPrefix input of
        Just (raw, bytes, rest@(q : _))
          | q == '"' || q == '\'' ->
              if bytes
                then lexBytesWith raw rest
                else lexStringWith raw True rest
        _ ->
          let (name, rest) = span isIdentChar input
           in (identTok (T.pack name) :) <$> go rest

    lexStringWith raw _consumedPrefix input = do
      (content, rest) <- scanQuoted raw input
      t <- processString raw content
      (TkStringLit t :) <$> go rest

    lexBytesWith raw input = do
      (content, rest) <- scanQuoted raw input
      b <- processBytes raw content
      (TkBytesLit b :) <$> go rest

    lexNumber input = do
      (t, rest) <- scanNumber input
      (t :) <$> go rest

    -- Backtick-quoted (escaped) identifier, e.g. @`content-type`@.
    lexBacktick input =
      case break (== '\x60') input of
        (_, []) -> Left "unterminated backtick-quoted identifier"
        (name, _ : rest) -> (TkEscIdent (T.pack name) :) <$> go rest

identTok :: Text -> Tok
identTok t = case t of
  "true" -> TkBool True
  "false" -> TkBool False
  "null" -> TkNull
  "in" -> TkIn
  _ -> TkIdent t

isStrPrefix :: Char -> Bool
isStrPrefix c = c == 'r' || c == 'R' || c == 'b' || c == 'B'

-- | Examine a run of prefix letters; succeed only if it is a valid string /
-- bytes prefix (at most one r/R, at most one b/B) directly followed by a
-- quote character. Returns (raw, bytes, remainder-including-quote).
scanPrefix :: String -> Maybe (Bool, Bool, String)
scanPrefix = goP False False
  where
    goP raw bytes (c : cs)
      | c == 'r' || c == 'R' = if raw then Nothing else goP True bytes cs
      | c == 'b' || c == 'B' = if bytes then Nothing else goP raw True cs
      | c == '"' || c == '\'' = Just (raw, bytes, c : cs)
    goP _ _ _ = Nothing

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isIdentChar :: Char -> Bool
isIdentChar c = isIdentStart c || (c >= '0' && c <= '9')

----------------------------------------------------------------------
-- Number lexing
----------------------------------------------------------------------

scanNumber :: String -> Either String (Tok, String)
scanNumber input
  | ('0' : x : rest) <- input
  , x == 'x' || x == 'X' =
      let (hexDigits, rest') = span isHexDigit rest
       in if null hexDigits
            then Left "malformed hex literal"
            else
              let n = foldl' (\acc d -> acc * 16 + toInteger (digitToInt d)) 0 hexDigits
               in case rest' of
                    (u : r2) | u == 'u' || u == 'U' -> Right (TkUIntLit n, r2)
                    _ -> Right (TkIntLit n, rest')
  | otherwise =
      let (intPart, r1) = span isDigit input
          -- fraction: '.' followed by at least one digit
          (fracPart, r2) = case r1 of
            ('.' : d : ds) | isDigit d ->
              let (more, r) = span isDigit ds in ('.' : d : more, r)
            _ -> ("", r1)
          (expPart, r3) = scanExponent r2
       in if null fracPart && null expPart
            then case r1 of
              (u : r) | u == 'u' || u == 'U' -> Right (TkUIntLit (readInteger intPart), r)
              _ ->
                if null intPart
                  then Left "malformed number"
                  else Right (TkIntLit (readInteger intPart), r1)
            else
              let lexeme = (if null intPart then "0" else intPart) ++ fracPart ++ expPart
               in case readDoubleMaybe lexeme of
                    Just d -> Right (TkDoubleLit d, r3)
                    Nothing -> Left ("malformed float literal: " ++ lexeme)

scanExponent :: String -> (String, String)
scanExponent (e : cs)
  | e == 'e' || e == 'E' =
      case cs of
        (s : ds) | s == '+' || s == '-' ->
          let (digs, r) = span isDigit ds
           in if null digs then ("", e : cs) else (e : s : digs, r)
        _ ->
          let (digs, r) = span isDigit cs
           in if null digs then ("", e : cs) else (e : digs, r)
scanExponent cs = ("", cs)

readInteger :: String -> Integer
readInteger = foldl' (\acc d -> acc * 10 + toInteger (digitToInt d)) 0

readDoubleMaybe :: String -> Maybe Double
readDoubleMaybe s = case reads s of
  [(d, "")] -> Just d
  _ -> Nothing

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

----------------------------------------------------------------------
-- String / bytes scanning and escape processing
----------------------------------------------------------------------

-- | Read a quoted literal starting at the opening delimiter. Returns the raw
-- (still-escaped, for non-raw) content and the remaining input. For non-raw
-- strings a backslash escapes the following character so it cannot terminate
-- the string prematurely.
scanQuoted :: Bool -> String -> Either String (String, String)
scanQuoted raw (q : rest)
  | q == '"' || q == '\'' =
      case rest of
        (a : b : cs) | a == q && b == q -> scanTriple raw q cs
        _ -> scanSingle raw q rest
scanQuoted _ _ = Left "expected string delimiter"

scanSingle :: Bool -> Char -> String -> Either String (String, String)
scanSingle raw q = go id
  where
    go _ [] = Left "unterminated string literal"
    go acc (c : cs)
      | c == q = Right (acc [], cs)
      | c == '\n' || c == '\r' = Left "newline in single-quoted string literal"
      | not raw && c == '\\' =
          case cs of
            (e : cs') -> go (acc . (c :) . (e :)) cs'
            [] -> Left "unterminated escape in string literal"
      | otherwise = go (acc . (c :)) cs

scanTriple :: Bool -> Char -> String -> Either String (String, String)
scanTriple raw q = go id
  where
    go _ [] = Left "unterminated triple-quoted string literal"
    go acc (c : cs)
      | c == q
      , (a : b : cs') <- cs
      , a == q && b == q = Right (acc [], cs')
      | not raw && c == '\\' =
          case cs of
            (e : cs') -> go (acc . (c :) . (e :)) cs'
            [] -> Left "unterminated escape in string literal"
      | otherwise = go (acc . (c :)) cs

-- | Process the content of a string literal, interpreting escapes (unless
-- raw) and validating Unicode code points.
processString :: Bool -> String -> Either String Text
processString True content = Right (T.pack content)
processString False content = T.pack <$> goS content
  where
    goS [] = Right []
    goS ('\\' : rest) = do
      (cp, rest') <- escapeCodePoint rest
      validateCodePoint cp
      (chr cp :) <$> goS rest'
    goS (c : cs) = (c :) <$> goS cs

-- | Process the content of a bytes literal into a raw byte sequence.
processBytes :: Bool -> String -> Either String ByteString
processBytes True content = Right (TE.encodeUtf8 (T.pack content))
processBytes False content = BS.pack <$> goB content
  where
    goB [] = Right []
    goB ('\\' : rest) = do
      (bytes, rest') <- escapeBytes rest
      (bytes ++) <$> goB rest'
    goB (c : cs) = (utf8Bytes c ++) <$> goB cs

utf8Bytes :: Char -> [Word8]
utf8Bytes = BS.unpack . TE.encodeUtf8 . T.singleton

-- | Decode one escape sequence (after the backslash) for a string literal,
-- returning a Unicode code point and the remaining input.
escapeCodePoint :: String -> Either String (Int, String)
escapeCodePoint [] = Left "unterminated escape sequence"
escapeCodePoint (c : cs) = case c of
  'a' -> Right (0x07, cs)
  'b' -> Right (0x08, cs)
  'f' -> Right (0x0C, cs)
  'n' -> Right (0x0A, cs)
  'r' -> Right (0x0D, cs)
  't' -> Right (0x09, cs)
  'v' -> Right (0x0B, cs)
  '\\' -> Right (ord '\\', cs)
  '?' -> Right (ord '?', cs)
  '"' -> Right (ord '"', cs)
  '\'' -> Right (ord '\'', cs)
  '`' -> Right (ord '`', cs)
  'x' -> hexN 2 cs
  'X' -> hexN 2 cs
  'u' -> hexN 4 cs
  'U' -> hexN 8 cs
  _ | c >= '0' && c <= '3' -> octal3 (c : cs)
    | otherwise -> Left ("invalid escape sequence: \\" ++ [c])

-- | Decode one escape sequence for a bytes literal, where @\\x@ and octal
-- escapes denote raw octets rather than code points.
escapeBytes :: String -> Either String ([Word8], String)
escapeBytes [] = Left "unterminated escape sequence"
escapeBytes (c : cs) = case c of
  'a' -> Right ([0x07], cs)
  'b' -> Right ([0x08], cs)
  'f' -> Right ([0x0C], cs)
  'n' -> Right ([0x0A], cs)
  'r' -> Right ([0x0D], cs)
  't' -> Right ([0x09], cs)
  'v' -> Right ([0x0B], cs)
  '\\' -> Right ([fromIntegral (ord '\\')], cs)
  '?' -> Right ([fromIntegral (ord '?')], cs)
  '"' -> Right ([fromIntegral (ord '"')], cs)
  '\'' -> Right ([fromIntegral (ord '\'')], cs)
  '`' -> Right ([fromIntegral (ord '`')], cs)
  'x' -> byteHex cs
  'X' -> byteHex cs
  'u' -> do (cp, r) <- hexN 4 cs; validateCodePoint cp; Right (utf8Bytes (chr cp), r)
  'U' -> do (cp, r) <- hexN 8 cs; validateCodePoint cp; Right (utf8Bytes (chr cp), r)
  _ | c >= '0' && c <= '7' -> do (cp, r) <- octalByte (c : cs); Right ([fromIntegral cp], r)
    | otherwise -> Left ("invalid escape sequence: \\" ++ [c])
  where
    byteHex s = do (n, r) <- hexN 2 s; Right ([fromIntegral n], r)

hexN :: Int -> String -> Either String (Int, String)
hexN n s =
  let (ds, rest) = splitAt n s
   in if length ds == n && all isHexDigit ds
        then Right (foldl' (\acc d -> acc * 16 + digitToInt d) 0 ds, rest)
        else Left "invalid hexadecimal escape sequence"

octal3 :: String -> Either String (Int, String)
octal3 s =
  let (ds, rest) = splitAt 3 s
   in if length ds == 3 && all isOctDigit ds
        then
          let v = foldl' (\acc d -> acc * 8 + digitToInt d) 0 ds
           in if v <= 0o377 then Right (v, rest) else Left "octal escape out of range"
        else Left "invalid octal escape sequence"

octalByte :: String -> Either String (Int, String)
octalByte = octal3

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

validateCodePoint :: Int -> Either String ()
validateCodePoint cp
  | cp < 0 || cp > 0x10FFFF = Left "invalid Unicode code point in escape sequence"
  | cp >= 0xD800 && cp <= 0xDFFF = Left "surrogate code point in escape sequence"
  | otherwise = Right ()

----------------------------------------------------------------------
-- Parser monad
----------------------------------------------------------------------

newtype P a = P {runP :: [Tok] -> Either String (a, [Tok])}

instance Functor P where
  fmap f (P g) = P $ \ts -> case g ts of
    Left e -> Left e
    Right (a, ts') -> Right (f a, ts')

instance Applicative P where
  pure a = P $ \ts -> Right (a, ts)
  P f <*> P g = P $ \ts -> case f ts of
    Left e -> Left e
    Right (h, ts') -> case g ts' of
      Left e -> Left e
      Right (a, ts'') -> Right (h a, ts'')

instance Monad P where
  P g >>= f = P $ \ts -> case g ts of
    Left e -> Left e
    Right (a, ts') -> runP (f a) ts'

instance Alternative P where
  empty = P $ \_ -> Left "parse error"
  P f <|> P g = P $ \ts -> case f ts of
    Left _ -> g ts
    r -> r

peek :: P Tok
peek = P $ \ts -> case ts of
  (t : _) -> Right (t, ts)
  [] -> Right (TkEOF, ts)

advance :: P Tok
advance = P $ \ts -> case ts of
  (t : rest) -> Right (t, rest)
  [] -> Right (TkEOF, [])

expectTok :: Tok -> P ()
expectTok expected = do
  t <- advance
  if t == expected
    then pure ()
    else parseError ("expected " ++ show expected ++ " but found " ++ show t)

parseError :: String -> P a
parseError msg = P $ \_ -> Left msg

-- | Consume an identifier in a position where it names a variable / function:
-- reserved words are rejected, but backtick-escaped identifiers are accepted.
identName :: P Text
identName = do
  t <- advance
  case t of
    TkIdent n
      | Set.member n reservedWords -> parseError ("reserved word used as name: " ++ T.unpack n)
      | otherwise -> pure n
    TkEscIdent n -> pure n
    _ -> parseError ("expected identifier but found " ++ show t)

-- | Consume a selector / field-init name. Per the grammar @SELECTOR@ only
-- excludes the keyword tokens (@true@/@false@/@null@/@in@, which are not
-- 'TkIdent's), so reserved words such as @as@ or @for@ /are/ valid here.
-- Backtick-escaped identifiers are also accepted.
selectorName :: P Text
selectorName = do
  t <- advance
  case t of
    TkIdent n -> pure n
    TkEscIdent n -> pure n
    _ -> parseError ("expected selector but found " ++ show t)

----------------------------------------------------------------------
-- Grammar
----------------------------------------------------------------------

-- | Top-level expression parser (exposed for testing against the token
-- stream is not needed; 'parse' is the public entry point).
parseExpr :: P Expr
parseExpr = do
  c <- parseOr
  t <- peek
  case t of
    TkQuestion -> do
      _ <- advance
      thenE <- parseOr
      expectTok TkColon
      elseE <- parseExpr
      pure (ECond c thenE elseE)
    _ -> pure c

parseOr :: P Expr
parseOr = do
  l <- parseAnd
  goOr l
  where
    goOr l = do
      t <- peek
      case t of
        TkOr -> do _ <- advance; r <- parseAnd; goOr (EOr l r)
        _ -> pure l

parseAnd :: P Expr
parseAnd = do
  l <- parseRel
  goAnd l
  where
    goAnd l = do
      t <- peek
      case t of
        TkAnd -> do _ <- advance; r <- parseRel; goAnd (EAnd l r)
        _ -> pure l

parseRel :: P Expr
parseRel = do
  l <- parseAdd
  goRel l
  where
    goRel l = do
      t <- peek
      case relOp t of
        Just op -> do _ <- advance; r <- parseAdd; goRel (ERel op l r)
        Nothing -> pure l

relOp :: Tok -> Maybe RelOp
relOp = \case
  TkEq -> Just Eq
  TkNe -> Just Ne
  TkLt -> Just Lt
  TkLe -> Just Le
  TkGt -> Just Gt
  TkGe -> Just Ge
  TkIn -> Just In
  _ -> Nothing

parseAdd :: P Expr
parseAdd = do
  l <- parseMul
  goAdd l
  where
    goAdd l = do
      t <- peek
      case t of
        TkPlus -> do _ <- advance; r <- parseMul; goAdd (EArith Add l r)
        TkMinus -> do _ <- advance; r <- parseMul; goAdd (EArith Sub l r)
        _ -> pure l

parseMul :: P Expr
parseMul = do
  l <- parseUnary
  goMul l
  where
    goMul l = do
      t <- peek
      case t of
        TkStar -> do _ <- advance; r <- parseUnary; goMul (EArith Mul l r)
        TkSlash -> do _ <- advance; r <- parseUnary; goMul (EArith Div l r)
        TkPercent -> do _ <- advance; r <- parseUnary; goMul (EArith Mod l r)
        _ -> pure l

parseUnary :: P Expr
parseUnary = do
  t <- peek
  case t of
    TkNot -> do
      n <- countRun TkNot
      m <- parseMember
      pure (applyN n ENot m)
    TkMinus -> do
      n <- countRun TkMinus
      parseNegated n
    _ -> parseMember

-- | After consuming a run of @n@ unary minus signs, parse the operand,
-- folding a directly-following integer literal into a signed literal so that
-- the minimum 64-bit integer can be represented.
parseNegated :: Int -> P Expr
parseNegated n = do
  t <- peek
  case t of
    TkIntLit mag -> do
      _ <- advance
      let signed = if odd n then negate mag else mag
      lit <- intLiteral signed
      parseMemberSuffix lit
    _ -> do
      m <- parseMember
      pure (applyN n ENeg m)

countRun :: Tok -> P Int
countRun tk = go 0
  where
    go !acc = do
      t <- peek
      if t == tk then advance >> go (acc + 1) else pure acc

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ x = x
applyN k f x = applyN (k - 1) f (f x)

intLiteral :: Integer -> P Expr
intLiteral n
  | n < intMin || n > intMax = parseError "integer literal out of range"
  | otherwise = pure (ELit (LInt (fromInteger n)))
  where
    intMin = toInteger (minBound :: Int64)
    intMax = toInteger (maxBound :: Int64)

uintLiteral :: Integer -> P Expr
uintLiteral n
  | n < 0 || n > toInteger (maxBound :: Word64) = parseError "unsigned integer literal out of range"
  | otherwise = pure (ELit (LUInt (fromInteger n)))

parseMember :: P Expr
parseMember = do
  p <- parsePrimary
  parseMemberSuffix p

parseMemberSuffix :: Expr -> P Expr
parseMemberSuffix e = do
  t <- peek
  case t of
    TkDot -> do
      _ <- advance
      sel <- selectorName
      t2 <- peek
      case t2 of
        TkLParen -> do
          args <- parseCallArgs
          parseMemberSuffix (ECall (Just e) sel args)
        _ -> parseMemberSuffix (ESelect e sel)
    TkLBracket -> do
      _ <- advance
      idx <- parseExpr
      expectTok TkRBracket
      parseMemberSuffix (EIndex e idx)
    TkLBrace
      | Just (root, segs) <- identPath e -> do
          fields <- parseFieldInits
          parseMemberSuffix (EStruct root segs fields)
    _ -> pure e

parsePrimary :: P Expr
parsePrimary = do
  t <- peek
  case t of
    TkIntLit n -> advance >> intLiteral n
    TkUIntLit n -> advance >> uintLiteral n
    TkDoubleLit d -> advance >> pure (ELit (LDouble d))
    TkStringLit s -> advance >> pure (ELit (LString s))
    TkBytesLit b -> advance >> pure (ELit (LBytes b))
    TkBool b -> advance >> pure (ELit (LBool b))
    TkNull -> advance >> pure (ELit LNull)
    TkLParen -> do
      _ <- advance
      e <- parseExpr
      expectTok TkRParen
      pure e
    TkLBracket -> do
      _ <- advance
      es <- parseExprListTrailing TkRBracket
      expectTok TkRBracket
      pure (EList es)
    TkLBrace -> do
      _ <- advance
      entries <- parseMapInitsTrailing
      expectTok TkRBrace
      pure (EMap entries)
    TkDot -> do
      _ <- advance
      name <- identName
      t2 <- peek
      case t2 of
        TkLParen -> do
          args <- parseCallArgs
          pure (ECall Nothing name args)
        _ -> pure (EIdent True name)
    TkIdent n
      | Set.member n reservedWords -> parseError ("reserved word used as name: " ++ T.unpack n)
      | otherwise -> do
          _ <- advance
          t2 <- peek
          case t2 of
            TkLParen -> do
              args <- parseCallArgs
              pure (ECall Nothing n args)
            _ -> pure (EIdent False n)
    _ -> parseError ("unexpected token: " ++ show t)

parseCallArgs :: P [Expr]
parseCallArgs = do
  expectTok TkLParen
  t <- peek
  case t of
    TkRParen -> advance >> pure []
    _ -> do
      args <- parseExprList
      expectTok TkRParen
      pure args

parseExprList :: P [Expr]
parseExprList = do
  e <- parseExpr
  go [e]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do _ <- advance; e <- parseExpr; go (e : acc)
        _ -> pure (reverse acc)

-- | Expression list permitting a single optional trailing comma before the
-- given closing token (used by list and map literals).
parseExprListTrailing :: Tok -> P [Expr]
parseExprListTrailing closeTok = do
  t <- peek
  if t == closeTok
    then pure []
    else do
      e <- parseExpr
      go [e]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == closeTok then pure (reverse acc) else do e <- parseExpr; go (e : acc)
        _ -> pure (reverse acc)

parseMapInitsTrailing :: P [(Expr, Expr)]
parseMapInitsTrailing = do
  t <- peek
  case t of
    TkRBrace -> pure []
    _ -> do
      entry <- parseMapEntry
      go [entry]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == TkRBrace then pure (reverse acc) else do e <- parseMapEntry; go (e : acc)
        _ -> pure (reverse acc)
    parseMapEntry = do
      k <- parseExpr
      expectTok TkColon
      v <- parseExpr
      pure (k, v)

parseFieldInits :: P [(Text, Expr)]
parseFieldInits = do
  expectTok TkLBrace
  t <- peek
  case t of
    TkRBrace -> advance >> pure []
    _ -> do
      f <- parseField
      go [f]
  where
    go acc = do
      t <- peek
      case t of
        TkComma -> do
          _ <- advance
          t2 <- peek
          if t2 == TkRBrace
            then advance >> pure (reverse acc)
            else do f <- parseField; go (f : acc)
        TkRBrace -> advance >> pure (reverse acc)
        _ -> parseError ("expected ',' or '}' in struct but found " ++ show t)
    parseField = do
      name <- selectorName
      expectTok TkColon
      v <- parseExpr
      pure (name, v)
