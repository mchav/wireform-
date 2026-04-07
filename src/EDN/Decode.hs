{-# LANGUAGE BangPatterns #-}
-- | EDN (Extensible Data Notation) text decoding via recursive descent.
module EDN.Decode
  ( decode
  , decodeBS
  ) where

import Data.ByteString (ByteString)
import Data.Char (isDigit, isSpace, digitToInt, chr, ord)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified EDN.Value as E

-- | Parse an EDN value from 'Text'.
decode :: Text -> Either String E.Value
decode !input =
  let !s = T.unpack input
  in case parseValue (skipWS s) of
       Left err -> Left err
       Right (val, rest) ->
         case skipWS rest of
           [] -> Right val
           _  -> Left "EDN.Decode: trailing characters"

-- | Parse an EDN value from a UTF-8 'ByteString'.
decodeBS :: ByteString -> Either String E.Value
decodeBS !bs =
  case TE.decodeUtf8' bs of
    Left _  -> Left "EDN.Decode: invalid UTF-8"
    Right t -> decode t

type Parser a = String -> Either String (a, String)

skipWS :: String -> String
skipWS [] = []
skipWS (';':cs) = skipWS (dropWhile (/= '\n') cs)
skipWS (c:cs)
  | c == ',' || isSpace c = skipWS cs
  | otherwise = c : cs

parseValue :: Parser E.Value
parseValue [] = Left "EDN.Decode: unexpected end of input"
parseValue s@(c:cs) = case c of
  'n' -> parseNilOrSymbol s
  't' -> parseTrueOrSymbol s
  'f' -> parseFalseOrSymbol s
  '"' -> parseString cs
  '\\' -> parseCharLiteral cs
  ':' -> parseKeyword cs
  '(' -> parseList cs
  '[' -> parseVector cs
  '{' -> parseMap cs
  '#' -> parseHash cs
  '-' -> parseNegativeOrSymbol s
  '+' -> parsePlusOrSymbol s
  _
    | isDigit c -> parseNumber s
    | isSymbolStart c -> parseSymbol s
    | otherwise -> Left $ "EDN.Decode: unexpected character: " ++ show c

parseNilOrSymbol :: Parser E.Value
parseNilOrSymbol s =
  let (tok, rest) = spanToken s
  in if tok == "nil"
     then Right (E.Nil, rest)
     else Right (toSymbol tok, rest)

parseTrueOrSymbol :: Parser E.Value
parseTrueOrSymbol s =
  let (tok, rest) = spanToken s
  in if tok == "true"
     then Right (E.Bool True, rest)
     else Right (toSymbol tok, rest)

parseFalseOrSymbol :: Parser E.Value
parseFalseOrSymbol s =
  let (tok, rest) = spanToken s
  in if tok == "false"
     then Right (E.Bool False, rest)
     else Right (toSymbol tok, rest)

parseNegativeOrSymbol :: Parser E.Value
parseNegativeOrSymbol ('-':c:cs)
  | isDigit c = parseNumber ('-':c:cs)
parseNegativeOrSymbol s = parseSymbol s

parsePlusOrSymbol :: Parser E.Value
parsePlusOrSymbol ('+':c:cs)
  | isDigit c = parseNumber ('+':c:cs)
parsePlusOrSymbol s = parseSymbol s

parseNumber :: Parser E.Value
parseNumber s =
  let (tok, rest) = spanToken s
  in case parseNumberToken tok of
       Left err -> Left err
       Right val -> Right (val, rest)

parseNumberToken :: String -> Either String E.Value
parseNumberToken tok =
  case break (\c -> c == '.' || c == 'e' || c == 'E') tok of
    (_, []) ->
      case tok of
        ('+':digits) -> Right (E.Integer (read digits))
        _            -> Right (E.Integer (read tok))
    _ ->
      let tokClean = case tok of { ('+':r) -> r; _ -> tok }
      in case reads tokClean :: [(Double, String)] of
           [(d, "")] -> Right (E.Float d)
           [(d, "M")] -> Right (E.Float d)
           _ -> Left $ "EDN.Decode: invalid number: " ++ tok

parseString :: Parser E.Value
parseString = go []
  where
    go _acc [] = Left "EDN.Decode: unterminated string"
    go acc ('"':rest) = Right (E.String (T.pack (reverse acc)), rest)
    go acc ('\\':c:rest) = case c of
      'n'  -> go ('\n':acc) rest
      't'  -> go ('\t':acc) rest
      'r'  -> go ('\r':acc) rest
      '"'  -> go ('"':acc) rest
      '\\' -> go ('\\':acc) rest
      'u'  -> case parseUnicodeEscape rest of
                Left err -> Left err
                Right (ch, rest') -> go (ch:acc) rest'
      _    -> Left $ "EDN.Decode: invalid string escape: \\" ++ [c]
    go acc (c:rest) = go (c:acc) rest

parseUnicodeEscape :: String -> Either String (Char, String)
parseUnicodeEscape s
  | length hex < 4 = Left "EDN.Decode: incomplete unicode escape"
  | otherwise = Right (chr (foldl (\a d -> a * 16 + digitToInt d) 0 hex), drop 4 s)
  where hex = take 4 s

parseCharLiteral :: Parser E.Value
parseCharLiteral [] = Left "EDN.Decode: unexpected end of input after \\"
parseCharLiteral s =
  let (tok, rest) = spanCharToken s
  in case tok of
       "newline" -> Right (E.Char '\n', rest)
       "return"  -> Right (E.Char '\r', rest)
       "space"   -> Right (E.Char ' ', rest)
       "tab"     -> Right (E.Char '\t', rest)
       ['u', a, b, c, d] | all isHexDigit [a,b,c,d] ->
         Right (E.Char (chr (foldl (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d])), rest)
       [c] -> Right (E.Char c, rest)
       _   -> Left $ "EDN.Decode: invalid character literal: \\" ++ tok

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

spanCharToken :: String -> (String, String)
spanCharToken [] = ([], [])
spanCharToken [c] = ([c], [])
spanCharToken (c:cs)
  | isAlphaNum c =
      let (rest, after) = span isAlphaNum cs
      in (c:rest, after)
  | otherwise = ([c], cs)
  where
    isAlphaNum x = (x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || isDigit x

parseKeyword :: Parser E.Value
parseKeyword s =
  let (tok, rest) = spanToken s
  in case break (== '/') tok of
       (_, [])     -> Right (E.Keyword Nothing (T.pack tok), rest)
       (ns, '/':n) -> Right (E.Keyword (Just (T.pack ns)) (T.pack n), rest)
       _           -> Left $ "EDN.Decode: invalid keyword: :" ++ tok

parseSymbol :: Parser E.Value
parseSymbol s =
  let (tok, rest) = spanToken s
  in Right (toSymbol tok, rest)

toSymbol :: String -> E.Value
toSymbol tok =
  case break (== '/') tok of
    (_, [])     -> E.Symbol Nothing (T.pack tok)
    (_, "/")    -> E.Symbol Nothing (T.pack tok)
    (ns, '/':n) -> E.Symbol (Just (T.pack ns)) (T.pack n)
    _           -> E.Symbol Nothing (T.pack tok)

parseList :: Parser E.Value
parseList = parseCollection ')' E.List

parseVector :: Parser E.Value
parseVector = parseCollection ']' E.Vector

parseCollection :: Char -> (V.Vector E.Value -> E.Value) -> Parser E.Value
parseCollection close ctor = go []
  where
    go !acc s =
      case skipWS s of
        [] -> Left $ "EDN.Decode: unterminated collection, expected " ++ show close
        (c:rest)
          | c == close -> Right (ctor (V.fromList (reverse acc)), rest)
          | otherwise -> do
              (val, rest') <- parseValue (c:rest)
              go (val:acc) rest'

parseMap :: Parser E.Value
parseMap = go []
  where
    go !acc s =
      case skipWS s of
        [] -> Left "EDN.Decode: unterminated map"
        ('}':rest) -> Right (E.Map (V.fromList (reverse acc)), rest)
        s' -> do
          (k, s'') <- parseValue s'
          case skipWS s'' of
            [] -> Left "EDN.Decode: unterminated map, missing value"
            s''' -> do
              (v, s'''') <- parseValue s'''
              go ((k, v):acc) s''''

parseHash :: Parser E.Value
parseHash [] = Left "EDN.Decode: unexpected end of input after #"
parseHash ('{':rest) = parseSet rest
parseHash ('_':rest) = parseDiscard rest
parseHash ('#':rest) = parseDispatch rest
parseHash s = parseTagged s

parseSet :: Parser E.Value
parseSet = go []
  where
    go !acc s =
      case skipWS s of
        [] -> Left "EDN.Decode: unterminated set"
        ('}':rest) -> Right (E.Set (V.fromList (reverse acc)), rest)
        s' -> do
          (val, rest) <- parseValue s'
          go (val:acc) rest

parseDiscard :: Parser E.Value
parseDiscard s =
  case skipWS s of
    [] -> Left "EDN.Decode: unexpected end of input after #_"
    s' -> do
      (_, rest) <- parseValue s'
      parseValue (skipWS rest)

parseDispatch :: Parser E.Value
parseDispatch s =
  let (tok, rest) = spanToken s
  in case tok of
       "Inf"  -> Right (E.Float (1/0), rest)
       "-Inf" -> Right (E.Float (-1/0), rest)
       "NaN"  -> Right (E.Float (0/0), rest)
       _      -> Left $ "EDN.Decode: unknown dispatch: ##" ++ tok

parseTagged :: Parser E.Value
parseTagged s =
  let (tok, rest) = spanToken s
  in case break (== '/') tok of
       (name, []) -> do
         (val, rest') <- parseValue (skipWS rest)
         Right (E.Tagged (T.pack "") (T.pack name) val, rest')
       (ns, '/':name) -> do
         (val, rest') <- parseValue (skipWS rest)
         Right (E.Tagged (T.pack ns) (T.pack name) val, rest')
       _ -> Left $ "EDN.Decode: invalid tag: #" ++ tok

spanToken :: String -> (String, String)
spanToken = span isTokenChar

isTokenChar :: Char -> Bool
isTokenChar c =
  not (isSpace c) && c /= ',' && c /= '"' && c /= ';'
  && c /= '(' && c /= ')' && c /= '[' && c /= ']'
  && c /= '{' && c /= '}' && c /= '\\' && c /= '#'

isSymbolStart :: Char -> Bool
isSymbolStart c =
  let o = ord c
  in (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
     || c == '.' || c == '*' || c == '!' || c == '_'
     || c == '?' || c == '$' || c == '%' || c == '&'
     || c == '=' || c == '<' || c == '>' || c == '/'
     || c == '+' || c == '-'
     || o > 127
