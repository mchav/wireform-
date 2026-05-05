{-# LANGUAGE BangPatterns #-}
-- | TOML text decoder (parser).
--
-- Parses TOML v1.0 text into a 'TOML.Value.Value'. Supports all TOML
-- types including multi-line strings, hex/octal/binary integers,
-- inline tables, array of tables, and date/time types.
module TOML.Decode
  ( decode
  , decodeBS
  ) where

import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt, isAlphaNum, isDigit, isHexDigit, isSpace, ord, toLower)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified TOML.Value as TV

decode :: Text -> Either String TV.Value
decode !input =
  let !ls0 = T.lines input
      !ls  = joinContinuations ls0
  in parseLines ls [] []

-- | Pre-pass that joins physical lines whose value spans multiple
-- lines (multi-line arrays, multi-line inline tables, multi-line
-- basic / literal strings) into a single logical line. This keeps
-- the line-oriented top-level parser simple at the cost of doing
-- two passes over the input.
joinContinuations :: [Text] -> [Text]
joinContinuations = go
  where
    go [] = []
    go (l : rest)
      | needsJoin l =
          let (joined, rest') = absorb l rest
          in joined : go rest'
      | otherwise = l : go rest

    needsJoin t =
      let (br, brc, dq, sq, mlBasic, mlLit) = balances t
      in br /= 0 || brc /= 0
         || (dq && not (T.null t))
         || (sq && not (T.null t))
         || mlBasic
         || mlLit

    absorb buf [] = (buf, [])
    absorb buf (l : rest) =
      let !next = buf <> T.singleton '\n' <> l
      in if needsJoin next
           then absorb next rest
           else (next, rest)

-- | Tally the relevant scanner state across one or more lines:
-- bracket depth, brace depth, whether we're inside an open
-- single-line basic / literal string, and whether we're inside a
-- multi-line basic (@\"\"\"@) / literal (@'''@) string. A non-zero
-- bracket / brace count or any open string indicates we need to
-- absorb the next physical line into this logical line.
balances :: Text -> (Int, Int, Bool, Bool, Bool, Bool)
balances t = goBal 0 (0 :: Int) (0 :: Int) StateNorm
  where
    !len = T.length t

    goBal !i !br !brc st
      | i >= len =
          let (dq, sq, mlB, mlL) = case st of
                StateNorm     -> (False, False, False, False)
                StateBasic    -> (True,  False, False, False)
                StateLiteral  -> (False, True,  False, False)
                StateMLBasic  -> (False, False, True,  False)
                StateMLLit    -> (False, False, False, True)
          in (br, brc, dq, sq, mlB, mlL)
      | otherwise =
          let c = T.index t i
          in case st of
               StateNorm
                 | c == '#'                     -> finalizeAt i
                 | c == '['                     -> goBal (i+1) (br+1) brc st
                 | c == ']'                     -> goBal (i+1) (br-1) brc st
                 | c == '{'                     -> goBal (i+1) br (brc+1) st
                 | c == '}'                     -> goBal (i+1) br (brc-1) st
                 | c == '"'  && triplePref i    -> goBal (i+3) br brc StateMLBasic
                 | c == '\'' && triplePref i    -> goBal (i+3) br brc StateMLLit
                 | c == '"'                     -> goBal (i+1) br brc StateBasic
                 | c == '\''                    -> goBal (i+1) br brc StateLiteral
                 | otherwise                    -> goBal (i+1) br brc st
               StateBasic
                 | c == '\\' && i + 1 < len     -> goBal (i+2) br brc st
                 | c == '"'                     -> goBal (i+1) br brc StateNorm
                 | otherwise                    -> goBal (i+1) br brc st
               StateLiteral
                 | c == '\''                    -> goBal (i+1) br brc StateNorm
                 | otherwise                    -> goBal (i+1) br brc st
               StateMLBasic
                 | c == '"' && triplePref i     -> goBal (i+3) br brc StateNorm
                 | c == '\\' && i + 1 < len     -> goBal (i+2) br brc st
                 | otherwise                    -> goBal (i+1) br brc st
               StateMLLit
                 | c == '\'' && triplePref i    -> goBal (i+3) br brc StateNorm
                 | otherwise                    -> goBal (i+1) br brc st

    triplePref !i = i + 2 < len
                 && T.index t i == T.index t (i+1)
                 && T.index t i == T.index t (i+2)

    finalizeAt _ = (0, 0, False, False, False, False)
      -- comment kills the rest of the line; the line cannot
      -- "continue" via a comment-only suffix.

data ScanState
  = StateNorm
  | StateBasic
  | StateLiteral
  | StateMLBasic
  | StateMLLit

decodeBS :: ByteString -> Either String TV.Value
decodeBS = decode . TE.decodeUtf8Lenient

data TableCtx = TableCtx
  { tcPath  :: ![Text]
  , tcPairs :: ![(Text, TV.Value)]
  }

parseLines :: [Text] -> [TableCtx] -> [(Text, TV.Value)] -> Either String TV.Value
parseLines [] tables topPairs =
  let !topTable = V.fromList (reverse topPairs)
      !allTables = reverse tables
  in Right (mergeTables topTable allTables)
parseLines (line : rest) tables topPairs =
  let !stripped = T.stripStart line
  in case () of
    _ | T.null stripped || T.isPrefixOf "#" stripped ->
          parseLines rest tables topPairs
      | T.isPrefixOf "[[" stripped ->
          case parseArrayTableHeader stripped of
            Left err -> Left err
            Right path ->
              let (sectionLines, remaining) = spanSection rest
              in case parseSectionPairs sectionLines of
                  Left err -> Left err
                  Right pairs ->
                    let !ctx = TableCtx path pairs
                    in parseLines remaining (ctx : tables) topPairs
      | T.isPrefixOf "[" stripped ->
          case parseTableHeader stripped of
            Left err -> Left err
            Right path ->
              let (sectionLines, remaining) = spanSection rest
              in case parseSectionPairs sectionLines of
                  Left err -> Left err
                  Right pairs ->
                    let !ctx = TableCtx path pairs
                    in parseLines remaining (ctx : tables) topPairs
      | otherwise ->
          case parseKeyValue stripped of
            Left err -> Left err
            Right (k, v) ->
              parseLines rest tables ((k, v) : topPairs)

spanSection :: [Text] -> ([Text], [Text])
spanSection = go []
  where
    go acc [] = (reverse acc, [])
    go acc (l:ls)
      | isTableOrArrayHeader (T.stripStart l) = (reverse acc, l:ls)
      | otherwise = go (l:acc) ls

isTableOrArrayHeader :: Text -> Bool
isTableOrArrayHeader t = T.isPrefixOf "[" t && not (T.null t)

parseSectionPairs :: [Text] -> Either String [(Text, TV.Value)]
parseSectionPairs = go []
  where
    go acc [] = Right (reverse acc)
    go acc (l:ls) =
      let !stripped = T.stripStart l
      in if T.null stripped || T.isPrefixOf "#" stripped
           then go acc ls
           else case parseKeyValue stripped of
                  Left err -> Left err
                  Right (k, v) -> go ((k,v):acc) ls

parseTableHeader :: Text -> Either String [Text]
parseTableHeader t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
      !commentStripped = stripInlineComment inner
  in Right (map T.strip (T.splitOn "." commentStripped))

parseArrayTableHeader :: Text -> Either String [Text]
parseArrayTableHeader t =
  let !afterOpen = T.drop 2 t
      !idx = T.breakOn "]]" afterOpen
  in case idx of
       (inner, rest)
         | T.isPrefixOf "]]" rest ->
             Right (map T.strip (T.splitOn "." (T.strip inner)))
         | otherwise -> Left "TOML: unterminated array table header"

-- | Strip a trailing @# comment@ from a value-bearing line, ignoring
-- any @#@ that appears inside a basic / literal string. This is the
-- minimum quote-awareness needed for the line-oriented top-level
-- parser; multi-line strings are joined into a single logical line
-- by 'joinContinuations' before they reach this function.
stripInlineComment :: Text -> Text
stripInlineComment t = T.stripEnd (go (0 :: Int) StateNorm)
  where
    !len = T.length t
    go !i st
      | i >= len = t
      | otherwise =
          let c = T.index t i
          in case st of
               StateNorm
                 | c == '#'                  -> T.take i t
                 | c == '"'  && triplePref i -> go (i+3) StateMLBasic
                 | c == '\'' && triplePref i -> go (i+3) StateMLLit
                 | c == '"'                  -> go (i+1) StateBasic
                 | c == '\''                 -> go (i+1) StateLiteral
                 | otherwise                 -> go (i+1) st
               StateBasic
                 | c == '\\' && i+1 < len    -> go (i+2) st
                 | c == '"'                  -> go (i+1) StateNorm
                 | otherwise                 -> go (i+1) st
               StateLiteral
                 | c == '\''                 -> go (i+1) StateNorm
                 | otherwise                 -> go (i+1) st
               StateMLBasic
                 | c == '"' && triplePref i  -> go (i+3) StateNorm
                 | c == '\\' && i+1 < len    -> go (i+2) st
                 | otherwise                 -> go (i+1) st
               StateMLLit
                 | c == '\'' && triplePref i -> go (i+3) StateNorm
                 | otherwise                 -> go (i+1) st

    triplePref !i = i + 2 < len
                 && T.index t i == T.index t (i+1)
                 && T.index t i == T.index t (i+2)

parseKeyValue :: Text -> Either String (Text, TV.Value)
parseKeyValue line =
  let (keyPart, rest) = breakOnEquals line
      !key = T.strip (unquoteKey keyPart)
      !valText = T.strip rest
  in case parseTomlValue valText of
       Left err -> Left $ "TOML key '" ++ T.unpack key ++ "': " ++ err
       Right v -> Right (key, v)

breakOnEquals :: Text -> (Text, Text)
breakOnEquals t = go 0 False
  where
    !len = T.length t
    go !i !inStr
      | i >= len = (t, T.empty)
      | T.index t i == '"' = go (i + 1) (not inStr)
      | T.index t i == '\'' = go (i + 1) (not inStr)
      | T.index t i == '=' && not inStr = (T.take i t, T.drop (i + 1) t)
      | otherwise = go (i + 1) inStr

unquoteKey :: Text -> Text
unquoteKey t
  | T.isPrefixOf "\"" t && T.isSuffixOf "\"" t = T.drop 1 (T.dropEnd 1 t)
  | T.isPrefixOf "'" t && T.isSuffixOf "'" t = T.drop 1 (T.dropEnd 1 t)
  | otherwise = t

parseTomlValue :: Text -> Either String TV.Value
parseTomlValue t =
  let !stripped = stripInlineComment t
      !s = T.strip stripped
  in parseTomlValueInner s

parseTomlValueInner :: Text -> Either String TV.Value
parseTomlValueInner s
  | T.null s = Left "empty value"
  | s == "true" = Right (TV.TBool True)
  | s == "false" = Right (TV.TBool False)
  | s == "inf" || s == "+inf" = Right (TV.TFloat (1/0))
  | s == "-inf" = Right (TV.TFloat (-1/0))
  | s == "nan" || s == "+nan" = Right (TV.TFloat (0/0))
  | s == "-nan" = Right (TV.TFloat (-(0/0)))
  | T.isPrefixOf "\"\"\"" s = parseMultilineBasicString s
  | T.isPrefixOf "'''" s = parseMultilineLiteralString s
  | T.isPrefixOf "\"" s = parseBasicString s
  | T.isPrefixOf "'" s = parseLiteralString s
  | T.isPrefixOf "[" s = parseArray s
  | T.isPrefixOf "{" s = parseInlineTable s
  | T.isPrefixOf "0x" s = parseHexInt s
  | T.isPrefixOf "0o" s = parseOctInt s
  | T.isPrefixOf "0b" s = parseBinInt s
  | looksLikeDateTime s = Right (classifyDateTime s)
  | looksLikeLocalTime s = Right (TV.TTime s)
  | looksLikeFloat s = parseFloat s
  | otherwise = parseInt s

parseBasicString :: Text -> Either String TV.Value
parseBasicString t =
  let !inner = T.drop 1 t
  in case T.breakOn "\"" inner of
       (content, rest)
         | T.isPrefixOf "\"" rest -> Right (TV.TString (unescapeBasic content))
         | otherwise -> Left "unterminated basic string"

parseLiteralString :: Text -> Either String TV.Value
parseLiteralString t =
  let !inner = T.drop 1 t
  in case T.breakOn "'" inner of
       (content, rest)
         | T.isPrefixOf "'" rest -> Right (TV.TString content)
         | otherwise -> Left "unterminated literal string"

parseMultilineBasicString :: Text -> Either String TV.Value
parseMultilineBasicString t =
  let !inner = T.drop 3 t
  in case T.breakOn "\"\"\"" inner of
       (content, rest)
         | T.isPrefixOf "\"\"\"" rest ->
             let !trimmed = if T.isPrefixOf "\n" content then T.drop 1 content else content
             in Right (TV.TString (unescapeBasic trimmed))
         | otherwise -> Left "unterminated multi-line basic string"

parseMultilineLiteralString :: Text -> Either String TV.Value
parseMultilineLiteralString t =
  let !inner = T.drop 3 t
  in case T.breakOn "'''" inner of
       (content, rest)
         | T.isPrefixOf "'''" rest ->
             let !trimmed = if T.isPrefixOf "\n" content then T.drop 1 content else content
             in Right (TV.TString trimmed)
         | otherwise -> Left "unterminated multi-line literal string"

unescapeBasic :: Text -> Text
unescapeBasic = T.pack . go . T.unpack
  where
    go [] = []
    go ('\\':'n':rest) = '\n' : go rest
    go ('\\':'t':rest) = '\t' : go rest
    go ('\\':'r':rest) = '\r' : go rest
    go ('\\':'\\':rest) = '\\' : go rest
    go ('\\':'"':rest) = '"' : go rest
    go ('\\':'b':rest) = '\b' : go rest
    go ('\\':'f':rest) = '\f' : go rest
    go ('\\':'u':a:b:c:d:rest)
      | all isHexDigit [a,b,c,d] =
          chr (foldl' (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d]) : go rest
    go ('\\':'U':a:b:c:d:e:f:g:h:rest)
      | all isHexDigit [a,b,c,d,e,f,g,h] =
          chr (foldl' (\acc x -> acc * 16 + digitToInt x) 0 [a,b,c,d,e,f,g,h]) : go rest
    go (c:rest) = c : go rest

parseArray :: Text -> Either String TV.Value
parseArray t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
  in if T.null inner
       then Right (TV.TArray V.empty)
       else do
         items <- splitCommaValues inner
         vals <- traverse parseTomlValueInner items
         Right (TV.TArray (V.fromList vals))

splitCommaValues :: Text -> Either String [Text]
splitCommaValues t = Right (map T.strip (splitTopLevel t))

splitTopLevel :: Text -> [Text]
splitTopLevel = go 0 0 0 0 []
  where
    go !depth !braceDepth !inStr !i !acc !t
      | i >= T.length t =
          let !final = T.strip (T.take i t)
          in reverse (if T.null final then acc else final : acc)
      | T.index t i == '"' && inStr == 0 = go depth braceDepth 1 (i+1) acc t
      | T.index t i == '"' && inStr == 1 = go depth braceDepth 0 (i+1) acc t
      | T.index t i == '\'' && inStr == 0 = go depth braceDepth 2 (i+1) acc t
      | T.index t i == '\'' && inStr == 2 = go depth braceDepth 0 (i+1) acc t
      | inStr /= 0 = go depth braceDepth inStr (i+1) acc t
      | T.index t i == '[' = go (depth+1) braceDepth inStr (i+1) acc t
      | T.index t i == ']' = go (depth-1) braceDepth inStr (i+1) acc t
      | T.index t i == '{' = go depth (braceDepth+1) inStr (i+1) acc t
      | T.index t i == '}' = go depth (braceDepth-1) inStr (i+1) acc t
      | T.index t i == ',' && depth == 0 && braceDepth == 0 =
          let !item = T.strip (T.take i t)
              !rest = T.drop (i+1) t
          in go 0 0 0 0 (item : acc) rest
      | otherwise = go depth braceDepth inStr (i+1) acc t

parseInlineTable :: Text -> Either String TV.Value
parseInlineTable t =
  let !inner = T.strip (T.dropEnd 1 (T.drop 1 t))
  in if T.null inner
       then Right (TV.TTable V.empty)
       else do
         let parts = splitTopLevel inner
         kvs <- traverse (\p -> parseKeyValue (T.strip p)) parts
         Right (TV.TTable (V.fromList kvs))

parseHexInt :: Text -> Either String TV.Value
parseHexInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all isHexDigit s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid hex integer: " ++ T.unpack t

parseOctInt :: Text -> Either String TV.Value
parseOctInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all (\c -> c >= '0' && c <= '7') s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid octal integer: " ++ T.unpack t

parseBinInt :: Text -> Either String TV.Value
parseBinInt t =
  let !digits = T.filter (/= '_') (T.drop 2 t)
      !s = T.unpack digits
  in if all (\c -> c == '0' || c == '1') s && not (null s)
       then Right (TV.TInteger (foldl' (\acc c -> acc * 2 + fromIntegral (digitToInt c)) 0 s))
       else Left $ "invalid binary integer: " ++ T.unpack t

parseInt :: Text -> Either String TV.Value
parseInt t =
  let !cleaned = T.unpack (T.filter (/= '_') t)
      !normalized = case cleaned of
        '+':rest -> rest
        other -> other
  in case reads normalized :: [(Integer, String)] of
       [(n, "")] -> Right (TV.TInteger n)
       _ -> Left $ "invalid integer: " ++ T.unpack t

parseFloat :: Text -> Either String TV.Value
parseFloat t =
  let !cleaned0 = T.unpack (T.filter (/= '_') t)
      -- Haskell's @reads@ doesn't accept a leading '+', strip it.
      !cleaned  = case cleaned0 of
                    '+':r -> r
                    _     -> cleaned0
  in case reads cleaned :: [(Double, String)] of
       [(d, "")] -> Right (TV.TFloat d)
       _ -> Left $ "invalid float: " ++ T.unpack t

looksLikeFloat :: Text -> Bool
looksLikeFloat t =
  let !c = T.filter (/= '_') t
  in T.any (== '.') c || T.any (\x -> x == 'e' || x == 'E') c

looksLikeDateTime :: Text -> Bool
looksLikeDateTime t =
  T.length t >= 10
  && T.index t 4 == '-'
  && T.index t 7 == '-'
  && all isDigit (T.unpack (T.take 4 t))

-- | Heuristic recogniser for a TOML local-time scalar
-- (@HH:MM:SS@ optionally followed by @.fff@).
looksLikeLocalTime :: Text -> Bool
looksLikeLocalTime t
  | T.length t < 5 = False
  | T.index t 2 /= ':' = False
  | not (isDigit (T.index t 0)) = False
  | not (isDigit (T.index t 1)) = False
  | T.length t == 5 = isDigit (T.index t 3) && isDigit (T.index t 4)
  | T.length t >= 8 && T.index t 5 == ':' =
      isDigit (T.index t 3) && isDigit (T.index t 4)
      && isDigit (T.index t 6) && isDigit (T.index t 7)
  | otherwise = False

classifyDateTime :: Text -> TV.Value
classifyDateTime t
  | T.any (== 'T') t || T.any (== 't') t || (T.length t > 10 && T.index t 10 == ' ') = TV.TDateTime t
  | T.length t == 10 = TV.TDate t
  | T.any (== ':') t = TV.TTime t
  | otherwise = TV.TDateTime t

mergeTables :: V.Vector (Text, TV.Value) -> [TableCtx] -> TV.Value
mergeTables topPairs tables =
  let !withSubTables = foldl' insertTable topPairs tables
  in TV.TTable withSubTables

insertTable :: V.Vector (Text, TV.Value) -> TableCtx -> V.Vector (Text, TV.Value)
insertTable top (TableCtx path pairs) =
  case path of
    [] -> top
    [name] -> top `V.snoc` (name, TV.TTable (V.fromList (reverse pairs)))
    (first : rest) ->
      let !subCtx = TableCtx rest pairs
          !existing = V.find (\(k,_) -> k == first) top
      in case existing of
           Just (_, TV.TTable subT) ->
             let !updated = insertTable subT subCtx
             in V.map (\(k, v) -> if k == first then (k, TV.TTable updated) else (k, v)) top
           _ ->
             let !subTable = insertTable V.empty subCtx
             in top `V.snoc` (first, TV.TTable subTable)
