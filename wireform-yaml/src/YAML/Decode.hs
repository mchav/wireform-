{-# LANGUAGE BangPatterns #-}
-- | YAML 1.2 decoder.
--
-- Parses a YAML stream into a 'YAML.Value.Stream' / 'Document' /
-- 'Value' according to the YAML 1.2 specification, applying the
-- /core schema/ for plain-scalar resolution. Supports:
--
-- * Block-style mappings and sequences (with arbitrary indentation).
-- * Flow-style mappings (@{a: b}@) and sequences (@[a, b]@), nested.
-- * Plain, single-quoted, and double-quoted scalars (with all
--   YAML 1.2 escapes for the latter, including @\\xNN@, @\\uNNNN@,
--   @\\UNNNNNNNN@).
-- * Block literal (@|@) and block folded (@>@) scalars with all four
--   chomping indicators (@-@, @+@, default-clip) and optional
--   indentation indicators.
-- * Anchors (@&name@) and aliases (@*name@). Aliases are expanded
--   inline so that downstream code can treat the result as a tree.
-- * Explicit tags (@!!str@, @!!int@, @!\<tag:yaml.org,2002:bool\>@,
--   etc.) with the standard short-hand expansions for the
--   @tag:yaml.org,2002:@ family.
-- * Comments, blank lines, the @---@ document-start and @...@
--   document-end markers, and multi-document streams.
module YAML.Decode
  ( decode
  , decodeBS
  , decodeStream
  , decodeStreamBS
  , decodeDocuments
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V

import YAML.Value

-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

decode :: Text -> Either String Value
decode t = case decodeDocuments t of
  Left err     -> Left err
  Right []     -> Right YNull
  Right (d:_)  -> Right (docBody d)

decodeBS :: ByteString -> Either String Value
decodeBS = decode . TE.decodeUtf8Lenient

decodeStream :: Text -> Either String Stream
decodeStream t = (Stream . V.fromList) <$> decodeDocuments t

decodeStreamBS :: ByteString -> Either String Stream
decodeStreamBS = decodeStream . TE.decodeUtf8Lenient

decodeDocuments :: Text -> Either String [Document]
decodeDocuments src = parseStream (preprocess src)

-- ---------------------------------------------------------------------------
-- Pre-processing: split into structured lines
-- ---------------------------------------------------------------------------

data PLine = PLine
  { lineNo     :: !Int
  , lineIndent :: !Int
  , lineKind   :: !LineKind
  , lineBody   :: !Text       -- ^ content after stripping indent
  } deriving (Show)

data LineKind
  = LBlank
  | LComment
  | LDocStart        -- ^ @---@
  | LDocEnd          -- ^ @...@
  | LDirective       -- ^ @%YAML 1.2@ etc.
  | LContent
  deriving (Eq, Show)

preprocess :: Text -> [PLine]
preprocess = go 1 . T.split (== '\n')
  where
    go !_ [] = []
    go !n (l:ls) =
      let !stripped = stripCR l
          !ind      = leadingSpaces stripped
          !body0    = T.drop ind stripped
          !body     = T.dropWhileEnd (\c -> c == ' ' || c == '\t') body0
          !kind     = classify body
      in PLine n ind kind body : go (n+1) ls

stripCR :: Text -> Text
stripCR t = case T.unsnoc t of
  Just (rest, '\r') -> rest
  _                 -> t

leadingSpaces :: Text -> Int
leadingSpaces = T.length . T.takeWhile (== ' ')

classify :: Text -> LineKind
classify t
  | T.null t                              = LBlank
  | T.head t == '#'                       = LComment
  | t == T.pack "---"
      || T.isPrefixOf (T.pack "--- ") t   = LDocStart
  | t == T.pack "..."
      || T.isPrefixOf (T.pack "... ") t   = LDocEnd
  | T.head t == '%'                       = LDirective
  | otherwise                             = LContent

isSkippable :: PLine -> Bool
isSkippable l = case lineKind l of
  LBlank     -> True
  LComment   -> True
  LDirective -> True
  _          -> False

-- ---------------------------------------------------------------------------
-- Parser monad: pure ([PLine], Map Text Value) -> Either String (a, ...)
-- ---------------------------------------------------------------------------

newtype P a = P { unP :: PS -> Either String (a, PS) }

data PS = PS
  { psLines   :: ![PLine]
  , psAnchors :: !(Map Text Value)
  }

instance Functor P where
  fmap f (P g) = P $ \s -> case g s of
    Left e         -> Left e
    Right (x, s')  -> Right (f x, s')

instance Applicative P where
  pure x = P $ \s -> Right (x, s)
  P pf <*> P px = P $ \s -> case pf s of
    Left e         -> Left e
    Right (f, s')  -> case px s' of
      Left e          -> Left e
      Right (x, s'')  -> Right (f x, s'')

instance Monad P where
  P g >>= k = P $ \s -> case g s of
    Left e        -> Left e
    Right (x, s') -> unP (k x) s'

instance MonadFail P where
  fail = failP

failP :: String -> P a
failP msg = P (const (Left msg))

getS :: P PS
getS = P (\s -> Right (s, s))

modifyS :: (PS -> PS) -> P ()
modifyS f = P (\s -> Right ((), f s))

getLines :: P [PLine]
getLines = psLines <$> getS

setLines :: [PLine] -> P ()
setLines ls = modifyS (\s -> s { psLines = ls })

popLine :: P (Maybe PLine)
popLine = do
  ls <- getLines
  case dropWhile isSkippable ls of
    []     -> pure Nothing
    (x:xs) -> do { setLines xs; pure (Just x) }

peekLine :: P (Maybe PLine)
peekLine = do
  ls <- getLines
  case dropWhile isSkippable ls of
    []     -> pure Nothing
    (x:_)  -> pure (Just x)

pushLine :: PLine -> P ()
pushLine l = modifyS (\s -> s { psLines = l : psLines s })

recordAnchor :: Text -> Value -> P ()
recordAnchor name v =
  modifyS (\s -> s { psAnchors = Map.insert name v (psAnchors s) })

resolveAnchor :: Text -> P Value
resolveAnchor name = do
  s <- getS
  case Map.lookup name (psAnchors s) of
    Just v  -> pure v
    Nothing -> failP ("YAML: alias *" ++ T.unpack name ++ " has no anchor")

resetAnchors :: P ()
resetAnchors = modifyS (\s -> s { psAnchors = Map.empty })

-- ---------------------------------------------------------------------------
-- Stream / document
-- ---------------------------------------------------------------------------

parseStream :: [PLine] -> Either String [Document]
parseStream lns =
  case unP loop (PS lns Map.empty) of
    Left err      -> Left err
    Right (ds, _) -> Right ds
  where
    loop = do
      ls <- getLines
      case dropWhile isSkippable ls of
        []      -> pure []
        _       -> do
          d  <- parseDocument
          ds <- loop
          pure (d : ds)

parseDocument :: P Document
parseDocument = do
  ls0 <- getLines
  let (directives, ls1) = case dropWhile isSkippable ls0 of
        (l : rest) | lineKind l == LDocStart -> (True, rest)
        _                                    -> (False, ls0)
  setLines ls1
  resetAnchors
  body <- parseDocBody
  ls2 <- getLines
  let (explicitEnd, ls3) = case dropWhile isSkippable ls2 of
        (l : rest) | lineKind l == LDocEnd -> (True, rest)
        _                                  -> (False, ls2)
  setLines ls3
  pure (Document directives explicitEnd body)

parseDocBody :: P Value
parseDocBody = do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd   -> pure YNull
      | otherwise               -> parseNode 0

-- ---------------------------------------------------------------------------
-- Node dispatch
-- ---------------------------------------------------------------------------

-- | Parse a node whose left margin is at least @minInd@.
parseNode :: Int -> P Value
parseNode !minInd = do
  mNext <- peekLine
  case mNext of
    Nothing -> pure YNull
    Just l
      | lineKind l == LDocStart -> pure YNull
      | lineKind l == LDocEnd   -> pure YNull
      | lineIndent l < minInd   -> pure YNull
      | otherwise -> dispatch l

dispatch :: PLine -> P Value
dispatch l =
  let body = lineBody l
  in case T.uncons body of
       Just ('!', _)  -> parseTagged
       Just ('&', _)  -> parseAnchored
       Just ('*', _)  -> parseAlias
       Just ('|', _)  -> parseBlockScalar Literal
       Just ('>', _)  -> parseBlockScalar Folded
       Just ('[', _)  -> consumeFlowFromHead
       Just ('{', _)  -> consumeFlowFromHead
       Just ('"', _)  -> parseQuotedScalarLine '"'  l
       Just ('\'', _) -> parseQuotedScalarLine '\'' l
       _              -> parseBlockOrPlain l

-- | Quoted scalars can be the entire node body, or the start of a
-- @key: \"…\"@ pair when the closing quote is followed by a colon. We
-- look ahead for a top-level @:@ after the closing quote and dispatch
-- to 'parseBlockMap' if found, otherwise consume the quoted scalar.
--
-- Quoted scalars may span multiple lines per YAML 1.2; if the close
-- quote is not on the same line we splice continuation lines into
-- the buffer until it is.
parseQuotedScalarLine :: Char -> PLine -> P Value
parseQuotedScalarLine q l = case findKeyValueSplit (lineBody l) of
  Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
  Nothing -> do
    _ <- popLine
    consumeQuoted q (lineBody l)

-- | Greedily extend a quoted-scalar buffer with successor lines
-- (joined by single spaces, blank lines becoming a literal newline)
-- until the matching close quote is found. Returns the parsed value;
-- any text after the close quote on the final line is pushed back as
-- a virtual line.
consumeQuoted :: Char -> Text -> P Value
consumeQuoted q = go
  where
    parser = case q of '"' -> parseDQ; _ -> parseSQ
    go !buf = case parser 0 buf of
      Just (v, p) -> do
        let rest = T.stripStart (T.drop p buf)
        if T.null rest
          then pure v
          else do
            pushLine (PLine 0 0 LContent rest)
            pure v
      Nothing -> do
        mNext <- popLine
        case mNext of
          Nothing -> failP "YAML: unterminated quoted scalar"
          Just l' -> do
            -- Per YAML 6.7: blank lines within a quoted scalar fold
            -- to a literal newline; otherwise lines join with a
            -- single space.
            let body = lineBody l'
                join_ = if T.null (T.strip body)
                          then T.pack "\n"
                          else T.pack " "
            go (buf <> join_ <> body)

parseTagged :: P Value
parseTagged = do
  Just l <- popLine
  let (tg, rest) = breakOnSpace (lineBody l)
      tag = expandTag tg
      after = T.stripStart rest
  if T.null after
    then do
      mNext <- peekLine
      case mNext of
        Just l2 | lineIndent l2 > lineIndent l -> do
          v <- parseNode (lineIndent l2)
          pure (YTagged tag v)
        _ -> pure (YTagged tag YNull)
    else do
      pushLine l { lineBody = after }
      v <- parseNode (lineIndent l)
      pure (YTagged tag v)

parseAnchored :: P Value
parseAnchored = do
  Just l <- popLine
  let body = lineBody l
      (name, rest) = T.span isAnchorChar (T.drop 1 body)
      after = T.stripStart rest
  v <- if T.null after
         then do
           mNext <- peekLine
           case mNext of
             Just l2 | lineIndent l2 > lineIndent l ->
                 parseNode (lineIndent l2)
             _ -> pure YNull
         else do
           pushLine l { lineBody = after }
           parseNode (lineIndent l)
  recordAnchor name v
  pure v

parseAlias :: P Value
parseAlias = do
  Just l <- popLine
  let body = lineBody l
      (al, rest) = T.span isAnchorChar (T.drop 1 body)
      name = al
      after = T.stripStart rest
  -- An alias appearing as a mapping value will be followed by ":"
  -- (or other dispatch chars) on the same line. Push remaining
  -- content back as a virtual line so the surrounding context can
  -- continue parsing.
  if T.null after
    then resolveAnchor name
    else do
      pushLine (PLine (lineNo l) (lineIndent l) LContent after)
      resolveAnchor name

-- | Characters legal in an anchor / alias name (YAML 1.2 §6.9.2).
isAnchorChar :: Char -> Bool
isAnchorChar c =
  not (c == ',' || c == '[' || c == ']' || c == '{' || c == '}'
       || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ':')

breakOnSpace :: Text -> (Text, Text)
breakOnSpace = T.break (\c -> c == ' ' || c == '\t')

-- ---------------------------------------------------------------------------
-- Flow style
-- ---------------------------------------------------------------------------

consumeFlowFromHead :: P Value
consumeFlowFromHead = do
  Just l <- popLine
  consumeFlow (lineBody l)

consumeFlow :: Text -> P Value
consumeFlow = go
  where
    go buf = case scanFlow buf of
      ScanComplete v rest -> do
        let !s = T.stripStart rest
        case T.uncons s of
          Nothing -> pure v
          Just _  -> do
            -- push remaining content as a virtual line at column 0
            pushLine (PLine 0 0 LContent s)
            pure v
      ScanIncomplete -> do
        mNext <- popLine
        case mNext of
          Nothing -> failP "YAML: unterminated flow node"
          Just l' -> go (buf <> T.pack " " <> lineBody l')

data ScanResult
  = ScanComplete !Value !Text
  | ScanIncomplete

scanFlow :: Text -> ScanResult
scanFlow buf = case parseFlowValue 0 buf of
  Just (v, p) -> ScanComplete v (T.drop p buf)
  Nothing     -> ScanIncomplete

parseFlowValue :: Int -> Text -> Maybe (Value, Int)
parseFlowValue !p0 t =
  let p = skipFlowWS p0 t
  in if p >= T.length t
       then Nothing
       else case T.index t p of
              '['  -> parseFlowSeq (p + 1) t
              '{'  -> parseFlowMap (p + 1) t
              '"'  -> parseDQ p t
              '\'' -> parseSQ p t
              _    -> parseFlowPlain p t

parseFlowSeq :: Int -> Text -> Maybe (Value, Int)
parseFlowSeq !p0 t = goV (skipFlowWS p0 t) []
  where
    goV !p acc
      | p >= T.length t = Nothing
      | T.index t p == ']' = Just (YSeq (V.fromList (reverse acc)), p + 1)
      | otherwise = case parseFlowEntry p t of
          Nothing      -> Nothing
          Just (v, p1) ->
            let p2 = skipFlowWS p1 t
            in if p2 >= T.length t
                 then Nothing
                 else case T.index t p2 of
                        ',' -> goV (skipFlowWS (p2 + 1) t) (v : acc)
                        ']' -> Just (YSeq (V.fromList (reverse (v : acc))), p2 + 1)
                        _   -> Nothing

-- | A flow-sequence entry can be a single value or a one-pair
-- mapping (with @key: value@ syntax, or just @: value@ for an empty
-- key).
parseFlowEntry :: Int -> Text -> Maybe (Value, Int)
parseFlowEntry !p0 t =
  let p = skipFlowWS p0 t
  in if p >= T.length t
       then Nothing
       else case T.index t p of
              ':' ->
                -- Bare colon: empty key, value follows.
                let p1 = skipFlowWS (p + 1) t
                in if p1 >= T.length t
                     then Nothing
                     else case T.index t p1 of
                       ',' -> Just (YMap (V.singleton (YNull, YNull)), p1)
                       ']' -> Just (YMap (V.singleton (YNull, YNull)), p1)
                       _   -> case parseFlowValue p1 t of
                         Nothing -> Just (YMap (V.singleton (YNull, YNull)), p1)
                         Just (v, p2) ->
                           Just (YMap (V.singleton (YNull, v)), p2)
              _ -> case parseFlowValue p t of
                Nothing -> Nothing
                Just (k, p1) ->
                  let p2 = skipFlowWS p1 t
                  in if p2 < T.length t && T.index t p2 == ':'
                       then case parseFlowValue (skipFlowWS (p2 + 1) t) t of
                              Nothing      -> Just (YMap (V.singleton (k, YNull)), p2 + 1)
                              Just (v, p3) -> Just (YMap (V.singleton (k, v)), p3)
                       else Just (k, p1)

parseFlowMap :: Int -> Text -> Maybe (Value, Int)
parseFlowMap !p0 t = goV (skipFlowWS p0 t) []
  where
    goV !p acc
      | p >= T.length t = Nothing
      | T.index t p == '}' = Just (YMap (V.fromList (reverse acc)), p + 1)
      | T.index t p == ',' = goV (skipFlowWS (p + 1) t) acc   -- tolerate empty entries
      | otherwise =
          let p0' = p
              (k, p1) = case T.index t p of
                ':' -> (YNull, p)
                _   -> case parseFlowValue p t of
                  Just (k', q) -> (k', q)
                  Nothing      -> (YNull, p)
          in if p1 == p0' && T.index t p1 /= ':'
               then Nothing
               else
                 let p2 = skipFlowWS p1 t
                     skipColon = if p2 < T.length t && T.index t p2 == ':'
                                   then Just (skipFlowWS (p2 + 1) t)
                                   else Nothing
                 in case skipColon of
                      Just p2'
                        | p2' < T.length t
                          && (T.index t p2' == ',' || T.index t p2' == '}') ->
                            finish p2' k YNull acc
                        | otherwise -> case parseFlowValue p2' t of
                            Nothing -> finish p2' k YNull acc
                            Just (v, p3) -> finish p3 k v acc
                      Nothing -> finish p2 k YNull acc

    finish !p k v acc =
      let p' = skipFlowWS p t
      in if p' >= T.length t
           then Nothing
           else case T.index t p' of
                  ',' -> goV (skipFlowWS (p' + 1) t) ((k, v) : acc)
                  '}' -> Just (YMap (V.fromList (reverse ((k, v) : acc))), p' + 1)
                  _   -> Nothing

parseDQ :: Int -> Text -> Maybe (Value, Int)
parseDQ !p0 t = go (p0 + 1) []
  where
    !len = T.length t
    go !i acc
      | i >= len = Nothing
      | otherwise = case T.index t i of
          '"' -> Just (YString (T.pack (reverse acc)), i + 1)
          '\\' | i + 1 < len -> case decodeDQEscape t (i + 1) of
                  Just (c, i') -> go i' (c : acc)
                  Nothing      -> Nothing
               | otherwise -> Nothing
          c   -> go (i + 1) (c : acc)

parseSQ :: Int -> Text -> Maybe (Value, Int)
parseSQ !p0 t = go (p0 + 1) []
  where
    !len = T.length t
    go !i acc
      | i >= len = Nothing
      | otherwise = case T.index t i of
          '\'' | i + 1 < len && T.index t (i + 1) == '\'' ->
                  go (i + 2) ('\'' : acc)
               | otherwise ->
                  Just (YString (T.pack (reverse acc)), i + 1)
          c -> go (i + 1) (c : acc)

parseFlowPlain :: Int -> Text -> Maybe (Value, Int)
parseFlowPlain !p t =
  let !len  = T.length t
      go !i = if i < len && not (stopper (T.index t i) (T.index t (min (i+1) (len-1))) (i+1 < len))
                then go (i + 1)
                else i
      !p' = go p
      raw = T.take (p' - p) (T.drop p t)
      strp = T.stripEnd raw
  in if T.null strp
       then Nothing
       else Just (resolvePlain strp, p')
  where
    -- A plain scalar in flow context ends at any of [ , ] { } and at
    -- ":" followed by space or end-of-token.
    stopper c next hasNext =
      c == ',' || c == '[' || c == ']' || c == '{' || c == '}'
      || (c == ':' && (not hasNext || next == ' ' || next == '\t'
                       || next == ',' || next == ']' || next == '}'))

skipFlowWS :: Int -> Text -> Int
skipFlowWS !p t
  | p >= T.length t = p
  | otherwise = case T.index t p of
      ' '  -> skipFlowWS (p + 1) t
      '\t' -> skipFlowWS (p + 1) t
      _    -> p

-- ---------------------------------------------------------------------------
-- Block style: dispatch from a line we haven't consumed yet.
-- ---------------------------------------------------------------------------

parseBlockOrPlain :: PLine -> P Value
parseBlockOrPlain l
  | T.isPrefixOf "- " body || body == "-" = parseBlockSeq (lineIndent l)
  | body == "?" || T.isPrefixOf "? " body = parseExplicitMap (lineIndent l)
  | otherwise = case findKeyValueSplit body of
      Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
      Nothing         -> parsePlainScalar (lineIndent l) body
  where
    body = lineBody l

-- ---------------------------------------------------------------------------
-- Block sequence
-- ---------------------------------------------------------------------------

parseBlockSeq :: Int -> P Value
parseBlockSeq !ind = collect [] >>= \xs -> pure (YSeq (V.fromList (reverse xs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | not (T.isPrefixOf "- " (lineBody l) || lineBody l == "-")
                                -> pure acc
          | otherwise -> do
              v <- parseSeqItem ind
              collect (v : acc)

parseSeqItem :: Int -> P Value
parseSeqItem !ind = do
  Just l <- popLine
  let body = lineBody l
      after | body == "-" = T.empty
            | otherwise   = T.drop 2 body
      after' = T.stripStart after
  if T.null after'
    then do
      mNext <- peekLine
      case mNext of
        Just l2 | lineIndent l2 > ind -> parseNode (lineIndent l2)
        _ -> pure YNull
    else do
      -- The value sits on the same physical line as the dash. We
      -- expose it to the regular dispatcher via a virtual line
      -- whose indent is @ind + 2@ (the position the value would
      -- normally appear at). All branches — nested sequence, nested
      -- mapping, scalar, flow — fall out of regular dispatch.
      let virt = PLine (lineNo l) (ind + 2) LContent after'
      pushLine virt
      parseNode (ind + 2)

-- ---------------------------------------------------------------------------
-- Block mapping
-- ---------------------------------------------------------------------------

parseBlockMap :: Int -> Text -> Text -> P Value
parseBlockMap !ind firstKey firstRest = do
  Just _ <- popLine
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(YString firstKey, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | T.isPrefixOf "- " (lineBody l) || lineBody l == "-" -> pure acc
          | lineBody l == "?" || T.isPrefixOf "? " (lineBody l) -> pure acc
          | otherwise -> case findKeyValueSplit (lineBody l) of
              Just (k, vRest) -> do
                _ <- popLine
                v <- parseImplicitMapValue ind vRest
                collect ((YString k, v) : acc)
              Nothing -> pure acc

parseImplicitMapValue :: Int -> Text -> P Value
parseImplicitMapValue !ind vRest =
  let after = T.stripStart vRest
  in if T.null after
       then do
         mNext <- peekLine
         case mNext of
           Just l2
             | lineIndent l2 > ind -> parseNode (lineIndent l2)
             | lineIndent l2 == ind
                 && (T.isPrefixOf "- " (lineBody l2) || lineBody l2 == "-")
                 -> parseBlockSeq ind
           _ -> pure YNull
       else case T.uncons after of
         Just ('|', _) -> do
           -- Block scalar body lines must be at indent > parent
           -- (== ind here). Encode that with a virtual line at @ind@
           -- so parseBlockScalar's collectScalarLines uses the right
           -- comparison.
           pushLine (PLine 0 ind LContent after)
           parseBlockScalar Literal
         Just ('>', _) -> do
           pushLine (PLine 0 ind LContent after)
           parseBlockScalar Folded
         Just ('[', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after)
           consumeFlowFromHead
         Just ('{', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after)
           consumeFlowFromHead
         Just ('&', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after)
           parseAnchored
         Just ('*', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after)
           parseAlias
         Just ('!', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after)
           parseTagged
         _ -> pure (parseInlineScalar after)

parseInlineScalar :: Text -> Value
parseInlineScalar t = case T.uncons t of
  Just ('"', _)  -> case parseDQ 0 t of
                      Just (v, _) -> v
                      Nothing     -> YString t
  Just ('\'', _) -> case parseSQ 0 t of
                      Just (v, _) -> v
                      Nothing     -> YString t
  _              -> resolvePlain (T.stripEnd (stripInlineComment t))

-- ---------------------------------------------------------------------------
-- Explicit-key mapping (?-form)
-- ---------------------------------------------------------------------------

parseExplicitMap :: Int -> P Value
parseExplicitMap !ind = collect [] >>= \kvs -> pure (YMap (V.fromList (reverse kvs)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | lineBody l == "?" || T.isPrefixOf "? " (lineBody l) -> do
              k <- readExplicitPart "?"
              v <- readExplicitValue
              collect ((k, v) : acc)
          | otherwise -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          rest = if body == marker then T.empty
                                   else T.stripStart (T.drop 2 body)
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            _ -> pure YNull
        else do
          pushLine (PLine (lineNo l) (lineIndent l + 2) LContent rest)
          parseNode (lineIndent l + 2)

    readExplicitValue = do
      mPL <- peekLine
      case mPL of
        Just l | lineIndent l == ind
                 && (lineBody l == ":" || T.isPrefixOf ": " (lineBody l)) ->
            readExplicitPart ":"
        _ -> pure YNull

-- ---------------------------------------------------------------------------
-- Plain scalars (multi-line)
-- ---------------------------------------------------------------------------

parsePlainScalar :: Int -> Text -> P Value
parsePlainScalar !ind firstBody = do
  Just _ <- popLine
  let !first = T.stripEnd (stripInlineComment firstBody)
  rest <- collectFolds ind []
  let !final = T.intercalate (T.pack " ") (first : rest)
  pure (resolvePlain final)
  where
    collectFolds baseInd acc = do
      mNext <- peekLine
      case mNext of
        Just l
          | lineKind l == LContent
            && lineIndent l > baseInd
            && not (T.isPrefixOf "- " (lineBody l))
            && lineBody l /= "-"
            && not (T.isPrefixOf "? " (lineBody l))
            && lineBody l /= "?"
            && case findKeyValueSplit (lineBody l) of
                 Just _  -> False
                 Nothing -> True
            -> do
              _ <- popLine
              let s = T.stripEnd (stripInlineComment (lineBody l))
              collectFolds baseInd (s : acc)
        _ -> pure (reverse acc)

-- ---------------------------------------------------------------------------
-- Block scalars
-- ---------------------------------------------------------------------------

data Chomp = Strip | Clip | Keep deriving (Eq, Show)
data BlockKind = Literal | Folded deriving (Eq, Show)

parseBlockScalar :: BlockKind -> P Value
parseBlockScalar k = do
  Just l <- popLine
  let header = T.drop 1 (lineBody l)   -- drop '|' or '>'
      (chomp, _hint) = parseHeader header
  body <- collectScalarLines (lineIndent l)
  let txt = case k of
        Literal -> joinLiteral chomp body
        Folded  -> joinFolded  chomp body
  pure (YString txt)
  where
    parseHeader :: Text -> (Chomp, Maybe Int)
    parseHeader h =
      let hs = T.unpack (T.strip (stripInlineComment h))
          chompOf '-' = Strip
          chompOf '+' = Keep
          chompOf _   = Clip
      in case hs of
           []                          -> (Clip,    Nothing)
           [c] | c == '-' || c == '+'  -> (chompOf c, Nothing)
               | isDigit c             -> (Clip, Just (digitToInt c))
               | otherwise             -> (Clip, Nothing)
           (a:b:_)
             | (a == '-' || a == '+') && isDigit b
                 -> (chompOf a, Just (digitToInt b))
             | isDigit a && (b == '-' || b == '+')
                 -> (chompOf b, Just (digitToInt a))
             | a == '-' || a == '+'
                 -> (chompOf a, Nothing)
             | isDigit a
                 -> (Clip, Just (digitToInt a))
             | otherwise
                 -> (Clip, Nothing)

collectScalarLines :: Int -> P [(Int, Text)]
collectScalarLines !parent = collect []
  where
    collect acc = do
      ls <- getLines
      case ls of
        []     -> pure (reverse acc)
        (l:_)
          | lineKind l == LBlank ->
              do _ <- consumeOne
                 collect ((-1, T.empty) : acc)
          | lineKind l == LDocStart || lineKind l == LDocEnd
              -> pure (reverse acc)
          | lineKind l == LComment
              -> if lineIndent l > parent
                   then do _ <- consumeOne; collect acc
                   else pure (reverse acc)
          | lineIndent l > parent ->
              do _ <- consumeOne
                 collect ((lineIndent l, lineBody l) : acc)
          | otherwise -> pure (reverse acc)

    consumeOne = do
      ls <- getLines
      case ls of
        (_:xs) -> setLines xs >> pure ()
        []     -> pure ()

joinLiteral :: Chomp -> [(Int, Text)] -> Text
joinLiteral chomp xs =
  let baseInd = minNonNegative xs
      lns     = map (renderLine baseInd) xs
      raw     = T.intercalate (T.pack "\n") lns <> T.pack "\n"
  in chompText chomp raw
  where
    renderLine bi (i, b)
      | i < 0     = T.empty
      | otherwise = T.replicate (max 0 (i - bi)) (T.pack " ") <> b

joinFolded :: Chomp -> [(Int, Text)] -> Text
joinFolded chomp xs =
  let baseInd = minNonNegative xs
      raw = T.concat (foldFirst xs baseInd) <> T.pack "\n"
  in chompText chomp raw
  where
    foldFirst [] _ = []
    foldFirst ((i, b) : rest) bi
      | i < 0     = T.pack "\n" : foldAfterBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
          in txt : foldNext rest bi (i > bi)

    foldNext [] _ _ = []
    foldNext ((i, b) : rest) bi prevMore
      | i < 0     = T.pack "\n" : foldAfterBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
              joinSep | prevMore || i > bi = T.pack "\n"
                      | otherwise           = T.pack " "
          in joinSep : txt : foldNext rest bi (i > bi)

    foldAfterBlank [] _ = []
    foldAfterBlank ((i, b) : rest) bi
      | i < 0     = T.pack "\n" : foldAfterBlank rest bi
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
          in txt : foldNext rest bi (i > bi)

-- | Smallest non-negative indent in the collected line list, or 0 if
-- there are no non-blank lines.
minNonNegative :: [(Int, Text)] -> Int
minNonNegative = go Nothing
  where
    go acc [] = case acc of
      Nothing -> 0
      Just !n -> n
    go acc ((i, _) : rest)
      | i < 0     = go acc rest
      | otherwise = case acc of
          Nothing             -> go (Just i) rest
          Just !n | i < n     -> go (Just i) rest
                  | otherwise -> go (Just n) rest

chompText :: Chomp -> Text -> Text
chompText Strip = T.dropWhileEnd (== '\n')
chompText Keep  = id
chompText Clip  = \t ->
  case T.dropWhileEnd (== '\n') t of
    t' | T.null t  -> t'
       | T.last t == '\n' -> t' <> T.pack "\n"
       | otherwise -> t'

-- ---------------------------------------------------------------------------
-- Plain-scalar resolution per the YAML 1.2 core schema
-- ---------------------------------------------------------------------------

resolvePlain :: Text -> Value
resolvePlain raw
  | T.null raw                 = YString T.empty
  | raw == "null" || raw == "~" || raw == "Null" || raw == "NULL" = YNull
  | raw == "true" || raw == "True" || raw == "TRUE"               = YBool True
  | raw == "false" || raw == "False" || raw == "FALSE"            = YBool False
  | raw == ".inf" || raw == ".Inf" || raw == ".INF"
      || raw == "+.inf" || raw == "+.Inf" || raw == "+.INF"       = YFloat (1/0)
  | raw == "-.inf" || raw == "-.Inf" || raw == "-.INF"            = YFloat (-1/0)
  | raw == ".nan" || raw == ".NaN" || raw == ".NAN"               = YFloat (0/0)
  | otherwise = case parseIntCore raw of
      Just n  -> YInt n
      Nothing -> case parseFloatCore raw of
        Just d  -> YFloat d
        Nothing -> YString raw

parseIntCore :: Text -> Maybe Int64
parseIntCore raw0 = case T.uncons raw0 of
  Just ('+', rest) -> parseUnsigned rest
  Just ('-', rest) -> negate <$> parseUnsigned rest
  _                -> parseUnsigned raw0
  where
    parseUnsigned r
      | T.isPrefixOf "0x" r || T.isPrefixOf "0X" r =
          let body = T.drop 2 r
          in if T.null body || T.any (not . isHexDigit) body
               then Nothing
               else Just (T.foldl' (\acc c -> acc * 16 + fromIntegral (digitToInt c)) 0 body)
      | T.isPrefixOf "0o" r || T.isPrefixOf "0O" r =
          let body = T.drop 2 r
          in if T.null body || T.any (\c -> c < '0' || c > '7') body
               then Nothing
               else Just (T.foldl' (\acc c -> acc * 8 + fromIntegral (digitToInt c)) 0 body)
      | T.null r                 = Nothing
      | T.any (not . isDigit) r  = Nothing
      | otherwise = Just (T.foldl' (\acc c -> acc * 10 + fromIntegral (digitToInt c)) 0 r)

parseFloatCore :: Text -> Maybe Double
parseFloatCore t = case TR.signed TR.double t of
  Right (d, leftover) | T.null leftover -> Just d
  _                                     -> Nothing

-- ---------------------------------------------------------------------------
-- Tags
-- ---------------------------------------------------------------------------

expandTag :: Text -> Tag
expandTag t
  | T.isPrefixOf "!!" t =
      Tag (T.pack "tag:yaml.org,2002:" <> T.drop 2 t)
  | T.isPrefixOf "!<" t && T.isSuffixOf ">" t =
      Tag (T.init (T.drop 2 t))
  | otherwise = Tag t

-- ---------------------------------------------------------------------------
-- @key: value@ split (top-level, respects quotes / brackets)
-- ---------------------------------------------------------------------------

findKeyValueSplit :: Text -> Maybe (Text, Text)
findKeyValueSplit t = go (0 :: Int) (0 :: Int) (0 :: Int) (0 :: Int)
  where
    !len = T.length t
    go !i !depth !brace !inStr
      | i >= len = Nothing
      | inStr == 1 =
          if T.index t i == '"' then go (i + 1) depth brace 0
          else if T.index t i == '\\' && i + 1 < len
                  then go (i + 2) depth brace inStr
                  else go (i + 1) depth brace inStr
      | inStr == 2 =
          if T.index t i == '\'' then go (i + 1) depth brace 0
          else go (i + 1) depth brace inStr
      | otherwise = case T.index t i of
          '"'  -> go (i + 1) depth brace 1
          '\'' -> go (i + 1) depth brace 2
          '['  -> go (i + 1) (depth + 1) brace inStr
          ']'  -> go (i + 1) (depth - 1) brace inStr
          '{'  -> go (i + 1) depth (brace + 1) inStr
          '}'  -> go (i + 1) depth (brace - 1) inStr
          ':' | depth == 0 && brace == 0 ->
                let nextEnd   = i + 1 >= len
                    nextSpace = i + 1 < len &&
                                  (T.index t (i + 1) == ' '
                                || T.index t (i + 1) == '\t')
                in if nextEnd || nextSpace
                     then
                       let key  = unquoteKey (T.stripEnd (T.take i t))
                           rest = T.drop (i + 1) t
                       in Just (key, rest)
                     else go (i + 1) depth brace inStr
          _    -> go (i + 1) depth brace inStr

unquoteKey :: Text -> Text
unquoteKey t
  | T.length t >= 2 && T.head t == '"' && T.last t == '"'
      = unescapeDQ (T.init (T.tail t))
  | T.length t >= 2 && T.head t == '\'' && T.last t == '\''
      = T.replace "''" "'" (T.init (T.tail t))
  | otherwise = T.strip t

-- | Lightweight unescape for the tiny escape vocabulary we accept in
-- a quoted /key/ position. Full DQ escapes are handled by 'parseDQ'.
unescapeDQ :: Text -> Text
unescapeDQ = T.pack . go . T.unpack
  where
    go [] = []
    go ('\\':'"':rest)  = '"'  : go rest
    go ('\\':'\\':rest) = '\\' : go rest
    go ('\\':'n':rest)  = '\n' : go rest
    go ('\\':'t':rest)  = '\t' : go rest
    go ('\\':'r':rest)  = '\r' : go rest
    go (c:rest)         = c    : go rest

-- ---------------------------------------------------------------------------
-- Inline-comment stripping (respects quotes)
-- ---------------------------------------------------------------------------

stripInlineComment :: Text -> Text
stripInlineComment t = T.pack (loop (T.unpack t) (Outer :: QState))
  where
    loop [] _              = []
    loop (' ':'#':_) Outer = []
    loop ('\t':'#':_) Outer = []
    loop (c:rest) Outer
      | c == '"'  = c : loop rest InDQ
      | c == '\'' = c : loop rest InSQ
      | otherwise = c : loop rest Outer
    loop ('\\':c:rest) InDQ = '\\' : c : loop rest InDQ
    loop ('"':rest) InDQ    = '"'  : loop rest Outer
    loop (c:rest) InDQ      = c    : loop rest InDQ
    loop ('\'':'\'':rest) InSQ = '\'' : '\'' : loop rest InSQ
    loop ('\'':rest) InSQ      = '\'' : loop rest Outer
    loop (c:rest) InSQ         = c    : loop rest InSQ

data QState = Outer | InDQ | InSQ

-- ---------------------------------------------------------------------------
-- Double-quoted escape decoding
-- ---------------------------------------------------------------------------

decodeDQEscape :: Text -> Int -> Maybe (Char, Int)
decodeDQEscape t !i = case T.index t i of
  '0'  -> Just ('\0', i + 1)
  'a'  -> Just ('\a', i + 1)
  'b'  -> Just ('\b', i + 1)
  't'  -> Just ('\t', i + 1)
  'n'  -> Just ('\n', i + 1)
  'v'  -> Just ('\v', i + 1)
  'f'  -> Just ('\f', i + 1)
  'r'  -> Just ('\r', i + 1)
  'e'  -> Just ('\x1B', i + 1)
  ' '  -> Just (' ', i + 1)
  '"'  -> Just ('"', i + 1)
  '/'  -> Just ('/', i + 1)
  '\\' -> Just ('\\', i + 1)
  'N'  -> Just ('\x85',   i + 1)
  '_'  -> Just ('\xA0',   i + 1)
  'L'  -> Just ('\x2028', i + 1)
  'P'  -> Just ('\x2029', i + 1)
  'x'  -> readHex t (i + 1) 2
  'u'  -> readHex t (i + 1) 4
  'U'  -> readHex t (i + 1) 8
  _    -> Nothing
  where
    readHex tx !j n
      | j + n > T.length tx = Nothing
      | otherwise =
          let chunk = T.take n (T.drop j tx)
          in if T.all isHexDigit chunk
               then Just ( chr (T.foldl' (\acc c -> acc * 16 + digitToInt c) 0 chunk)
                         , j + n)
               else Nothing
