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

import Control.Monad (unless, when)
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
                              --   AND trailing whitespace (the form
                              --   most parser paths want)
  , lineRawBody :: !Text      -- ^ content after stripping indent
                              --   only — trailing whitespace kept,
                              --   for block-scalar collection
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
preprocess = go 1 . dropFinalEmpty . T.split (== '\n')
  where
    -- 'T.split' on a string ending in '\n' yields a trailing empty
    -- chunk; drop it so we don't synthesize a phantom blank line at
    -- EOF (which throws off block-scalar collection).
    dropFinalEmpty xs = case reverse xs of
      ("" : rest) -> reverse rest
      _           -> xs

    go !_ [] = []
    go !n (l:ls) =
      let !stripped = stripCR l
          !ind      = leadingSpaces stripped
          !body0    = T.drop ind stripped
          !body     = T.dropWhileEnd (\c -> c == ' ' || c == '\t') body0
          -- A line whose only content is a trailing tab (no
          -- structural indicator) reads as 'blank' for most
          -- parsing decisions, but 'collectScalarLines' for block
          -- scalars consults 'lineRawBody' and re-injects the
          -- whitespace as content.
          !kind     = classify body
          -- Track whether the indent column contains a literal TAB
          -- (i.e. whitespace mix that the YAML 1.2 spec §6.1
          -- forbids as block-context indentation). We don't fail
          -- here — many parser paths legitimately consume tabs as
          -- intra-line separation — but we keep the flag around
          -- for the structural parsers that /do/ care.
      in PLine n ind kind body body0 : go (n+1) ls

stripCR :: Text -> Text
stripCR t = case T.unsnoc t of
  Just (rest, '\r') -> rest
  _                 -> t

leadingSpaces :: Text -> Int
leadingSpaces = T.length . T.takeWhile (== ' ')

classify :: Text -> LineKind
classify t
  | T.null t                                  = LBlank
  | T.head t == '#'                           = LComment
  | t == T.pack "---"
      || T.isPrefixOf (T.pack "--- ")  t
      || T.isPrefixOf (T.pack "---\t") t      = LDocStart
  | t == T.pack "..."
      || T.isPrefixOf (T.pack "... ")  t
      || T.isPrefixOf (T.pack "...\t") t      = LDocEnd
  | T.head t == '%'                           = LDirective
  | otherwise                                 = LContent

isSkippable :: PLine -> Bool
isSkippable l = case lineKind l of
  LBlank     -> True
  LComment   -> True
  LDirective -> True
  _          -> False

-- | Like 'isSkippable' but does NOT include directives. Used at
-- the document-prologue boundary where directives carry meaning.
isSkippableNonDirective :: PLine -> Bool
isSkippableNonDirective l = case lineKind l of
  LBlank   -> True
  LComment -> True
  _        -> False

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
        (l : _) -> do
          d        <- parseDocument
          progress <- checkProgress (lineNo l)
          if progress
            then do
              ds <- loop
              pure (d : ds)
            else
              failP $ "stray content (line " ++ show (lineNo l) ++ ")"

    checkProgress prevLine = do
      ls <- getLines
      pure $ case dropWhile isSkippable ls of
        []      -> True
        (l : _) -> lineNo l /= prevLine

-- | Consume any leading directives ('%YAML ...', '%TAG ...') from
-- the line stream. Returns whether at least one directive was
-- present. Validates the directive syntax: '%YAML' takes a single
-- version token, '%TAG' takes exactly two arguments, and '%YAML'
-- can appear at most once per document.
consumeDirectives :: P Bool
consumeDirectives = go False False
  where
    go !sawAny !sawYaml = do
      ls <- getLines
      case dropWhile isSkippableNonDirective ls of
        (l : rest) | lineKind l == LDirective -> do
          setLines rest
          let body = stripInlineComment (lineBody l)
              args = T.words (T.drop 1 body)   -- drop leading '%'
          case args of
            ("YAML" : ver : extra) -> do
              when sawYaml $
                failP ("duplicate %YAML directive (line "
                       ++ show (lineNo l) ++ ")")
              when (not (null extra)) $
                failP ("extra words on %YAML directive (line "
                       ++ show (lineNo l) ++ ")")
              when (not (validYamlVersion ver)) $
                failP ("invalid %YAML version " ++ T.unpack ver
                       ++ " (line " ++ show (lineNo l) ++ ")")
              go True True
            ("TAG" : _handle : _prefix : []) ->
              go True sawYaml
            ("TAG" : _) ->
              failP ("malformed %TAG directive (line "
                     ++ show (lineNo l) ++ ")")
            ("YAML" : _) ->
              failP ("malformed %YAML directive (line "
                     ++ show (lineNo l) ++ ")")
            _ -> go True sawYaml   -- unknown / reserved directive
        _ -> pure sawAny

validYamlVersion :: Text -> Bool
validYamlVersion t = case T.splitOn (T.pack ".") t of
  [maj, min_]
    | T.all isDigit_ maj && T.all isDigit_ min_
    , not (T.null maj) && not (T.null min_) -> True
  _ -> False
  where
    isDigit_ c = c >= '0' && c <= '9'

parseDocument :: P Document
parseDocument = do
  -- Validate any directives that precede the document body. We
  -- accept blank / comment lines between directives, but reject
  -- duplicate %YAML / malformed directive lines, and require an
  -- explicit '---' marker after one or more directives.
  hadDirective <- consumeDirectives
  ls0' <- getLines
  let nextSig = dropWhile isSkippableNonDirective ls0'
  (directives, ls1) <- case nextSig of
    (l : rest) | lineKind l == LDocStart ->
      let body = lineBody l
          tail_ = T.stripStart (T.drop 3 body)
      in pure $ if T.null tail_
                  then (True, rest)
                  else (True,
                        PLine (lineNo l) (lineIndent l)
                              LContent tail_ tail_ : rest)
    (l : _) | hadDirective ->
      failP ("missing '---' after directive (line "
             ++ show (lineNo l) ++ ")")
    [] | hadDirective ->
      failP "directive without document"
    _ -> pure (False, ls0')
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
dispatch l0 = do
  -- Strip leading TAB characters. They're not allowed as block
  -- indentation per spec §6.1 but real-world inputs use them as
  -- 'separation' whitespace between a structural marker and the
  -- following node — '\\t{}', '\\t- x', '\\t"…"' all parse OK
  -- in libfyaml etc.
  let body0 = lineBody l0
  case T.uncons body0 of
    Just ('\t', _) -> do
      let l = l0 { lineBody    = T.dropWhile (== '\t') body0
                 , lineRawBody = T.dropWhile (== '\t') (lineRawBody l0)
                 }
      modifyS (\s -> case psLines s of
                       (top : rs) | lineNo top == lineNo l0 ->
                         s { psLines = l : rs }
                       _ -> s)
      dispatchOn l
    _ -> dispatchOn l0
  where
    dispatchOn l = case T.uncons (lineBody l) of
       Just ('!', _)  -> parseTagged
       Just ('&', _)  -> parseAnchored
       Just ('*', _)  -> case findAliasKeySplit (lineBody l) of
                           Just (aliasName, vRest) ->
                             parseBlockMapAliasFirst (lineIndent l)
                               aliasName vRest
                           Nothing -> parseAlias
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
    consumeQuoted q (preserveTrailingEscape (lineBody l) (lineRawBody l))

-- | If the trimmed line body ends in @\\@ that's not itself
-- escaped, take the next character /verbatim/ from the raw body
-- (so an escape argument like @\\<TAB>@ survives the trailing-WS
-- strip done by 'preprocess'). Any whitespace /after/ that escape
-- argument is still stripped.
preserveTrailingEscape :: Text -> Text -> Text
preserveTrailingEscape stripped raw = case T.unsnoc stripped of
  Just (_, '\\') | not (endsEvenBackslashes stripped) ->
    -- Reach into raw at the position right after the trailing '\'.
    let idx = T.length stripped
    in if idx < T.length raw
         then stripped <> T.singleton (T.index raw idx)
         else stripped
  _ -> stripped
  where
    endsEvenBackslashes t =
      even (T.length (T.takeWhileEnd (== '\\') t))

-- | Greedily extend a quoted-scalar buffer with successor lines
-- until the matching close quote is found. Per YAML 1.2 §7.3.1-2:
--
-- * A single line break between non-empty lines folds to a single
--   space.
-- * A run of @n@ empty lines between non-empty content yields @n@
--   line breaks (the surrounding break itself is consumed).
--
-- Any text after the close quote on the final line is pushed back
-- as a virtual line so the surrounding context can keep parsing.
consumeQuoted :: Char -> Text -> P Value
consumeQuoted q = go0
  where
    parser = case q of '"' -> parseDQ; _ -> parseSQ

    -- The very first attempt; no fold prefix has been emitted yet.
    go0 !buf = case parser 0 buf of
      Just (v, p)   -> finish v (T.drop p buf)
      Nothing       -> readMore buf 0

    -- @blanks@ counts consecutive empty continuation lines we've
    -- absorbed since the last non-empty (or the opening) line.
    -- We pop the raw next line (not 'popLine', which would skip
    -- blank / comment lines — those are significant inside a
    -- multi-line quoted scalar).
    readMore !buf !blanks = do
      ls <- getLines
      case ls of
        []       -> failP "YAML: unterminated quoted scalar"
        (l' : rest) -> do
          setLines rest
          let body0 = lineBody l'
              raw   = lineRawBody l'
              -- Use the raw body when the trimmed line body ends
              -- in '\\' so that '\\<TAB>' survives.
              body  = preserveTrailingEscape body0 raw
              isBlank = T.null (T.strip body)
              body' = T.dropWhile (\c -> c == ' ' || c == '\t') body
              -- DQ-only: a bare trailing backslash on the previous
              -- line eats the newline plus any leading whitespace
              -- on the next line (YAML 1.2 §5.7 / §7.5).
              endsWithEscape = q == '"'
                            && case T.unsnoc buf of
                                 Just (_, '\\') -> not (endsEvenBackslashes buf)
                                 _              -> False
          if isBlank
            then readMore buf (blanks + 1)
            else
              let (buf', joined)
                    | endsWithEscape =
                        (T.init buf <> body', True)
                    | otherwise =
                        let joinSep
                              | blanks == 0 = T.pack " "
                              | otherwise   = T.replicate blanks (T.pack "\n")
                        in (buf <> joinSep <> body', True)
              in joined `seq` case parser 0 buf' of
                   Just (v, p)   -> finish v (T.drop p buf')
                   Nothing       -> readMore buf' 0

    -- A run of trailing backslashes counts as "even" when it
    -- pairs up to "\\\\…", which means no escape at end.
    endsEvenBackslashes t = even (T.length (T.takeWhileEnd (== '\\') t))

    finish v rest =
      let trimmed = T.stripStart rest
          stripped = case T.uncons trimmed of
            -- A '#' immediately after the closing quote is a
            -- comment too (no preceding space required, since the
            -- quote itself is the boundary).
            Just ('#', _) -> T.empty
            _             -> T.stripEnd (stripInlineComment trimmed)
      in if T.null stripped
           then pure v
           -- Trailing content on the same line after the closing
           -- quote is OK if it's a flow-context comma, mapping
           -- separator, or a flow-collection close bracket; anything
           -- else is malformed (Q4CL / JY7Z / trailing-content-
           -- after-quoted-value).
           else case T.uncons stripped of
                  Just (c, _)
                    | c == ','  -> pushBack stripped
                    | c == ':'  -> pushBack stripped
                    | c == ']'  -> pushBack stripped
                    | c == '}'  -> pushBack stripped
                    | otherwise ->
                        failP $ "trailing content after quoted scalar: "
                                ++ show stripped
                  Nothing -> pure v
      where
        pushBack s = do
          pushLine (PLine 0 0 LContent s s)
          pure v

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
        Just l2 | lineIndent l2 >= lineIndent l -> do
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
      (name, rest) = takeAnchorName (T.drop 1 body)
      after = T.stripStart rest
  -- If the rest of the line introduces a mapping (i.e. there's a
  -- top-level @": "@ in the remainder), the anchor only binds to
  -- the /key/, not to the surrounding mapping. This matches the
  -- YAML 1.2 node-anchoring model where the anchor precedes the
  -- specific node it labels.
  case findKeyValueSplit after of
    Just (k, vRest) -> do
      let keyVal = YString k
      recordAnchor name keyVal
      pushLine l { lineBody = after }
      -- Replay the line through the regular block-mapping path so
      -- the surrounding context still parses. The recorded anchor
      -- already points at the correct key node.
      parseBlockMap (lineIndent l) k vRest
    Nothing -> do
      v <- if T.null after
             then do
               mNext <- peekLine
               case mNext of
                 Just l2 | lineIndent l2 >= lineIndent l ->
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
      (name, rest) = takeAnchorName (T.drop 1 body)
      after = T.stripStart rest
  -- An alias appearing as a mapping value will be followed by ":"
  -- (or other dispatch chars) on the same line. Push remaining
  -- content back as a virtual line so the surrounding context can
  -- continue parsing.
  if T.null after
    then resolveAnchor name
    else do
      pushLine (PLine (lineNo l) (lineIndent l) LContent after after)
      resolveAnchor name

-- | Characters legal in an anchor / alias name (YAML 1.2 §6.9.2).
-- Anchors exclude flow indicators and whitespace; the colon
-- /is/ allowed inside an anchor name (so @&an:chor@ is legal),
-- but only when followed by another anchor char — see
-- 'takeAnchorName'.
isAnchorChar :: Char -> Bool
isAnchorChar c =
  not (c == ',' || c == '[' || c == ']' || c == '{' || c == '}'
       || c == ' ' || c == '\t' || c == '\n' || c == '\r')

-- | Read an anchor / alias name. Treats @:@ as part of the name
-- only when followed by another anchor character (so
-- @&an:chor@ → @\"an:chor\"@) but as a terminator otherwise (so
-- @&a:@ followed by a space → @\"a\"@ + remainder @\":\"@).
takeAnchorName :: Text -> (Text, Text)
takeAnchorName t = goT 0
  where
    !len = T.length t
    goT !i
      | i >= len = (t, T.empty)
      | otherwise =
          let c = T.index t i
              nextIsAnchor =
                i + 1 < len && isAnchorChar (T.index t (i + 1))
                            && T.index t (i + 1) /= ':'
          in if c == ':' && not nextIsAnchor
               then (T.take i t, T.drop i t)
               else if isAnchorChar c
                      then goT (i + 1)
                      else (T.take i t, T.drop i t)

breakOnSpace :: Text -> (Text, Text)
breakOnSpace = T.break (\c -> c == ' ' || c == '\t')

-- ---------------------------------------------------------------------------
-- Flow style
-- ---------------------------------------------------------------------------

consumeFlowFromHead :: P Value
consumeFlowFromHead = do
  Just l <- popLine
  consumeFlow (lineBody l)

-- | Walk the parsed flow value and (a) register any embedded
-- 'YAnchored' nodes, (b) resolve any alias placeholders left by
-- 'parseFlowAlias'.
recordFlowAnchors :: Value -> P ()
recordFlowAnchors = goV
  where
    goV v = case v of
      YAnchored (Anchor n) inner -> do
        recordAnchor n inner
        goV inner
      YTagged _ inner            -> goV inner
      YSeq xs                    -> mapM_ goV (toListV xs)
      YMap kvs                   -> mapM_ (\(k, x) -> goV k >> goV x)
                                          (toListV kvs)
      _                          -> pure ()

    toListV v = V.toList v

resolveFlowAliases :: Value -> P Value
resolveFlowAliases = goV
  where
    goV (YString t)
      | T.isPrefixOf (T.pack "\0alias\0") t = do
          let nm = T.drop (T.length (T.pack "\0alias\0")) t
          resolveAnchor nm
    goV (YAnchored a v)         = YAnchored a <$> goV v
    goV (YTagged   a v)         = YTagged   a <$> goV v
    goV (YSeq xs)               = YSeq <$> V.mapM goV xs
    goV (YMap kvs)              = YMap <$> V.mapM
                                     (\(k, x) -> (,) <$> goV k <*> goV x) kvs
    goV v                       = pure v

consumeFlow :: Text -> P Value
consumeFlow = go
  where
    -- Strip end-of-line comments before adding a new chunk to the
    -- buffer. Flow nodes may contain comments between elements,
    -- per the YAML 1.2 grammar.
    stripFlowComment t = T.stripEnd (stripInlineComment t)

    go buf0 = let buf = stripFlowComment buf0 in case scanFlow buf of
      ScanComplete v rest -> do
        recordFlowAnchors v
        v' <- resolveFlowAliases v
        let !s = T.stripStart rest
        case T.uncons s of
          Nothing -> pure v'
          Just _  -> do
            pushLine (PLine 0 0 LContent s s)
            pure v'
      ScanIncomplete -> do
        -- Read the raw next line; inside a flow node, a '#'
        -- /comment/ line is content (we already strip end-of-line
        -- comments per chunk in 'go') and a '%' line is a plain
        -- scalar character, not a directive.
        ls <- getLines
        case ls of
          []         -> failP "YAML: unterminated flow node"
          (l' : rs)  -> do
            setLines rs
            go (buf <> T.pack " " <> lineBody l')

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
              '!'  -> parseFlowTagged p t
              '&'  -> parseFlowAnchored p t
              '*'  -> parseFlowAlias p t
              _    -> parseFlowPlain p t

-- | Tagged node in flow context. Reads the tag token (everything
-- up to whitespace / flow stopper) and then optionally parses a
-- following node; @!!str@ alone is allowed and means "tagged
-- null".
parseFlowTagged :: Int -> Text -> Maybe (Value, Int)
parseFlowTagged !p t =
  let !len = T.length t
      goT !i
        | i >= len = i
        | otherwise = case T.index t i of
            ' '  -> i
            '\t' -> i
            ','  -> i
            ']'  -> i
            '}'  -> i
            _    -> goT (i + 1)
      !p1   = goT p
      tagText = T.take (p1 - p) (T.drop p t)
      tag = expandTag tagText
      p2  = skipFlowWS p1 t
  in if p2 >= T.length t
       then Just (YTagged tag YNull, p2)
       else case T.index t p2 of
              ',' -> Just (YTagged tag YNull, p1)
              ']' -> Just (YTagged tag YNull, p1)
              '}' -> Just (YTagged tag YNull, p1)
              ':' | colonIsSeparator (p2 + 1) t ->
                     Just (YTagged tag YNull, p1)
              _   -> case parseFlowValue p2 t of
                       Just (v, p3) -> Just (YTagged tag v, p3)
                       Nothing      -> Just (YTagged tag YNull, p2)

-- | Anchor in flow context: read the anchor name and parse the
-- labelled value, wrapping it in 'YAnchored' so the post-pass
-- 'recordFlowAnchors' picks it up and registers it with the
-- enclosing parser state.
parseFlowAnchored :: Int -> Text -> Maybe (Value, Int)
parseFlowAnchored !p t =
  let !len = T.length t
      goN !i
        | i >= len = i
        | otherwise = case T.index t i of
            ' '  -> i
            '\t' -> i
            ','  -> i
            ']'  -> i
            '}'  -> i
            _    -> goN (i + 1)
      !endName = goN (p + 1)
      name    = T.take (endName - (p + 1)) (T.drop (p + 1) t)
      p2 = skipFlowWS endName t
  in if p2 >= T.length t
       then Just (YAnchored (Anchor name) YNull, p2)
       else case T.index t p2 of
              ','  -> Just (YAnchored (Anchor name) YNull, endName)
              ']'  -> Just (YAnchored (Anchor name) YNull, endName)
              '}'  -> Just (YAnchored (Anchor name) YNull, endName)
              ':' | colonIsSeparator (p2 + 1) t ->
                      Just (YAnchored (Anchor name) YNull, endName)
              _   -> case parseFlowValue p2 t of
                       Just (v, p3) -> Just (YAnchored (Anchor name) v, p3)
                       Nothing      -> Just (YAnchored (Anchor name) YNull, p2)

-- | Alias in flow context: emit a 'YAnchored'-tagged placeholder
-- whose value is a sentinel 'YString' starting with @"\\0alias\\0"@.
-- The post-pass resolves these to the registered anchor value.
parseFlowAlias :: Int -> Text -> Maybe (Value, Int)
parseFlowAlias !p t =
  let !len = T.length t
      goN !i
        | i >= len = i
        | otherwise = case T.index t i of
            ' '  -> i
            '\t' -> i
            ','  -> i
            ']'  -> i
            '}'  -> i
            _    -> goN (i + 1)
      !p1   = goN (p + 1)
      name  = T.take (p1 - (p + 1)) (T.drop (p + 1) t)
  in Just (YString (T.pack "\0alias\0" <> name), p1)

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
       else
         -- A leading '?' followed by space marks an explicit-key
         -- mapping pair in flow (spec §7.5). Skip the marker and
         -- continue parsing the key.
         if T.index t p == '?'
            && p + 1 < T.length t
            && (T.index t (p + 1) == ' ' || T.index t (p + 1) == '\t')
           then parseFlowEntry (skipFlowWS (p + 1) t) t
         else
         if T.index t p == ':' && colonIsSeparator (p + 1) t
           then
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
           else
             let !flowOpener = case T.index t p of
                   '"'  -> True
                   '\'' -> True
                   '['  -> True
                   '{'  -> True
                   _    -> False
             in case parseFlowValue p t of
                  Nothing -> Nothing
                  Just (k, p1) ->
                    let p2 = skipFlowWS p1 t
                    in if p2 < T.length t
                         && T.index t p2 == ':'
                         && (flowOpener
                             || colonIsSeparator (p2 + 1) t)
                         then case parseFlowValue (skipFlowWS (p2 + 1) t) t of
                                Nothing      -> Just (YMap (V.singleton (k, YNull)), p2 + 1)
                                Just (v, p3) -> Just (YMap (V.singleton (k, v)), p3)
                         else Just (k, p1)

-- | Whether a colon at position @p@ acts as a key/value separator
-- in flow context: only when the very next character is a flow
-- stopper or whitespace.
colonIsSeparator :: Int -> Text -> Bool
colonIsSeparator !p t
  | p >= T.length t = True
  | otherwise = case T.index t p of
      ' '  -> True
      '\t' -> True
      ','  -> True
      ']'  -> True
      '}'  -> True
      _    -> False

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

-- | A line starts a block-sequence entry if its body is exactly
-- @-@ or starts with @- @ / @-<TAB>@.
isSeqItem :: Text -> Bool
isSeqItem b =
  b == T.pack "-"
  || T.isPrefixOf (T.pack "- ")  b
  || T.isPrefixOf (T.pack "-\t") b

-- | Same shape for the explicit-key marker @?@.
isExplicitKey :: Text -> Bool
isExplicitKey b =
  b == T.pack "?"
  || T.isPrefixOf (T.pack "? ")  b
  || T.isPrefixOf (T.pack "?\t") b

parseBlockOrPlain :: PLine -> P Value
parseBlockOrPlain l
  | isSeqItem body     = parseBlockSeq (lineIndent l)
  | isExplicitKey body = parseExplicitMap (lineIndent l)
  | otherwise = case findAliasKeySplit body of
      Just (aliasName, vRest) -> parseBlockMapAliasFirst (lineIndent l)
                                   aliasName vRest
      Nothing -> case findKeyValueSplit body of
        Just (k, vRest) -> parseBlockMap (lineIndent l) k vRest
        Nothing         -> parsePlainScalar (lineIndent l) body
  where
    body = lineBody l

-- | Block mapping whose first key is an alias node (parsed via
-- 'findAliasKeySplit'). Same shape as 'parseBlockMap' otherwise.
parseBlockMapAliasFirst :: Int -> Text -> Text -> P Value
parseBlockMapAliasFirst !ind aliasName firstRest = do
  Just _ <- popLine
  k0 <- resolveAnchor aliasName
  v0 <- parseImplicitMapValue ind firstRest
  rest <- collect [(k0, v0)]
  pure (YMap (V.fromList (reverse rest)))
  where
    collect acc = do
      mPL <- peekLine
      case mPL of
        Nothing -> pure acc
        Just l
          | lineIndent l /= ind -> pure acc
          | isSeqItem (lineBody l) -> pure acc
          | isExplicitKey (lineBody l) -> pure acc
          | startsWithTab (lineBody l) ->
              failP $ "tab character used as indentation (line "
                      ++ show (lineNo l) ++ ")"
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (a, vRest) -> do
                _ <- popLine
                k <- resolveAnchor a
                v <- parseImplicitMapValue ind vRest
                collect ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  _ <- popLine
                  v <- parseImplicitMapValue ind vRest
                  collect ((YString k, v) : acc)
                Nothing -> pure acc

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
          | not (isSeqItem (lineBody l))
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
  -- '-<TAB><INDICATOR>' or '- <TAB><INDICATOR>' (nested block
  -- marker reached via a tab in the indent column) is 'tab as
  -- indentation' per spec §6.1. The plain-scalar form
  -- '-<TAB>x' is fine because no further indent calculation
  -- happens; but '-\\t-' / '- \\t-' / '-\\t?' / etc. would set
  -- the nested block's indent to a tab-containing column.
  let separatorHasTab = T.any (== '\t')
                          (T.takeWhile (\c -> c == ' ' || c == '\t')
                             (T.drop 1 body))
  when (separatorHasTab && startsWithBlockIndicator after') $
    failP $ "tab character used as indentation before nested block marker (line "
            ++ show (lineNo l) ++ ")"
  if T.null after'
    then do
      mNext <- peekLine
      case mNext of
        Just l2 | lineIndent l2 > ind -> parseNode (lineIndent l2)
        _ -> pure YNull
    else do
      let virt = PLine (lineNo l) (ind + 2) LContent after' after'
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
          | isSeqItem (lineBody l) -> pure acc
          | isExplicitKey (lineBody l) -> pure acc
          -- A continuation line whose first character is a TAB
          -- means the user used a tab as additional indentation
          -- — illegal per spec §6.1.
          | startsWithTab (lineBody l) ->
              failP $ "tab character used as indentation (line "
                      ++ show (lineNo l) ++ ")"
          | otherwise -> case findAliasKeySplit (lineBody l) of
              Just (aliasName, vRest) -> do
                _ <- popLine
                k <- resolveAnchor aliasName
                v <- parseImplicitMapValue ind vRest
                collect ((k, v) : acc)
              Nothing -> case findKeyValueSplit (lineBody l) of
                Just (k, vRest) -> do
                  _ <- popLine
                  let (anchors, k') = stripKeyProperties k
                  v <- parseImplicitMapValue ind vRest
                  let kv = YString k'
                  mapM_ (\an -> recordAnchor an kv) anchors
                  collect ((kv, v) : acc)
                Nothing -> pure acc

startsWithTab :: Text -> Bool
startsWithTab t = case T.uncons t of
  Just ('\t', _) -> True
  _              -> False

-- | True when @t@ begins with a block-context structural marker
-- ('-' or '?' followed by space / tab / EOL).
startsWithBlockIndicator :: Text -> Bool
startsWithBlockIndicator t = case T.uncons t of
  Just ('-', rest) -> isBlockSep rest
  Just ('?', rest) -> isBlockSep rest
  _                -> False
  where
    isBlockSep r = case T.uncons r of
      Nothing         -> True
      Just (' ', _)   -> True
      Just ('\t', _)  -> True
      _               -> False

-- | Strip leading anchor / tag tokens (separated by spaces) from
-- a block-mapping key string. Returns the list of anchor names
-- encountered and the remainder text. Tags are dropped silently
-- (they don't change the key projection).
stripKeyProperties :: Text -> ([Text], Text)
stripKeyProperties = go []
  where
    go acc t = case T.uncons (T.stripStart t) of
      Just ('&', rest) ->
        let (name, after) = takeAnchorName rest
        in go (name : acc) after
      Just ('!', rest) ->
        let (_tg, after) = T.span (\c -> not (c == ' ' || c == '\t')) rest
        in go acc after
      _ -> (reverse acc, T.stripStart t)

-- | Recognise @*alias : value@ style mapping entries where the key
-- is an alias node. Returns the alias name (without the @*@) and
-- the value text after the colon, or 'Nothing' when the line
-- doesn't have this shape.
findAliasKeySplit :: Text -> Maybe (Text, Text)
findAliasKeySplit t = case T.uncons t of
  Just ('*', rest) ->
    let (name, after) = takeAnchorName rest
        afterTrim = T.stripStart after
    in case T.uncons afterTrim of
         Just (':', tail_)
           | T.null tail_ || T.head tail_ == ' ' || T.head tail_ == '\t'
               -> Just (name, T.drop 1 afterTrim)
         _   -> Nothing
  _ -> Nothing

parseImplicitMapValue :: Int -> Text -> P Value
parseImplicitMapValue !ind vRest =
  let after = T.stripStart vRest
  in if T.null after
       then do
         mNext <- peekLine
         case mNext of
           Just l2
             | lineIndent l2 > ind -> do
                 -- Strip leading tabs from the body, which YAML
                 -- treats as additional whitespace (not part of
                 -- the scalar text).
                 let body' = T.dropWhile (== '\t') (lineBody l2)
                 modifyS (\s ->
                   s { psLines = case psLines s of
                         (h : rs) -> h { lineBody = body' } : rs
                         []       -> [] })
                 parseNode (lineIndent l2)
             | lineIndent l2 == ind
                 && (isSeqItem (lineBody l2))
                 -> parseBlockSeq ind
           _ -> pure YNull
       else case T.uncons after of
         Just ('|', _) -> do
           -- Block scalar body lines must be at indent > parent
           -- (== ind here). Encode that with a virtual line at @ind@
           -- so parseBlockScalar's collectScalarLines uses the right
           -- comparison.
           pushLine (PLine 0 ind LContent after after)
           parseBlockScalar Literal
         Just ('>', _) -> do
           pushLine (PLine 0 ind LContent after after)
           parseBlockScalar Folded
         Just ('[', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after after)
           consumeFlowFromHead
         Just ('{', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after after)
           consumeFlowFromHead
         Just ('&', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after after)
           parseAnchored
         Just ('*', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after after)
           parseAlias
         Just ('!', _) -> do
           pushLine (PLine 0 (ind + 2) LContent after after)
           parseTagged
         Just ('"', _)  -> consumeQuoted '"'  after
         Just ('\'', _) -> consumeQuoted '\'' after
         _ -> pure (resolvePlain (T.stripEnd (stripInlineComment after)))

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
          | isExplicitKey (lineBody l) -> do
              k <- readExplicitPart "?"
              v <- readExplicitValue
              collect ((k, v) : acc)
          -- A bare ':' or ': value' line at the mapping indent is
          -- an entry with an implicit (null) key. Per spec §8.18,
          -- omitting the '?' is permitted.
          | lineBody l == ":" || T.isPrefixOf ": " (lineBody l) -> do
              v <- readExplicitPart ":"
              collect ((YNull, v) : acc)
          | otherwise -> pure acc

    readExplicitPart marker = do
      Just l <- popLine
      let body = lineBody l
          afterMarker = if body == marker then T.empty
                                          else T.drop 1 body
          rest = T.stripStart (T.drop 1 afterMarker)
      -- '?<TAB>...' / ':<TAB>...' uses a tab where YAML 1.2 §6.1
      -- requires a space (the explicit-key marker's separation
      -- contributes to indentation calculation).
      case T.uncons afterMarker of
        Just ('\t', _) ->
          failP $ "tab character after explicit-key marker (line "
                  ++ show (lineNo l) ++ ")"
        _ -> pure ()
      if T.null rest
        then do
          mNext <- peekLine
          case mNext of
            Just l2 | lineIndent l2 > lineIndent l -> parseNode (lineIndent l2)
            _ -> pure YNull
        else do
          pushLine (PLine (lineNo l) (lineIndent l + 2) LContent rest rest)
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
  rest <- collectFolds ind 0 []
  let !final = joinPlain (first : rest)
  pure (resolvePlain final)
  where
    -- @blanks@ counts the run of consecutive blank lines we've
    -- absorbed since the last non-blank continuation line. The
    -- collected list interleaves non-blank line bodies with marker
    -- entries representing blank-line runs (encoded as the empty
    -- string preceded by a special sentinel, see joinPlain).
    --
    -- A continuation line is accepted when its indent is /strictly
    -- greater/ than the scalar's base indent and it doesn't look
    -- like a new collection entry (mapping key, seq item, explicit
    -- '?' key).
    collectFolds baseInd blanks acc = do
      ls <- getLines
      case ls of
        []     -> pure (reverse acc)
        (l:_)
          | lineKind l == LBlank ->
              do consumeOne; collectFolds baseInd (blanks + 1) acc
          | lineKind l == LComment
            && lineIndent l > baseInd ->
              do consumeOne; collectFolds baseInd blanks acc
          | (lineKind l == LContent || lineKind l == LDirective)
            && (lineIndent l > baseInd
                -- A bare plain scalar at the top level (baseInd 0)
                -- can continue into more lines at column 0 as
                -- long as they're not structural.
                || (baseInd == 0 && lineIndent l == 0))
            && not (isSeqItem (lineBody l))
            && not (isExplicitKey (lineBody l))
            && case findKeyValueSplit (lineBody l) of
                 Just _  -> False
                 Nothing -> True
            -> do
              consumeOne
              let s0 = T.stripEnd (stripInlineComment (lineBody l))
                  -- Leading whitespace on a plain-scalar continuation
                  -- line is part of the framing, not the scalar
                  -- content (spec §6.5).
                  s = T.dropWhile (\c -> c == ' ' || c == '\t') s0
                  prefix
                    | blanks == 0 = s
                    | otherwise   = T.replicate blanks (T.pack "\n") <> s
              collectFolds baseInd 0 (prefix : acc)
          | otherwise -> pure (reverse acc)

    consumeOne = do
      ls <- getLines
      case ls of
        (_:xs) -> setLines xs
        []     -> pure ()

    -- Join the collected pieces; pieces that already start with a
    -- newline marker are joined with no separator.
    joinPlain = go
      where
        go []     = T.empty
        go [x]    = x
        go (x:y:zs)
          | T.isPrefixOf (T.pack "\n") y = x <> go (y:zs)
          | otherwise                    = x <> T.pack " " <> go (y:zs)

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
  -- Fallback baseInd when no non-empty content line exists: use
  -- parent+1 so that a single more-indented blank still chomps
  -- correctly to a single newline rather than preserving its
  -- leading spaces.
  let bodyAdj = case nonEmptyContent body of
        True  -> body
        False -> map (\(i, b) -> if i < 0 then (i, b) else (-1, b)) body
      txt = case k of
        Literal -> joinLiteral chomp bodyAdj
        Folded  -> joinFolded  chomp bodyAdj
  pure (YString txt)
  where
    nonEmptyContent = any (\(i, b) -> i >= 0 && not (T.null b))

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

-- | Collect the body lines of a block scalar.
--
-- The semantics: blank / more-indented blank lines belong to the
-- scalar regardless of their column. Once we've seen a content
-- line, that line's indent /is/ the "base indent" of the scalar;
-- the scalar terminates on the first subsequent line whose indent
-- falls /at or below/ that base. Lines whose source classified as
-- @LComment@ but sit at indent > base are treated as scalar
-- content (the '#' is data); comments at base or shallower
-- terminate.
collectScalarLines :: Int -> P [(Int, Text)]
collectScalarLines !parent = collect Nothing []
  where
    -- @mBase@ is the established base indent (after the first
    -- content line). @acc@ is the reverse-accumulated body.
    collect mBase acc = do
      ls <- getLines
      case ls of
        []     -> pure (reverse acc)
        (l:_)
          | lineKind l == LBlank ->
              do _ <- consumeOne
                 let ind = lineIndent l
                     raw = lineRawBody l
                     hasTabs = not (T.null raw)
                     isMoreIndented = case mBase of
                       Just b  -> ind > b
                       Nothing -> ind > parent
                 if isMoreIndented
                   then collect mBase ((ind, raw) : acc)
                   else if hasTabs
                          then case mBase of
                                 Just b | ind >= b ->
                                    collect mBase ((ind, raw) : acc)
                                 _ -> collect mBase ((-1, T.empty) : acc)
                          else collect mBase ((-1, T.empty) : acc)
          | lineKind l == LDocStart || lineKind l == LDocEnd
              -> pure (reverse acc)
          | otherwise ->
              let ind = lineIndent l
                  inside = case mBase of
                    Just b  -> ind >= b
                    Nothing -> ind > parent
              in if not inside
                   then pure (reverse acc)
                   else if lineKind l == LComment
                          then case mBase of
                            -- Once a base indent is set, any
                            -- comment at a deeper indent is
                            -- content; one at base terminates.
                            Just b | ind > b -> do
                              _ <- consumeOne
                              collect mBase ((ind, lineBody l) : acc)
                            Just _ -> pure (reverse acc)
                            -- Before a base is set, a comment
                            -- counts as the first content line.
                            Nothing -> do
                              _ <- consumeOne
                              collect (Just ind)
                                ((ind, lineRawBody l) : acc)
                          else do
                            _ <- consumeOne
                            let mBase' = case mBase of
                                  Just _  -> mBase
                                  Nothing -> Just ind
                            collect mBase'
                              ((ind, lineRawBody l) : acc)

    consumeOne = do
      ls <- getLines
      case ls of
        (_:xs) -> setLines xs >> pure ()
        []     -> pure ()

joinLiteral :: Chomp -> [(Int, Text)] -> Text
joinLiteral chomp xs =
  let baseInd = minNonNegative xs
      lns     = map (renderLine baseInd) xs
      raw | null xs   = T.empty
          | otherwise = T.intercalate (T.pack "\n") lns <> T.pack "\n"
  in chompText chomp raw
  where
    renderLine bi (i, b)
      | i < 0     = T.empty
      | otherwise = T.replicate (max 0 (i - bi)) (T.pack " ") <> b

joinFolded :: Chomp -> [(Int, Text)] -> Text
joinFolded chomp xs =
  let baseInd = minNonNegative xs
      raw | null xs   = T.empty
          | otherwise = T.concat (foldFirst xs baseInd) <> T.pack "\n"
  in chompText chomp raw
  where
    isBlank (i, b) = i < 0 || T.null b

    -- A line is "more-indented" if its source column is past the
    -- base indent OR its body starts with whitespace (a leading
    -- tab counts).
    isMoreIndented bi (i, b) =
      i > bi
      || case T.uncons b of
           Just (' ',  _) -> True
           Just ('\t', _) -> True
           _              -> False

    foldFirst [] _ = []
    foldFirst ((i, b) : rest) bi
      | isBlank (i, b) = T.pack "\n" : foldAfterBlank rest bi False
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
              more = isMoreIndented bi (i, b)
          in txt : foldNext rest bi more

    foldNext [] _ _ = []
    foldNext ((i, b) : rest) bi prevMore
      | isBlank (i, b) =
          -- A break right after a more-indented line is preserved
          -- as a literal newline; the upcoming blank emits another
          -- on top of that.
          let pre = if prevMore then [T.pack "\n"] else []
          in pre ++ T.pack "\n" : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
              nowMore = isMoreIndented bi (i, b)
              joinSep
                | prevMore || nowMore = T.pack "\n"
                | otherwise           = T.pack " "
          in joinSep : txt : foldNext rest bi nowMore

    -- @prevMore@ here refers to whether the line that opened the
    -- blank run was more-indented; when leaving a blank run we
    -- need to emit one more break if either side is more-indented.
    foldAfterBlank [] _ _ = []
    foldAfterBlank ((i, b) : rest) bi prevMore
      | isBlank (i, b) = T.pack "\n" : foldAfterBlank rest bi prevMore
      | otherwise =
          let txt = T.replicate (max 0 (i - bi)) (T.pack " ") <> b
              nowMore = isMoreIndented bi (i, b)
              -- If we're leaving a blank-run into a more-indented
              -- line and the previous content line was /not/
              -- itself more-indented, the spec requires an extra
              -- preserved break.
              extra = if nowMore && not prevMore
                        then [T.pack "\n"]
                        else []
          in extra ++ txt : foldNext rest bi nowMore

-- | Smallest indent from a non-blank line in the collected list,
-- or 0 if there are no non-blank lines. Blank lines (including
-- "more-indented blanks" we keep around for spacing) do not
-- contribute, since the spec defines the body indent as the indent
-- of the first non-empty line.
minNonNegative :: [(Int, Text)] -> Int
minNonNegative = go Nothing
  where
    go acc [] = case acc of
      Nothing -> 0
      Just !n -> n
    go acc ((i, b) : rest)
      | i < 0 || T.null b = go acc rest
      | otherwise = case acc of
          Nothing             -> go (Just i) rest
          Just !n | i < n     -> go (Just i) rest
                  | otherwise -> go (Just n) rest

chompText :: Chomp -> Text -> Text
chompText Strip = T.dropWhileEnd (== '\n')
chompText Keep  = id
chompText Clip  = \t ->
  let stripped = T.dropWhileEnd (== '\n') t
  in if T.null stripped
       then T.empty               -- "no content" → no trailing newline
       else stripped <> T.pack "\n"

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
          '"' | atTokenStart i -> go (i + 1) depth brace 1
          '\''| atTokenStart i -> go (i + 1) depth brace 2
          '[' | atTokenStart i || depth > 0 || brace > 0
                                -> go (i + 1) (depth + 1) brace inStr
          ']' | depth > 0       -> go (i + 1) (depth - 1) brace inStr
          '{' | atTokenStart i || depth > 0 || brace > 0
                                -> go (i + 1) depth (brace + 1) inStr
          '}' | brace > 0       -> go (i + 1) depth (brace - 1) inStr
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

    -- Quote characters only act as string delimiters at the start
    -- of input or right after whitespace; mid-token quotes are
    -- ordinary characters of a plain scalar.
    atTokenStart 0 = True
    atTokenStart i = case T.index t (i - 1) of
      ' '  -> True
      '\t' -> True
      _    -> False

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
  '\t' -> Just ('\t', i + 1)   -- '\<TAB>' as literal tab
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
