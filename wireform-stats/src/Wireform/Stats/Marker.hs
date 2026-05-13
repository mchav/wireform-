-- | AUTOGEN marker grammar and rewriter.
--
-- Each managed README region is wrapped in a paired HTML comment:
--
-- @
-- \<!-- BEGIN_AUTOGEN \<key\> --\>
-- ... content owned by the regen tool ...
-- \<!-- END_AUTOGEN \<key\> --\>
-- @
--
-- The key is a free-form text identifier (e.g. @tests@, @coverage@,
-- @bench:cbor-vs-cborg-encode@). Anything outside the markers is
-- hand-edited and never touched. Anything inside is owned by the
-- regen tool.
--
-- The rewriter preserves the surrounding text verbatim, including
-- the marker lines themselves; only the body between them is
-- replaced.
module Wireform.Stats.Marker
  ( -- * Marker keys
    MarkerKey
  , markerKey
  , unMarkerKey
    -- * Region grammar
  , Region (..)
  , parseRegions
  , renderRegions
    -- * Rewriting
  , Replacement
  , rewriteMarkers
  , rewriteFile
    -- * Inspection
  , markersIn
    -- * Marker line helpers (for fresh templates)
  , renderBegin
  , renderEnd
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

-- ---------------------------------------------------------------------------
-- Keys
-- ---------------------------------------------------------------------------

-- | Marker key. Stored as 'Text'; the smart constructor 'markerKey'
-- enforces the printable-no-whitespace shape.
newtype MarkerKey = MarkerKey { unMarkerKey :: Text }
  deriving stock (Eq, Ord, Show)

-- | Smart constructor. Accepts ASCII letters, digits, @-@, @_@, @.@,
-- @:@, @\/@. Rejects whitespace and HTML-comment-terminator runs.
markerKey :: Text -> Either String MarkerKey
markerKey t
  | T.null t                                 = Left "empty key"
  | T.any (== ' ') t || T.any (== '\t') t    = Left "whitespace in key"
  | T.any (== '\n') t                        = Left "newline in key"
  | T.isInfixOf "-->" t                      = Left "key contains '-->'"
  | otherwise                                = Right (MarkerKey t)

-- ---------------------------------------------------------------------------
-- Regions
-- ---------------------------------------------------------------------------

-- | A document is a sequence of regions: literal text the rewriter
-- never touches, and managed regions whose body the rewriter owns.
data Region
  = Literal !Text
    -- ^ Text outside any marker. Includes the @\<!-- BEGIN_AUTOGEN --\>@
    -- and @\<!-- END_AUTOGEN --\>@ lines themselves.
  | Managed !MarkerKey !Text
    -- ^ A managed region. The 'Text' is the current body between
    -- the markers (without the marker lines themselves), with a
    -- single leading and trailing newline preserved.
  deriving stock (Eq, Show)

-- | Parse a document into its regions. Always succeeds; unmatched
-- @BEGIN@ / @END@ markers fall back to literal text so we never
-- silently drop content.
--
-- Output shape: for every well-formed marker pair the region list
-- contains three entries in sequence: a 'Literal' for the BEGIN line,
-- a 'Managed' for the body, a 'Literal' for the END line. The
-- 'Literal' entries before / after / between marker pairs hold all
-- other text verbatim (including their trailing newlines).
parseRegions :: Text -> [Region]
parseRegions input = outside (T.lines input) []
  where
    -- 'outside' state: we are not currently inside a marker pair.
    -- 'lineAcc' is the reverse-order accumulator of literal lines
    -- since the last region boundary.
    outside :: [Text] -> [Text] -> [Region]
    outside [] lineAcc =
      flushLiteral lineAcc []
    outside (l : ls) lineAcc =
      case parseBegin l of
        Just key ->
          flushLiteral lineAcc
            ( Literal (l <> "\n")
            : inside key ls []
            )
        Nothing ->
          outside ls (l : lineAcc)

    -- 'inside' state: we have seen BEGIN_AUTOGEN <key>; collect lines
    -- until the matching END_AUTOGEN <key>.
    inside :: MarkerKey -> [Text] -> [Text] -> [Region]
    inside _key [] bodyAcc =
      -- Unterminated BEGIN: emit the body lines as a plain Literal so
      -- nothing is lost. The BEGIN marker line was already pushed by
      -- 'outside' as Literal.
      flushLiteral bodyAcc []
    inside key (l : ls) bodyAcc =
      case parseEnd l of
        Just k | k == key ->
          Managed key (renderBody (reverse bodyAcc))
            : Literal (l <> "\n")
            : outside ls []
        _ ->
          inside key ls (l : bodyAcc)

    flushLiteral :: [Text] -> [Region] -> [Region]
    flushLiteral []      tl = tl
    flushLiteral lineAcc tl = Literal (T.unlines (reverse lineAcc)) : tl

    renderBody :: [Text] -> Text
    renderBody [] = "\n"
    renderBody xs = "\n" <> T.unlines xs

-- | Render the regions back to a single document. Inverse of
-- 'parseRegions' on well-formed input.
renderRegions :: [Region] -> Text
renderRegions = T.concat . map render
  where
    render (Literal t)     = t
    render (Managed _ body) = body

-- | Rewrite a single marker's body. Returns the input unchanged if
-- the key isn't present.
rewriteMarkers :: Map MarkerKey Replacement -> Text -> Text
rewriteMarkers replacements doc =
  renderRegions (map applyOne (parseRegions doc))
  where
    applyOne r@(Literal _) = r
    applyOne r@(Managed k _) =
      case Map.lookup k replacements of
        Nothing  -> r
        Just rep -> Managed k (normalise rep)

    -- Ensure the rewritten body starts and ends with exactly one
    -- newline so the markers themselves stay on their own lines.
    normalise t =
      let trimmed = T.stripEnd (T.stripStart t)
      in if T.null trimmed
           then "\n"
           else "\n" <> trimmed <> "\n"

-- | The new body to substitute. Leading and trailing whitespace is
-- normalised by 'rewriteMarkers'; you don't need to add the wrapping
-- newlines yourself.
type Replacement = Text

-- | List the marker keys present in a document, in document order.
markersIn :: Text -> [MarkerKey]
markersIn = foldr collect [] . parseRegions
  where
    collect (Managed k _) ks = k : ks
    collect _              ks = ks

-- | Read a file, apply 'rewriteMarkers', and write back if and only
-- if the content changed. Returns 'True' iff the file was modified.
rewriteFile :: FilePath -> Map MarkerKey Replacement -> IO Bool
rewriteFile path replacements = do
  before <- TIO.readFile path
  let after = rewriteMarkers replacements before
  if after == before
    then pure False
    else do
      TIO.writeFile path after
      pure True

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

-- | Parse a @\<!-- BEGIN_AUTOGEN \<key\> --\>@ line. Tolerates
-- leading or trailing whitespace.
parseBegin :: Text -> Maybe MarkerKey
parseBegin = parseDirective "BEGIN_AUTOGEN"

-- | Parse a @\<!-- END_AUTOGEN \<key\> --\>@ line.
parseEnd :: Text -> Maybe MarkerKey
parseEnd = parseDirective "END_AUTOGEN"

parseDirective :: Text -> Text -> Maybe MarkerKey
parseDirective kw line =
  case T.stripPrefix "<!--" (T.stripStart line) of
    Nothing -> Nothing
    Just r1 -> case T.stripPrefix kw (T.stripStart r1) of
      Nothing -> Nothing
      Just r2 -> case T.stripSuffix "-->" (T.stripEnd r2) of
        Nothing -> Nothing
        Just r3 ->
          let key = T.strip r3
          in case markerKey key of
               Right k -> Just k
               Left _  -> Nothing

-- | Render a key as its BEGIN marker line. Exposed for callers that
-- want to inject a complete marker pair into a fresh template
-- programmatically (the in-tree caller is the smoke-test helper for
-- 'rewriteMarkers').
renderBegin :: MarkerKey -> Text
renderBegin (MarkerKey k) = "<!-- BEGIN_AUTOGEN " <> k <> " -->"

-- | The companion to 'renderBegin'.
renderEnd :: MarkerKey -> Text
renderEnd (MarkerKey k) = "<!-- END_AUTOGEN " <> k <> " -->"
