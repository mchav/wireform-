-- | HPC coverage data layer.
--
-- Parses the textual @hpc report --per-module@ output. Format:
--
-- @
-- 100% expressions used (...)
--  85% boolean coverage (...)
--      ...
--  92% alternatives used (...)
--  90% local declarations used (...)
--  95% top-level declarations used (...)
-- ...
-- per-module breakdown
--  92% expressions used in module CBOR.Encode
--  85% expressions used in module CBOR.Decode
-- ...
-- @
--
-- We surface the totals (top of file) and the per-module
-- expressions-used percentages (bottom of file) since those are the
-- numbers worth committing to a README. Branch / alternative coverage
-- can be added later behind another marker key without changing the
-- interface.
module Wireform.Stats.Coverage
  ( -- * Summary
    CoverageSummary (..)
  , ModuleCoverage (..)
  , parseHpcReport
  , readHpcReport
    -- * Render
  , summaryToCoverageLine
  , summaryToCoverageTable
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Read qualified as TR

import Wireform.Stats.Table qualified as Tbl

-- ---------------------------------------------------------------------------
-- Summary
-- ---------------------------------------------------------------------------

-- | Top-level coverage numbers for one cabal package + the per-
-- module expressions-used breakdown.
data CoverageSummary = CoverageSummary
  { covExpressions       :: !Double
    -- ^ Top-line expressions-used percent (0..100).
  , covBoolean           :: !Double
  , covAlternatives      :: !Double
  , covLocalDeclarations :: !Double
  , covTopDeclarations   :: !Double
  , covModules           :: ![ModuleCoverage]
  } deriving stock (Eq, Show)

-- | Per-module expressions-used percent.
data ModuleCoverage = ModuleCoverage
  { mcModule      :: !Text
  , mcExpressions :: !Double
  } deriving stock (Eq, Show)

-- | Parse the textual @hpc report@ output (with @--per-module@).
parseHpcReport :: Text -> CoverageSummary
parseHpcReport input =
  let ls = T.lines input
      pct kw = findFirstPercent kw ls
  in CoverageSummary
       { covExpressions       = pct "expressions used"
       , covBoolean           = pct "boolean coverage"
       , covAlternatives      = pct "alternatives used"
       , covLocalDeclarations = pct "local declarations used"
       , covTopDeclarations   = pct "top-level declarations used"
       , covModules           = perModule ls
       }

readHpcReport :: FilePath -> IO CoverageSummary
readHpcReport p = parseHpcReport <$> TIO.readFile p

-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

summaryToCoverageLine :: CoverageSummary -> Text
summaryToCoverageLine s =
  let pct n = T.pack (showFixed1 n) <> "%"
  in pct (covExpressions s) <> " expressions, "
     <> pct (covAlternatives s) <> " alternatives, "
     <> pct (covTopDeclarations s) <> " top-level declarations"

-- | Per-module expressions table.
summaryToCoverageTable :: CoverageSummary -> Tbl.Table
summaryToCoverageTable s = Tbl.Table
  { Tbl.tableHeader = ["Module", "Expressions"]
  , Tbl.tableAlign  = [Tbl.AlignLeft, Tbl.AlignRight]
  , Tbl.tableRows   = [[Tbl.renderInlineCode (mcModule m),
                        T.pack (showFixed1 (mcExpressions m)) <> "%"]
                       | m <- covModules s]
  }

-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

-- The first line that ends with the keyword and starts with a numeric
-- percent. Returns 0 if not found.
findFirstPercent :: Text -> [Text] -> Double
findFirstPercent _ []        = 0
findFirstPercent kw (l : ls) =
  case parsePercentLine l of
    Just (pct, kw') | kw `T.isInfixOf` kw' -> pct
    _ -> findFirstPercent kw ls

-- Parse a line like:
--   " 92% expressions used (123/134)"
-- Returns (92.0, "expressions used (123/134)") on success.
parsePercentLine :: Text -> Maybe (Double, Text)
parsePercentLine raw =
  let l = T.stripStart raw
  in case TR.double l of
       Right (n, rest) ->
         case T.uncons (T.stripStart rest) of
           Just ('%', rest2) -> Just (n, T.strip rest2)
           _                 -> Nothing
       Left _ -> Nothing

-- Extract per-module entries from the lines following the
-- "per-module breakdown" header (or the equivalent textual marker
-- that hpc emits at the end of its output).
perModule :: [Text] -> [ModuleCoverage]
perModule ls =
  let body = dropUntilModulesSection ls
  in [ ModuleCoverage modName pct
     | line <- body
     , Just (pct, kw) <- pure (parsePercentLine line)
     , Just modName   <- pure (extractModuleName kw)
     ]

dropUntilModulesSection :: [Text] -> [Text]
dropUntilModulesSection [] = []
dropUntilModulesSection (l : ls)
  | "per-module" `T.isInfixOf` T.toLower l = ls
  | otherwise                              = dropUntilModulesSection ls

-- Extract the module name out of a per-module line's keyword:
--   "expressions used in module CBOR.Encode (...)"
extractModuleName :: Text -> Maybe Text
extractModuleName s =
  case T.breakOn " in module " s of
    (_, after) | not (T.null after) ->
      let rest = T.drop (T.length " in module ") after
          name = T.takeWhile (\c -> c /= ' ' && c /= '(' && c /= '\t') rest
      in if T.null name then Nothing else Just name
    _ -> Nothing

showFixed1 :: Double -> String
showFixed1 x =
  let n = round (x * 10) :: Int
  in show (n `div` 10) <> "." <> show (n `mod` 10)
