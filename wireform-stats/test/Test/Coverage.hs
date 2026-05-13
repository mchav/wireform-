{-# LANGUAGE OverloadedStrings #-}
module Test.Coverage (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import qualified Wireform.Stats.Coverage as Cov

tests :: TestTree
tests = testGroup "Coverage"
  [ testCase "parseHpcReport: top-line numbers" topLine
  , testCase "parseHpcReport: per-module"        perModule
  , testCase "summaryToCoverageLine: shape"      lineShape
  ]

hpcDoc :: Text
hpcDoc = T.unlines
  [ " 92% expressions used (123/134)"
  , " 85% boolean coverage (10/12)"
  , "      ..."
  , " 89% alternatives used (100/112)"
  , " 90% local declarations used (45/50)"
  , " 95% top-level declarations used (40/42)"
  , ""
  , "per-module breakdown"
  , " 92% expressions used in module CBOR.Encode (50/54)"
  , " 89% expressions used in module CBOR.Decode (33/37)"
  , " 95% expressions used in module CBOR.Value (40/42)"
  ]

topLine :: Assertion
topLine = do
  let s = Cov.parseHpcReport hpcDoc
  Cov.covExpressions       s @?= 92
  Cov.covAlternatives      s @?= 89
  Cov.covLocalDeclarations s @?= 90
  Cov.covTopDeclarations   s @?= 95

perModule :: Assertion
perModule = do
  let s = Cov.parseHpcReport hpcDoc
  length (Cov.covModules s) @?= 3
  -- Names extracted correctly?
  let names = map Cov.mcModule (Cov.covModules s)
  names @?= ["CBOR.Encode", "CBOR.Decode", "CBOR.Value"]

lineShape :: Assertion
lineShape = do
  let s = Cov.parseHpcReport hpcDoc
      l = Cov.summaryToCoverageLine s
  assertBool "carries the headline percent" ("92.0%" `T.isInfixOf` l)
