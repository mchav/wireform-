{-# LANGUAGE OverloadedStrings #-}
module Test.Coverage (tests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import qualified Wireform.Stats.Coverage as Cov

tests :: Spec
tests = describe "Coverage" $ sequence_
  [ it "parseHpcReport: top-line numbers" topLine
  , it "parseHpcReport: per-module"        perModule
  , it "summaryToCoverageLine: shape"      lineShape
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

topLine :: IO ()
topLine = do
  let s = Cov.parseHpcReport hpcDoc
  Cov.covExpressions       s `shouldBe` 92
  Cov.covAlternatives      s `shouldBe` 89
  Cov.covLocalDeclarations s `shouldBe` 90
  Cov.covTopDeclarations   s `shouldBe` 95

perModule :: IO ()
perModule = do
  let s = Cov.parseHpcReport hpcDoc
  length (Cov.covModules s) `shouldBe` 3
  -- Names extracted correctly?
  let names = map Cov.mcModule (Cov.covModules s)
  names `shouldBe` ["CBOR.Encode", "CBOR.Decode", "CBOR.Value"]

lineShape :: IO ()
lineShape = do
  let s = Cov.parseHpcReport hpcDoc
      l = Cov.summaryToCoverageLine s
  ("92.0%" `T.isInfixOf` l) `shouldBe` True
