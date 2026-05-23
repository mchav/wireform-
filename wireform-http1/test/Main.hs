module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Chunked
import qualified Test.Encode
import qualified Test.Integration
import qualified Test.Parser
import qualified Test.RoundTrip
import qualified Test.ServerEdgeCases
import qualified Test.StreamingParser

main :: IO ()
main = defaultMain $ testGroup "wireform-http1"
  [ Test.Parser.tests
  , Test.StreamingParser.tests
  , Test.Encode.tests
  , Test.Chunked.tests
  , Test.RoundTrip.tests
  , Test.Integration.tests
  , Test.ServerEdgeCases.tests
  ]
