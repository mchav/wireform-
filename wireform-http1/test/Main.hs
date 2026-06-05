module Main (main) where

import Test.Syd

import qualified Test.Chunked
import qualified Test.Encode
import qualified Test.Integration
import qualified Test.Parser
import qualified Test.RoundTrip
import qualified Test.ServerEdgeCases

main :: IO ()
main = sydTest $ describe "wireform-http1" $ sequence_
  [ Test.Parser.tests
  , Test.Encode.tests
  , Test.Chunked.tests
  , Test.RoundTrip.tests
  , Test.Integration.tests
  , Test.ServerEdgeCases.tests
  ]
