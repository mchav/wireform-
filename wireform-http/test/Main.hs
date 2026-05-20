module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Http1Integration
import qualified Test.Http2Integration
import qualified Test.Negotiation
import qualified Test.VersionTypes

main :: IO ()
main = defaultMain $ testGroup "wireform-http"
  [ Test.VersionTypes.tests
  , Test.Negotiation.tests
  , Test.Http1Integration.tests
  , Test.Http2Integration.tests
  ]
