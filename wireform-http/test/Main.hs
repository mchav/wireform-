module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Http1Integration
import qualified Test.Http1EdgeCases
import qualified Test.Http2Integration
import qualified Test.Http2EdgeCases
import qualified Test.ConcurrencyStress
import qualified Test.AuthChallenge
import qualified Test.Conditional
import qualified Test.IDN
import qualified Test.Negotiation
import qualified Test.SSE
import qualified Test.SSEIntegration
import qualified Test.UrlDecode
import qualified Test.VersionTypes
import qualified Test.Client

main :: IO ()
main = defaultMain $ testGroup "wireform-http"
  [ Test.VersionTypes.tests
  , Test.Negotiation.tests
  , Test.Http1Integration.tests
  , Test.Http1EdgeCases.tests
  , Test.Http2Integration.tests
  , Test.Http2EdgeCases.tests
  , Test.ConcurrencyStress.tests
  , Test.Client.tests
  , Test.SSE.tests
  , Test.SSEIntegration.tests
  , Test.UrlDecode.tests
  , Test.IDN.tests
  , Test.AuthChallenge.tests
  , Test.Conditional.tests
  ]
