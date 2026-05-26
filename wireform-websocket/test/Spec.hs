module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Frame
import qualified Test.Handshake
import qualified Test.URI
import qualified Test.Echo
import qualified Test.PerMessageDeflate

main :: IO ()
main = defaultMain $ testGroup "wireform-websocket"
  [ Test.Frame.tests
  , Test.Handshake.tests
  , Test.URI.tests
  , Test.PerMessageDeflate.tests
  , Test.Echo.tests
  ]
