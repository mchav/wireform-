module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Frame
import qualified Test.Handshake
import qualified Test.Echo

main :: IO ()
main = defaultMain $ testGroup "wireform-websocket"
  [ Test.Frame.tests
  , Test.Handshake.tests
  , Test.Echo.tests
  ]
