module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Frame
import qualified Test.HPACK
import qualified Test.Connection
import qualified Test.Defensive

main :: IO ()
main = defaultMain $ testGroup "wireform-http2"
  [ Test.Frame.tests
  , Test.HPACK.tests
  , Test.Connection.tests
  , Test.Defensive.tests
  ]
