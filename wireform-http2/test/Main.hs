module Main (main) where

import Test.Tasty

import qualified Test.HPACK
import qualified Test.Frame
import qualified Test.Connection

main :: IO ()
main = defaultMain $ testGroup "wireform-http2"
  [ Test.HPACK.tests
  , Test.Frame.tests
  , Test.Connection.tests
  ]
