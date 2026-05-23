module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Test.Frame
import qualified Test.FrameEdgeCases
import qualified Test.FrameStream
import qualified Test.HPACK
import qualified Test.HPACKEdgeCases
import qualified Test.HPACKConcurrency
import qualified Test.Connection
import qualified Test.Defensive

main :: IO ()
main = defaultMain $ testGroup "wireform-http2"
  [ Test.Frame.tests
  , Test.FrameEdgeCases.tests
  , Test.FrameStream.tests
  , Test.HPACK.tests
  , Test.HPACKEdgeCases.tests
  , Test.HPACKConcurrency.tests
  , Test.Connection.tests
  , Test.Defensive.tests
  ]
