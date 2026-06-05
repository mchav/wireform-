module Main (main) where

import Test.Syd
import qualified Test.Fory.Direct
import qualified Test.Fory.Encoding
import qualified Test.Fory.Value
import qualified Test.Fory.Derive
import qualified Test.Fory.MetaStringInterop
import qualified Test.Fory.SpecExtensions

main :: IO ()
main = sydTest $ describe "wireform-fory" $ sequence_
  [ Test.Fory.Encoding.tests
  , Test.Fory.Value.tests
  , Test.Fory.SpecExtensions.tests
  , Test.Fory.MetaStringInterop.tests
  , Test.Fory.Derive.tests
  , Test.Fory.Direct.tests
  ]
