module Main (main) where

import Test.Fory.Derive qualified
import Test.Fory.Direct qualified
import Test.Fory.Encoding qualified
import Test.Fory.MetaStringInterop qualified
import Test.Fory.SpecExtensions qualified
import Test.Fory.Value qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-fory" $
      sequence_
        [ Test.Fory.Encoding.tests
        , Test.Fory.Value.tests
        , Test.Fory.SpecExtensions.tests
        , Test.Fory.MetaStringInterop.tests
        , Test.Fory.Derive.tests
        , Test.Fory.Direct.tests
        ]
