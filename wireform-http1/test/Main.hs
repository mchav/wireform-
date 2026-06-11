module Main (main) where

import Test.Chunked qualified
import Test.Encode qualified
import Test.Integration qualified
import Test.Parser qualified
import Test.RoundTrip qualified
import Test.ServerEdgeCases qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-http1" $
      sequence_
        [ Test.Parser.tests
        , Test.Encode.tests
        , Test.Chunked.tests
        , Test.RoundTrip.tests
        , Test.Integration.tests
        , Test.ServerEdgeCases.tests
        ]
