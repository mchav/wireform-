module Main (main) where

import Test.CodeGen (codeGenTests)
import Test.Collect (collectTests)
import Test.Compat (compatTests)
import Test.Hooks (hooksTests)
import Test.JSON (jsonTests)
import Test.Lens (lensTests)
import Test.Options (optionsTests)
import Test.Parser (parserTests)
import Test.PrintInspect (printInspectTests)
import Test.Resolver (resolverTests)
import Test.Roundtrip (roundtripTests)
import Test.Schema (schemaTests)
import Test.StreamCodec (streamCodecTests)
import Test.Syd
import Test.TDP (dynamicSchemaTests)
import Test.WellKnown (wellKnownTests)
import Test.WellKnownUtil (wellKnownUtilTests)
import Test.Wire (wireTests)


main :: IO ()
main =
  sydTest
    $ describe
      "wireform-proto"
    $ sequence_
      [ parserTests
      , wireTests
      , roundtripTests
      , codeGenTests
      , wellKnownTests
      , wellKnownUtilTests
      , printInspectTests
      , compatTests
      , schemaTests
      , optionsTests
      , lensTests
      , streamCodecTests
      , jsonTests
      , hooksTests
      , dynamicSchemaTests
      , resolverTests
      , collectTests
      ]
