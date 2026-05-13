module Main (main) where

import Test.Tasty

import Test.Parser (parserTests)
import Test.Wire (wireTests)
import Test.Roundtrip (roundtripTests)
import Test.CodeGen (codeGenTests)
import Test.WellKnown (wellKnownTests)
import Test.WellKnownUtil (wellKnownUtilTests)
import Test.PrintInspect (printInspectTests)
import Test.Compat (compatTests)
import Test.Schema (schemaTests)
import Test.Options (optionsTests)
import Test.Lens (lensTests)
import Test.StreamCodec (streamCodecTests)
import Test.JSON (jsonTests)
import Test.Hooks (hooksTests)
import Test.TDP (tdpTests)
import Test.GRPC (grpcTests)
import Test.Resolver (resolverTests)

main :: IO ()
main = defaultMain $ testGroup "wireform-proto"
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
  , tdpTests
  , grpcTests
  , resolverTests
  ]
