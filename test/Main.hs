module Main (main) where

import Test.Tasty

import Test.Parser (parserTests)
import Test.Wire (wireTests)
import Test.Roundtrip (roundtripTests)
import Test.CodeGen (codeGenTests)

main :: IO ()
main = defaultMain $ testGroup "hs-proto"
  [ parserTests
  , wireTests
  , roundtripTests
  , codeGenTests
  ]
