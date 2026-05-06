{-# LANGUAGE OverloadedStrings #-}
module Test.YAML.Encode (tests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=), assertFailure)

import YAML.Decode (decode)
import YAML.Encode (encode)
import YAML.Value

tests :: TestTree
tests = testGroup "encode"
  [ scalarOutput
  , collectionOutput
  , quotingTests
  ]

scalarOutput :: TestTree
scalarOutput = testGroup "scalars"
  [ testCase "null"  $ encode YNull         @?= "null\n"
  , testCase "true"  $ encode (YBool True)  @?= "true\n"
  , testCase "false" $ encode (YBool False) @?= "false\n"
  , testCase "int"   $ encode (YInt 42)     @?= "42\n"
  , testCase "float" $ assertBool "ends in newline" (T.last (encode (YFloat 3.14)) == '\n')
  ]

collectionOutput :: TestTree
collectionOutput = testGroup "collections"
  [ testCase "empty seq"   $ encode (YSeq V.empty) @?= "[]\n"
  , testCase "empty map"   $ encode (YMap V.empty) @?= "{}\n"
  , testCase "block map"   $ do
      let v = YMap (V.fromList [(YString "a", YInt 1), (YString "b", YInt 2)])
      encode v @?= "a: 1\nb: 2\n"
  , testCase "nested map"  $ do
      let v = YMap (V.fromList
                [(YString "outer",
                   YMap (V.fromList [(YString "x", YInt 1)]))])
      encode v @?= "outer:\n  x: 1\n"
  , testCase "block seq"   $ do
      let v = YSeq (V.fromList [YInt 1, YInt 2])
      encode v @?= "- 1\n- 2\n"
  ]

quotingTests :: TestTree
quotingTests = testGroup "quoting"
  [ testCase "string that looks like int is quoted" $ do
      let v = YString "42"
      let out = encode v
      case decode out of
        Right (YString "42") -> pure ()
        r -> assertFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)

  , testCase "string that looks like bool is quoted" $ do
      let v = YString "true"
      let out = encode v
      case decode out of
        Right (YString "true") -> pure ()
        r -> assertFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)

  , testCase "string with newline" $ do
      let v = YString "a\nb"
      let out = encode v
      case decode out of
        Right (YString "a\nb") -> pure ()
        r -> assertFailure (show r)
  ]
