{-# LANGUAGE OverloadedStrings #-}
module Test.YAML.Decode (tests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=), assertFailure)

import YAML.Decode
import YAML.Value

tests :: TestTree
tests = testGroup "decode"
  [ scalarTests
  , collectionTests
  , flowTests
  , stringTests
  , blockScalarTests
  , anchorTests
  , streamTests
  ]

scalarTests :: TestTree
scalarTests = testGroup "scalars"
  [ testCase "null"  $ decode "null"  @?= Right YNull
  , testCase "tilde" $ decode "~"     @?= Right YNull
  , testCase "true"  $ decode "true"  @?= Right (YBool True)
  , testCase "false" $ decode "false" @?= Right (YBool False)
  , testCase "int"   $ decode "42"    @?= Right (YInt 42)
  , testCase "neg"   $ decode "-17"   @?= Right (YInt (-17))
  , testCase "hex"   $ decode "0x2A"  @?= Right (YInt 42)
  , testCase "oct"   $ decode "0o52"  @?= Right (YInt 42)
  , testCase "float" $ case decode "3.14" of
      Right (YFloat d) -> assertBool "near pi" (abs (d - 3.14) < 1e-9)
      r                -> assertFailure (show r)
  , testCase ".inf"  $ case decode ".inf" of
      Right (YFloat d) -> assertBool "+inf" (isInfinite d && d > 0)
      r                -> assertFailure (show r)
  , testCase "-.inf" $ case decode "-.inf" of
      Right (YFloat d) -> assertBool "-inf" (isInfinite d && d < 0)
      r                -> assertFailure (show r)
  , testCase ".nan"  $ case decode ".nan" of
      Right (YFloat d) -> assertBool "nan" (isNaN d)
      r                -> assertFailure (show r)
  , testCase "unquoted string" $ decode "hello" @?= Right (YString "hello")
  ]

collectionTests :: TestTree
collectionTests = testGroup "block style"
  [ testCase "block mapping" $ do
      let src = T.unlines [ "name: alice"
                          , "age: 30"
                          ]
      case decode src of
        Right v -> do
          lookupKey "name" v @?= Just (YString "alice")
          lookupKey "age"  v @?= Just (YInt 30)
        Left e -> assertFailure e

  , testCase "block sequence" $ do
      let src = T.unlines [ "- 1"
                          , "- 2"
                          , "- 3"
                          ]
      case decode src of
        Right (YSeq xs) -> V.toList xs @?= [YInt 1, YInt 2, YInt 3]
        r -> assertFailure (show r)

  , testCase "nested mapping" $ do
      let src = T.unlines
            [ "server:"
            , "  host: localhost"
            , "  port: 8080"
            ]
      case decode src of
        Right v ->
          case lookupKey "server" v of
            Just inner -> do
              lookupKey "host" inner @?= Just (YString "localhost")
              lookupKey "port" inner @?= Just (YInt 8080)
            _ -> assertFailure "no server"
        Left e -> assertFailure e

  , testCase "sequence of mappings" $ do
      let src = T.unlines
            [ "- name: a"
            , "  v: 1"
            , "- name: b"
            , "  v: 2"
            ]
      case decode src of
        Right (YSeq xs) -> do
          V.length xs @?= 2
          let m0 = xs V.! 0
          lookupKey "name" m0 @?= Just (YString "a")
          lookupKey "v"    m0 @?= Just (YInt 1)
        r -> assertFailure (show r)

  , testCase "comment + blank lines" $ do
      let src = T.unlines
            [ "# header"
            , ""
            , "foo: bar  # trailing"
            ]
      case decode src of
        Right v -> lookupKey "foo" v @?= Just (YString "bar")
        Left e  -> assertFailure e
  ]

flowTests :: TestTree
flowTests = testGroup "flow style"
  [ testCase "flow seq"  $ decode "[1, 2, 3]"
      @?= Right (YSeq (V.fromList [YInt 1, YInt 2, YInt 3]))
  , testCase "flow map"  $ case decode "{a: 1, b: 2}" of
      Right (YMap kvs) -> do
        V.length kvs @?= 2
      r -> assertFailure (show r)
  , testCase "nested flow" $
      case decode "[[1, 2], [3, 4]]" of
        Right (YSeq xs) -> V.length xs @?= 2
        r -> assertFailure (show r)
  ]

stringTests :: TestTree
stringTests = testGroup "strings"
  [ testCase "double quoted" $
      decode "\"hello world\"" @?= Right (YString "hello world")
  , testCase "single quoted" $
      decode "'it''s ok'" @?= Right (YString "it's ok")
  , testCase "escape \\n" $
      decode "\"a\\nb\"" @?= Right (YString "a\nb")
  , testCase "escape \\u" $
      decode "\"\\u00E9\"" @?= Right (YString "\233")
  , testCase "quoted in mapping" $ do
      case decode "msg: \"hello\\tworld\"" of
        Right v -> lookupKey "msg" v @?= Just (YString "hello\tworld")
        Left e  -> assertFailure e
  ]

blockScalarTests :: TestTree
blockScalarTests = testGroup "block scalars"
  [ testCase "literal" $ do
      let src = T.unlines
            [ "txt: |"
            , "  one"
            , "  two"
            ]
      case decode src of
        Right v -> lookupKey "txt" v @?= Just (YString "one\ntwo\n")
        Left e  -> assertFailure e

  , testCase "literal strip" $ do
      let src = T.unlines
            [ "txt: |-"
            , "  one"
            , "  two"
            ]
      case decode src of
        Right v -> lookupKey "txt" v @?= Just (YString "one\ntwo")
        Left e  -> assertFailure e

  , testCase "folded" $ do
      let src = T.unlines
            [ "txt: >"
            , "  one"
            , "  two"
            ]
      case decode src of
        Right v -> lookupKey "txt" v @?= Just (YString "one two\n")
        Left e  -> assertFailure e
  ]

anchorTests :: TestTree
anchorTests = testGroup "anchors"
  [ testCase "anchor + alias scalar" $ do
      let src = T.unlines
            [ "a: &x 1"
            , "b: *x"
            ]
      case decode src of
        Right v -> do
          lookupKey "a" v @?= Just (YInt 1)
          lookupKey "b" v @?= Just (YInt 1)
        Left e -> assertFailure e
  ]

streamTests :: TestTree
streamTests = testGroup "stream"
  [ testCase "single doc, no markers" $
      case decodeStream "k: 1" of
        Right (Stream xs) -> V.length xs @?= 1
        Left e -> assertFailure e
  , testCase "two docs" $ do
      let src = T.unlines
            [ "---"
            , "a: 1"
            , "..."
            , "---"
            , "b: 2"
            ]
      case decodeStream src of
        Right (Stream xs) -> V.length xs @?= 2
        Left e -> assertFailure e
  ]
