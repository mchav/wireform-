{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Decode (tests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import YAML.Decode
import YAML.Value


tests :: Spec
tests =
  describe "decode" $
    sequence_
      [ scalarTests
      , collectionTests
      , flowTests
      , stringTests
      , blockScalarTests
      , anchorTests
      , streamTests
      ]


scalarTests :: Spec
scalarTests =
  describe "scalars" $
    sequence_
      [ it "null" $ decode "null" `shouldBe` Right YNull
      , it "tilde" $ decode "~" `shouldBe` Right YNull
      , it "true" $ decode "true" `shouldBe` Right (YBool True)
      , it "false" $ decode "false" `shouldBe` Right (YBool False)
      , it "int" $ decode "42" `shouldBe` Right (YInt 42)
      , it "neg" $ decode "-17" `shouldBe` Right (YInt (-17))
      , it "hex" $ decode "0x2A" `shouldBe` Right (YInt 42)
      , it "oct" $ decode "0o52" `shouldBe` Right (YInt 42)
      , it "float" $ case decode "3.14" of
          Right (YFloat d) -> (abs (d - 3.14) < 1e-9) `shouldBe` True
          r -> expectationFailure (show r)
      , it ".inf" $ case decode ".inf" of
          Right (YFloat d) -> (isInfinite d && d > 0) `shouldBe` True
          r -> expectationFailure (show r)
      , it "-.inf" $ case decode "-.inf" of
          Right (YFloat d) -> (isInfinite d && d < 0) `shouldBe` True
          r -> expectationFailure (show r)
      , it ".nan" $ case decode ".nan" of
          Right (YFloat d) -> (isNaN d) `shouldBe` True
          r -> expectationFailure (show r)
      , it "unquoted string" $ decode "hello" `shouldBe` Right (YString "hello")
      ]


collectionTests :: Spec
collectionTests =
  describe "block style" $
    sequence_
      [ it "block mapping" $ do
          let src =
                T.unlines
                  [ "name: alice"
                  , "age: 30"
                  ]
          case decode src of
            Right v -> do
              lookupKey "name" v `shouldBe` Just (YString "alice")
              lookupKey "age" v `shouldBe` Just (YInt 30)
            Left e -> expectationFailure e
      , it "block sequence" $ do
          let src =
                T.unlines
                  [ "- 1"
                  , "- 2"
                  , "- 3"
                  ]
          case decode src of
            Right (YSeq xs) -> V.toList xs `shouldBe` [YInt 1, YInt 2, YInt 3]
            r -> expectationFailure (show r)
      , it "nested mapping" $ do
          let src =
                T.unlines
                  [ "server:"
                  , "  host: localhost"
                  , "  port: 8080"
                  ]
          case decode src of
            Right v ->
              case lookupKey "server" v of
                Just inner -> do
                  lookupKey "host" inner `shouldBe` Just (YString "localhost")
                  lookupKey "port" inner `shouldBe` Just (YInt 8080)
                _ -> expectationFailure "no server"
            Left e -> expectationFailure e
      , it "sequence of mappings" $ do
          let src =
                T.unlines
                  [ "- name: a"
                  , "  v: 1"
                  , "- name: b"
                  , "  v: 2"
                  ]
          case decode src of
            Right (YSeq xs) -> do
              V.length xs `shouldBe` 2
              let m0 = xs V.! 0
              lookupKey "name" m0 `shouldBe` Just (YString "a")
              lookupKey "v" m0 `shouldBe` Just (YInt 1)
            r -> expectationFailure (show r)
      , it "comment + blank lines" $ do
          let src =
                T.unlines
                  [ "# header"
                  , ""
                  , "foo: bar  # trailing"
                  ]
          case decode src of
            Right v -> lookupKey "foo" v `shouldBe` Just (YString "bar")
            Left e -> expectationFailure e
      ]


flowTests :: Spec
flowTests =
  describe "flow style" $
    sequence_
      [ it "flow seq" $
          decode "[1, 2, 3]"
            `shouldBe` Right (YSeq (V.fromList [YInt 1, YInt 2, YInt 3]))
      , it "flow map" $ case decode "{a: 1, b: 2}" of
          Right (YMap kvs) -> do
            V.length kvs `shouldBe` 2
          r -> expectationFailure (show r)
      , it "nested flow" $
          case decode "[[1, 2], [3, 4]]" of
            Right (YSeq xs) -> V.length xs `shouldBe` 2
            r -> expectationFailure (show r)
      ]


stringTests :: Spec
stringTests =
  describe "strings" $
    sequence_
      [ it "double quoted" $
          decode "\"hello world\"" `shouldBe` Right (YString "hello world")
      , it "single quoted" $
          decode "'it''s ok'" `shouldBe` Right (YString "it's ok")
      , it "escape \\n" $
          decode "\"a\\nb\"" `shouldBe` Right (YString "a\nb")
      , it "escape \\u" $
          decode "\"\\u00E9\"" `shouldBe` Right (YString "\233")
      , it "quoted in mapping" $ do
          case decode "msg: \"hello\\tworld\"" of
            Right v -> lookupKey "msg" v `shouldBe` Just (YString "hello\tworld")
            Left e -> expectationFailure e
      ]


blockScalarTests :: Spec
blockScalarTests =
  describe "block scalars" $
    sequence_
      [ it "literal" $ do
          let src =
                T.unlines
                  [ "txt: |"
                  , "  one"
                  , "  two"
                  ]
          case decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one\ntwo\n")
            Left e -> expectationFailure e
      , it "literal strip" $ do
          let src =
                T.unlines
                  [ "txt: |-"
                  , "  one"
                  , "  two"
                  ]
          case decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one\ntwo")
            Left e -> expectationFailure e
      , it "folded" $ do
          let src =
                T.unlines
                  [ "txt: >"
                  , "  one"
                  , "  two"
                  ]
          case decode src of
            Right v -> lookupKey "txt" v `shouldBe` Just (YString "one two\n")
            Left e -> expectationFailure e
      ]


anchorTests :: Spec
anchorTests =
  describe "anchors" $
    sequence_
      [ it "anchor + alias scalar" $ do
          let src =
                T.unlines
                  [ "a: &x 1"
                  , "b: *x"
                  ]
          case decode src of
            Right v -> do
              -- Aliases come back wrapped in 'YAnchored' so the
              -- size-of walk can identify shared subtrees by name;
              -- 'unwrap' strips the wrapper for value comparison.
              fmap unwrap (lookupKey "a" v) `shouldBe` Just (YInt 1)
              fmap unwrap (lookupKey "b" v) `shouldBe` Just (YInt 1)
            Left e -> expectationFailure e
      ]


streamTests :: Spec
streamTests =
  describe "stream" $
    sequence_
      [ it "single doc, no markers" $
          case decodeStream "k: 1" of
            Right (Stream xs) -> V.length xs `shouldBe` 1
            Left e -> expectationFailure e
      , it "two docs" $ do
          let src =
                T.unlines
                  [ "---"
                  , "a: 1"
                  , "..."
                  , "---"
                  , "b: 2"
                  ]
          case decodeStream src of
            Right (Stream xs) -> V.length xs `shouldBe` 2
            Left e -> expectationFailure e
      ]
