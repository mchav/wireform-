{-# LANGUAGE OverloadedStrings #-}

module Test.YAML.Encode (tests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Test.Syd
import YAML.Decode (decode)
import YAML.Encode (encode)
import YAML.Value


tests :: Spec
tests =
  describe "encode" $
    sequence_
      [ scalarOutput
      , collectionOutput
      , quotingTests
      ]


scalarOutput :: Spec
scalarOutput =
  describe "scalars" $
    sequence_
      [ it "null" $ encode YNull `shouldBe` "null\n"
      , it "true" $ encode (YBool True) `shouldBe` "true\n"
      , it "false" $ encode (YBool False) `shouldBe` "false\n"
      , it "int" $ encode (YInt 42) `shouldBe` "42\n"
      , it "float" $ (T.last (encode (YFloat 3.14)) == '\n') `shouldBe` True
      ]


collectionOutput :: Spec
collectionOutput =
  describe "collections" $
    sequence_
      [ it "empty seq" $ encode (YSeq V.empty) `shouldBe` "[]\n"
      , it "empty map" $ encode (YMap V.empty) `shouldBe` "{}\n"
      , it "block map" $ do
          let v = YMap (V.fromList [(YString "a", YInt 1), (YString "b", YInt 2)])
          encode v `shouldBe` "a: 1\nb: 2\n"
      , it "nested map" $ do
          let v =
                YMap
                  ( V.fromList
                      [
                        ( YString "outer"
                        , YMap (V.fromList [(YString "x", YInt 1)])
                        )
                      ]
                  )
          encode v `shouldBe` "outer:\n  x: 1\n"
      , it "block seq" $ do
          let v = YSeq (V.fromList [YInt 1, YInt 2])
          encode v `shouldBe` "- 1\n- 2\n"
      ]


quotingTests :: Spec
quotingTests =
  describe "quoting" $
    sequence_
      [ it "string that looks like int is quoted" $ do
          let v = YString "42"
          let out = encode v
          case decode out of
            Right (YString "42") -> pure ()
            r -> expectationFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)
      , it "string that looks like bool is quoted" $ do
          let v = YString "true"
          let out = encode v
          case decode out of
            Right (YString "true") -> pure ()
            r -> expectationFailure ("did not roundtrip: " ++ show r ++ " from " ++ T.unpack out)
      , it "string with newline" $ do
          let v = YString "a\nb"
          let out = encode v
          case decode out of
            Right (YString "a\nb") -> pure ()
            r -> expectationFailure (show r)
      ]
