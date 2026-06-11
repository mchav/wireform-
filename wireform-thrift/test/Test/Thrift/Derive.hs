{-# LANGUAGE OverloadedStrings #-}

module Test.Thrift.Derive (tests) where

import Data.Vector qualified as V
import Test.Syd
import Test.Thrift.Derive.Instances ()
import Test.Thrift.Derive.Types
import Thrift.Class qualified as TC
import Thrift.Value qualified as TV


tests :: Spec
tests =
  describe "Thrift.Derive" $
    sequence_
      [ recordTests
      , newtypeTests
      , enumTests
      , unionTests
      ]


-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "field IDs default to positional, tag overrides, skip drops" $ do
          let e = LogEntry 1700000000 "boot" 42 "abc"
          case TC.toThrift e of
            TV.Struct kvs -> do
              (V.any (idIs 1) kvs) `shouldBe` True
              (V.any (idIs 2) kvs) `shouldBe` True
              (V.any (idIs 7) kvs) `shouldBe` True
              -- Note: positional default for logCode (id 3) is shadowed by
              -- the explicit `tag 7`, so id 3 must NOT be present.
              (not (V.any (idIs 3) kvs)) `shouldBe` True
              (not (V.any (idIs 4) kvs)) `shouldBe` True
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip fills skipped from defaults" $ do
          let e = LogEntry 1700000000 "boot" 42 "abc"
          case TC.fromThrift (TC.toThrift e) of
            Right e' -> do
              logTimestamp e' `shouldBe` logTimestamp e
              logMessage e' `shouldBe` logMessage e
              logCode e' `shouldBe` logCode e
              logRequestId e' `shouldBe` defaultRequestId
            Left err -> expectationFailure err
      ]
  where
    idIs n (k, _) = k == n


-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests =
  describe "newtype" $
    sequence_
      [ it "round-trip" $
          TC.fromThrift (TC.toThrift (RequestId 7)) `shouldBe` Right (RequestId 7)
      ]


-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests =
  describe "enum (I32-encoded)" $
    sequence_
      [ it "Debug -> 0 (default positional)" $
          TC.toThrift Debug `shouldBe` TV.I32 0
      , it "Info -> 1" $
          TC.toThrift Info `shouldBe` TV.I32 1
      , it "Critical -> 99 (explicit tag)" $
          TC.toThrift Critical `shouldBe` TV.I32 99
      , it "round-trip" $ mapM_ rt [Debug, Info, Warn, Critical]
      , it "unknown value fails" $
          case TC.fromThrift (TV.I32 17) :: Either String Severity of
            Left _ -> pure ()
            Right s -> expectationFailure ("unexpected " ++ show s)
      ]
  where
    rt :: Severity -> IO ()
    rt s = TC.fromThrift (TC.toThrift s) `shouldBe` Right s


-- ---------------------------------------------------------------------------

unionTests :: Spec
unionTests =
  describe "sum (Thrift union)" $
    sequence_
      [ it "EvHeartbeat -> field id 1, payload Bool True" $
          TC.toThrift EvHeartbeat
            `shouldBe` TV.Struct (V.singleton (1, TV.Bool True))
      , it "EvData 5 -> field id 2, payload I64 5" $
          TC.toThrift (EvData 5)
            `shouldBe` TV.Struct (V.singleton (2, TV.I64 5))
      , it "EvAlert -> tag-overridden field id 10" $
          case TC.toThrift (EvAlert "fire" 7) of
            TV.Struct kvs ->
              (V.any (\(k, _) -> k == 10) kvs) `shouldBe` True
            v -> expectationFailure ("expected Struct, got " ++ show v)
      , it "round-trip EvHeartbeat" $ rt EvHeartbeat
      , it "round-trip EvData" $ rt (EvData 99)
      , it "round-trip EvAlert" $ rt (EvAlert "x" 1)
      , it "unknown field id fails" $ do
          let bad = TV.Struct (V.singleton (255, TV.Bool True))
          case TC.fromThrift bad :: Either String Event of
            Left _ -> pure ()
            Right e -> expectationFailure ("unexpected " ++ show e)
      ]
  where
    rt :: Event -> IO ()
    rt e = TC.fromThrift (TC.toThrift e) `shouldBe` Right e
