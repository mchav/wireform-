{-# LANGUAGE OverloadedStrings #-}

module Test.Thrift.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified Thrift.Class as TC
import qualified Thrift.Value as TV

import Test.Thrift.Derive.Instances ()
import Test.Thrift.Derive.Types

tests :: TestTree
tests = testGroup "Thrift.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , unionTests
  ]

-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "field IDs default to positional, tag overrides, skip drops" $ do
      let e = LogEntry 1700000000 "boot" 42 "abc"
      case TC.toThrift e of
        TV.Struct kvs -> do
          assertBool "field id 1 = timestamp"
            (V.any (idIs 1) kvs)
          assertBool "field id 2 = message"
            (V.any (idIs 2) kvs)
          assertBool "field id 7 = code (tag override)"
            (V.any (idIs 7) kvs)
          -- Note: positional default for logCode (id 3) is shadowed by
          -- the explicit `tag 7`, so id 3 must NOT be present.
          assertBool "no field id 3 (overridden away)"
            (not (V.any (idIs 3) kvs))
          assertBool "logRequestId skipped — no field id 4"
            (not (V.any (idIs 4) kvs))
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let e = LogEntry 1700000000 "boot" 42 "abc"
      case TC.fromThrift (TC.toThrift e) of
        Right e' -> do
          logTimestamp e' @?= logTimestamp e
          logMessage   e' @?= logMessage e
          logCode      e' @?= logCode e
          logRequestId e' @?= defaultRequestId
        Left err -> fail err
  ]
  where
    idIs n (k, _) = k == n

-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "round-trip" $
      TC.fromThrift (TC.toThrift (RequestId 7)) @?= Right (RequestId 7)
  ]

-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum (I32-encoded)"
  [ testCase "Debug -> 0 (default positional)" $
      TC.toThrift Debug @?= TV.I32 0
  , testCase "Info -> 1" $
      TC.toThrift Info @?= TV.I32 1
  , testCase "Critical -> 99 (explicit tag)" $
      TC.toThrift Critical @?= TV.I32 99
  , testCase "round-trip" $ mapM_ rt [Debug, Info, Warn, Critical]
  , testCase "unknown value fails" $
      case TC.fromThrift (TV.I32 17) :: Either String Severity of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Severity -> IO ()
    rt s = TC.fromThrift (TC.toThrift s) @?= Right s

-- ---------------------------------------------------------------------------

unionTests :: TestTree
unionTests = testGroup "sum (Thrift union)"
  [ testCase "EvHeartbeat -> field id 1, payload Bool True" $
      TC.toThrift EvHeartbeat @?=
        TV.Struct (V.singleton (1, TV.Bool True))

  , testCase "EvData 5 -> field id 2, payload I64 5" $
      TC.toThrift (EvData 5) @?=
        TV.Struct (V.singleton (2, TV.I64 5))

  , testCase "EvAlert -> tag-overridden field id 10" $
      case TC.toThrift (EvAlert "fire" 7) of
        TV.Struct kvs ->
          assertBool "field id 10 present"
            (V.any (\(k, _) -> k == 10) kvs)
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip EvHeartbeat" $ rt EvHeartbeat
  , testCase "round-trip EvData"      $ rt (EvData 99)
  , testCase "round-trip EvAlert"     $ rt (EvAlert "x" 1)

  , testCase "unknown field id fails" $ do
      let bad = TV.Struct (V.singleton (255, TV.Bool True))
      case TC.fromThrift bad :: Either String Event of
        Left _ -> pure ()
        Right e -> fail ("unexpected " ++ show e)
  ]
  where
    rt :: Event -> IO ()
    rt e = TC.fromThrift (TC.toThrift e) @?= Right e
