{-# LANGUAGE OverloadedStrings #-}

module Test.Bond.Derive (tests) where

import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Bond.Derive
import qualified Bond.Value as BV

import Test.Bond.Derive.Instances ()
import Test.Bond.Derive.Types

tests :: TestTree
tests = testGroup "Bond.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "field IDs default to positional, tag overrides, skip drops" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case toBond p of
        BV.Struct base kvs -> do
          base @?= V.empty
          assertBool "field id 1 = name"   (V.any (idIs 1) kvs)
          assertBool "field id 2 = age"    (V.any (idIs 2) kvs)
          -- profileScore is explicitly tagged 7; positional id 3 must
          -- NOT appear, but id 7 must.
          assertBool "field id 7 = score (tag override)"
            (V.any (idIs 7) kvs)
          assertBool "no field id 3 (overridden away)"
            (not (V.any (idIs 3) kvs))
          assertBool "field id 4 = active"
            (V.any (idIs 4) kvs)
          -- profileSecret is skipped under Bond — neither its
          -- positional id (5) nor any other id should hold it.
          assertBool "profileSecret skipped — no field id 5"
            (not (V.any (idIs 5) kvs))
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "field values round-trip back into the record" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case fromBond (toBond p) of
        Right p' -> do
          profileName   p' @?= profileName   p
          profileAge    p' @?= profileAge    p
          profileScore  p' @?= profileScore  p
          profileActive p' @?= profileActive p
          profileSecret p' @?= defaultSecret
        Left err -> fail err

  , testCase "field BondTypes match Haskell types" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case toBond p of
        BV.Struct _ kvs -> do
          lookupBT 1 kvs @?= Just BV.BT_STRING
          lookupBT 2 kvs @?= Just BV.BT_INT32
          lookupBT 7 kvs @?= Just BV.BT_DOUBLE
          lookupBT 4 kvs @?= Just BV.BT_BOOL
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "missing required field id is reported" $ do
      let bad = BV.Struct V.empty (V.fromList
            [ (1, BV.BT_STRING, BV.String "Alice")
            , (2, BV.BT_INT32,  BV.Int32 30)
              -- no field id 7 / 4
            ])
      case (fromBond bad :: Either String Profile) of
        Left _  -> pure ()
        Right v -> fail ("unexpected " ++ show v)
  ]
  where
    idIs n (k, _, _) = k == n

    lookupBT n kvs = case V.find (\(k, _, _) -> k == n) kvs of
      Just (_, bt, _) -> Just bt
      Nothing         -> Nothing

-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through to inner Int32" $
      toBond (Tag 42) @?= BV.Int32 42

  , testCase "round-trip" $
      fromBond (toBond (Tag 7)) @?= Right (Tag 7)

  , testCase "bondType is the inner type's tag" $
      bondType (Proxy :: Proxy Tag) @?= BV.BT_INT32
  ]

-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum (Int32-encoded)"
  [ testCase "Red -> 0 (default positional)" $
      toBond Red      @?= BV.Int32 0
  , testCase "Green -> 1" $
      toBond Green    @?= BV.Int32 1
  , testCase "DarkBlue -> 99 (explicit tag)" $
      toBond DarkBlue @?= BV.Int32 99
  , testCase "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown value fails" $
      case (fromBond (BV.Int32 17) :: Either String Color) of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = fromBond (toBond c) @?= Right c

-- ---------------------------------------------------------------------------

sumTests :: TestTree
sumTests = testGroup "sum (Bond single-field-struct union)"
  [ testCase "Origin (nullary) -> field id 1, payload Bool True" $
      toBond Origin @?=
        BV.Struct V.empty
          (V.singleton (1, BV.BT_BOOL, BV.Bool True))

  , testCase "Circle (unary) -> field id 2, payload = inner value" $
      toBond (Circle 1.5) @?=
        BV.Struct V.empty
          (V.singleton (2, BV.BT_DOUBLE, BV.Double 1.5))

  , testCase "Rect (n-ary, tag-overridden) -> field id 10" $
      case toBond (Rect 2 3) of
        BV.Struct _ kvs ->
          assertBool "field id 10 present"
            (V.any (\(k, _, _) -> k == 10) kvs)
        v -> fail ("expected Struct, got " ++ show v)

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown ctor id fails" $ do
      let bad = BV.Struct V.empty
                  (V.singleton (255, BV.BT_BOOL, BV.Bool True))
      case (fromBond bad :: Either String Shape) of
        Left _  -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = fromBond (toBond s) @?= Right s
