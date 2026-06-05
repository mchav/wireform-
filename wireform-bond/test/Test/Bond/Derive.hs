{-# LANGUAGE OverloadedStrings #-}

module Test.Bond.Derive (tests) where

import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Test.Syd

import Bond.Derive
import qualified Bond.Value as BV

import Test.Bond.Derive.Instances ()
import Test.Bond.Derive.Types

tests :: Spec
tests = describe "Bond.Derive" $ sequence_
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "field IDs default to positional, tag overrides, skip drops" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case toBond p of
        BV.Struct base kvs -> do
          base `shouldBe` V.empty
          (V.any (idIs 1) kvs) `shouldBe` True
          (V.any (idIs 2) kvs) `shouldBe` True
          -- profileScore is explicitly tagged 7; positional id 3 must
          -- NOT appear, but id 7 must.
          (V.any (idIs 7) kvs) `shouldBe` True
          (not (V.any (idIs 3) kvs)) `shouldBe` True
          (V.any (idIs 4) kvs) `shouldBe` True
          -- profileSecret is skipped under Bond — neither its
          -- positional id (5) nor any other id should hold it.
          (not (V.any (idIs 5) kvs)) `shouldBe` True
        v -> expectationFailure ("expected Struct, got " ++ show v)

  , it "field values round-trip back into the record" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case fromBond (toBond p) of
        Right p' -> do
          profileName   p' `shouldBe` profileName   p
          profileAge    p' `shouldBe` profileAge    p
          profileScore  p' `shouldBe` profileScore  p
          profileActive p' `shouldBe` profileActive p
          profileSecret p' `shouldBe` defaultSecret
        Left err -> expectationFailure err

  , it "field BondTypes match Haskell types" $ do
      let p = Profile "Alice" 30 99.5 True "shh"
      case toBond p of
        BV.Struct _ kvs -> do
          lookupBT 1 kvs `shouldBe` Just BV.BT_STRING
          lookupBT 2 kvs `shouldBe` Just BV.BT_INT32
          lookupBT 7 kvs `shouldBe` Just BV.BT_DOUBLE
          lookupBT 4 kvs `shouldBe` Just BV.BT_BOOL
        v -> expectationFailure ("expected Struct, got " ++ show v)

  , it "missing required field id is reported" $ do
      let bad = BV.Struct V.empty (V.fromList
            [ (1, BV.BT_STRING, BV.String "Alice")
            , (2, BV.BT_INT32,  BV.Int32 30)
              -- no field id 7 / 4
            ])
      case (fromBond bad :: Either String Profile) of
        Left _  -> pure ()
        Right v -> expectationFailure ("unexpected " ++ show v)
  ]
  where
    idIs n (k, _, _) = k == n

    lookupBT n kvs = case V.find (\(k, _, _) -> k == n) kvs of
      Just (_, bt, _) -> Just bt
      Nothing         -> Nothing

-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests = describe "newtype" $ sequence_
  [ it "pass-through to inner Int32" $
      toBond (Tag 42) `shouldBe` BV.Int32 42

  , it "round-trip" $
      fromBond (toBond (Tag 7)) `shouldBe` Right (Tag 7)

  , it "bondType is the inner type's tag" $
      bondType (Proxy :: Proxy Tag) `shouldBe` BV.BT_INT32
  ]

-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests = describe "enum (Int32-encoded)" $ sequence_
  [ it "Red -> 0 (default positional)" $
      toBond Red      `shouldBe` BV.Int32 0
  , it "Green -> 1" $
      toBond Green    `shouldBe` BV.Int32 1
  , it "DarkBlue -> 99 (explicit tag)" $
      toBond DarkBlue `shouldBe` BV.Int32 99
  , it "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
  , it "unknown value fails" $
      case (fromBond (BV.Int32 17) :: Either String Color) of
        Left _  -> pure ()
        Right c -> expectationFailure ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = fromBond (toBond c) `shouldBe` Right c

-- ---------------------------------------------------------------------------

sumTests :: Spec
sumTests = describe "sum (Bond single-field-struct union)" $ sequence_
  [ it "Origin (nullary) -> field id 1, payload Bool True" $
      toBond Origin `shouldBe`
        BV.Struct V.empty
          (V.singleton (1, BV.BT_BOOL, BV.Bool True))

  , it "Circle (unary) -> field id 2, payload = inner value" $
      toBond (Circle 1.5) `shouldBe`
        BV.Struct V.empty
          (V.singleton (2, BV.BT_DOUBLE, BV.Double 1.5))

  , it "Rect (n-ary, tag-overridden) -> field id 10" $
      case toBond (Rect 2 3) of
        BV.Struct _ kvs ->
          (V.any (\(k, _, _) -> k == 10) kvs) `shouldBe` True
        v -> expectationFailure ("expected Struct, got " ++ show v)

  , it "round-trip Origin" $ rt Origin
  , it "round-trip Circle" $ rt (Circle 2.5)
  , it "round-trip Rect"   $ rt (Rect 4 5)

  , it "unknown ctor id fails" $ do
      let bad = BV.Struct V.empty
                  (V.singleton (255, BV.BT_BOOL, BV.Bool True))
      case (fromBond bad :: Either String Shape) of
        Left _  -> pure ()
        Right s -> expectationFailure ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = fromBond (toBond s) `shouldBe` Right s
