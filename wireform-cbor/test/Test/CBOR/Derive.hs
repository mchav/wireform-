{-# LANGUAGE OverloadedStrings #-}

module Test.CBOR.Derive (spec) where

import qualified Data.Vector as V
import Test.Syd

import qualified CBOR.Class as C
import qualified CBOR.Value as CV

import Test.CBOR.Derive.Instances ()
import Test.CBOR.Derive.Types

spec :: Spec
spec = describe "CBOR.Derive" $ do
  recordTests
  newtypeTests
  enumTests
  sumTests

-- ---------------------------------------------------------------------------

recordTests :: Spec
recordTests = describe "record" $ do
  it "encode applies rename + renameStyle, drops skipped" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case C.toCBOR p of
      CV.Map kvs -> do
        V.elem (CV.TextString "name", CV.TextString "Alice") kvs `shouldBe` True
        V.any (keyIs "profile_age") kvs `shouldBe` True
        V.any (keyIs "email") kvs `shouldBe` True
        not (V.any (keyIs "profilePrivate") kvs) `shouldBe` True
      v -> expectationFailure ("expected Map, got " ++ show v)

  it "round-trip fills skipped from defaults" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case C.fromCBOR (C.toCBOR p) of
      Right p' -> do
        profileName  p' `shouldBe` profileName p
        profileAge   p' `shouldBe` profileAge p
        profileEmail p' `shouldBe` profileEmail p
        profilePrivate p' `shouldBe` defaultPrivate
      Left e -> expectationFailure e
  where
    keyIs t (CV.TextString k, _) = k == t
    keyIs _ _                    = False

-- ---------------------------------------------------------------------------

newtypeTests :: Spec
newtypeTests = describe "newtype" $ do
  it "pass-through" $
    C.toCBOR (Tag 42) `shouldBe` CV.UInt 42
  it "round-trip" $
    C.fromCBOR (C.toCBOR (Tag 7)) `shouldBe` Right (Tag 7)

-- ---------------------------------------------------------------------------

enumTests :: Spec
enumTests = describe "enum" $ do
  it "Red"      $ C.toCBOR Red      `shouldBe` CV.TextString "red"
  it "DarkBlue" $ C.toCBOR DarkBlue `shouldBe` CV.TextString "dark-blue"
  it "round-trip" $
    mapM_ rt [Red, Green, DarkBlue]
  it "unknown fails" $
    case C.fromCBOR (CV.TextString "purple") :: Either String Color of
      Left _  -> pure ()
      Right c -> expectationFailure ("unexpected " ++ show c)
  where
    rt :: Color -> IO ()
    rt c = C.fromCBOR (C.toCBOR c) `shouldBe` Right c

-- ---------------------------------------------------------------------------

sumTests :: Spec
sumTests = describe "sum" $ do
  it "Origin (nullary) -> tag/contents=Null" $
    C.toCBOR Origin `shouldBe`
      CV.Map (V.fromList
        [ (CV.TextString "tag",      CV.TextString "origin")
        , (CV.TextString "contents", CV.Null)
        ])

  it "Circle (unary)   -> contents = inner value" $
    C.toCBOR (Circle 1.5) `shouldBe`
      CV.Map (V.fromList
        [ (CV.TextString "tag",      CV.TextString "circle")
        , (CV.TextString "contents", CV.Float64 1.5)
        ])

  it "Rect   (n-ary)   -> contents = Array" $
    C.toCBOR (Rect 2 3) `shouldBe`
      CV.Map (V.fromList
        [ (CV.TextString "tag",      CV.TextString "rect")
        , (CV.TextString "contents",
            CV.Array (V.fromList [CV.Float64 2, CV.Float64 3]))
        ])

  it "round-trip Origin" $ rt Origin
  it "round-trip Circle" $ rt (Circle 2.5)
  it "round-trip Rect"   $ rt (Rect 4 5)

  it "unknown tag fails" $ do
    let bad = CV.Map (V.fromList
          [ (CV.TextString "tag",      CV.TextString "ellipse")
          , (CV.TextString "contents", CV.Null)
          ])
    case C.fromCBOR bad :: Either String Shape of
      Left _ -> pure ()
      Right s -> expectationFailure ("unexpected " ++ show s)
  where
    rt :: Shape -> IO ()
    rt s = C.fromCBOR (C.toCBOR s) `shouldBe` Right s
