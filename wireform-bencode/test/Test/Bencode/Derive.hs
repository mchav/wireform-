{-# LANGUAGE OverloadedStrings #-}

module Test.Bencode.Derive (spec) where

import Bencode.Class qualified as B
import Bencode.Value qualified as BV
import Data.Vector qualified as V
import Test.Bencode.Derive.Instances ()
import Test.Bencode.Derive.Types
import Test.Syd


spec :: Spec
spec = describe "Bencode.Derive" $ do
  recordTests
  newtypeTests
  enumTests
  sumTests


recordTests :: Spec
recordTests = describe "record" $ do
  it "encode applies rename + renameStyle, drops skipped" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case B.toBencode p of
      BV.BDict kvs -> do
        V.elem ("name", BV.BString "Alice") kvs `shouldBe` True
        V.any (keyIs "profile_age") kvs `shouldBe` True
        V.any (keyIs "email") kvs `shouldBe` True
        not (V.any (keyIs "profilePrivate") kvs) `shouldBe` True
      v -> expectationFailure ("expected BDict, got " ++ show v)

  it "round-trip fills skipped from defaults" $ do
    let p = Profile "Alice" 30 "a@x" "secret"
    case B.fromBencode (B.toBencode p) of
      Right p' -> do
        profileName p' `shouldBe` profileName p
        profileAge p' `shouldBe` profileAge p
        profileEmail p' `shouldBe` profileEmail p
        profilePrivate p' `shouldBe` defaultPrivate
      Left e -> expectationFailure e
  where
    keyIs t (k, _) = k == t


newtypeTests :: Spec
newtypeTests = describe "newtype" $ do
  it "pass-through" $
    B.toBencode (Tag 42) `shouldBe` BV.BInteger 42
  it "round-trip" $
    B.fromBencode (B.toBencode (Tag 7)) `shouldBe` Right (Tag 7)


enumTests :: Spec
enumTests = describe "enum" $ do
  it "Red" $ B.toBencode Red `shouldBe` BV.BString "red"
  it "DarkBlue" $ B.toBencode DarkBlue `shouldBe` BV.BString "dark-blue"
  it "round-trip" $
    mapM_ rt [Red, Green, DarkBlue]
  it "unknown fails" $
    case B.fromBencode (BV.BString "purple") :: Either String Color of
      Left _ -> pure ()
      Right c -> expectationFailure ("unexpected " ++ show c)
  where
    rt :: Color -> IO ()
    rt c = B.fromBencode (B.toBencode c) `shouldBe` Right c


sumTests :: Spec
sumTests = describe "sum" $ do
  it "Origin (nullary) -> single-key dict (no contents)" $
    B.toBencode Origin
      `shouldBe` BV.BDict
        ( V.fromList
            [ ("tag", BV.BString "origin")
            ]
        )

  it "Circle (unary)   -> contents = inner value" $
    B.toBencode (Circle 1)
      `shouldBe` BV.BDict
        ( V.fromList
            [ ("tag", BV.BString "circle")
            , ("contents", BV.BInteger 1)
            ]
        )

  it "Rect   (n-ary)   -> contents = BList" $
    B.toBencode (Rect 2 3)
      `shouldBe` BV.BDict
        ( V.fromList
            [ ("tag", BV.BString "rect")
            ,
              ( "contents"
              , BV.BList (V.fromList [BV.BInteger 2, BV.BInteger 3])
              )
            ]
        )

  it "round-trip Origin" $ rt Origin
  it "round-trip Circle" $ rt (Circle 5)
  it "round-trip Rect" $ rt (Rect 4 5)

  it "unknown tag fails" $ do
    let bad =
          BV.BDict
            ( V.fromList
                [ ("tag", BV.BString "ellipse")
                ]
            )
    case B.fromBencode bad :: Either String Shape of
      Left _ -> pure ()
      Right s -> expectationFailure ("unexpected " ++ show s)
  where
    rt :: Shape -> IO ()
    rt s = B.fromBencode (B.toBencode s) `shouldBe` Right s
