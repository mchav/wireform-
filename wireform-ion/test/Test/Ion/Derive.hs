{-# LANGUAGE OverloadedStrings #-}

module Test.Ion.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified Ion.Class as I
import qualified Ion.Value as IV

import Test.Ion.Derive.Instances ()
import Test.Ion.Derive.Types

tests :: Spec
tests = describe "Ion.Derive" $ sequence_
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case I.toIon p of
        IV.Struct kvs -> do
          (V.elem ("name", IV.String "Alice") kvs) `shouldBe` True
          (V.any (keyIs "profile_age") kvs) `shouldBe` True
          (V.any (keyIs "email") kvs) `shouldBe` True
          (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
        v -> expectationFailure ("expected Struct, got " ++ show v)

  , it "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case I.fromIon (I.toIon p) of
        Right p' -> do
          profileName  p' `shouldBe` profileName p
          profileAge   p' `shouldBe` profileAge p
          profileEmail p' `shouldBe` profileEmail p
          profilePrivate p' `shouldBe` defaultPrivate
        Left e -> expectationFailure e
  ]
  where
    keyIs t (k, _) = k == t

newtypeTests :: Spec
newtypeTests = describe "newtype" $ sequence_
  [ it "pass-through" $
      I.toIon (Tag 42) `shouldBe` IV.Int 42
  , it "round-trip" $
      I.fromIon (I.toIon (Tag 7)) `shouldBe` Right (Tag 7)
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "Red"      $ I.toIon Red      `shouldBe` IV.String "red"
  , it "DarkBlue" $ I.toIon DarkBlue `shouldBe` IV.String "dark-blue"
  , it "round-trip" $ do
      mapM_ rt [Red, Green, DarkBlue]
  , it "unknown fails" $
      case I.fromIon (IV.String "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> expectationFailure ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = I.fromIon (I.toIon c) `shouldBe` Right c

sumTests :: Spec
sumTests = describe "sum" $ sequence_
  [ it "Origin (nullary) -> tag/contents=Null" $
      I.toIon Origin `shouldBe`
        IV.Struct (V.fromList
          [ ("tag",      IV.String "origin")
          , ("contents", IV.Null)
          ])

  , it "Circle (unary)   -> contents = inner value" $
      I.toIon (Circle 1.5) `shouldBe`
        IV.Struct (V.fromList
          [ ("tag",      IV.String "circle")
          , ("contents", IV.Float 1.5)
          ])

  , it "Rect   (n-ary)   -> contents = List" $
      I.toIon (Rect 2 3) `shouldBe`
        IV.Struct (V.fromList
          [ ("tag",      IV.String "rect")
          , ("contents",
              IV.List (V.fromList [IV.Float 2, IV.Float 3]))
          ])

  , it "round-trip Origin" $ rt Origin
  , it "round-trip Circle" $ rt (Circle 2.5)
  , it "round-trip Rect"   $ rt (Rect 4 5)

  , it "unknown tag fails" $ do
      let bad = IV.Struct (V.fromList
            [ ("tag",      IV.String "ellipse")
            , ("contents", IV.Null)
            ])
      case I.fromIon bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> expectationFailure ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = I.fromIon (I.toIon s) `shouldBe` Right s
