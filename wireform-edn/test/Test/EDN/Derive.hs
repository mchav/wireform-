{-# LANGUAGE OverloadedStrings #-}

module Test.EDN.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified EDN.Class as E
import qualified EDN.Value as EV

import Test.EDN.Derive.Instances ()
import Test.EDN.Derive.Types

tests :: Spec
tests = describe "EDN.Derive" $ sequence_
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case E.toEDN p of
        EV.Map kvs -> do
          (V.elem ( EV.Keyword Nothing "name"
                    , EV.String "Alice"
                    ) kvs) `shouldBe` True
          (V.any (keyIs "profile_age") kvs) `shouldBe` True
          (V.any (keyIs "email") kvs) `shouldBe` True
          (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
        v -> expectationFailure ("expected Map, got " ++ show v)

  , it "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case E.fromEDN (E.toEDN p) of
        Right p' -> do
          profileName    p' `shouldBe` profileName p
          profileAge     p' `shouldBe` profileAge  p
          profileEmail   p' `shouldBe` profileEmail p
          profilePrivate p' `shouldBe` defaultPrivate
        Left e -> expectationFailure e
  ]
  where
    keyIs t (EV.Keyword Nothing k, _) = k == t
    keyIs _ _                         = False

newtypeTests :: Spec
newtypeTests = describe "newtype" $ sequence_
  [ it "pass-through" $
      E.toEDN (Tag 42) `shouldBe` EV.Integer 42
  , it "round-trip" $
      E.fromEDN (E.toEDN (Tag 7)) `shouldBe` Right (Tag 7)
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "Red"      $ E.toEDN Red      `shouldBe` EV.Keyword Nothing "red"
  , it "DarkBlue" $ E.toEDN DarkBlue `shouldBe` EV.Keyword Nothing "dark-blue"
  , it "round-trip" $
      mapM_ rt [Red, Green, DarkBlue]
  , it "unknown fails" $
      case E.fromEDN (EV.Keyword Nothing "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> expectationFailure ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = E.fromEDN (E.toEDN c) `shouldBe` Right c

sumTests :: Spec
sumTests = describe "sum" $ sequence_
  [ it "Origin (nullary) -> tag/contents=Nil" $
      E.toEDN Origin `shouldBe`
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "origin"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Nil
            )
          ])

  , it "Circle (unary) -> contents = inner value" $
      E.toEDN (Circle 1.5) `shouldBe`
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "circle"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Float 1.5
            )
          ])

  , it "Rect (n-ary) -> contents = Vector" $
      E.toEDN (Rect 2 3) `shouldBe`
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "rect"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Vector (V.fromList [EV.Float 2, EV.Float 3])
            )
          ])

  , it "round-trip Origin" $ rt Origin
  , it "round-trip Circle" $ rt (Circle 2.5)
  , it "round-trip Rect"   $ rt (Rect 4 5)

  , it "unknown tag fails" $ do
      let bad = EV.Map (V.fromList
            [ ( EV.Keyword Nothing "tag"
              , EV.Keyword Nothing "ellipse"
              )
            , ( EV.Keyword Nothing "contents"
              , EV.Nil
              )
            ])
      case E.fromEDN bad :: Either String Shape of
        Left _  -> pure ()
        Right s -> expectationFailure ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = E.fromEDN (E.toEDN s) `shouldBe` Right s
