{-# LANGUAGE OverloadedStrings #-}

module Test.EDN.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified EDN.Class as E
import qualified EDN.Value as EV

import Test.EDN.Derive.Instances ()
import Test.EDN.Derive.Types

tests :: TestTree
tests = testGroup "EDN.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case E.toEDN p of
        EV.Map kvs -> do
          assertBool "name keyword present"
            (V.elem ( EV.Keyword Nothing "name"
                    , EV.String "Alice"
                    ) kvs)
          assertBool "snake-cased age keyword present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email keyword (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected Map, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case E.fromEDN (E.toEDN p) of
        Right p' -> do
          profileName    p' @?= profileName p
          profileAge     p' @?= profileAge  p
          profileEmail   p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (EV.Keyword Nothing k, _) = k == t
    keyIs _ _                         = False

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "pass-through" $
      E.toEDN (Tag 42) @?= EV.Integer 42
  , testCase "round-trip" $
      E.fromEDN (E.toEDN (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ E.toEDN Red      @?= EV.Keyword Nothing "red"
  , testCase "DarkBlue" $ E.toEDN DarkBlue @?= EV.Keyword Nothing "dark-blue"
  , testCase "round-trip" $
      mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case E.fromEDN (EV.Keyword Nothing "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = E.fromEDN (E.toEDN c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin (nullary) -> tag/contents=Nil" $
      E.toEDN Origin @?=
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "origin"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Nil
            )
          ])

  , testCase "Circle (unary) -> contents = inner value" $
      E.toEDN (Circle 1.5) @?=
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "circle"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Float 1.5
            )
          ])

  , testCase "Rect (n-ary) -> contents = Vector" $
      E.toEDN (Rect 2 3) @?=
        EV.Map (V.fromList
          [ ( EV.Keyword Nothing "tag"
            , EV.Keyword Nothing "rect"
            )
          , ( EV.Keyword Nothing "contents"
            , EV.Vector (V.fromList [EV.Float 2, EV.Float 3])
            )
          ])

  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)

  , testCase "unknown tag fails" $ do
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
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = E.fromEDN (E.toEDN s) @?= Right s
