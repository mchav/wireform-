{-# LANGUAGE OverloadedStrings #-}

module Test.MsgPack.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import qualified MsgPack.Class as M
import qualified MsgPack.Value as MV

import Test.MsgPack.Derive.Instances ()
import Test.MsgPack.Derive.Types

tests :: TestTree
tests = testGroup "MsgPack.Derive"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case M.toMsgPack p of
        MV.Map kvs -> do
          assertBool "name key present"
            (V.elem (MV.String "name", MV.String "Alice") kvs)
          assertBool "snake-cased age key present"
            (V.any (keyIs "profile_age") kvs)
          assertBool "email key (StripPrefix + snake)"
            (V.any (keyIs "email") kvs)
          assertBool "private skipped"
            (not (V.any (keyIs "profilePrivate") kvs))
        v -> fail ("expected Map, got " ++ show v)

  , testCase "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case M.fromMsgPack (M.toMsgPack p) of
        Right p' -> do
          profileName  p' @?= profileName p
          profileAge   p' @?= profileAge p
          profileEmail p' @?= profileEmail p
          profilePrivate p' @?= defaultPrivate
        Left e -> fail e
  ]
  where
    keyIs t (MV.String k, _) = k == t
    keyIs _ _                = False

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "round-trip" $
      M.fromMsgPack (M.toMsgPack (Tag 7)) @?= Right (Tag 7)
  ]

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "Red"      $ M.toMsgPack Red      @?= MV.String "red"
  , testCase "DarkBlue" $ M.toMsgPack DarkBlue @?= MV.String "dark-blue"
  , testCase "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
  , testCase "unknown fails" $
      case M.fromMsgPack (MV.String "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> fail ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = M.fromMsgPack (M.toMsgPack c) @?= Right c

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Origin -> tag/contents=Nil" $
      M.toMsgPack Origin @?=
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "origin")
          , (MV.String "contents", MV.Nil)
          ])
  , testCase "Circle -> contents = inner" $
      M.toMsgPack (Circle 1.5) @?=
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "circle")
          , (MV.String "contents", MV.Double 1.5)
          ])
  , testCase "Rect   -> contents = Array" $
      M.toMsgPack (Rect 2 3) @?=
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "rect")
          , (MV.String "contents",
              MV.Array (V.fromList [MV.Double 2, MV.Double 3]))
          ])
  , testCase "round-trip Origin" $ rt Origin
  , testCase "round-trip Circle" $ rt (Circle 2.5)
  , testCase "round-trip Rect"   $ rt (Rect 4 5)
  , testCase "unknown tag fails" $ do
      let bad = MV.Map (V.fromList
            [ (MV.String "tag",      MV.String "ellipse")
            , (MV.String "contents", MV.Nil)
            ])
      case M.fromMsgPack bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = M.fromMsgPack (M.toMsgPack s) @?= Right s
