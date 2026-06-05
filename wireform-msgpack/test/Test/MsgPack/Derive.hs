{-# LANGUAGE OverloadedStrings #-}

module Test.MsgPack.Derive (tests) where

import qualified Data.Vector as V
import Test.Syd

import qualified MsgPack.Class as M
import qualified MsgPack.Value as MV

import Test.MsgPack.Derive.Instances ()
import Test.MsgPack.Derive.Types

tests :: Spec
tests = describe "MsgPack.Derive" $ sequence_
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

recordTests :: Spec
recordTests = describe "record" $ sequence_
  [ it "encode applies rename + renameStyle, drops skipped" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case M.toMsgPack p of
        MV.Map kvs -> do
          (V.elem (MV.String "name", MV.String "Alice") kvs) `shouldBe` True
          (V.any (keyIs "profile_age") kvs) `shouldBe` True
          (V.any (keyIs "email") kvs) `shouldBe` True
          (not (V.any (keyIs "profilePrivate") kvs)) `shouldBe` True
        v -> expectationFailure ("expected Map, got " ++ show v)

  , it "round-trip fills skipped from defaults" $ do
      let p = Profile "Alice" 30 "a@x" "secret"
      case M.fromMsgPack (M.toMsgPack p) of
        Right p' -> do
          profileName  p' `shouldBe` profileName p
          profileAge   p' `shouldBe` profileAge p
          profileEmail p' `shouldBe` profileEmail p
          profilePrivate p' `shouldBe` defaultPrivate
        Left e -> expectationFailure e
  ]
  where
    keyIs t (MV.String k, _) = k == t
    keyIs _ _                = False

newtypeTests :: Spec
newtypeTests = describe "newtype" $ sequence_
  [ it "round-trip" $
      M.fromMsgPack (M.toMsgPack (Tag 7)) `shouldBe` Right (Tag 7)
  ]

enumTests :: Spec
enumTests = describe "enum" $ sequence_
  [ it "Red"      $ M.toMsgPack Red      `shouldBe` MV.String "red"
  , it "DarkBlue" $ M.toMsgPack DarkBlue `shouldBe` MV.String "dark-blue"
  , it "round-trip" $ mapM_ rt [Red, Green, DarkBlue]
  , it "unknown fails" $
      case M.fromMsgPack (MV.String "purple") :: Either String Color of
        Left _  -> pure ()
        Right c -> expectationFailure ("unexpected " ++ show c)
  ]
  where
    rt :: Color -> IO ()
    rt c = M.fromMsgPack (M.toMsgPack c) `shouldBe` Right c

sumTests :: Spec
sumTests = describe "sum" $ sequence_
  [ it "Origin -> tag/contents=Nil" $
      M.toMsgPack Origin `shouldBe`
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "origin")
          , (MV.String "contents", MV.Nil)
          ])
  , it "Circle -> contents = inner" $
      M.toMsgPack (Circle 1.5) `shouldBe`
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "circle")
          , (MV.String "contents", MV.Double 1.5)
          ])
  , it "Rect   -> contents = Array" $
      M.toMsgPack (Rect 2 3) `shouldBe`
        MV.Map (V.fromList
          [ (MV.String "tag",      MV.String "rect")
          , (MV.String "contents",
              MV.Array (V.fromList [MV.Double 2, MV.Double 3]))
          ])
  , it "round-trip Origin" $ rt Origin
  , it "round-trip Circle" $ rt (Circle 2.5)
  , it "round-trip Rect"   $ rt (Rect 4 5)
  , it "unknown tag fails" $ do
      let bad = MV.Map (V.fromList
            [ (MV.String "tag",      MV.String "ellipse")
            , (MV.String "contents", MV.Nil)
            ])
      case M.fromMsgPack bad :: Either String Shape of
        Left _ -> pure ()
        Right s -> expectationFailure ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = M.fromMsgPack (M.toMsgPack s) `shouldBe` Right s
