{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Aeson (tests) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import Test.Derive.Aeson.Instances ()
import Test.Derive.Aeson.Types

tests :: TestTree
tests = testGroup "Aeson deriver"
  [ recordTests
  , newtypeTests
  , enumTests
  , sumTests
  ]

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "encode applies rename / renameStyle" $ do
      let a = Address "1 Main"  "Springfield" "12345" "secret"
      let v = A.toJSON a
      case v of
        A.Object o -> do
          assertEqual "street key (literal rename)"
            (Just (A.String "1 Main"))
            (KM.lookup (Key.fromText "street") o)
          assertEqual "city key (snake rename)"
            (Just (A.String "Springfield"))
            (KM.lookup (Key.fromText "addr_city") o)
          assertEqual "zip key (strip-prefix + snake)"
            (Just (A.String "12345"))
            (KM.lookup (Key.fromText "zip") o)
          assertBool "internal key skipped under JSON"
            (not (KM.member (Key.fromText "addrInternal") o))
        _ -> fail "expected JSON object"

  , testCase "decode round-trips (skipped field filled by defaults)" $ do
      let a = Address "1 Main" "Springfield" "12345" "secret"
      case A.fromJSON (A.toJSON a) of
        A.Success a' -> do
          assertEqual "street" (addrStreet a) (addrStreet a')
          assertEqual "city"   (addrCity a)   (addrCity a')
          assertEqual "zip"    (addrZip a)    (addrZip a')
          assertEqual "internal -> defaultAddrInternal"
            defaultAddrInternal
            (addrInternal a')
        A.Error e -> fail ("decode failed: " ++ e)
  ]

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtypeTests :: TestTree
newtypeTests = testGroup "newtype"
  [ testCase "encode passes through" $
      A.toJSON (UserId 42) @?= A.Number 42

  , testCase "round-trip" $ do
      case A.fromJSON (A.toJSON (UserId 7)) of
        A.Success (UserId n) -> n @?= 7
        A.Error e -> fail e
  ]

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

enumTests :: TestTree
enumTests = testGroup "enum"
  [ testCase "encode literal-renamed constructor"
      (A.toJSON Red @?= A.String "red")
  , testCase "encode style-renamed constructor (DarkBlue -> dark-blue)"
      (A.toJSON DarkBlue @?= A.String "dark-blue")
  , testCase "round-trip Red" $
      A.fromJSON (A.String "red")     @?= A.Success Red
  , testCase "round-trip Green" $
      A.fromJSON (A.String "green")   @?= A.Success Green
  , testCase "round-trip DarkBlue" $
      A.fromJSON (A.String "dark-blue") @?= A.Success DarkBlue
  , testCase "unknown value fails" $ do
      case A.fromJSON (A.String "purple") :: A.Result Color of
        A.Error _ -> pure ()
        A.Success c -> fail ("unexpected " ++ show c)
  ]

-- ---------------------------------------------------------------------------
-- Sum
-- ---------------------------------------------------------------------------

sumTests :: TestTree
sumTests = testGroup "sum"
  [ testCase "Point  -> tag/contents (null payload)" $
      A.toJSON Point @?= A.object
        [ Key.fromText "tag"      A..= A.String "point"
        , Key.fromText "contents" A..= A.Null
        ]

  , testCase "Circle -> tag/contents (single payload)" $
      A.toJSON (Circle 1.5) @?= A.object
        [ Key.fromText "tag"      A..= A.String "circle"
        , Key.fromText "contents" A..= A.Number 1.5
        ]

  , testCase "Rect (renameStyle SnakeCase) -> tag = \"rect\", array contents" $ do
      let v = A.toJSON (Rect 2 3)
      case v of
        A.Object o -> do
          KM.lookup (Key.fromText "tag") o
            @?= Just (A.String "rect")
          KM.lookup (Key.fromText "contents") o
            @?= Just (A.toJSON ([A.Number 2, A.Number 3] :: [A.Value]))
        _ -> fail "expected JSON object"

  , testCase "round-trip Point"     $ rt Point
  , testCase "round-trip Circle"    $ rt (Circle 2.5)
  , testCase "round-trip Rect"      $ rt (Rect 4 5)
  , testCase "unknown tag fails" $ do
      let bad = A.object
            [ Key.fromText "tag"      A..= A.String "triangle"
            , Key.fromText "contents" A..= A.Null
            ]
      case A.fromJSON bad :: A.Result Shape of
        A.Error _ -> pure ()
        A.Success s -> fail ("unexpected " ++ show s)
  ]
  where
    rt :: Shape -> IO ()
    rt s = case A.fromJSON (A.toJSON s) of
      A.Success s' -> s' @?= s
      A.Error e    -> fail ("round-trip failed: " ++ e)
