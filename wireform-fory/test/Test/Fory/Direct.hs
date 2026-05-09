{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | Tests for the typed 'Fory.Direct' encoder / decoder. Each
-- case asserts that 'encodeDirect' produces the same bytes as
-- 'Fory.Encode.encode' would for the corresponding 'Value', and
-- that 'decodeDirect' round-trips the original Haskell value.
module Test.Fory.Direct (tests) where

import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word8, Word16, Word32, Word64)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified Fory.Direct as FD
import qualified Fory.Encode as E
import qualified Fory.Value as VV

tests :: TestTree
tests = testGroup "Fory.Direct"
  [ testGroup "primitives match Value bytes"
      [ testCase "Int 1234567890" $
          FD.encodeDirect (1234567890 :: Int)
            @?= E.encode (VV.VarInt64Val 1234567890)
      , testCase "Int8 -42" $
          FD.encodeDirect (-42 :: Int8)
            @?= E.encode (VV.Int8Val (-42))
      , testCase "Int32 maxBound" $
          FD.encodeDirect (maxBound :: Int32)
            @?= E.encode (VV.Int32Val maxBound)
      , testCase "Word16 0xABCD" $
          FD.encodeDirect (0xABCD :: Word16)
            @?= E.encode (VV.Uint16Val 0xABCD)
      , testCase "Float 3.14" $
          FD.encodeDirect (3.14 :: Float)
            @?= E.encode (VV.Float32Val 3.14)
      , testCase "Double 3.14159" $
          FD.encodeDirect (3.14159 :: Double)
            @?= E.encode (VV.Float64Val 3.14159)
      , testCase "Bool True" $
          FD.encodeDirect True
            @?= E.encode (VV.BoolVal True)
      , testCase "Text \"hello\"" $
          FD.encodeDirect ("hello" :: Text)
            @?= E.encode (VV.StringVal "hello")
      , testCase "ByteString [42, 43, 44]" $
          FD.encodeDirect (BS.pack [42, 43, 44])
            @?= E.encode (VV.BinaryVal (BS.pack [42, 43, 44]))
      ]

  , testGroup "[a] (boxed lists) match Value bytes"
      [ testCase "[Int] [1..5]" $ do
          let xs = [1..5] :: [Int]
              direct = FD.encodeDirect xs
              valForm = VV.ListVal
                (V.fromList (map (VV.VarInt64Val . fromIntegral) xs))
              viaValue = E.encode valForm
          direct @?= viaValue
      , testCase "[Int] empty" $
          FD.encodeDirect ([] :: [Int])
            @?= E.encode (VV.ListVal V.empty)
      , testCase "[Text] [\"hi\", \"world\"]" $ do
          let xs = ["hi", "world"] :: [Text]
              direct = FD.encodeDirect xs
              valForm = VV.ListVal (V.fromList (map VV.StringVal xs))
          direct @?= E.encode valForm
      , testCase "[Int32] [10, 20, 30]" $ do
          let xs = [10, 20, 30] :: [Int32]
              direct = FD.encodeDirect xs
              valForm = VV.ListVal (V.fromList (map VV.Int32Val xs))
          direct @?= E.encode valForm
      ]

  , testGroup "primitive arrays match Value bytes"
      [ testCase "VS.Vector Int32 [1,2,3]" $ do
          let v = VS.fromList [1, 2, 3 :: Int32]
              direct = FD.encodeDirect v
              valForm = VV.Int32ArrayVal v
          direct @?= E.encode valForm
      , testCase "VS.Vector Float64 [1.5, -2.5]" $ do
          let v = VS.fromList [1.5, -2.5]
              direct = FD.encodeDirect v
              valForm = VV.Float64ArrayVal v
          direct @?= E.encode valForm
      , testCase "VS.Vector Word8 [0xDE, 0xAD]" $ do
          let v = VS.fromList [0xDE, 0xAD :: Word8]
              direct = FD.encodeDirect v
              valForm = VV.Uint8ArrayVal v
          direct @?= E.encode valForm
      ]

  , testGroup "Map / HashMap match Value bytes"
      [ testCase "Map Text Int [(k0,0)..(k4,4)]" $ do
          let kvs = [("k" <> "0", 0), ("k1", 1), ("k2", 2)
                    , ("k3", 3), ("k4", 4)] :: [(Text, Int)]
              m = M.fromList kvs
              direct = FD.encodeDirect m
              valForm = VV.MapVal $ V.fromList
                [ (VV.StringVal k, VV.VarInt64Val (fromIntegral v))
                | (k, v) <- M.toAscList m]
          direct @?= E.encode valForm
      , testCase "HashMap Text Int round-trips" $ do
          let kvs = [("a", 1), ("b", 2), ("c", 3)] :: [(Text, Int)]
              m = HM.fromList kvs :: HashMap Text Int
              bs = FD.encodeDirect m
          FD.decodeDirect bs @?= Right m
      ]

  , testGroup "round-trips"
      [ testCase "Int" $ rtEq @Int 1234567890
      , testCase "Int32" $ rtEq @Int32 (-1)
      , testCase "Word64" $ rtEq @Word64 0xDEADBEEFCAFEBABE
      , testCase "Float" $ rtEq @Float 1.25
      , testCase "Double" $ rtEq @Double 3.141592653589793
      , testCase "Bool" $ rtEq @Bool False
      , testCase "Text" $ rtEq @Text "hello world"
      , testCase "ByteString" $ rtEq @BS.ByteString (BS.pack [1,2,3,4])
      , testCase "[Int]" $ rtEq @[Int] [1, 2, 3, 4, 5]
      , testCase "[Text]" $ rtEq @[Text] ["alpha", "beta", "gamma"]
      , testCase "VS.Vector Int32" $ rtEq @(VS.Vector Int32)
          (VS.fromList [-1, 0, 1, 2, 3])
      , testCase "Map Text Int" $ rtEq @(Map Text Int)
          (M.fromList [("a", 1), ("b", 2), ("c", 3)])
      , testCase "Map Text Text" $ rtEq @(Map Text Text)
          (M.fromList [("a", "alpha"), ("b", "beta")])
      ]

  , testGroup "encode size sanity"
      [ testCase "encodeDirect [1..100 :: Int] is non-empty" $ do
          let bs = FD.encodeDirect ([1..100] :: [Int])
          assertBool "non-empty" (BS.length bs > 100)
      ]
  ]

-- | Round-trip property: 'decodeDirect' inverts 'encodeDirect'
-- exactly, even for collections.
rtEq
  :: forall a. (FD.EncodeDirect a, FD.DecodeDirect a, Eq a, Show a)
  => a -> IO ()
rtEq x = FD.decodeDirect (FD.encodeDirect x) @?= Right x
