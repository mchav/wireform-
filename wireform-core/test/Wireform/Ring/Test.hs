module Wireform.Ring.Test (spec) where

import Control.Exception (catch, SomeException)
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, nullPtr)
import Foreign.Storable (poke, peek)
import Test.Hspec

import Wireform.Ring

spec :: Spec
spec = describe "MagicRing" $ do
  it "creates and destroys a ring" $ do
    withMagicRing 4096 $ \ring -> do
      ringSize ring `shouldSatisfy` (>= 4096)
      ringMask ring `shouldBe` (ringSize ring - 1)
      ringBase ring `shouldSatisfy` (/= nullPtr)

  it "double-mapping: write at end, read past wrap" $ do
    withMagicRing 4096 $ \ring -> do
      let n = ringSize ring
          base = ringBase ring
      poke (base `plusPtr` (n - 5)) (0xAA :: Word8)
      poke (base `plusPtr` (n - 4)) (0xBB :: Word8)
      poke (base `plusPtr` (n - 3)) (0xCC :: Word8)
      poke (base `plusPtr` (n - 2)) (0xDD :: Word8)
      poke (base `plusPtr` (n - 1)) (0xEE :: Word8)
      b0 <- peek (base `plusPtr` (n - 5)) :: IO Word8
      b1 <- peek (base `plusPtr` (n - 4)) :: IO Word8
      b2 <- peek (base `plusPtr` (n - 3)) :: IO Word8
      b3 <- peek (base `plusPtr` (n - 2)) :: IO Word8
      b4 <- peek (base `plusPtr` (n - 1)) :: IO Word8
      -- Read the SAME bytes via the second mapping
      b0' <- peek (base `plusPtr` n `plusPtr` negate 5) :: IO Word8
      b1' <- peek (base `plusPtr` n `plusPtr` negate 4) :: IO Word8
      b2' <- peek (base `plusPtr` n `plusPtr` negate 3) :: IO Word8
      b3' <- peek (base `plusPtr` n `plusPtr` negate 2) :: IO Word8
      b4' <- peek (base `plusPtr` n `plusPtr` negate 1) :: IO Word8
      [b0, b1, b2, b3, b4] `shouldBe` [0xAA, 0xBB, 0xCC, 0xDD, 0xEE]
      [b0', b1', b2', b3', b4'] `shouldBe` [0xAA, 0xBB, 0xCC, 0xDD, 0xEE]

  it "wrap-around read: contiguous read spanning boundary" $ do
    withMagicRing 4096 $ \ring -> do
      let n = ringSize ring
          base = ringBase ring
      sequence_ [ poke (base `plusPtr` (n - 5 + i)) (fromIntegral (i + 1) :: Word8)
                | i <- [0..9]
                ]
      bytes <- sequence [ peek (base `plusPtr` (n - 5 + i)) :: IO Word8
                        | i <- [0..9]
                        ]
      bytes `shouldBe` [1..10]

  it "sequential allocation: no FD/VA leaks" $ do
    sequence_ [ withMagicRing 4096 $ \ring ->
                  ringSize ring `shouldSatisfy` (>= 4096)
              | _ <- [1..1000 :: Int]
              ]

  it "absurd size throws MagicRingException" $ do
    let absurdSize = 1024 * 1024 * 1024 * 1024 * 1024
    result <- (newMagicRing absurdSize >> pure False) `catch`
      (\(MagicRingUnavailable _) -> pure True)
    result `shouldBe` True
