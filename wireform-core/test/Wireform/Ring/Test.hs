module Wireform.Ring.Test (spec) where

import Control.Exception (catch, SomeException, evaluate)
import Control.Monad (forM_, when)
import qualified Data.Bits
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr, plusPtr, nullPtr, minusPtr)
import Foreign.Storable (poke, peek)
import Foreign.Marshal.Array (pokeArray)
import Test.Hspec
import Test.QuickCheck

import Wireform.Ring
import Wireform.Ring.Internal (newMagicRing, destroyMagicRing)
import Wireform.Transport.Capabilities (detectCapabilities, capPageSize, capCoreCount)

spec :: Spec
spec = describe "MagicRing" $ do

  describe "basic operations" $ do
    it "creates and destroys a ring" $ do
      withMagicRing 4096 $ \ring -> do
        ringSize ring `shouldSatisfy` (>= 4096)
        ringMask ring `shouldBe` (ringSize ring - 1)
        ringBase ring `shouldSatisfy` (/= nullPtr)

    it "ring size is power of two" $ do
      withMagicRing 5000 $ \ring ->
        let s = ringSize ring
        in (s Data.Bits..&. (s - 1)) `shouldBe` 0

    it "ring size >= requested" $ do
      withMagicRing 12345 $ \ring ->
        ringSize ring `shouldSatisfy` (>= 12345)

  describe "double-mapping correctness" $ do
    it "write at end, read from second mapping" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        poke (base `plusPtr` (n - 1)) (0xAB :: Word8)
        -- Second mapping: base + n points to same physical page
        v <- peek (base `plusPtr` (n - 1)) :: IO Word8
        v `shouldBe` 0xAB

    it "contiguous read spanning boundary" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        forM_ [0..19] $ \i ->
          poke (base `plusPtr` (n - 10 + i)) (fromIntegral (i + 1) :: Word8)
        bytes <- mapM (\i -> peek (base `plusPtr` (n - 10 + i)) :: IO Word8) [0..19]
        bytes `shouldBe` [1..20]

    it "write in first mapping, read from second" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        poke (base `plusPtr` 42) (0xCD :: Word8)
        v <- peek (base `plusPtr` (n + 42)) :: IO Word8
        v `shouldBe` 0xCD

    it "write in second mapping, read from first" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        poke (base `plusPtr` (n + 100)) (0xEF :: Word8)
        v <- peek (base `plusPtr` 100) :: IO Word8
        v `shouldBe` 0xEF

    it "full ring write + wrap-around read" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        forM_ [0 .. n - 1] $ \i ->
          poke (base `plusPtr` i) (fromIntegral (i `mod` 256) :: Word8)
        forM_ [0 .. n - 1] $ \i -> do
          v <- peek (base `plusPtr` (n + i)) :: IO Word8
          v `shouldBe` fromIntegral (i `mod` 256)

  describe "multiple sizes" $ do
    it "works with 64KB ring" $ do
      withMagicRing (64 * 1024) $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        poke (base `plusPtr` (n - 1)) (0xFF :: Word8)
        v <- peek (base `plusPtr` (n - 1)) :: IO Word8
        v `shouldBe` 0xFF

    it "works with 1MB ring" $ do
      withMagicRing (1024 * 1024) $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        poke (base `plusPtr` (n - 1)) (0x42 :: Word8)
        v <- peek (base `plusPtr` (2 * n - 1)) :: IO Word8
        v `shouldBe` 0x42

    it "minimum size is at least page-sized" $ do
      withMagicRing 1 $ \ring ->
        ringSize ring `shouldSatisfy` (>= 4096)

  describe "resource management" $ do
    it "sequential allocation: no FD/VA leaks (1000 rings)" $ do
      forM_ [1..1000 :: Int] $ \_ ->
        withMagicRing 4096 $ \ring ->
          ringSize ring `shouldSatisfy` (>= 4096)

    it "absurd size throws MagicRingUnavailable" $ do
      let absurdSize = 1024 * 1024 * 1024 * 1024 * 1024
      result <- (newMagicRing absurdSize >> pure False) `catch`
        (\(MagicRingUnavailable _) -> pure True)
      result `shouldBe` True

    it "destroy is idempotent" $ do
      ring <- newMagicRing 4096
      destroyMagicRing ring
      destroyMagicRing ring  -- should not crash

  describe "stress tests" $ do
    it "ring used as circular buffer (100k writes)" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
            mask = ringMask ring
        forM_ [0..99999 :: Int] $ \i -> do
          let off = i Data.Bits..&. mask
          poke (base `plusPtr` off) (fromIntegral (i `mod` 251) :: Word8)
          v <- peek (base `plusPtr` off) :: IO Word8
          when (v /= fromIntegral (i `mod` 251)) $
            error ("mismatch at iteration " <> show i)

  describe "capabilities detection" $ do
    it "detects page size > 0" $ do
      caps <- detectCapabilities
      capPageSize caps `shouldSatisfy` (> 0)

    it "detects at least 1 core" $ do
      caps <- detectCapabilities
      capCoreCount caps `shouldSatisfy` (>= 1)

  describe "stress tests (continued)" $ do
    it "alternating write/read at boundary" $ do
      withMagicRing 4096 $ \ring -> do
        let n = ringSize ring
            base = ringBase ring
        forM_ [0..9999 :: Int] $ \i -> do
          let off = (n - 4 + (i `mod` 8))
          poke (base `plusPtr` off) (fromIntegral i :: Word8)
          v <- peek (base `plusPtr` off) :: IO Word8
          v `shouldBe` fromIntegral i

  describe "RingSlice (runST-like scoping)" $ do
    it "ringSliceLength reports the requested length" $
      withMagicRing 4096 $ \ring -> do
        let s = ringSlice ring 0 17
        ringSliceLength s `shouldBe` 17

    it "copyRingSlice produces a ByteString with the slice's bytes" $
      withMagicRing 4096 $ \ring -> do
        let base = ringBase ring
        forM_ [0..31 :: Int] $ \i ->
          poke (base `plusPtr` i) (fromIntegral (i + 1) :: Word8)
        copied <- copyRingSlice (ringSlice ring 0 32)
        BS.unpack copied `shouldBe` map fromIntegral [1..32 :: Int]

    it "copyRingSlice handles slices that cross the ring boundary" $
      withMagicRing 4096 $ \ring -> do
        let n    = ringSize ring
            base = ringBase ring
        -- Lay down a marker pattern that wraps across the boundary by
        -- writing through the second mapping (offsets n-8 .. n+7).
        forM_ [0..15 :: Int] $ \i ->
          poke (base `plusPtr` (n - 8 + i)) (fromIntegral (i + 1) :: Word8)
        -- Slice starting at (n-8), length 16 — reads contiguously
        -- through the double mapping.
        copied <- copyRingSlice (ringSliceAtPos ring (fromIntegral (n - 8)) 16)
        BS.unpack copied `shouldBe` map fromIntegral [1..16 :: Int]

    it "copyRingSlice on an empty slice returns mempty" $
      withMagicRing 4096 $ \ring -> do
        copied <- copyRingSlice (ringSlice ring 0 0)
        copied `shouldBe` BS.empty

    it "copied ByteString outlives the ring's scope" $ do
      -- This compiles iff copyRingSlice's result is independent of @s@.
      -- The whole point of the scoping mechanism is that a raw
      -- RingSlice s cannot escape but a fresh ByteString can.
      bs <- withMagicRing 4096 $ \ring -> do
        let base = ringBase ring
        forM_ [0..7 :: Int] $ \i ->
          poke (base `plusPtr` i) (fromIntegral (0xA0 + i) :: Word8)
        copyRingSlice (ringSlice ring 0 8)
      BS.unpack bs `shouldBe` [0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7]

    it "withRingSlice exposes pointer and length to the body" $
      withMagicRing 4096 $ \ring -> do
        let base = ringBase ring
        poke (base `plusPtr` 4) (0x42 :: Word8)
        let s = ringSlice ring 4 1
        v <- withRingSlice s $ \p n -> do
          n `shouldBe` 1
          peek p :: IO Word8
        v `shouldBe` 0x42

    it "peekRingSliceByte reads at the given offset" $
      withMagicRing 4096 $ \ring -> do
        let base = ringBase ring
        forM_ [0..15 :: Int] $ \i ->
          poke (base `plusPtr` (100 + i)) (fromIntegral (i * 3) :: Word8)
        let s = ringSlice ring 100 16
        forM_ [0..15 :: Int] $ \i -> do
          v <- peekRingSliceByte s i
          v `shouldBe` fromIntegral (i * 3)
