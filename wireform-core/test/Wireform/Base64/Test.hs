{-# LANGUAGE OverloadedStrings #-}

module Wireform.Base64.Test (spec) where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import Test.QuickCheck
import Test.Syd
import Wireform.Base64


spec :: Spec
spec = describe "Wireform.Base64" $ do
  describe "RFC 4648 sec 10 test vectors" $ do
    it "encodes the canonical vectors" $ do
      encodeBase64 "" `shouldBe` ""
      encodeBase64 "f" `shouldBe` "Zg=="
      encodeBase64 "fo" `shouldBe` "Zm8="
      encodeBase64 "foo" `shouldBe` "Zm9v"
      encodeBase64 "foob" `shouldBe` "Zm9vYg=="
      encodeBase64 "fooba" `shouldBe` "Zm9vYmE="
      encodeBase64 "foobar" `shouldBe` "Zm9vYmFy"

    it "decodes the canonical vectors" $ do
      decodeBase64 "" `shouldBe` Just ""
      decodeBase64 "Zg==" `shouldBe` Just "f"
      decodeBase64 "Zm8=" `shouldBe` Just "fo"
      decodeBase64 "Zm9v" `shouldBe` Just "foo"
      decodeBase64 "Zm9vYg==" `shouldBe` Just "foob"
      decodeBase64 "Zm9vYmE=" `shouldBe` Just "fooba"
      decodeBase64 "Zm9vYmFy" `shouldBe` Just "foobar"

  describe "round-trip" $ do
    it "decodes its own encoding for inputs up to 256 bytes" $
      property $ \(bytes :: [Word8Wrap]) ->
        let bs = BS.pack (map unwrap (take 256 bytes))
        in decodeBase64 (encodeBase64 bs) === Just bs

    it "drives the SIMD body (>= 16 chars)" $ do
      let bs = BS.pack [0 .. 47]
      decodeBase64 (encodeBase64 bs) `shouldBe` Just bs

    it "drives the SIMD body and tail (4096 bytes)" $ do
      let bs = BS.pack (take 4096 (cycle [0 .. 255]))
      decodeBase64 (encodeBase64 bs) `shouldBe` Just bs

  describe "rejection" $ do
    it "rejects non-multiple-of-4 inputs" $
      decodeBase64 "Zm9v=" `shouldBe` Nothing

    it "rejects out-of-alphabet bytes" $
      decodeBase64 "Zm9*" `shouldBe` Nothing

    it "rejects bad padding position" $
      decodeBase64 "Z===" `shouldBe` Nothing

  describe "length helpers" $ do
    it "encodeBase64Length matches the actual encoded length" $
      property $ \n ->
        let n' = abs n `mod` 1024
            bs = BS.replicate n' 0
        in BS.length (encodeBase64 bs) === encodeBase64Length n'


-- Small wrapper to bound Arbitrary Word8 in the property test
-- without dragging in QuickCheck-instances.
newtype Word8Wrap = Word8Wrap {unwrap :: Word8}
  deriving stock (Show)


instance Arbitrary Word8Wrap where
  arbitrary = Word8Wrap . fromIntegral <$> chooseInt (0, 255)
