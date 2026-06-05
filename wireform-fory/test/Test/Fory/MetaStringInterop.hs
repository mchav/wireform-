{-# LANGUAGE OverloadedStrings #-}
-- | Pinned-byte tests verifying our 'Fory.MetaString.Encoder' and
-- 'Fory.MetaString.Hash' produce output byte-for-byte identical
-- to @pyfory.meta.metastring@ + @pyfory.context.hash_meta_string_data@.
--
-- The expected byte strings and 64-bit hashcodes were captured
-- directly from pyfory 0.17 via @pyfory.meta.metastring.MetaStringEncoder@
-- and committed inline below; if a future pyfory update breaks
-- compatibility we want a tight failure surface here.
module Test.Fory.MetaStringInterop (tests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (showHex)
import Test.Syd

import qualified Fory.MetaString.Encoder as MSE
import qualified Fory.MetaString.Hash as MSH

tests :: Spec
tests = describe "Fory.MetaString.Encoder vs pyfory" $ sequence_
  [ it "namespace 'example' (LOWER_SPECIAL, length 5)" $
      check MSE.namespaceSpecialChars "example"
        MSE.LowerSpecial
        "12e063d640"
        0xea8563ec8fce1001

  , it "typename 'Person' (FIRST_TO_LOWER_SPECIAL, length 4)" $
      check MSE.typenameSpecialChars "Person"
        MSE.FirstToLowerSpecial
        "3c91939a"
        0x97ac20e26842ba03

  , it "typename 'MyType' (LOWER_UPPER_DIGIT_SPECIAL, length 5)" $
      check MSE.typenameSpecialChars "MyType"
        MSE.LowerUpperDigitSpecial
        "4cc5ac1e20"
        0x9129d23a71594702

  , it "field 'camelCase' (ALL_TO_LOWER_SPECIAL, length 7)" $
      check MSE.namespaceSpecialChars "camelCase"
        MSE.AllToLowerSpecial
        "880c22fa204880"
        0x231cc2193c726904

  , it "field 'snake_case' (LOWER_SPECIAL, length 7)" $
      check MSE.namespaceSpecialChars "snake_case"
        MSE.LowerSpecial
        "c9a05136204880"
        0xd8b44642d0f5a201

  , it "namespace 'with.dots' (LOWER_SPECIAL, length 6)" $
      check MSE.namespaceSpecialChars "with.dots"
        MSE.LowerSpecial
        "59133e86e9c8"
        0xa4e0fa301e3d4f01

  , it "typename 'Hello' (FIRST_TO_LOWER_SPECIAL, length 4)" $
      check MSE.typenameSpecialChars "Hello"
        MSE.FirstToLowerSpecial
        "9c8b5b80"
        0xaad3b322bd1aa403

  , it "field 'a' (LOWER_SPECIAL, length 1)" $
      check MSE.namespaceSpecialChars "a"
        MSE.LowerSpecial
        "00"
        0xe01f8b1e4cad3601

  , it "typename 'ABC123' (LOWER_UPPER_DIGIT_SPECIAL, length 5)" $
      check MSE.typenameSpecialChars "ABC123"
        MSE.LowerUpperDigitSpecial
        "34db9aedb8"
        0xb79cd8bd004ee702

  , it "long field (LOWER_SPECIAL, length 30, exercises MurmurHash3)" $
      check MSE.namespaceSpecialChars
        "a_long_field_name_more_than_sixteen_bytes_total"
        MSE.LowerSpecial
        ("036b734db2a08b1eda06136c7449b99c"
         <> "0ddc9179908dd871324b7374c0b0")
        0x0f36e3d542bc8901

  , describe "decoder round-trip" $ sequence_
      [ it ("decodes " ++ show s)
          (MSE.decodeMetaString sc enc bs `shouldBe` s)
      | (s, sc, enc, bs) <-
          [ ("example",     MSE.namespaceSpecialChars, MSE.LowerSpecial,           BS.pack [0x12,0xe0,0x63,0xd6,0x40])
          , ("Person",      MSE.typenameSpecialChars,  MSE.FirstToLowerSpecial,    BS.pack [0x3c,0x91,0x93,0x9a])
          , ("MyType",      MSE.typenameSpecialChars,  MSE.LowerUpperDigitSpecial, BS.pack [0x4c,0xc5,0xac,0x1e,0x20])
          , ("camelCase",   MSE.namespaceSpecialChars, MSE.AllToLowerSpecial,      BS.pack [0x88,0x0c,0x22,0xfa,0x20,0x48,0x80])
          , ("snake_case",  MSE.namespaceSpecialChars, MSE.LowerSpecial,           BS.pack [0xc9,0xa0,0x51,0x36,0x20,0x48,0x80])
          , ("with.dots",   MSE.namespaceSpecialChars, MSE.LowerSpecial,           BS.pack [0x59,0x13,0x3e,0x86,0xe9,0xc8])
          , ("ABC123",      MSE.typenameSpecialChars,  MSE.LowerUpperDigitSpecial, BS.pack [0x34,0xdb,0x9a,0xed,0xb8])
          ]
      ]
  ]
  where
    check sc s expEnc expHexData expHash = do
      let (enc, bs) = MSE.encodeMetaString sc s
          actualHex = concatMap (pad2 . flip showHex "") (BS.unpack bs)
          actualHash = MSH.metaStringHashcode bs
                         (fromIntegral (MSE.encodingId enc) :: Word64)
      enc       `shouldBe` expEnc
      actualHex `shouldBe` expHexData
      actualHash `assertEqHash` expHash

    pad2 s | length s == 1 = '0' : s
           | otherwise     = s

    assertEqHash got expected =
      if got == expected
        then pure ()
        else expectationFailure $ unwords
          [ "hash mismatch:"
          , "got"
          , "0x" ++ pad16 (showHex got "")
          , "vs expected"
          , "0x" ++ pad16 (showHex expected "")
          ]

    pad16 s | length s < 16 = replicate (16 - length s) '0' ++ s
            | otherwise     = s
