{-# LANGUAGE OverloadedStrings #-}

module Test.Frame (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Test.Syd

import qualified Wireform.Builder as B
import Wireform.Parser.Driver (parseByteString)

import Network.WebSocket.Frame

tests :: Spec
tests = describe "Frame" $ sequence_
  [ rfcVectors
  , roundTrip
  , maskingProperty
  , controlFrameLimit
  ]

------------------------------------------------------------------------
-- RFC 6455 \u00a75.7 wire vectors
------------------------------------------------------------------------

rfcVectors :: Spec
rfcVectors = describe "RFC 6455 sec 5.7 vectors" $ sequence_
  [ it "unmasked text 'Hello'" $ do
      let bytes = BS.pack [0x81, 0x05, 0x48, 0x65, 0x6c, 0x6c, 0x6f]
      case parseByteString (parseFrame defaultPayloadLimit) bytes of
        Right f -> do
          frameFin f       `shouldBe` True
          frameOpcode f    `shouldBe` OpText
          frameMask f      `shouldBe` Nothing
          framePayload f   `shouldBe` "Hello"
        Left e -> expectationFailure ("parse failed: " <> show e)

  , it "masked client text 'Hello'" $ do
      let bytes = BS.pack
            [ 0x81, 0x85
            , 0x37, 0xfa, 0x21, 0x3d
            , 0x7f, 0x9f, 0x4d, 0x51, 0x58
            ]
      case parseByteString (parseFrame defaultPayloadLimit) bytes of
        Right f -> do
          frameOpcode f  `shouldBe` OpText
          framePayload f `shouldBe` "Hello"
          frameMask f    `shouldBe` Just (mkMask 0x37 0xfa 0x21 0x3d)
        Left e -> expectationFailure ("parse failed: " <> show e)

  , it "256-byte unmasked binary (16-bit length encoding)" $ do
      let payload = BS.replicate 256 0xAA
          bytes   = BS.pack [0x82, 0x7E, 0x01, 0x00] <> payload
      case parseByteString (parseFrame defaultPayloadLimit) bytes of
        Right f -> do
          frameOpcode f   `shouldBe` OpBinary
          BS.length (framePayload f) `shouldBe` 256
          framePayload f  `shouldBe` payload
        Left e -> expectationFailure ("parse failed: " <> show e)

  , it "65 536-byte unmasked binary (64-bit length encoding)" $ do
      let n       = 65536 :: Int
          payload = BS.replicate n 0x55
          bytes   = BS.pack
                      [ 0x82, 0x7F
                      , 0, 0, 0, 0
                      , 0, 1, 0, 0
                      ] <> payload
      case parseByteString
              (parseFrame (PayloadLimit (fromIntegral n)))
              bytes of
        Right f -> BS.length (framePayload f) `shouldBe` n
        Left e  -> expectationFailure ("parse failed: " <> show e)
  ]

------------------------------------------------------------------------
-- Round-trip
------------------------------------------------------------------------

roundTrip :: Spec
roundTrip = describe "round-trip" $ sequence_
  [ it "unmasked text" $ do
      let f = textFrame Nothing "round-trip"
      decode (encode f) `shouldBe` Right f

  , it "masked text" $ do
      let m = mkMask 0xCA 0xFE 0xBA 0xBE
          f = textFrame (Just m) "round-trip"
      decode (encode f) `shouldBe` Right f

  , it "ping has tiny payload" $ do
      let f = Frame
            { frameFin     = True
            , frameRsv1    = False
            , frameRsv2    = False
            , frameRsv3    = False
            , frameOpcode  = OpPing
            , frameMask    = Nothing
            , framePayload = "ping?"
            }
      decode (encode f) `shouldBe` Right f

  , it "large payload (128 KiB) round-trips" $ do
      let payload = BS.replicate (128 * 1024) 0x42
          f = Frame
            { frameFin     = True
            , frameRsv1    = False
            , frameRsv2    = False
            , frameRsv3    = False
            , frameOpcode  = OpBinary
            , frameMask    = Nothing
            , framePayload = payload
            }
      case decodeWith (PayloadLimit (256 * 1024)) (encode f) of
        Right f' -> framePayload f' `shouldBe` payload
        Left e   -> expectationFailure ("parse failed: " <> show e)
  ]

------------------------------------------------------------------------
-- Masking
------------------------------------------------------------------------

maskingProperty :: Spec
maskingProperty = it "masking is self-inverse" $ do
  let m = mkMask 0x12 0x34 0x56 0x78
      payload = BS.pack [0..255]
  maskPayload m (maskPayload m payload) `shouldBe` payload

------------------------------------------------------------------------
-- Control frame limit
------------------------------------------------------------------------

controlFrameLimit :: Spec
controlFrameLimit = it "rejects oversized control frame" $ do
  -- Build a ping with a 200-byte payload; the parser itself does
  -- not enforce RFC 6455 sec 5.5 (that is the connection layer's
  -- job) but a 'PayloadLimit' below the announced length must
  -- reject.
  let payload = BS.replicate 200 0xFF
      f = Frame
            { frameFin     = True
            , frameRsv1    = False
            , frameRsv2    = False
            , frameRsv3    = False
            , frameOpcode  = OpPing
            , frameMask    = Nothing
            , framePayload = payload
            }
      bytes = encode f
  case decodeWith (PayloadLimit 64) bytes of
    Left _  -> pure ()
    Right _ -> expectationFailure "expected rejection from PayloadLimit"

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

encode :: Frame -> BS.ByteString
encode = BSL.toStrict . B.toLazyByteString . buildFrame

decode :: BS.ByteString -> Either String Frame
decode = decodeWith defaultPayloadLimit

decodeWith :: PayloadLimit -> BS.ByteString -> Either String Frame
decodeWith lim bs = case parseByteString (parseFrame lim) bs of
  Right f -> Right f
  Left  e -> Left (show e)

textFrame :: Maybe Mask -> BS.ByteString -> Frame
textFrame m p = Frame
  { frameFin     = True
  , frameRsv1    = False
  , frameRsv2    = False
  , frameRsv3    = False
  , frameOpcode  = OpText
  , frameMask    = m
  , framePayload = p
  }
