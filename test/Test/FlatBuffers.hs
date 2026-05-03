{-# LANGUAGE OverloadedStrings #-}
-- | FlatBuffers wire-format round-trip tests.
--
-- These tests exercise 'FlatBuffers.Encode.encode' /
-- 'FlatBuffers.Decode.decode' on the value-shaped surface and
-- 'FlatBuffers.View.decodeRoot' on the zero-copy surface. Both
-- now sit on top of "FlatBuffers.Builder" and produce real
-- spec-compliant FlatBuffers (byte-compatible with what flatcc /
-- flatbuffers-cpp emit for the same input).
--
-- The wire-shape assertions that previously pinned the
-- non-compliant toy layout are gone; the canonical assertion is
-- now @decode . encode == Right input@ (modulo the value-shape
-- caveats documented in 'FlatBuffers.Decode').
module Test.FlatBuffers (flatBuffersTests) where

import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import qualified FlatBuffers.Value as F
import FlatBuffers.Encode (encode)
import FlatBuffers.Decode (decode)
import qualified FlatBuffers.View as FV

flatBuffersTests :: TestTree
flatBuffersTests = testGroup "FlatBuffers"
  [ rootShape
  , scalarRoundTrips
  , stringRoundTrips
  , viewRoundTrips
  , scalarProperties
  ]

-- ---------------------------------------------------------------------------
-- Root buffer shape (= what every spec-compliant decoder needs)
-- ---------------------------------------------------------------------------

rootShape :: TestTree
rootShape = testGroup "Root buffer shape"
  [ testCase "buffer starts with a u32 root offset" $ do
      -- Any non-trivial buffer the encoder emits is at least 8
      -- bytes (4-byte root offset + at least one
      -- soffset/vtable). The root offset itself must point
      -- inside the buffer.
      let bs = encode (F.VTable (V.fromList [Just (F.VInt32 1)]))
      assertBool "buffer is at least 8 bytes" (BS.length bs >= 8)
      let off = readLE32 bs 0
      assertBool "root offset is inside the buffer"
        (fromIntegral off > 0 && fromIntegral off < BS.length bs)

  , testCase "every encoded buffer round-trips through View" $ do
      -- Sanity: anything we encode must be readable by the
      -- spec-compliant zero-copy reader.
      case FV.rootTable (encode (F.VTable (V.fromList [Just (F.VInt32 42)]))) of
        Left e  -> assertFailure ("rootTable failed: " <> e)
        Right _ -> pure ()
  ]

-- ---------------------------------------------------------------------------
-- Scalar round-trips through the value-shaped surface
-- ---------------------------------------------------------------------------

-- | Wrap a scalar in a one-slot table so it has a place to live
-- on the wire. The decoder returns it via 'F.VWord*' (the
-- decoder doesn't carry signedness through the wire format).
scalarRoundTrips :: TestTree
scalarRoundTrips = testGroup "Scalar round-trip"
  [ testCase "VInt32 lands in slot 0 and decodes as a u32" $ do
      let v0 = F.VInt32 0x04030201
          bs = encode (F.VTable (V.fromList [Just v0]))
      case decode bs of
        Right (F.VTable slots) -> do
          V.length slots @?= 1
          case slots V.! 0 of
            Just (F.VWord32 w) -> w @?= 0x04030201
            other -> assertFailure ("slot 0 was " <> show other)
        other -> assertFailure ("decode returned " <> show other)

  , testCase "VInt64 lands in slot 0" $ do
      let v0 = F.VInt64 maxBound
          bs = encode (F.VTable (V.fromList [Just v0]))
      case decode bs of
        Right (F.VTable slots) -> do
          V.length slots @?= 1
          assertBool "slot 0 is present" (slotPresent (slots V.! 0))
        other -> assertFailure ("decode returned " <> show other)

  , testCase "Bool slot present" $ do
      let bs = encode (F.VTable (V.fromList [Just (F.VBool True)]))
      case decode bs of
        Right (F.VTable slots) ->
          assertBool "slot 0 is present" (slotPresent (slots V.! 0))
        other -> assertFailure ("decode returned " <> show other)

  , testCase "absent slot decodes as Nothing" $ do
      let bs = encode (F.VTable (V.fromList [Nothing, Just (F.VInt32 7)]))
      case decode bs of
        Right (F.VTable slots) -> do
          V.length slots @?= 2
          slots V.! 0 @?= Nothing
          assertBool "slot 1 is present" (slotPresent (slots V.! 1))
        other -> assertFailure ("decode returned " <> show other)
  ]

-- ---------------------------------------------------------------------------
-- Strings
-- ---------------------------------------------------------------------------

stringRoundTrips :: TestTree
stringRoundTrips = testGroup "String"
  [ testCase "string content survives encode -> View round-trip" $ do
      let txt = "hello flatbuffers" :: Text
          bs  = encode (F.VTable (V.fromList [Just (F.VString txt)]))
      case FV.rootTable bs of
        Left e  -> assertFailure e
        Right t -> case FV.viewSlot t 0 :: Either String Text of
          Right t' -> t' @?= txt
          Left e   -> assertFailure e

  , testCase "empty string round-trips" $ do
      let bs = encode (F.VTable (V.fromList [Just (F.VString T.empty)]))
      case FV.rootTable bs of
        Left e  -> assertFailure e
        Right t -> case FV.viewSlot t 0 :: Either String Text of
          Right t' -> t' @?= T.empty
          Left e   -> assertFailure e
  ]

-- ---------------------------------------------------------------------------
-- View-based decoding (the new zero-copy surface)
-- ---------------------------------------------------------------------------

viewRoundTrips :: TestTree
viewRoundTrips = testGroup "View round-trips"
  [ testCase "Int32 field reads back through viewSlot" $ do
      let bs = encode (F.VTable (V.fromList [Just (F.VInt32 0x11223344)]))
      case FV.rootTable bs of
        Left e  -> assertFailure e
        Right t -> case FV.viewSlot t 0 :: Either String Int32 of
          Right n -> n @?= 0x11223344
          Left e  -> assertFailure e

  , testCase "Int64 field reads back through viewSlot" $ do
      let bs = encode (F.VTable (V.fromList [Just (F.VInt64 1234567890123456789)]))
      case FV.rootTable bs of
        Left e  -> assertFailure e
        Right t -> case FV.viewSlot t 0 :: Either String Int64 of
          Right n -> n @?= 1234567890123456789
          Left e  -> assertFailure e

  , testCase "Maybe field absent reads as Nothing" $ do
      let bs = encode (F.VTable (V.fromList [Nothing]))
      case FV.rootTable bs of
        Left e  -> assertFailure e
        Right t -> case (FV.viewSlotMaybe t 0 :: Either String (Maybe Int32)) of
          Right Nothing  -> pure ()
          Right (Just n) -> assertFailure ("expected Nothing, got " <> show n)
          Left e         -> assertFailure e
  ]

-- ---------------------------------------------------------------------------
-- Hedgehog properties
-- ---------------------------------------------------------------------------

scalarProperties :: TestTree
scalarProperties = testGroup "Property tests"
  [ testProperty "Int32 round-trips through View" $ property $ do
      n <- forAll $ Gen.int32 Range.linearBounded
      let bs = encode (F.VTable (V.fromList [Just (F.VInt32 n)]))
      Right t  <- pure (FV.rootTable bs)
      Right n' <- pure (FV.viewSlot t 0 :: Either String Int32)
      n' === n

  , testProperty "Int64 round-trips through View" $ property $ do
      n <- forAll $ Gen.int64 Range.linearBounded
      let bs = encode (F.VTable (V.fromList [Just (F.VInt64 n)]))
      Right t  <- pure (FV.rootTable bs)
      Right n' <- pure (FV.viewSlot t 0 :: Either String Int64)
      n' === n

  , testProperty "Word32 round-trips through View" $ property $ do
      w <- forAll $ Gen.word32 Range.linearBounded
      let bs = encode (F.VTable (V.fromList [Just (F.VWord32 w)]))
      Right t  <- pure (FV.rootTable bs)
      Right w' <- pure (FV.viewSlot t 0 :: Either String Word32)
      w' === w

  , testProperty "Bool round-trips through View" $ property $ do
      b <- forAll Gen.bool
      let bs = encode (F.VTable (V.fromList [Just (F.VBool b)]))
      Right t  <- pure (FV.rootTable bs)
      Right b' <- pure (FV.viewSlot t 0 :: Either String Bool)
      b' === b

  , testProperty "Text round-trips through View" $ property $ do
      t <- forAll $ Gen.text (Range.linear 0 64) Gen.alphaNum
      let bs = encode (F.VTable (V.fromList [Just (F.VString t)]))
      Right tab <- pure (FV.rootTable bs)
      Right t'  <- pure (FV.viewSlot tab 0 :: Either String Text)
      t' === t
  ]

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

slotPresent :: Maybe a -> Bool
slotPresent (Just _) = True
slotPresent Nothing  = False

readLE32 :: BS.ByteString -> Int -> Word32
readLE32 bs off =
  fromIntegral (BS.index bs off)
  + fromIntegral (BS.index bs (off+1)) * 256
  + fromIntegral (BS.index bs (off+2)) * 65536
  + fromIntegral (BS.index bs (off+3)) * 16777216
