{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the enriched record / error metadata module
(KIP-359 / 597 / 843 / 1054 / 1218).
-}
module Client.RecordMetadataSpec (tests) where

import Kafka.Client.RecordMetadata qualified as R
import Test.Syd


tests :: Spec
tests =
  describe "Record metadata helpers" $
    sequence_
      [ it
          "utf8HeaderSerde round-trips text"
          utf8_round_trip
      , it
          "doubleHeaderSerde round-trips"
          double_round_trip
      , it
          "readHeader returns Nothing for missing header"
          missing_header
      , it
          "readHeader surfaces decode errors as Left"
          bad_header
      , it
          "withLeaderEpoch sets the field"
          with_leader_epoch
      , it
          "isCorruptRecordError matches PECorruptRecord"
          corrupt_check
      , it
          "producerErrorMessage is human-readable for every constructor"
          every_message
      ]


utf8_round_trip :: IO ()
utf8_round_trip = do
  let b = R.writeHeader R.utf8HeaderSerde "hello"
  R.readHeader "k" R.utf8HeaderSerde [("k", b)] `shouldBe` Just (Right "hello")


double_round_trip :: IO ()
double_round_trip = do
  let b = R.writeHeader R.doubleHeaderSerde 3.14
  case R.readHeader "k" R.doubleHeaderSerde [("k", b)] of
    Just (Right d) -> (abs (d - 3.14) < 1e-9) `shouldBe` True
    _ -> error "expected Just (Right ...)"


missing_header :: IO ()
missing_header =
  R.readHeader "absent" R.utf8HeaderSerde [] `shouldBe` Nothing


bad_header :: IO ()
bad_header =
  -- Invalid UTF-8 byte 0xC0 is recognised by decodeUtf8' as bad.
  case R.readHeader "k" R.utf8HeaderSerde [("k", "\xC0\xC0")] of
    Just (Left _) -> pure ()
    other -> error ("expected Left, got " <> show other)


with_leader_epoch :: IO ()
with_leader_epoch = do
  let r = R.EnrichedRecord "t" 0 0 0 Nothing "v" [] Nothing Nothing
      r' = R.withLeaderEpoch r 7
  R.erLeaderEpoch r' `shouldBe` Just 7


corrupt_check :: IO ()
corrupt_check = do
  R.isCorruptRecordError (R.PECorruptRecord "x") `shouldBe` True
  R.isCorruptRecordError (R.PEAccumulatorClosed) `shouldBe` False


every_message :: IO ()
every_message = do
  -- Just verify each constructor produces a non-empty, non-trivial
  -- message; if a future constructor is added without a matching
  -- branch GHC's exhaustiveness warning will fire on the case
  -- expression in producerErrorMessage.
  let cases =
        [ R.PEDeliveryTimeout 1000
        , R.PEAccumulatorClosed
        , R.PEAccumulatorFull
        , R.PEBrokerError 17 "x"
        , R.PERequestFailed "y"
        , R.PEFenced "z"
        , R.PEAuthorizationFailed "no"
        , R.PERecordTooLarge 9999
        , R.PECorruptRecord "crc"
        , R.PEUnknown "?"
        ]
  mapM_ (\e -> (length (show (R.producerErrorMessage e)) > 4) `shouldBe` True) cases
