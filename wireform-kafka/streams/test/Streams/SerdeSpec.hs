{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Streams.SerdeSpec (tests) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.IORef
import Data.Int (Int32)
import qualified Data.Text as T
import Hedgehog ((===), forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Kafka.Streams

tests :: TestTree
tests = testGroup "Serde"
  [ testGroup "round-trip"
      [ testProperty "byteStringSerde" $ property $ do
          bs <- forAll (Gen.bytes (Range.linear 0 256))
          case deserialize byteStringSerde (serialize byteStringSerde bs) of
            Right bs' -> bs === bs'
            Left e    -> fail (T.unpack e)
      , testProperty "textSerde" $ property $ do
          t <- forAll (Gen.text (Range.linear 0 64) Gen.unicode)
          case deserialize textSerde (serialize textSerde t) of
            Right t' -> t === t'
            Left e    -> fail (T.unpack e)
      , testProperty "int32Serde" $ property $ do
          n <- forAll (Gen.int32 Range.constantBounded)
          case deserialize int32Serde (serialize int32Serde n) of
            Right n' -> n === n'
            Left e    -> fail (T.unpack e)
      , testProperty "int64Serde" $ property $ do
          n <- forAll (Gen.int64 Range.constantBounded)
          case deserialize int64Serde (serialize int64Serde n) of
            Right n' -> n === n'
            Left e    -> fail (T.unpack e)
      , testProperty "doubleSerde" $ property $ do
          d <- forAll (Gen.double (Range.linearFracFrom 0 (-1e9) 1e9))
          case deserialize doubleSerde (serialize doubleSerde d) of
            Right d' -> d === d'
            Left e    -> fail (T.unpack e)
      , testProperty "lengthPrefixedSerde" $ property $ do
          t <- forAll (Gen.text (Range.linear 0 32) Gen.unicode)
          let s = lengthPrefixedSerde textSerde
          case deserialize s (serialize s t) of
            Right t' -> t === t'
            Left e    -> fail (T.unpack e)
      , testProperty "prefixedSerde" $ property $ do
          t <- forAll (Gen.text (Range.linear 0 32) Gen.unicode)
          let s = prefixedSerde 0x42 textSerde
          case deserialize s (serialize s t) of
            Right t' -> t === t'
            Left e    -> fail (T.unpack e)
      , testProperty "imap of int32 over string-of-digits" $ property $ do
          n <- forAll (Gen.int32 (Range.constantFrom 0 0 1000))
          let textOfInt :: Serde Int32
              textOfInt = imap (T.pack . show) (read . T.unpack) textSerde
          case deserialize textOfInt (serialize textOfInt n) of
            Right n' -> n === n'
            Left e    -> fail (T.unpack e)
      ]
  , testGroup "specific encodings"
      [ testCase "int32 BE wire format" $
          serialize int32Serde 1 @?= BS.pack [0,0,0,1]
      , testCase "int64 BE wire format" $
          serialize int64Serde 1 @?= BS.pack [0,0,0,0,0,0,0,1]
      , testCase "int32 negative" $
          serialize int32Serde (-1) @?= BS.pack [0xFF,0xFF,0xFF,0xFF]
      , testCase "voidSerde rejects non-empty" $
          case deserialize voidSerde (BS.pack [1]) of
            Left _  -> pure ()
            Right _ -> fail "voidSerde should reject non-empty input"
      , testCase "prefixedSerde rejects wrong tag" $
          let s = prefixedSerde 0x42 textSerde
              encoded = BS.cons 0x99 (serialize textSerde "hello")
           in case deserialize s encoded of
                Left _  -> pure ()
                Right _ -> fail "prefixedSerde should reject wrong tag"
      , testCase "lengthPrefixedSerde rejects truncated header" $
          case deserialize (lengthPrefixedSerde textSerde) (BS.pack [0,0]) of
            Left _  -> pure ()
            Right _ -> fail "should reject"
      , testCase "lengthPrefixedSerde rejects size mismatch" $
          let bogus = BS.pack [0,0,0,5,1,2,3]  -- claims 5 bytes, has 3
           in case deserialize (lengthPrefixedSerde byteStringSerde) bogus of
                Left _  -> pure ()
                Right _ -> fail "should reject"
      ]
  , testGroup "DeserializationHandler"
      [ deser_log_and_continue
      , deser_log_and_fail
      ]
  ]

----------------------------------------------------------------------
-- Deserialization handler integration tests via TopologyTestDriver
----------------------------------------------------------------------

-- A serde that fails to decode anything that isn't the literal
-- @\"good\"@ and otherwise returns the original text.
strictGoodSerde :: Serde T.Text
strictGoodSerde = mkSerde
  (\t -> serialize textSerde t)
  (\b -> case deserialize textSerde b of
           Right t | t == "good" -> Right t
           Right t -> Left ("rejected: " <> t)
           Left e  -> Left e)

deser_log_and_continue :: TestTree
deser_log_and_continue =
  testCase "logAndContinue handler skips bad records, processes good ones" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde strictGoodSerde)
    toTopic (topicName "out") (produced textSerde strictGoodSerde) s
    topo <- buildTopology b
    case validateTopology topo of
      Left  err -> error (show err)
      Right v   -> do
        driver <- newDriverWith v "deser-app" logAndContinue
        pipeInput driver (topicName "in") Nothing (BSC.pack "bad1") (Timestamp 0) 0
        pipeInput driver (topicName "in") Nothing (BSC.pack "good") (Timestamp 1) 0
        pipeInput driver (topicName "in") Nothing (BSC.pack "bad2") (Timestamp 2) 0
        out <- readOutput driver (topicName "out")
        length out @?= 1
        closeDriver driver

deser_log_and_fail :: TestTree
deser_log_and_fail =
  testCase "logAndFail handler raises on the first bad record" $ do
    b <- newStreamsBuilder
    s <- streamFromTopic b (topicName "in") (consumed textSerde strictGoodSerde)
    toTopic (topicName "out") (produced textSerde strictGoodSerde) s
    topo <- buildTopology b
    case validateTopology topo of
      Left  err -> error (show err)
      Right v   -> do
        driver <- newDriverWith v "deser-app" logAndFail
        ePipe <-
          try (pipeInput driver (topicName "in")
                Nothing (BSC.pack "bad") (Timestamp 0) 0)
            :: IO (Either SomeException ())
        case ePipe of
          Left _  -> pure ()
          Right _ -> fail "expected logAndFail to raise"
        closeDriver driver
