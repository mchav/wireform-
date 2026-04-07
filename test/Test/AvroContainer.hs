module Test.AvroContainer (avroContainerTests) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Avro.Container
import Avro.Schema
import qualified Avro.Value as AV

avroContainerTests :: TestTree
avroContainerTests = testGroup "Avro Container"
  [ testCase "writeContainer/readContainer roundtrip — primitives" $ do
      let schema = AvroPrimitive AvroInt
          vals = V.fromList [AV.Int 1, AV.Int 2, AV.Int 42, AV.Int (-7)]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure $ "readContainer failed: " ++ err
        Right (schema', vals') -> do
          schema' @?= schema
          vals' @?= vals

  , testCase "roundtrip — record schema" $ do
      let schema = AvroRecord
            { avroRecordName      = "Person"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordFields    = V.fromList
                [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing
                , AvroField "age"  (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing
                ]
            }
          vals = V.fromList
            [ AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
            , AV.Record (V.fromList [AV.String "Bob", AV.Int 25])
            ]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure $ "readContainer failed: " ++ err
        Right (schema', vals') -> do
          vals' @?= vals

  , testCase "roundtrip — empty values" $ do
      let schema = AvroPrimitive AvroString
          vals = V.empty
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' @?= V.empty

  , testCase "roundtrip — single value" $ do
      let schema = AvroPrimitive AvroLong
          vals = V.singleton (AV.Long 999999)
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' @?= vals

  , testCase "roundtrip — array schema" $ do
      let schema = AvroArray (AvroPrimitive AvroInt)
          vals = V.fromList
            [ AV.Array (V.fromList [AV.Int 1, AV.Int 2])
            , AV.Array V.empty
            , AV.Array (V.singleton (AV.Int 42))
            ]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' @?= vals

  , testCase "magic bytes present" $ do
      let schema = AvroPrimitive AvroNull
          bs = writeContainer schema V.empty
      BS.take 4 bs @?= BS.pack [0x4F, 0x62, 0x6A, 0x01]

  , testCase "invalid magic rejected" $ do
      let bs = BS.pack [0x00, 0x00, 0x00, 0x00]
      case readContainer bs of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error for invalid magic"

  , testCase "ContainerHeader fields" $ do
      let schema = AvroPrimitive AvroInt
          vals = V.singleton (AV.Int 1)
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> assertFailure err
        Right (s, _) -> s @?= schema
  ]
