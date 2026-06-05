module Test.AvroContainer (avroContainerTests) where

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Syd

import Avro.Container
import Avro.Decode (decodeAvroResolved)
import Avro.Encode (encodeAvro)
import Avro.Schema
import qualified Avro.Value as AV

avroContainerTests :: Spec
avroContainerTests = describe "Avro Container" $ sequence_
  [ it "writeContainer/readContainer roundtrip — primitives" $ do
      let schema = AvroPrimitive AvroInt
          vals = V.fromList [AV.Int 1, AV.Int 2, AV.Int 42, AV.Int (-7)]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer failed: " ++ err
        Right (schema', vals') -> do
          schema' `shouldBe` schema
          vals' `shouldBe` vals

  , it "roundtrip — record schema" $ do
      let schema = AvroRecord
            { avroRecordName      = "Person"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "age"  (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
                ]
            }
          vals = V.fromList
            [ AV.Record (V.fromList [AV.String "Alice", AV.Int 30])
            , AV.Record (V.fromList [AV.String "Bob", AV.Int 25])
            ]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer failed: " ++ err
        Right (schema', vals') -> do
          vals' `shouldBe` vals

  , it "roundtrip — empty values" $ do
      let schema = AvroPrimitive AvroString
          vals = V.empty
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' `shouldBe` V.empty

  , it "roundtrip — single value" $ do
      let schema = AvroPrimitive AvroLong
          vals = V.singleton (AV.Long 999999)
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' `shouldBe` vals

  , it "roundtrip — array schema" $ do
      let schema = AvroArray (AvroPrimitive AvroInt)
          vals = V.fromList
            [ AV.Array (V.fromList [AV.Int 1, AV.Int 2])
            , AV.Array V.empty
            , AV.Array (V.singleton (AV.Int 42))
            ]
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer failed: " ++ err
        Right (_, vals') ->
          vals' `shouldBe` vals

  , it "magic bytes present" $ do
      let schema = AvroPrimitive AvroNull
          bs = writeContainer schema V.empty
      BS.take 4 bs `shouldBe` BS.pack [0x4F, 0x62, 0x6A, 0x01]

  , it "invalid magic rejected" $ do
      let bs = BS.pack [0x00, 0x00, 0x00, 0x00]
      case readContainer bs of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected error for invalid magic"

  , it "ContainerHeader fields" $ do
      let schema = AvroPrimitive AvroInt
          vals = V.singleton (AV.Int 1)
          bs = writeContainer schema vals
      case readContainer bs of
        Left err -> expectationFailure err
        Right (s, _) -> s `shouldBe` schema

  , it "decodeAvroResolved — writer fewer fields than reader" $ do
      let writerSchema = AvroRecord
            { avroRecordName      = "Rec"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "a" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty ]
            }
          readerSchema = AvroRecord
            { avroRecordName      = "Rec"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "a" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "b" (AvroPrimitive AvroString) (Just AvroString) Nothing V.empty Nothing Map.empty
                ]
            }
          writerVal = AV.Record (V.fromList [AV.Int 42])
          encoded = encodeAvro writerSchema writerVal
      case decodeAvroResolved writerSchema readerSchema encoded of
        Left err -> expectationFailure $ "decodeAvroResolved failed: " ++ err
        Right resolved ->
          resolved `shouldBe` AV.Record (V.fromList [AV.Int 42, AV.String ""])

  , it "deflate codec — write and read back" $ do
      let schema = AvroPrimitive AvroInt
          vals = V.fromList [AV.Int 10, AV.Int 20, AV.Int 30]
          bs = writeContainerWith "deflate" schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer (deflate) failed: " ++ err
        Right (schema', vals') -> do
          schema' `shouldBe` schema
          vals' `shouldBe` vals

  , it "deflate codec — record roundtrip" $ do
      let schema = AvroRecord
            { avroRecordName      = "Event"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "id"   (AvroPrimitive AvroLong) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                ]
            }
          vals = V.fromList
            [ AV.Record (V.fromList [AV.Long 1, AV.String "click"])
            , AV.Record (V.fromList [AV.Long 2, AV.String "view"])
            , AV.Record (V.fromList [AV.Long 3, AV.String "purchase"])
            ]
          bs = writeContainerWith "deflate" schema vals
      case readContainer bs of
        Left err -> expectationFailure $ "readContainer (deflate record) failed: " ++ err
        Right (_, vals') ->
          vals' `shouldBe` vals

  , it "readContainerResolved — added field with default" $ do
      let writerSchema = AvroRecord
            { avroRecordName      = "Msg"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "id" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty ]
            }
          readerSchema = AvroRecord
            { avroRecordName      = "Msg"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "id"   (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "tag"  (AvroPrimitive AvroString) (Just AvroString) Nothing V.empty Nothing Map.empty
                ]
            }
          vals = V.fromList
            [ AV.Record (V.fromList [AV.Int 1])
            , AV.Record (V.fromList [AV.Int 2])
            ]
          containerBytes = writeContainer writerSchema vals
      case readContainerResolved readerSchema containerBytes of
        Left err -> expectationFailure $ "readContainerResolved failed: " ++ err
        Right resolved -> do
          V.length resolved `shouldBe` 2
          resolved V.! 0 `shouldBe` AV.Record (V.fromList [AV.Int 1, AV.String ""])
          resolved V.! 1 `shouldBe` AV.Record (V.fromList [AV.Int 2, AV.String ""])

  , it "readContainerResolved with deflate codec" $ do
      let writerSchema = AvroRecord
            { avroRecordName      = "Item"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "x" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty ]
            }
          readerSchema = AvroRecord
            { avroRecordName      = "Item"
            , avroRecordNamespace = Nothing
            , avroRecordDoc       = Nothing
            , avroRecordAliases   = V.empty
            , avroRecordProps     = Map.empty
            , avroRecordFields    = V.fromList
                [ AvroField "x" (AvroPrimitive AvroInt) Nothing Nothing V.empty Nothing Map.empty
                , AvroField "y" (AvroPrimitive AvroLong) (Just AvroLong) Nothing V.empty Nothing Map.empty
                ]
            }
          vals = V.fromList [ AV.Record (V.fromList [AV.Int 5]) ]
          containerBytes = writeContainerWith "deflate" writerSchema vals
      case readContainerResolved readerSchema containerBytes of
        Left err -> expectationFailure $ "readContainerResolved (deflate) failed: " ++ err
        Right resolved -> do
          V.length resolved `shouldBe` 1
          resolved V.! 0 `shouldBe` AV.Record (V.fromList [AV.Int 5, AV.Long 0])

  , it "unsupported codec returns error" $ do
      case decompressBlock "wireform-test-unknown-codec" "data" of
        Left err ->
          ("Unsupported codec" `isInfixOf` err) `shouldBe` True
        Right _ -> expectationFailure "expected error for unknown codec"
  ]
