module Test.AvroSchemaParse (avroSchemaParseTests) where

import Avro.JSON (avroSchemaFromJSON, avroSchemaToJSON)
import Avro.Schema
import Avro.Schema.Parse
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BS8
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Test.Syd


avroSchemaParseTests :: Spec
avroSchemaParseTests =
  describe "Avro Schema Parse" $
    sequence_
      [ it "parse primitive null" $ do
          case parseAvroSchema "\"null\"" of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroPrimitive AvroNull
      , it "parse primitive string" $ do
          case parseAvroSchema "\"string\"" of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroPrimitive AvroString
      , it "parse primitive int" $ do
          case parseAvroSchema "\"int\"" of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroPrimitive AvroInt
      , it "parse primitive boolean" $ do
          case parseAvroSchema "\"boolean\"" of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroPrimitive AvroBool
      , it "parse record schema" $ do
          let json =
                BS8.pack $
                  unlines
                    [ "{"
                    , "  \"type\": \"record\","
                    , "  \"name\": \"User\","
                    , "  \"fields\": ["
                    , "    {\"name\": \"name\", \"type\": \"string\"},"
                    , "    {\"name\": \"age\", \"type\": \"int\"}"
                    , "  ]"
                    , "}"
                    ]
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordName = name, avroRecordFields = fields} -> do
                name `shouldBe` "User"
                V.length fields `shouldBe` 2
              _ -> expectationFailure "expected record type"
      , it "parse enum schema" $ do
          let json =
                BS8.pack $
                  unlines
                    [ "{"
                    , "  \"type\": \"enum\","
                    , "  \"name\": \"Color\","
                    , "  \"symbols\": [\"RED\", \"GREEN\", \"BLUE\"]"
                    , "}"
                    ]
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroEnum {avroEnumName = name, avroEnumSymbols = syms} -> do
                name `shouldBe` "Color"
                syms `shouldBe` V.fromList ["RED", "GREEN", "BLUE"]
              _ -> expectationFailure "expected enum type"
      , it "parse array schema" $ do
          let json = BS8.pack "{\"type\": \"array\", \"items\": \"int\"}"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroArray (AvroPrimitive AvroInt)
      , it "parse map schema" $ do
          let json = BS8.pack "{\"type\": \"map\", \"values\": \"string\"}"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroMap (AvroPrimitive AvroString)
      , it "parse union schema" $ do
          let json = BS8.pack "[\"null\", \"string\"]"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
      , it "parse fixed schema" $ do
          let json = BS8.pack "{\"type\": \"fixed\", \"name\": \"MD5\", \"size\": 16}"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroFixed {avroFixedName = name, avroFixedSize = sz} -> do
                name `shouldBe` "MD5"
                sz `shouldBe` 16
              _ -> expectationFailure "expected fixed type"
      , it "invalid JSON rejected" $ do
          case parseAvroSchema "not valid json" of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error for invalid JSON"
      , it "parse all primitive types" $ do
          let prims =
                [ ("\"null\"", AvroPrimitive AvroNull)
                , ("\"boolean\"", AvroPrimitive AvroBool)
                , ("\"int\"", AvroPrimitive AvroInt)
                , ("\"long\"", AvroPrimitive AvroLong)
                , ("\"float\"", AvroPrimitive AvroFloat)
                , ("\"double\"", AvroPrimitive AvroDouble)
                , ("\"bytes\"", AvroPrimitive AvroBytes)
                , ("\"string\"", AvroPrimitive AvroString)
                ]
          mapM_
            ( \(json, expected) ->
                case parseAvroSchema (BS8.pack json) of
                  Left err -> expectationFailure $ "Failed to parse " ++ json ++ ": " ++ err
                  Right ty -> ty `shouldBe` expected
            )
            prims
      , it "parse logicalType date" $ do
          let json = BS8.pack "{\"type\": \"int\", \"logicalType\": \"date\"}"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroLogical {avroLogicalBase = base, avroLogicalType = lt} -> do
                base `shouldBe` AvroPrimitive AvroInt
                lt `shouldBe` DateLogical
              _ -> expectationFailure "expected AvroLogical"
      , it "parse logicalType timestamp-millis" $ do
          let json = BS8.pack "{\"type\": \"long\", \"logicalType\": \"timestamp-millis\"}"
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroLogical {avroLogicalBase = base, avroLogicalType = lt} -> do
                base `shouldBe` AvroPrimitive AvroLong
                lt `shouldBe` TimestampMillisLogical
              _ -> expectationFailure "expected AvroLogical"
      , it "logicalType roundtrip" $ do
          let dateType =
                AvroLogical
                  { avroLogicalBase = AvroPrimitive AvroInt
                  , avroLogicalType = DateLogical
                  }
          let json = avroSchemaToJSON dateType
          case avroSchemaFromJSON json of
            Left err -> expectationFailure err
            Right ty -> ty `shouldBe` dateType
      , it "parse record with aliases" $ do
          let json =
                BS8.pack $
                  unlines
                    [ "{"
                    , "  \"type\": \"record\","
                    , "  \"name\": \"User\","
                    , "  \"aliases\": [\"Person\", \"Account\"],"
                    , "  \"fields\": ["
                    , "    {\"name\": \"name\", \"type\": \"string\"}"
                    , "  ]"
                    , "}"
                    ]
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordAliases = aliases} ->
                aliases `shouldBe` V.fromList ["Person", "Account"]
              _ -> expectationFailure "expected record"
      , it "aliases roundtrip" $ do
          let recType =
                AvroRecord
                  { avroRecordName = "User"
                  , avroRecordNamespace = Nothing
                  , avroRecordDoc = Nothing
                  , avroRecordAliases = V.fromList ["Person"]
                  , avroRecordFields = V.empty
                  , avroRecordProps = Map.empty
                  }
          let json = avroSchemaToJSON recType
          case avroSchemaFromJSON json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordAliases = aliases} ->
                aliases `shouldBe` V.fromList ["Person"]
              _ -> expectationFailure "expected record"
      , it "parse field order" $ do
          let json =
                BS8.pack $
                  unlines
                    [ "{"
                    , "  \"type\": \"record\","
                    , "  \"name\": \"SortTest\","
                    , "  \"fields\": ["
                    , "    {\"name\": \"a\", \"type\": \"string\", \"order\": \"ascending\"},"
                    , "    {\"name\": \"b\", \"type\": \"int\", \"order\": \"descending\"},"
                    , "    {\"name\": \"c\", \"type\": \"bytes\", \"order\": \"ignore\"}"
                    , "  ]"
                    , "}"
                    ]
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordFields = fields} -> do
                V.length fields `shouldBe` 3
                avroFieldOrder (fields V.! 0) `shouldBe` Just Ascending
                avroFieldOrder (fields V.! 1) `shouldBe` Just Descending
                avroFieldOrder (fields V.! 2) `shouldBe` Just Ignore
              _ -> expectationFailure "expected record"
      , it "field order roundtrip" $ do
          let field =
                AvroField
                  { avroFieldName = "x"
                  , avroFieldType = AvroPrimitive AvroInt
                  , avroFieldDefault = Nothing
                  , avroFieldOrder = Just Descending
                  , avroFieldAliases = V.empty
                  , avroFieldDoc = Nothing
                  , avroFieldProps = Map.empty
                  }
              recType =
                AvroRecord
                  { avroRecordName = "R"
                  , avroRecordNamespace = Nothing
                  , avroRecordDoc = Nothing
                  , avroRecordAliases = V.empty
                  , avroRecordFields = V.singleton field
                  , avroRecordProps = Map.empty
                  }
          let json = avroSchemaToJSON recType
          case avroSchemaFromJSON json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordFields = fields} ->
                avroFieldOrder (fields V.! 0) `shouldBe` Just Descending
              _ -> expectationFailure "expected record"
      , it "parse field doc and aliases" $ do
          let json =
                BS8.pack $
                  unlines
                    [ "{"
                    , "  \"type\": \"record\","
                    , "  \"name\": \"Documented\","
                    , "  \"fields\": ["
                    , "    {\"name\": \"x\", \"type\": \"int\", \"doc\": \"The X value\", \"aliases\": [\"old_x\"]}"
                    , "  ]"
                    , "}"
                    ]
          case parseAvroSchema json of
            Left err -> expectationFailure err
            Right ty -> case ty of
              AvroRecord {avroRecordFields = fields} -> do
                avroFieldDoc (fields V.! 0) `shouldBe` Just "The X value"
                avroFieldAliases (fields V.! 0) `shouldBe` V.fromList ["old_x"]
              _ -> expectationFailure "expected record"
      ]
