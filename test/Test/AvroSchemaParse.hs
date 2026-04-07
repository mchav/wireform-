module Test.AvroSchemaParse (avroSchemaParseTests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Avro.Schema
import Avro.Schema.Parse
import Avro.JSON (avroSchemaToJSON, avroSchemaFromJSON)

avroSchemaParseTests :: TestTree
avroSchemaParseTests = testGroup "Avro Schema Parse"
  [ testCase "parse primitive null" $ do
      case parseAvroSchema "\"null\"" of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroPrimitive AvroNull

  , testCase "parse primitive string" $ do
      case parseAvroSchema "\"string\"" of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroPrimitive AvroString

  , testCase "parse primitive int" $ do
      case parseAvroSchema "\"int\"" of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroPrimitive AvroInt

  , testCase "parse primitive boolean" $ do
      case parseAvroSchema "\"boolean\"" of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroPrimitive AvroBool

  , testCase "parse record schema" $ do
      let json = BS8.pack $ unlines
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
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordName = name, avroRecordFields = fields} -> do
            name @?= "User"
            V.length fields @?= 2
          _ -> assertFailure "expected record type"

  , testCase "parse enum schema" $ do
      let json = BS8.pack $ unlines
            [ "{"
            , "  \"type\": \"enum\","
            , "  \"name\": \"Color\","
            , "  \"symbols\": [\"RED\", \"GREEN\", \"BLUE\"]"
            , "}"
            ]
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroEnum{avroEnumName = name, avroEnumSymbols = syms} -> do
            name @?= "Color"
            syms @?= V.fromList ["RED", "GREEN", "BLUE"]
          _ -> assertFailure "expected enum type"

  , testCase "parse array schema" $ do
      let json = BS8.pack "{\"type\": \"array\", \"items\": \"int\"}"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroArray (AvroPrimitive AvroInt)

  , testCase "parse map schema" $ do
      let json = BS8.pack "{\"type\": \"map\", \"values\": \"string\"}"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroMap (AvroPrimitive AvroString)

  , testCase "parse union schema" $ do
      let json = BS8.pack "[\"null\", \"string\"]"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> ty @?= AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])

  , testCase "parse fixed schema" $ do
      let json = BS8.pack "{\"type\": \"fixed\", \"name\": \"MD5\", \"size\": 16}"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroFixed{avroFixedName = name, avroFixedSize = sz} -> do
            name @?= "MD5"
            sz @?= 16
          _ -> assertFailure "expected fixed type"

  , testCase "invalid JSON rejected" $ do
      case parseAvroSchema "not valid json" of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error for invalid JSON"

  , testCase "parse all primitive types" $ do
      let prims = [ ("\"null\"", AvroPrimitive AvroNull)
                   , ("\"boolean\"", AvroPrimitive AvroBool)
                   , ("\"int\"", AvroPrimitive AvroInt)
                   , ("\"long\"", AvroPrimitive AvroLong)
                   , ("\"float\"", AvroPrimitive AvroFloat)
                   , ("\"double\"", AvroPrimitive AvroDouble)
                   , ("\"bytes\"", AvroPrimitive AvroBytes)
                   , ("\"string\"", AvroPrimitive AvroString)
                   ]
      mapM_ (\(json, expected) ->
        case parseAvroSchema (BS8.pack json) of
          Left err -> assertFailure $ "Failed to parse " ++ json ++ ": " ++ err
          Right ty -> ty @?= expected
        ) prims

  , testCase "parse logicalType date" $ do
      let json = BS8.pack "{\"type\": \"int\", \"logicalType\": \"date\"}"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroLogical{avroLogicalBase = base, avroLogicalType = lt} -> do
            base @?= AvroPrimitive AvroInt
            lt @?= DateLogical
          _ -> assertFailure "expected AvroLogical"

  , testCase "parse logicalType timestamp-millis" $ do
      let json = BS8.pack "{\"type\": \"long\", \"logicalType\": \"timestamp-millis\"}"
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroLogical{avroLogicalBase = base, avroLogicalType = lt} -> do
            base @?= AvroPrimitive AvroLong
            lt @?= TimestampMillisLogical
          _ -> assertFailure "expected AvroLogical"

  , testCase "logicalType roundtrip" $ do
      let dateType = AvroLogical
            { avroLogicalBase = AvroPrimitive AvroInt
            , avroLogicalType = DateLogical
            }
      let json = avroSchemaToJSON dateType
      case avroSchemaFromJSON json of
        Left err -> assertFailure err
        Right ty -> ty @?= dateType

  , testCase "parse record with aliases" $ do
      let json = BS8.pack $ unlines
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
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordAliases = aliases} ->
            aliases @?= V.fromList ["Person", "Account"]
          _ -> assertFailure "expected record"

  , testCase "aliases roundtrip" $ do
      let recType = AvroRecord
            { avroRecordName = "User"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.fromList ["Person"]
            , avroRecordFields = V.empty
            , avroRecordProps = Map.empty
            }
      let json = avroSchemaToJSON recType
      case avroSchemaFromJSON json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordAliases = aliases} ->
            aliases @?= V.fromList ["Person"]
          _ -> assertFailure "expected record"

  , testCase "parse field order" $ do
      let json = BS8.pack $ unlines
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
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordFields = fields} -> do
            V.length fields @?= 3
            avroFieldOrder (fields V.! 0) @?= Just Ascending
            avroFieldOrder (fields V.! 1) @?= Just Descending
            avroFieldOrder (fields V.! 2) @?= Just Ignore
          _ -> assertFailure "expected record"

  , testCase "field order roundtrip" $ do
      let field = AvroField
            { avroFieldName = "x"
            , avroFieldType = AvroPrimitive AvroInt
            , avroFieldDefault = Nothing
            , avroFieldOrder = Just Descending
            , avroFieldAliases = V.empty
            , avroFieldDoc = Nothing
            , avroFieldProps = Map.empty
            }
          recType = AvroRecord
            { avroRecordName = "R"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordFields = V.singleton field
            , avroRecordProps = Map.empty
            }
      let json = avroSchemaToJSON recType
      case avroSchemaFromJSON json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordFields = fields} ->
            avroFieldOrder (fields V.! 0) @?= Just Descending
          _ -> assertFailure "expected record"

  , testCase "parse field doc and aliases" $ do
      let json = BS8.pack $ unlines
            [ "{"
            , "  \"type\": \"record\","
            , "  \"name\": \"Documented\","
            , "  \"fields\": ["
            , "    {\"name\": \"x\", \"type\": \"int\", \"doc\": \"The X value\", \"aliases\": [\"old_x\"]}"
            , "  ]"
            , "}"
            ]
      case parseAvroSchema json of
        Left err -> assertFailure err
        Right ty -> case ty of
          AvroRecord{avroRecordFields = fields} -> do
            avroFieldDoc (fields V.! 0) @?= Just "The X value"
            avroFieldAliases (fields V.! 0) @?= V.fromList ["old_x"]
          _ -> assertFailure "expected record"
  ]
