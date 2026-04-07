module Test.AvroSchemaParse (avroSchemaParseTests) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Avro.Schema
import Avro.Schema.Parse

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
  ]
