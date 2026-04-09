module Test.ISLParser (islParserTests) where

import qualified Data.Text as T
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Ion.ISLSchema
import Ion.SchemaLang

islParserTests :: TestTree
islParserTests = testGroup "ISL Parser"
  [ testCase "parse schema with struct type, field constraints, valid_values" $ do
      let input = T.pack $ unlines
            [ "schema_header::{}"
            , "type::{"
            , "  name: person,"
            , "  type: struct,"
            , "  fields: {"
            , "    name: { type: string, occurs: required },"
            , "    age: { type: int, valid_values: range::[0, 150] },"
            , "    email: { type: string, occurs: optional },"
            , "  }"
            , "}"
            , "schema_footer::{}"
            ]
      case parseISL input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (islTypes schema) @?= 1
          let ty = islTypes schema V.! 0
          islTypeName ty @?= "person"
          islBaseType ty @?= Just "struct"
          case islFields ty of
            Nothing -> assertFailure "expected fields"
            Just fields -> do
              V.length fields @?= 3
              let ISLField fn1 ft1 = fields V.! 0
              fn1 @?= "name"
              islBaseType ft1 @?= Just "string"
              islOccurs ft1 @?= Just ORequired
              let ISLField fn2 ft2 = fields V.! 1
              fn2 @?= "age"
              islBaseType ft2 @?= Just "int"
              case islValidValues ft2 of
                Just (RangeVal (Just 0) (Just 150)) -> pure ()
                other -> assertFailure ("expected RangeVal 0..150, got " ++ show other)
              let ISLField fn3 ft3 = fields V.! 2
              fn3 @?= "email"
              islBaseType ft3 @?= Just "string"
              islOccurs ft3 @?= Just OOptional

  , testCase "parse minimal schema without header/footer" $ do
      let input = T.pack $ unlines
            [ "type::{"
            , "  name: simple,"
            , "  type: int"
            , "}"
            ]
      case parseISL input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (islTypes schema) @?= 1
          let ty = islTypes schema V.! 0
          islTypeName ty @?= "simple"
          islBaseType ty @?= Just "int"

  , testCase "parse multiple type definitions" $ do
      let input = T.pack $ unlines
            [ "schema_header::{}"
            , "type::{"
            , "  name: name_type,"
            , "  type: string"
            , "}"
            , "type::{"
            , "  name: age_type,"
            , "  type: int"
            , "}"
            , "schema_footer::{}"
            ]
      case parseISL input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (islTypes schema) @?= 2
          islTypeName (islTypes schema V.! 0) @?= "name_type"
          islTypeName (islTypes schema V.! 1) @?= "age_type"

  , testCase "parse empty schema" $ do
      let input = T.pack $ unlines
            [ "schema_header::{}"
            , "schema_footer::{}"
            ]
      case parseISL input of
        Left err -> assertFailure err
        Right schema -> do
          V.length (islTypes schema) @?= 0
          V.length (islImports schema) @?= 0
  ]
