module Test.ISLParser (islParserTests) where

import Data.Text qualified as T
import Data.Vector qualified as V
import Ion.ISLSchema
import Ion.SchemaLang
import Test.Syd


islParserTests :: Spec
islParserTests =
  describe "ISL Parser" $
    sequence_
      [ it "parse schema with struct type, field constraints, valid_values" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema -> do
              V.length (islTypes schema) `shouldBe` 1
              let ty = islTypes schema V.! 0
              islTypeName ty `shouldBe` "person"
              islBaseType ty `shouldBe` Just "struct"
              case islFields ty of
                Nothing -> expectationFailure "expected fields"
                Just fields -> do
                  V.length fields `shouldBe` 3
                  let ISLField fn1 ft1 = fields V.! 0
                  fn1 `shouldBe` "name"
                  islBaseType ft1 `shouldBe` Just "string"
                  islOccurs ft1 `shouldBe` Just ORequired
                  let ISLField fn2 ft2 = fields V.! 1
                  fn2 `shouldBe` "age"
                  islBaseType ft2 `shouldBe` Just "int"
                  case islValidValues ft2 of
                    Just (RangeVal (Just 0) (Just 150)) -> pure ()
                    other -> expectationFailure ("expected RangeVal 0..150, got " ++ show other)
                  let ISLField fn3 ft3 = fields V.! 2
                  fn3 `shouldBe` "email"
                  islBaseType ft3 `shouldBe` Just "string"
                  islOccurs ft3 `shouldBe` Just OOptional
      , it "parse minimal schema without header/footer" $ do
          let input =
                T.pack $
                  unlines
                    [ "type::{"
                    , "  name: simple,"
                    , "  type: int"
                    , "}"
                    ]
          case parseISL input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (islTypes schema) `shouldBe` 1
              let ty = islTypes schema V.! 0
              islTypeName ty `shouldBe` "simple"
              islBaseType ty `shouldBe` Just "int"
      , it "parse multiple type definitions" $ do
          let input =
                T.pack $
                  unlines
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
            Left err -> expectationFailure err
            Right schema -> do
              V.length (islTypes schema) `shouldBe` 2
              islTypeName (islTypes schema V.! 0) `shouldBe` "name_type"
              islTypeName (islTypes schema V.! 1) `shouldBe` "age_type"
      , it "parse empty schema" $ do
          let input =
                T.pack $
                  unlines
                    [ "schema_header::{}"
                    , "schema_footer::{}"
                    ]
          case parseISL input of
            Left err -> expectationFailure err
            Right schema -> do
              V.length (islTypes schema) `shouldBe` 0
              V.length (islImports schema) `shouldBe` 0
      ]
