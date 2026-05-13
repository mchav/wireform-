module Test.CodeGen (codeGenTests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Proto.AST
import Proto.Parser
import qualified Data.Map.Strict as Map
import Proto.CodeGen
import Proto.CodeGen.Types
import Proto.Annotations

codeGenTests :: TestTree
codeGenTests = testGroup "Code Generation"
  [ testGroup "Type name conversion"
      [ testCase "simple name" $
          hsTypeName "person" @?= "Person"
      , testCase "already capitalized" $
          hsTypeName "Person" @?= "Person"
      , testCase "field name conversion" $
          hsFieldName "first_name" @?= "firstName"
      , testCase "field name no underscore" $
          hsFieldName "name" @?= "name"
      , testCase "enum constructor" $
          hsEnumCon "Status" "STATUS_ACTIVE" @?= "StatusActive"
      , testCase "module name" $
          hsModuleName "com.example.api" @?= "Com.Example.Api"
      ]

  , testGroup "Code generation from parsed proto"
      [ testCase "generates module for simple message" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "package test;"
                , "message Person {"
                , "  string name = 1;"
                , "  int32 age = 2;"
                , "}"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              let emptyReg = Map.empty :: TypeRegistry
                  code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
              assertBool "Should contain data Person" (T.isInfixOf "data Person" code)
              assertBool "Should contain name field" (T.isInfixOf "name" code)
              assertBool "Should contain module header" (T.isInfixOf "module" code)

      , testCase "proto3 optional scalar gets Maybe wrapper" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "package test;"
                , "message Presence {"
                , "  int32 required_field = 1;"
                , "  optional int32 optional_field = 2;"
                , "  optional string optional_name = 3;"
                , "}"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              let emptyReg = Map.empty :: TypeRegistry
                  code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
              assertBool "optional int32 should be Maybe Int32"
                (T.isInfixOf "Maybe Int32" code)
              assertBool "optional string should be Maybe Text"
                (T.isInfixOf "Maybe Text" code)
              assertBool "required int32 should not be Maybe (bare Int32)"
                (T.isInfixOf "Int32" code)

      , testCase "generates enum" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "enum Status {"
                , "  UNKNOWN = 0;"
                , "  ACTIVE = 1;"
                , "}"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              let emptyReg = Map.empty :: TypeRegistry
                  code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
              assertBool "Should contain data Status" (T.isInfixOf "data Status" code)
              assertBool "Should contain Active" (T.isInfixOf "Active" code)
      ]

  , testGroup "Annotations"
      [ testCase "extract custom annotation" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "option (my_annotation) = true;"
                , "option (another_opt) = { key: \"value\" };"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              let anns = extractAnnotations (protoOptions pf)
              length anns @?= 2
              case lookupAnnotation "my_annotation" anns of
                Just (CBool True) -> pure ()
                other -> assertFailure ("Expected CBool True, got: " <> show other)

      , testCase "lookup simple option" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "option java_package = \"com.example\";"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              case lookupSimpleOption "java_package" (protoOptions pf) of
                Just (CString "com.example") -> pure ()
                other -> assertFailure ("Expected CString, got: " <> show other)

      , testCase "extension option lookup" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "option (custom.opt) = 42;"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              case lookupExtensionOption "custom.opt" (protoOptions pf) of
                Just (CInt 42) -> pure ()
                other -> assertFailure ("Expected CInt 42, got: " <> show other)

      , testCase "hasOption check" $ do
          let input = unlines'
                [ "syntax = \"proto3\";"
                , "option deprecated = true;"
                ]
          case parseProtoFile "<test>" input of
            Left e -> assertFailure (show e)
            Right pf -> do
              assertBool "Should have deprecated" (hasOption "deprecated" (protoOptions pf))
              assertBool "Should not have java_package" (not (hasOption "java_package" (protoOptions pf)))

      , testCase "typed option extraction" $ do
          optionAsInt (CInt 42) @?= Just 42
          optionAsFloat (CFloat 3.14) @?= Just 3.14
          optionAsBool (CBool True) @?= Just True
          optionAsString (CString "hello") @?= Just "hello"
          optionAsIdent (CIdent "FOO") @?= Just "FOO"
          optionAsAggregate (CAggregate [("k", CInt 1)]) @?= Just [("k", CInt 1)]
          optionAsInt (CString "nope") @?= Nothing
      ]
  ]

unlines' :: [Text] -> Text
unlines' = mconcat . fmap (<> "\n")
