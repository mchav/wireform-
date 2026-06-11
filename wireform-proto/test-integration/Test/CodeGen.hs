module Test.CodeGen (codeGenTests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Proto.CodeGen
import Proto.CodeGen.Types
import Proto.IDL.AST
import Proto.IDL.Annotations
import Proto.IDL.Parser
import Test.Syd


codeGenTests :: Spec
codeGenTests =
  describe
    "Code Generation"
    $ sequence_
      [ describe
          "Type name conversion"
          $ sequence_
            [ it "simple name" $
                hsTypeName "person" `shouldBe` "Person"
            , it "already capitalized" $
                hsTypeName "Person" `shouldBe` "Person"
            , it "field name conversion" $
                hsFieldName "first_name" `shouldBe` "firstName"
            , it "field name no underscore" $
                hsFieldName "name" `shouldBe` "name"
            , it "enum constructor" $
                hsEnumCon "Status" "STATUS_ACTIVE" `shouldBe` "StatusActive"
            , it "module name" $
                hsModuleName "com.example.api" `shouldBe` "Com.Example.Api"
            ]
      , describe
          "Code generation from parsed proto"
          $ sequence_
            [ it "generates module for simple message" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "package test;"
                        , "message Person {"
                        , "  string name = 1;"
                        , "  int32 age = 2;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "data Person" code) `shouldBe` True
                    (T.isInfixOf "name" code) `shouldBe` True
                    (T.isInfixOf "module" code) `shouldBe` True
            , it "proto3 optional scalar gets Maybe wrapper" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "package test;"
                        , "message Presence {"
                        , "  int32 required_field = 1;"
                        , "  optional int32 optional_field = 2;"
                        , "  optional string optional_name = 3;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "Maybe Int32" code) `shouldBe` True
                    (T.isInfixOf "Maybe Text" code) `shouldBe` True
                    (T.isInfixOf "Int32" code) `shouldBe` True
            , it "generates enum" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "enum Status {"
                        , "  UNKNOWN = 0;"
                        , "  ACTIVE = 1;"
                        , "}"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let emptyReg = Map.empty :: TypeRegistry
                        code = generateModuleText defaultGenerateOpts emptyReg "<test>" pf
                    (T.isInfixOf "data Status" code) `shouldBe` True
                    (T.isInfixOf "Active" code) `shouldBe` True
            ]
      , describe
          "Annotations"
          $ sequence_
            [ it "extract custom annotation" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (my_annotation) = true;"
                        , "option (another_opt) = { key: \"value\" };"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    let anns = extractAnnotations (protoOptions pf)
                    length anns `shouldBe` 2
                    case lookupAnnotation "my_annotation" anns of
                      Just (CBool True) -> pure ()
                      other -> expectationFailure ("Expected CBool True, got: " <> show other)
            , it "lookup simple option" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option java_package = \"com.example\";"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    case lookupSimpleOption "java_package" (protoOptions pf) of
                      Just (CString "com.example") -> pure ()
                      other -> expectationFailure ("Expected CString, got: " <> show other)
            , it "extension option lookup" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option (custom.opt) = 42;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    case lookupExtensionOption "custom.opt" (protoOptions pf) of
                      Just (CInt 42) -> pure ()
                      other -> expectationFailure ("Expected CInt 42, got: " <> show other)
            , it "hasOption check" $ do
                let input =
                      unlines'
                        [ "syntax = \"proto3\";"
                        , "option deprecated = true;"
                        ]
                case parseProtoFile "<test>" input of
                  Left e -> expectationFailure (show e)
                  Right pf -> do
                    (hasOption "deprecated" (protoOptions pf)) `shouldBe` True
                    (not (hasOption "java_package" (protoOptions pf))) `shouldBe` True
            , it "typed option extraction" $ do
                optionAsInt (CInt 42) `shouldBe` Just 42
                optionAsFloat (CFloat 3.14) `shouldBe` Just 3.14
                optionAsBool (CBool True) `shouldBe` Just True
                optionAsString (CString "hello") `shouldBe` Just "hello"
                optionAsIdent (CIdent "FOO") `shouldBe` Just "FOO"
                optionAsAggregate (CAggregate [("k", CInt 1)]) `shouldBe` Just [("k", CInt 1)]
                optionAsInt (CString "nope") `shouldBe` Nothing
            ]
      ]


unlines' :: [Text] -> Text
unlines' = mconcat . fmap (<> "\n")
