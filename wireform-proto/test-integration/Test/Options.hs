module Test.Options (optionsTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Proto.IDL.AST
import Proto.IDL.Options
import Proto.IDL.Parser
import Test.Syd


optionsTests :: Spec
optionsTests =
  describe
    "Standard Options" $ sequence_
    [ describe
        "File options" $ sequence_
        [ it "java_package" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option java_package = \"com.example\";"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foJavaPackage fo `shouldBe` Just "com.example"
        , it "go_package" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option go_package = \"example.com/pkg\";"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foGoPackage fo `shouldBe` Just "example.com/pkg"
        , it "optimize_for" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option optimize_for = LITE_RUNTIME;"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foOptimizeFor fo `shouldBe` LiteRuntime
        , it "deprecated file" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option deprecated = true;"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foDeprecated fo `shouldBe` True
        , it "defaults when no options" $ do
            let pf = parseOrDie "syntax = \"proto3\";\n"
                fo = extractFileOptions (protoOptions pf)
            foJavaPackage fo `shouldBe` Nothing
            foGoPackage fo `shouldBe` Nothing
            foOptimizeFor fo `shouldBe` Speed
            foDeprecated fo `shouldBe` False
        ]
    , describe
        "Field options" $ sequence_
        [ it "deprecated field" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Msg {"
                      , "  string old_name = 1 [deprecated = true];"
                      , "}"
                      ]
            case protoTopLevels pf of
              [TLMessage msg] -> case msgElements msg of
                [MEField fd] -> do
                  let fo = extractFieldOptions (fieldOptions fd)
                  fldDeprecated fo `shouldBe` True
                _ -> expectationFailure "expected one field"
              _ -> expectationFailure "expected one message"
        , it "json_name" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Msg {"
                      , "  string full_name = 1 [json_name = \"fullName\"];"
                      , "}"
                      ]
            case protoTopLevels pf of
              [TLMessage msg] -> case msgElements msg of
                [MEField fd] -> do
                  let fo = extractFieldOptions (fieldOptions fd)
                  fldJsonName fo `shouldBe` Just "fullName"
                _ -> expectationFailure "expected one field"
              _ -> expectationFailure "expected one message"
        ]
    , describe
        "Enum options" $ sequence_
        [ it "allow_alias" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "enum Foo {"
                      , "  option allow_alias = true;"
                      , "  A = 0;"
                      , "  B = 1;"
                      , "  C = 1;"
                      , "}"
                      ]
            case protoTopLevels pf of
              [TLEnum ed] -> do
                let eo = extractEnumOptions (enumOptions ed)
                eoAllowAlias eo `shouldBe` True
              _ -> expectationFailure "expected one enum"
        ]
    , describe
        "Cross-language packages" $ sequence_
        [ it "extractLanguagePackages" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "package example.api;"
                      , "option java_package = \"com.example.api\";"
                      , "option go_package = \"example.com/api\";"
                      , "option csharp_namespace = \"Example.Api\";"
                      ]
                lp = extractLanguagePackages pf
            lpProtoPackage lp `shouldBe` Just "example.api"
            lpJavaPackage lp `shouldBe` Just "com.example.api"
            lpGoPackage lp `shouldBe` Just "example.com/api"
            lpCsharpNamespace lp `shouldBe` Just "Example.Api"
            lpHaskellModule lp `shouldBe` "Example.Api"
        ]
    , describe
        "Deprecation helpers" $ sequence_
        [ it "deprecatedFields" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Msg {"
                      , "  string name = 1;"
                      , "  string old = 2 [deprecated = true];"
                      , "  int32 value = 3;"
                      , "}"
                      ]
            case protoTopLevels pf of
              [TLMessage msg] -> do
                let deps = deprecatedFields msg
                length deps `shouldBe` 1
                fieldName (head deps) `shouldBe` "old"
              _ -> expectationFailure "expected one message"
        , it "isDeprecated" $ do
            isDeprecated [] `shouldBe` False
            let opt = OptionDef () (OptionName [SimpleOption "deprecated"]) (CBool True)
            isDeprecated [opt] `shouldBe` True
        ]
    ]


parseOrDie :: Text -> ProtoFile
parseOrDie src = case parseProtoFile "<test>" src of
  Left err -> error ("Parse failed: " <> show err)
  Right pf -> pf
