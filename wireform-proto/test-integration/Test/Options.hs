module Test.Options (optionsTests) where

import Data.Text (Text)
import Data.Text qualified as T
import Proto.IDL.AST
import Proto.IDL.Options
import Proto.IDL.Parser
import Test.Tasty
import Test.Tasty.HUnit


optionsTests :: TestTree
optionsTests =
  testGroup
    "Standard Options"
    [ testGroup
        "File options"
        [ testCase "java_package" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option java_package = \"com.example\";"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foJavaPackage fo @?= Just "com.example"
        , testCase "go_package" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option go_package = \"example.com/pkg\";"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foGoPackage fo @?= Just "example.com/pkg"
        , testCase "optimize_for" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option optimize_for = LITE_RUNTIME;"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foOptimizeFor fo @?= LiteRuntime
        , testCase "deprecated file" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option deprecated = true;"
                      ]
                fo = extractFileOptions (protoOptions pf)
            foDeprecated fo @?= True
        , testCase "defaults when no options" $ do
            let pf = parseOrDie "syntax = \"proto3\";\n"
                fo = extractFileOptions (protoOptions pf)
            foJavaPackage fo @?= Nothing
            foGoPackage fo @?= Nothing
            foOptimizeFor fo @?= Speed
            foDeprecated fo @?= False
        ]
    , testGroup
        "Field options"
        [ testCase "deprecated field" $ do
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
                  fldDeprecated fo @?= True
                _ -> assertFailure "expected one field"
              _ -> assertFailure "expected one message"
        , testCase "json_name" $ do
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
                  fldJsonName fo @?= Just "fullName"
                _ -> assertFailure "expected one field"
              _ -> assertFailure "expected one message"
        ]
    , testGroup
        "Enum options"
        [ testCase "allow_alias" $ do
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
                eoAllowAlias eo @?= True
              _ -> assertFailure "expected one enum"
        ]
    , testGroup
        "Cross-language packages"
        [ testCase "extractLanguagePackages" $ do
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
            lpProtoPackage lp @?= Just "example.api"
            lpJavaPackage lp @?= Just "com.example.api"
            lpGoPackage lp @?= Just "example.com/api"
            lpCsharpNamespace lp @?= Just "Example.Api"
            lpHaskellModule lp @?= "Example.Api"
        ]
    , testGroup
        "Deprecation helpers"
        [ testCase "deprecatedFields" $ do
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
                length deps @?= 1
                fieldName (head deps) @?= "old"
              _ -> assertFailure "expected one message"
        , testCase "isDeprecated" $ do
            isDeprecated [] @?= False
            let opt = OptionDef () (OptionName [SimpleOption "deprecated"]) (CBool True)
            isDeprecated [opt] @?= True
        ]
    ]


parseOrDie :: Text -> ProtoFile
parseOrDie src = case parseProtoFile "<test>" src of
  Left err -> error ("Parse failed: " <> show err)
  Right pf -> pf
