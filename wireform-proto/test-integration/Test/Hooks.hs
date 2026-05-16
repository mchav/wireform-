module Test.Hooks (hooksTests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Proto.CodeGen
import Proto.CodeGen.Hooks
import Proto.IDL.AST
import Proto.IDL.Parser
import Test.Tasty
import Test.Tasty.HUnit


hooksTests :: TestTree
hooksTests =
  testGroup
    "CodeGen Hooks"
    [ testGroup
        "Hook context construction"
        [ testCase "message hook receives correct type name" $ do
            let hook =
                  mempty
                    { onMessageCodeGen = \ctx ->
                        ["-- type: " <> mhcHsTypeName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "package test;"
                      , "message Person {"
                      , "  string name = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Hook output should appear in generated code"
              (T.isInfixOf "-- type: Person" code)
        , testCase "message hook receives FQ proto name" $ do
            let hook =
                  mempty
                    { onMessageCodeGen = \ctx ->
                        ["-- fq: " <> mhcFqProtoName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "package my.pkg;"
                      , "message Foo {"
                      , "  int32 x = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Hook output should contain FQ name"
              (T.isInfixOf "-- fq: my.pkg.Foo" code)
        , testCase "enum hook receives correct type name" $ do
            let hook =
                  mempty
                    { onEnumCodeGen = \ctx ->
                        ["-- enum: " <> ehcHsTypeName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "enum Status {"
                      , "  UNKNOWN = 0;"
                      , "  ACTIVE = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Hook output should appear for enum"
              (T.isInfixOf "-- enum: Status" code)
        , testCase "file hook receives module name" $ do
            let hook =
                  mempty
                    { onFileCodeGen = \ctx ->
                        ["-- module: " <> fhcModuleName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook}
                pf = parseOrDie "syntax = \"proto3\";\n"
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Hook output should appear at file level"
              (T.isInfixOf "-- module: Proto.Gen" code)
        ]
    , testGroup
        "Attribute-driven hooks"
        [ testCase "onMessageAttribute fires when attribute present" $ do
            let hook = onMessageAttribute "my_custom" $ \val ctx ->
                  case val of
                    CBool True -> ["-- custom triggered on " <> mhcHsTypeName ctx]
                    _ -> []
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Annotated {"
                      , "  option (my_custom) = true;"
                      , "  string name = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Attribute hook should fire"
              (T.isInfixOf "-- custom triggered on Annotated" code)
        , testCase "onMessageAttribute does not fire without attribute" $ do
            let hook = onMessageAttribute "my_custom" $ \_ ctx ->
                  ["-- should not appear for " <> mhcHsTypeName ctx]
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Plain {"
                      , "  string name = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Attribute hook should not fire"
              (not (T.isInfixOf "-- should not appear" code))
        , testCase "onEnumAttribute fires with matching option" $ do
            let hook = onEnumAttribute "special_enum" $ \val ctx ->
                  case val of
                    CString s -> ["-- special: " <> s <> " on " <> ehcHsTypeName ctx]
                    _ -> []
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "enum Color {"
                      , "  option (special_enum) = \"rainbow\";"
                      , "  RED = 0;"
                      , "  GREEN = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "Enum attribute hook should fire"
              (T.isInfixOf "-- special: rainbow on Color" code)
        , testCase "onFileAttribute fires with matching file option" $ do
            let hook = onFileAttribute "codegen_extra" $ \val ctx ->
                  case val of
                    CBool True -> ["-- extra codegen for " <> fhcModuleName ctx]
                    _ -> []
                opts = defaultGenerateOpts {genHooks = hook}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "option (codegen_extra) = true;"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool
              "File attribute hook should fire"
              (T.isInfixOf "-- extra codegen for" code)
        ]
    , testGroup
        "Hook composition"
        [ testCase "composed hooks both produce output" $ do
            let hook1 =
                  mempty
                    { onMessageCodeGen = \ctx ->
                        ["-- hook1: " <> mhcHsTypeName ctx]
                    }
                hook2 =
                  mempty
                    { onMessageCodeGen = \ctx ->
                        ["-- hook2: " <> mhcHsTypeName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook1 <> hook2}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Msg {"
                      , "  int32 x = 1;"
                      , "}"
                      ]
                code = generateModuleText opts Map.empty "<test>" pf
            assertBool "hook1 output present" (T.isInfixOf "-- hook1: Msg" code)
            assertBool "hook2 output present" (T.isInfixOf "-- hook2: Msg" code)
        , testCase "mempty produces no extra output" $ do
            let opts1 = defaultGenerateOpts
                opts2 = defaultGenerateOpts {genHooks = mempty}
                pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message M { int32 x = 1; }"
                      ]
                code1 = generateModuleText opts1 Map.empty "<test>" pf
                code2 = generateModuleText opts2 Map.empty "<test>" pf
            code1 @?= code2
        ]
    , testGroup
        "Attribute query helpers"
        [ testCase "lookupAttribute finds extension option" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "my_opt"]) (CInt 42)]
            lookupAttribute "my_opt" opts @?= Just (CInt 42)
        , testCase "lookupAttribute returns Nothing for missing" $ do
            let opts = [OptionDef () (OptionName [SimpleOption "java_package"]) (CString "com.example")]
            lookupAttribute "java_package" opts @?= Nothing
        , testCase "hasAttribute" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "present"]) (CBool True)]
            hasAttribute "present" opts @?= True
            hasAttribute "absent" opts @?= False
        , testCase "attributeAsText" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "tag"]) (CString "hello")]
            attributeAsText "tag" opts @?= Just "hello"
            attributeAsText "missing" opts @?= Nothing
        , testCase "attributeAsBool" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "flag"]) (CBool True)]
            attributeAsBool "flag" opts @?= Just True
        , testCase "attributeAsInt" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "count"]) (CInt 99)]
            attributeAsInt "count" opts @?= Just 99
        , testCase "attributeAsFloat" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "rate"]) (CFloat 3.14)]
            attributeAsFloat "rate" opts @?= Just 3.14
        , testCase "attributeAsAggregate" $ do
            let agg = [("key", CString "val")]
                opts = [OptionDef () (OptionName [ExtensionOption "meta"]) (CAggregate agg)]
            attributeAsAggregate "meta" opts @?= Just agg
        , testCase "type mismatch returns Nothing" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "x"]) (CInt 1)]
            attributeAsText "x" opts @?= Nothing
            attributeAsBool "x" opts @?= Nothing
        ]
    , testGroup
        "messageOptions extraction"
        [ testCase "extracts MEOption from message elements" $ do
            let pf =
                  parseOrDie $
                    T.unlines
                      [ "syntax = \"proto3\";"
                      , "message Msg {"
                      , "  option (my_opt) = true;"
                      , "  option deprecated = true;"
                      , "  string name = 1;"
                      , "}"
                      ]
            case protoTopLevels pf of
              [TLMessage msg] -> do
                let opts = messageOptions msg
                length opts @?= 2
              _ -> assertFailure "expected one message"
        ]
    , testGroup
        "THHooks construction"
        [ testCase "defaultTHHooks is mempty" $ do
            let h1 = defaultTHHooks
                h2 = mempty :: THHooks
            -- Both should be constructible (type-checks)
            assertBool "defaultTHHooks should be the identity" True
        , testCase "THHooks compose with <>" $ do
            let h1 = defaultTHHooks
                h2 = defaultTHHooks
                combined = h1 <> h2
            assertBool "THHooks should compose" True
        , testCase "thOnMessageAttribute constructs without error" $ do
            let hook = thOnMessageAttribute "my_attr" $ \_val _ctx ->
                  pure []
            assertBool "thOnMessageAttribute should construct" True
        , testCase "thOnEnumAttribute constructs without error" $ do
            let hook = thOnEnumAttribute "my_attr" $ \_val _ctx ->
                  pure []
            assertBool "thOnEnumAttribute should construct" True
        , testCase "thOnFileAttribute constructs without error" $ do
            let hook = thOnFileAttribute "my_attr" $ \_val _ctx ->
                  pure []
            assertBool "thOnFileAttribute should construct" True
        ]
    ]


parseOrDie :: Text -> ProtoFile
parseOrDie src = case parseProtoFile "<test>" src of
  Left err -> error ("Parse failed: " <> show err)
  Right pf -> pf
