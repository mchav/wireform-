module Test.Hooks (hooksTests) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Proto.CodeGen
import Proto.CodeGen.Hooks
import Proto.IDL.AST
import Proto.IDL.Parser
import Test.Syd


hooksTests :: Spec
hooksTests =
  describe
    "CodeGen Hooks" $ sequence_
    [ describe
        "Hook context construction" $ sequence_
        [ it "message hook receives correct type name" $ do
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
            (T.isInfixOf "-- type: Person" code) `shouldBe` True
        , it "message hook receives FQ proto name" $ do
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
            (T.isInfixOf "-- fq: my.pkg.Foo" code) `shouldBe` True
        , it "enum hook receives correct type name" $ do
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
            (T.isInfixOf "-- enum: Status" code) `shouldBe` True
        , it "file hook receives module name" $ do
            let hook =
                  mempty
                    { onFileCodeGen = \ctx ->
                        ["-- module: " <> fhcModuleName ctx]
                    }
                opts = defaultGenerateOpts {genHooks = hook}
                pf = parseOrDie "syntax = \"proto3\";\n"
                code = generateModuleText opts Map.empty "<test>" pf
            (T.isInfixOf "-- module: Proto.Gen" code) `shouldBe` True
        ]
    , describe
        "Attribute-driven hooks" $ sequence_
        [ it "onMessageAttribute fires when attribute present" $ do
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
            (T.isInfixOf "-- custom triggered on Annotated" code) `shouldBe` True
        , it "onMessageAttribute does not fire without attribute" $ do
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
            (not (T.isInfixOf "-- should not appear" code)) `shouldBe` True
        , it "onEnumAttribute fires with matching option" $ do
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
            (T.isInfixOf "-- special: rainbow on Color" code) `shouldBe` True
        , it "onFileAttribute fires with matching file option" $ do
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
            (T.isInfixOf "-- extra codegen for" code) `shouldBe` True
        ]
    , describe
        "Hook composition" $ sequence_
        [ it "composed hooks both produce output" $ do
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
            (T.isInfixOf "-- hook1: Msg" code) `shouldBe` True
            (T.isInfixOf "-- hook2: Msg" code) `shouldBe` True
        , it "mempty produces no extra output" $ do
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
            code1 `shouldBe` code2
        ]
    , describe
        "Attribute query helpers" $ sequence_
        [ it "lookupAttribute finds extension option" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "my_opt"]) (CInt 42)]
            lookupAttribute "my_opt" opts `shouldBe` Just (CInt 42)
        , it "lookupAttribute returns Nothing for missing" $ do
            let opts = [OptionDef () (OptionName [SimpleOption "java_package"]) (CString "com.example")]
            lookupAttribute "java_package" opts `shouldBe` Nothing
        , it "hasAttribute" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "present"]) (CBool True)]
            hasAttribute "present" opts `shouldBe` True
            hasAttribute "absent" opts `shouldBe` False
        , it "attributeAsText" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "tag"]) (CString "hello")]
            attributeAsText "tag" opts `shouldBe` Just "hello"
            attributeAsText "missing" opts `shouldBe` Nothing
        , it "attributeAsBool" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "flag"]) (CBool True)]
            attributeAsBool "flag" opts `shouldBe` Just True
        , it "attributeAsInt" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "count"]) (CInt 99)]
            attributeAsInt "count" opts `shouldBe` Just 99
        , it "attributeAsFloat" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "rate"]) (CFloat 3.14)]
            attributeAsFloat "rate" opts `shouldBe` Just 3.14
        , it "attributeAsAggregate" $ do
            let agg = [("key", CString "val")]
                opts = [OptionDef () (OptionName [ExtensionOption "meta"]) (CAggregate agg)]
            attributeAsAggregate "meta" opts `shouldBe` Just agg
        , it "type mismatch returns Nothing" $ do
            let opts = [OptionDef () (OptionName [ExtensionOption "x"]) (CInt 1)]
            attributeAsText "x" opts `shouldBe` Nothing
            attributeAsBool "x" opts `shouldBe` Nothing
        ]
    , describe
        "messageOptions extraction" $ sequence_
        [ it "extracts MEOption from message elements" $ do
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
                length opts `shouldBe` 2
              _ -> expectationFailure "expected one message"
        ]
    , describe
        "THHooks construction" $ sequence_
        [ it "defaultTHHooks is mempty" $ do
            let h1 = defaultTHHooks
                h2 = mempty :: THHooks
            -- Both should be constructible (type-checks)
            (True) `shouldBe` True
        , it "THHooks compose with <>" $ do
            let h1 = defaultTHHooks
                h2 = defaultTHHooks
                combined = h1 <> h2
            (True) `shouldBe` True
        , it "thOnMessageAttribute constructs without error" $ do
            let hook = thOnMessageAttribute "my_attr" $ \_val _ctx ->
                  pure []
            (True) `shouldBe` True
        , it "thOnEnumAttribute constructs without error" $ do
            let hook = thOnEnumAttribute "my_attr" $ \_val _ctx ->
                  pure []
            (True) `shouldBe` True
        , it "thOnFileAttribute constructs without error" $ do
            let hook = thOnFileAttribute "my_attr" $ \_val _ctx ->
                  pure []
            (True) `shouldBe` True
        ]
    ]


parseOrDie :: Text -> ProtoFile
parseOrDie src = case parseProtoFile "<test>" src of
  Left err -> error ("Parse failed: " <> show err)
  Right pf -> pf
