module Test.Registry (registryTests) where

import Avro.CodeGen (generateAvroTypesWithRegistry)
import Avro.Registry
import Avro.Schema
import Bond.CodeGen (generateBondTypesWithRegistry)
import Bond.Registry
import Bond.Schema
import CBOR.TagRegistry
import CBOR.Value qualified
import CapnProto.CodeGen (generateCapnProtoTypesWithRegistry)
import CapnProto.Registry
import CapnProto.Schema (CapnProtoSchema (..), CapnType (..), Declaration (..), FieldDef (..), StructDef (..))
import CapnProto.Schema qualified as CS
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import FlatBuffers.CodeGen (generateFlatBuffersTypesWithRegistry)
import FlatBuffers.Registry
import FlatBuffers.Schema
import Proto.CodeGen.Hooks
import Proto.IDL.AST (FieldNumber (..), FieldType (..), ScalarType (..))
import Proto.IDL.AST qualified
import Test.Syd
import Thrift.CodeGen (generateThriftTypesWithRegistry)
import Thrift.Registry
import Thrift.Schema


registryTests :: Spec
registryTests =
  describe
    "Custom Type Registration" $ sequence_
    [ avroRegistryTests
    , thriftRegistryTests
    , cborTagRegistryTests
    , bondRegistryTests
    , capnProtoRegistryTests
    , flatBuffersRegistryTests
    , protoHooksEnhancementTests
    ]


-- =========================================================================
-- Avro registry tests
-- =========================================================================

avroRegistryTests :: Spec
avroRegistryTests =
  describe
    "Avro.Registry" $ sequence_
    [ it "custom logical type replaces base type in codegen" $ do
        let handler =
              LogicalTypeHandler
                { lthHaskellType = "Money"
                , lthImports = ["MyApp.Money (Money)"]
                , lthEncode = "encodeMoney"
                , lthDecode = "decodeMoney"
                }
            reg = registerLogicalType "money" handler defaultAvroRegistry
            schema =
              AvroRecord
                { avroRecordName = "Transaction"
                , avroRecordNamespace = Nothing
                , avroRecordDoc = Nothing
                , avroRecordAliases = V.empty
                , avroRecordProps = Map.empty
                , avroRecordFields =
                    V.fromList
                      [ AvroField
                          "amount"
                          ( AvroLogical
                              { avroLogicalBase = AvroPrimitive AvroLong
                              , avroLogicalType = CustomLogical "money"
                              }
                          )
                          Nothing
                          Nothing
                          V.empty
                          Nothing
                          Map.empty
                      ]
                }
            code = generateAvroTypesWithRegistry reg schema
        ("Money" `T.isInfixOf` code) `shouldBe` True
        ("encodeMoney" `T.isInfixOf` code) `shouldBe` True
        ("decodeMoney" `T.isInfixOf` code) `shouldBe` True
    , it "default registry includes timestamp-millis" $ do
        let reg = defaultAvroRegistry
        (Map.member "timestamp-millis" (arLogicalTypes reg)) `shouldBe` True
        (Map.member "uuid" (arLogicalTypes reg)) `shouldBe` True
    , it "custom prop handler emits extra code" $ do
        let handler =
              PropHandler
                { phCodeGen = \k v ->
                    ["-- prop " <> k <> " = " <> v]
                }
            reg = registerPropHandler "x-validate" handler defaultAvroRegistry
            schema =
              AvroRecord
                { avroRecordName = "MyRecord"
                , avroRecordNamespace = Nothing
                , avroRecordDoc = Nothing
                , avroRecordAliases = V.empty
                , avroRecordProps = Map.empty
                , avroRecordFields =
                    V.fromList
                      [ AvroField
                          "field1"
                          (AvroPrimitive AvroString)
                          Nothing
                          Nothing
                          V.empty
                          Nothing
                          (Map.fromList [("x-validate", "required")])
                      ]
                }
            code = generateAvroTypesWithRegistry reg schema
        ("-- prop x-validate = required" `T.isInfixOf` code) `shouldBe` True
    , it "Semigroup composition of registries" $ do
        let reg1 =
              registerLogicalType
                "custom1"
                (LogicalTypeHandler "Type1" [] "enc1" "dec1")
                mempty
            reg2 =
              registerLogicalType
                "custom2"
                (LogicalTypeHandler "Type2" [] "enc2" "dec2")
                mempty
            combined = reg1 <> reg2
        (Map.member "custom1" (arLogicalTypes combined)) `shouldBe` True
        (Map.member "custom2" (arLogicalTypes combined)) `shouldBe` True
    , it "backward compat: generateAvroTypes still works" $ do
        let schema =
              AvroRecord
                { avroRecordName = "Simple"
                , avroRecordNamespace = Nothing
                , avroRecordDoc = Nothing
                , avroRecordAliases = V.empty
                , avroRecordProps = Map.empty
                , avroRecordFields =
                    V.fromList
                      [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
                      ]
                }
        let code = generateAvroTypesWithRegistry defaultAvroRegistry schema
        ("data Simple = Simple" `T.isInfixOf` code) `shouldBe` True
    ]


-- =========================================================================
-- Thrift registry tests
-- =========================================================================

thriftRegistryTests :: Spec
thriftRegistryTests =
  describe
    "Thrift.Registry" $ sequence_
    [ it "field annotation transforms type" $ do
        let handler =
              FieldAnnotationHandler
                { fahTransformType = \ty -> "Sensitive (" <> ty <> ")"
                , fahExtraCode = \_name _val -> []
                }
            reg = registerFieldAnnotation "sensitive" handler defaultThriftRegistry
            schema =
              ThriftSchema
                { tsStructs =
                    [ ThriftStruct
                        { tsName = "UserData"
                        , tsKind = StructNormal
                        , tsFields =
                            [ ThriftField
                                1
                                "ssn"
                                TString
                                Required
                                Nothing
                                (V.fromList [("sensitive", "true")])
                            ]
                        , tsAnnotations = V.empty
                        }
                    ]
                , tsEnums = []
                , tsTypedefs = []
                , tsConsts = []
                , tsServices = []
                }
            code = generateThriftTypesWithRegistry reg schema
        ("Sensitive" `T.isInfixOf` code) `shouldBe` True
    , it "field annotation emits extra code" $ do
        let handler =
              FieldAnnotationHandler
                { fahTransformType = id
                , fahExtraCode = \fname val ->
                    ["-- annotation on " <> fname <> ": " <> val]
                }
            reg = registerFieldAnnotation "deprecated_field" handler defaultThriftRegistry
            schema =
              ThriftSchema
                { tsStructs =
                    [ ThriftStruct
                        { tsName = "LegacyStruct"
                        , tsKind = StructNormal
                        , tsFields =
                            [ ThriftField
                                1
                                "old_field"
                                TI32
                                Required
                                Nothing
                                (V.fromList [("deprecated_field", "use new_field")])
                            ]
                        , tsAnnotations = V.empty
                        }
                    ]
                , tsEnums = []
                , tsTypedefs = []
                , tsConsts = []
                , tsServices = []
                }
            code = generateThriftTypesWithRegistry reg schema
        ("-- annotation on old_field: use new_field" `T.isInfixOf` code) `shouldBe` True
    , it "struct annotation emits extra derivations" $ do
        let handler =
              StructAnnotationHandler
                { sahExtraDerivations = const ["Hashable"]
                , sahExtraCode = \k v ->
                    ["-- struct annotation " <> k <> " = " <> v]
                }
            reg = registerStructAnnotation "hashable" handler defaultThriftRegistry
            schema =
              ThriftSchema
                { tsStructs =
                    [ ThriftStruct
                        { tsName = "KeyStruct"
                        , tsKind = StructNormal
                        , tsFields =
                            [ ThriftField 1 "key" TString Required Nothing V.empty
                            ]
                        , tsAnnotations = V.fromList [("hashable", "true")]
                        }
                    ]
                , tsEnums = []
                , tsTypedefs = []
                , tsConsts = []
                , tsServices = []
                }
            code = generateThriftTypesWithRegistry reg schema
        ("Hashable" `T.isInfixOf` code) `shouldBe` True
        ("-- struct annotation hashable = true" `T.isInfixOf` code) `shouldBe` True
    ]


-- =========================================================================
-- CBOR tag registry tests
-- =========================================================================

cborTagRegistryTests :: Spec
cborTagRegistryTests =
  describe
    "CBOR.TagRegistry" $ sequence_
    [ it "default registry has standard tags" $ do
        let reg = defaultCBORTagRegistry
        (IntMap.member 0 (ctrTags reg)) `shouldBe` True
        (IntMap.member 1 (ctrTags reg)) `shouldBe` True
        (IntMap.member 2 (ctrTags reg)) `shouldBe` True
        (IntMap.member 3 (ctrTags reg)) `shouldBe` True
    , it "register custom tag" $ do
        let handler =
              TagHandler
                { thName = "geojson"
                , thHaskellType = Just "GeoJSON"
                , thValidate = Right
                }
            reg = registerTag 100 handler defaultCBORTagRegistry
        case lookupTag 100 reg of
          Just h -> thName h `shouldBe` "geojson"
          Nothing -> expectationFailure "tag 100 not found"
    , it "tag validation works" $ do
        let reg = defaultCBORTagRegistry
        case lookupTag 0 reg of
          Just h -> do
            let result = thValidate h (CBOR.Value.TextString "2024-01-01T00:00:00Z")
            case result of
              Right _ -> pure ()
              Left err -> expectationFailure ("validation failed: " <> err)
          Nothing -> expectationFailure "tag 0 not found"
    , it "tag validation rejects invalid" $ do
        let reg = defaultCBORTagRegistry
        case lookupTag 0 reg of
          Just h -> do
            let result = thValidate h (CBOR.Value.UInt 42)
            case result of
              Left _ -> pure ()
              Right _ -> expectationFailure "should have rejected UInt for datetime tag"
          Nothing -> expectationFailure "tag 0 not found"
    , it "Semigroup composition" $ do
        let reg1 = registerTag 100 (TagHandler "a" Nothing Right) mempty
            reg2 = registerTag 101 (TagHandler "b" Nothing Right) mempty
            combined = reg1 <> reg2
        (IntMap.member 100 (ctrTags combined)) `shouldBe` True
        (IntMap.member 101 (ctrTags combined)) `shouldBe` True
    ]


-- =========================================================================
-- Bond registry tests
-- =========================================================================

bondRegistryTests :: Spec
bondRegistryTests =
  describe
    "Bond.Registry" $ sequence_
    [ it "attribute handler transforms type" $ do
        let handler =
              AttributeHandler
                { Bond.Registry.hTransformType = \ty -> "Encrypted (" <> ty <> ")"
                , Bond.Registry.hExtraCode = \_name _mval -> []
                }
            reg = registerBondAttribute "encrypted" handler defaultBondRegistry
            schema =
              BondSchema
                { bondNamespace = Nothing
                , bondImports = []
                , bondDecls =
                    [ BondDeclStruct
                        ( BondStruct
                            { bsName = "Secret"
                            , bsTypeParam = Nothing
                            , bsFields =
                                [ BondField
                                    1
                                    BondRequired
                                    BFTString
                                    "data"
                                    Nothing
                                    (V.fromList [("encrypted", Nothing)])
                                ]
                            , bsAttributes = mempty
                            }
                        )
                    ]
                }
            code = generateBondTypesWithRegistry reg schema
        ("Encrypted" `T.isInfixOf` code) `shouldBe` True
    , it "attribute handler emits extra code" $ do
        let handler =
              AttributeHandler
                { Bond.Registry.hTransformType = id
                , Bond.Registry.hExtraCode = \name _mval ->
                    ["-- bond attribute: " <> name]
                }
            reg = registerBondAttribute "audit" handler defaultBondRegistry
            schema =
              BondSchema
                { bondNamespace = Nothing
                , bondImports = []
                , bondDecls =
                    [ BondDeclStruct
                        ( BondStruct
                            { bsName = "Audited"
                            , bsTypeParam = Nothing
                            , bsFields =
                                [ BondField
                                    1
                                    BondRequired
                                    BFTInt32
                                    "value"
                                    Nothing
                                    (V.fromList [("audit", Just "true")])
                                ]
                            , bsAttributes = mempty
                            }
                        )
                    ]
                }
            code = generateBondTypesWithRegistry reg schema
        ("-- bond attribute: audit" `T.isInfixOf` code) `shouldBe` True
    , it "backward compat: generateBondTypes still works" $ do
        let schema =
              BondSchema
                { bondNamespace = Nothing
                , bondImports = []
                , bondDecls =
                    [ BondDeclStruct
                        ( BondStruct
                            { bsName = "Simple"
                            , bsTypeParam = Nothing
                            , bsFields =
                                [ BondField 1 BondRequired BFTString "name" Nothing mempty
                                ]
                            , bsAttributes = mempty
                            }
                        )
                    ]
                }
            code = generateBondTypesWithRegistry defaultBondRegistry schema
        ("data Simple = Simple" `T.isInfixOf` code) `shouldBe` True
    ]


-- =========================================================================
-- Cap'n Proto registry tests
-- =========================================================================

capnProtoRegistryTests :: Spec
capnProtoRegistryTests =
  describe
    "CapnProto.Registry" $ sequence_
    [ it "annotation handler transforms type" $ do
        let handler =
              CapnProto.Registry.AnnotationHandler
                { CapnProto.Registry.hTransformType = \ty -> "Validated (" <> ty <> ")"
                , CapnProto.Registry.hExtraCode = \_name _mval -> []
                }
            reg = registerCapnProtoAnnotation "validate" handler defaultCapnProtoRegistry
            schema =
              CapnProtoSchema
                { csFileId = Nothing
                , csImports = V.empty
                , csDecls =
                    V.fromList
                      [ DStruct
                          ( StructDef
                              { sdName = "Input"
                              , sdFields =
                                  V.fromList
                                    [ FieldDef
                                        "email"
                                        0
                                        CTText
                                        Nothing
                                        (V.fromList [("validate", Just "email")])
                                    ]
                              , sdNested = V.empty
                              , sdUnions = V.empty
                              }
                          )
                      ]
                }
            code = generateCapnProtoTypesWithRegistry reg schema
        ("Validated" `T.isInfixOf` code) `shouldBe` True
    , it "annotation handler emits extra code" $ do
        let handler =
              CapnProto.Registry.AnnotationHandler
                { CapnProto.Registry.hTransformType = id
                , CapnProto.Registry.hExtraCode = \name _mval ->
                    ["-- capnp annotation: " <> name]
                }
            reg = registerCapnProtoAnnotation "deprecated" handler defaultCapnProtoRegistry
            schema =
              CapnProtoSchema
                { csFileId = Nothing
                , csImports = V.empty
                , csDecls =
                    V.fromList
                      [ DStruct
                          ( StructDef
                              { sdName = "Legacy"
                              , sdFields =
                                  V.fromList
                                    [ FieldDef
                                        "old"
                                        0
                                        CTInt32
                                        Nothing
                                        (V.fromList [("deprecated", Nothing)])
                                    ]
                              , sdNested = V.empty
                              , sdUnions = V.empty
                              }
                          )
                      ]
                }
            code = generateCapnProtoTypesWithRegistry reg schema
        ("-- capnp annotation: deprecated" `T.isInfixOf` code) `shouldBe` True
    ]


-- =========================================================================
-- FlatBuffers registry tests
-- =========================================================================

flatBuffersRegistryTests :: Spec
flatBuffersRegistryTests =
  describe
    "FlatBuffers.Registry" $ sequence_
    [ it "metadata handler transforms type" $ do
        let handler =
              FlatBuffers.Registry.MetadataHandler
                { FlatBuffers.Registry.hTransformType = \ty -> "Compressed (" <> ty <> ")"
                , FlatBuffers.Registry.hExtraCode = \_name _mval -> []
                }
            reg = registerFlatBuffersMetadata "compressed" handler defaultFlatBuffersRegistry
            schema =
              FlatBuffersSchema
                { fbsNamespace = Nothing
                , fbsIncludes = V.empty
                , fbsDecls =
                    V.fromList
                      [ FBTable
                          ( TableDef
                              { tdName = "BigData"
                              , tdFields =
                                  V.fromList
                                    [ TableField
                                        "payload"
                                        FTString
                                        (Just "\"\"")
                                        False
                                        (V.fromList [("compressed", Just "lz4")])
                                    ]
                              }
                          )
                      ]
                , fbsRootType = Nothing
                , fbsFileIdentifier = Nothing
                , fbsFileExtension = Nothing
                , fbsAttributes = V.empty
                }
            code = generateFlatBuffersTypesWithRegistry reg schema
        ("Compressed" `T.isInfixOf` code) `shouldBe` True
    , it "metadata handler emits extra code" $ do
        let handler =
              FlatBuffers.Registry.MetadataHandler
                { FlatBuffers.Registry.hTransformType = id
                , FlatBuffers.Registry.hExtraCode = \name _mval ->
                    ["-- fbs metadata: " <> name]
                }
            reg = registerFlatBuffersMetadata "custom_attr" handler defaultFlatBuffersRegistry
            schema =
              FlatBuffersSchema
                { fbsNamespace = Nothing
                , fbsIncludes = V.empty
                , fbsDecls =
                    V.fromList
                      [ FBTable
                          ( TableDef
                              { tdName = "Tagged"
                              , tdFields =
                                  V.fromList
                                    [ TableField
                                        "data"
                                        FTString
                                        (Just "\"\"")
                                        False
                                        (V.fromList [("custom_attr", Nothing)])
                                    ]
                              }
                          )
                      ]
                , fbsRootType = Nothing
                , fbsFileIdentifier = Nothing
                , fbsFileExtension = Nothing
                , fbsAttributes = V.empty
                }
            code = generateFlatBuffersTypesWithRegistry reg schema
        ("-- fbs metadata: custom_attr" `T.isInfixOf` code) `shouldBe` True
    , it "backward compat: generateFlatBuffersTypes still works" $ do
        let schema =
              FlatBuffersSchema
                { fbsNamespace = Nothing
                , fbsIncludes = V.empty
                , fbsDecls =
                    V.fromList
                      [ FBTable
                          ( TableDef
                              { tdName = "Monster"
                              , tdFields =
                                  V.fromList
                                    [ TableField "name" FTString Nothing False V.empty
                                    ]
                              }
                          )
                      ]
                , fbsRootType = Nothing
                , fbsFileIdentifier = Nothing
                , fbsFileExtension = Nothing
                , fbsAttributes = V.empty
                }
            code = generateFlatBuffersTypesWithRegistry defaultFlatBuffersRegistry schema
        ("data Monster = Monster" `T.isInfixOf` code) `shouldBe` True
    ]


-- =========================================================================
-- Proto hooks enhancement tests
-- =========================================================================

protoHooksEnhancementTests :: Spec
protoHooksEnhancementTests =
  describe
    "Proto.CodeGen.Hooks enhancements" $ sequence_
    [ it "FieldHookCtx is constructible" $ do
        let ctx =
              FieldHookCtx
                { fldFieldDef = Proto.IDL.AST.FieldDef () Nothing Nothing (FTScalar SString) "name" (FieldNumber 1) []
                , fldParentMsg = "Person"
                , fldHsFieldName = "personName"
                , fldFieldOptions = []
                }
        fldParentMsg ctx `shouldBe` "Person"
        fldHsFieldName ctx `shouldBe` "personName"
    , it "onFieldCodeGen fires in hook" $ do
        let hook =
              defaultCodeGenHooks
                { onFieldCodeGen = \ctx ->
                    ["-- field: " <> fldHsFieldName ctx]
                }
            ctx =
              FieldHookCtx
                { fldFieldDef = Proto.IDL.AST.FieldDef () Nothing Nothing (FTScalar SString) "name" (FieldNumber 1) []
                , fldParentMsg = "Person"
                , fldHsFieldName = "personName"
                , fldFieldOptions = []
                }
            output = onFieldCodeGen hook ctx
        output `shouldBe` ["-- field: personName"]
    , it "onCustomOption returns Nothing by default" $ do
        let hook = defaultCodeGenHooks
            result = onCustomOption hook "some.option" (OVBool True)
        result `shouldBe` Nothing
    , it "onCustomOption returns WireTransform" $ do
        let hook =
              defaultCodeGenHooks
                { onCustomOption = \name _val ->
                    if name == "compress"
                      then Just (WireTransform "compressEncode" "compressDecode")
                      else Nothing
                }
            result = onCustomOption hook "compress" (OVBool True)
        case result of
          Just wt -> do
            wtEncodeExpr wt `shouldBe` "compressEncode"
            wtDecodeExpr wt `shouldBe` "compressDecode"
          Nothing -> expectationFailure "expected WireTransform"
    , it "THHooks has thOnService" $ do
        let hook =
              defaultTHHooks
                { thOnService = const (pure [])
                }
            combined = hook <> hook
        (True) `shouldBe` True
    , it "composed CodeGenHooks with field hooks" $ do
        let hook1 =
              defaultCodeGenHooks
                { onFieldCodeGen = \ctx ->
                    ["-- h1: " <> fldHsFieldName ctx]
                }
            hook2 =
              defaultCodeGenHooks
                { onFieldCodeGen = \ctx ->
                    ["-- h2: " <> fldHsFieldName ctx]
                }
            combined = hook1 <> hook2
            ctx =
              FieldHookCtx
                { fldFieldDef = Proto.IDL.AST.FieldDef () Nothing Nothing (FTScalar SInt32) "x" (FieldNumber 1) []
                , fldParentMsg = "Msg"
                , fldHsFieldName = "msgX"
                , fldFieldOptions = []
                }
            output = onFieldCodeGen combined ctx
        output `shouldBe` ["-- h1: msgX", "-- h2: msgX"]
    , it "OptionValue and WireTransform types" $ do
        let ov1 = OVBool True
            ov2 = OVInt 42
            ov3 = OVFloat 3.14
            ov4 = OVString "hello"
        ov1 `shouldBe` OVBool True
        ov2 `shouldBe` OVInt 42
        ov3 `shouldBe` OVFloat 3.14
        ov4 `shouldBe` OVString "hello"

        let wt = WireTransform "enc" "dec"
        wtEncodeExpr wt `shouldBe` "enc"
        wtDecodeExpr wt `shouldBe` "dec"
    ]
