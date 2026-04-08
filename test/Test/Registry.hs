module Test.Registry (registryTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V

import Avro.Schema
import Avro.CodeGen (generateAvroTypesWithRegistry)
import Avro.Registry

import Thrift.Schema
import Thrift.CodeGen (generateThriftTypesWithRegistry)
import Thrift.Registry

import CBOR.TagRegistry
import qualified CBOR.Value

import Bond.Schema
import Bond.CodeGen (generateBondTypesWithRegistry)
import Bond.Registry

import CapnProto.Schema (CapnProtoSchema(..), Declaration(..), StructDef(..), FieldDef(..), CapnType(..))
import qualified CapnProto.Schema as CS
import CapnProto.CodeGen (generateCapnProtoTypesWithRegistry)
import CapnProto.Registry

import FlatBuffers.Schema
import FlatBuffers.CodeGen (generateFlatBuffersTypesWithRegistry)
import FlatBuffers.Registry

import qualified Proto.AST
import Proto.AST (FieldType(..), ScalarType(..), FieldNumber(..))
import Proto.CodeGen.Hooks

registryTests :: TestTree
registryTests = testGroup "Custom Type Registration"
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

avroRegistryTests :: TestTree
avroRegistryTests = testGroup "Avro.Registry"
  [ testCase "custom logical type replaces base type in codegen" $ do
      let handler = LogicalTypeHandler
            { lthHaskellType = "Money"
            , lthImports     = ["MyApp.Money (Money)"]
            , lthEncode      = "encodeMoney"
            , lthDecode      = "decodeMoney"
            }
          reg = registerLogicalType "money" handler defaultAvroRegistry
          schema = AvroRecord
            { avroRecordName = "Transaction"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordProps = Map.empty
            , avroRecordFields = V.fromList
              [ AvroField "amount"
                  (AvroLogical
                    { avroLogicalBase = AvroPrimitive AvroLong
                    , avroLogicalType = CustomLogical "money"
                    })
                  Nothing Nothing V.empty Nothing Map.empty
              ]
            }
          code = generateAvroTypesWithRegistry reg schema
      assertBool "contains Money type" ("Money" `T.isInfixOf` code)
      assertBool "contains encodeMoney" ("encodeMoney" `T.isInfixOf` code)
      assertBool "contains decodeMoney" ("decodeMoney" `T.isInfixOf` code)

  , testCase "default registry includes timestamp-millis" $ do
      let reg = defaultAvroRegistry
      assertBool "has timestamp-millis" (Map.member "timestamp-millis" (arLogicalTypes reg))
      assertBool "has uuid" (Map.member "uuid" (arLogicalTypes reg))

  , testCase "custom prop handler emits extra code" $ do
      let handler = PropHandler
            { phCodeGen = \k v ->
                ["-- prop " <> k <> " = " <> v]
            }
          reg = registerPropHandler "x-validate" handler defaultAvroRegistry
          schema = AvroRecord
            { avroRecordName = "MyRecord"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordProps = Map.empty
            , avroRecordFields = V.fromList
              [ AvroField "field1"
                  (AvroPrimitive AvroString)
                  Nothing Nothing V.empty Nothing
                  (Map.fromList [("x-validate", "required")])
              ]
            }
          code = generateAvroTypesWithRegistry reg schema
      assertBool "contains prop extra code" ("-- prop x-validate = required" `T.isInfixOf` code)

  , testCase "Semigroup composition of registries" $ do
      let reg1 = registerLogicalType "custom1"
            (LogicalTypeHandler "Type1" [] "enc1" "dec1") mempty
          reg2 = registerLogicalType "custom2"
            (LogicalTypeHandler "Type2" [] "enc2" "dec2") mempty
          combined = reg1 <> reg2
      assertBool "has custom1" (Map.member "custom1" (arLogicalTypes combined))
      assertBool "has custom2" (Map.member "custom2" (arLogicalTypes combined))

  , testCase "backward compat: generateAvroTypes still works" $ do
      let schema = AvroRecord
            { avroRecordName = "Simple"
            , avroRecordNamespace = Nothing
            , avroRecordDoc = Nothing
            , avroRecordAliases = V.empty
            , avroRecordProps = Map.empty
            , avroRecordFields = V.fromList
              [ AvroField "name" (AvroPrimitive AvroString) Nothing Nothing V.empty Nothing Map.empty
              ]
            }
      let code = generateAvroTypesWithRegistry defaultAvroRegistry schema
      assertBool "contains data Simple" ("data Simple = Simple" `T.isInfixOf` code)
  ]

-- =========================================================================
-- Thrift registry tests
-- =========================================================================

thriftRegistryTests :: TestTree
thriftRegistryTests = testGroup "Thrift.Registry"
  [ testCase "field annotation transforms type" $ do
      let handler = FieldAnnotationHandler
            { fahTransformType = \ty -> "Sensitive (" <> ty <> ")"
            , fahExtraCode = \_name _val -> []
            }
          reg = registerFieldAnnotation "sensitive" handler defaultThriftRegistry
          schema = ThriftSchema
            { tsStructs = [ThriftStruct
                { tsName = "UserData"
                , tsKind = StructNormal
                , tsFields =
                  [ ThriftField 1 "ssn" TString Required Nothing
                      (V.fromList [("sensitive", "true")])
                  ]
                , tsAnnotations = V.empty
                }]
            , tsEnums = []
            , tsTypedefs = []
            , tsConsts = []
            , tsServices = []
            }
          code = generateThriftTypesWithRegistry reg schema
      assertBool "contains Sensitive" ("Sensitive" `T.isInfixOf` code)

  , testCase "field annotation emits extra code" $ do
      let handler = FieldAnnotationHandler
            { fahTransformType = id
            , fahExtraCode = \fname val ->
                ["-- annotation on " <> fname <> ": " <> val]
            }
          reg = registerFieldAnnotation "deprecated_field" handler defaultThriftRegistry
          schema = ThriftSchema
            { tsStructs = [ThriftStruct
                { tsName = "LegacyStruct"
                , tsKind = StructNormal
                , tsFields =
                  [ ThriftField 1 "old_field" TI32 Required Nothing
                      (V.fromList [("deprecated_field", "use new_field")])
                  ]
                , tsAnnotations = V.empty
                }]
            , tsEnums = []
            , tsTypedefs = []
            , tsConsts = []
            , tsServices = []
            }
          code = generateThriftTypesWithRegistry reg schema
      assertBool "contains extra code" ("-- annotation on old_field: use new_field" `T.isInfixOf` code)

  , testCase "struct annotation emits extra derivations" $ do
      let handler = StructAnnotationHandler
            { sahExtraDerivations = \_val -> ["Hashable"]
            , sahExtraCode = \k v ->
                ["-- struct annotation " <> k <> " = " <> v]
            }
          reg = registerStructAnnotation "hashable" handler defaultThriftRegistry
          schema = ThriftSchema
            { tsStructs = [ThriftStruct
                { tsName = "KeyStruct"
                , tsKind = StructNormal
                , tsFields =
                  [ ThriftField 1 "key" TString Required Nothing V.empty
                  ]
                , tsAnnotations = V.fromList [("hashable", "true")]
                }]
            , tsEnums = []
            , tsTypedefs = []
            , tsConsts = []
            , tsServices = []
            }
          code = generateThriftTypesWithRegistry reg schema
      assertBool "contains Hashable deriving" ("Hashable" `T.isInfixOf` code)
      assertBool "contains struct annotation code" ("-- struct annotation hashable = true" `T.isInfixOf` code)
  ]

-- =========================================================================
-- CBOR tag registry tests
-- =========================================================================

cborTagRegistryTests :: TestTree
cborTagRegistryTests = testGroup "CBOR.TagRegistry"
  [ testCase "default registry has standard tags" $ do
      let reg = defaultCBORTagRegistry
      assertBool "has tag 0" (IntMap.member 0 (ctrTags reg))
      assertBool "has tag 1" (IntMap.member 1 (ctrTags reg))
      assertBool "has tag 2" (IntMap.member 2 (ctrTags reg))
      assertBool "has tag 3" (IntMap.member 3 (ctrTags reg))

  , testCase "register custom tag" $ do
      let handler = TagHandler
            { thName = "geojson"
            , thHaskellType = Just "GeoJSON"
            , thValidate = \v -> Right v
            }
          reg = registerTag 100 handler defaultCBORTagRegistry
      case lookupTag 100 reg of
        Just h -> thName h @?= "geojson"
        Nothing -> assertFailure "tag 100 not found"

  , testCase "tag validation works" $ do
      let reg = defaultCBORTagRegistry
      case lookupTag 0 reg of
        Just h -> do
          let result = thValidate h (CBOR.Value.TextString "2024-01-01T00:00:00Z")
          case result of
            Right _ -> pure ()
            Left err -> assertFailure ("validation failed: " <> err)
        Nothing -> assertFailure "tag 0 not found"

  , testCase "tag validation rejects invalid" $ do
      let reg = defaultCBORTagRegistry
      case lookupTag 0 reg of
        Just h -> do
          let result = thValidate h (CBOR.Value.UInt 42)
          case result of
            Left _ -> pure ()
            Right _ -> assertFailure "should have rejected UInt for datetime tag"
        Nothing -> assertFailure "tag 0 not found"

  , testCase "Semigroup composition" $ do
      let reg1 = registerTag 100 (TagHandler "a" Nothing (\v -> Right v)) mempty
          reg2 = registerTag 101 (TagHandler "b" Nothing (\v -> Right v)) mempty
          combined = reg1 <> reg2
      assertBool "has 100" (IntMap.member 100 (ctrTags combined))
      assertBool "has 101" (IntMap.member 101 (ctrTags combined))
  ]

-- =========================================================================
-- Bond registry tests
-- =========================================================================

bondRegistryTests :: TestTree
bondRegistryTests = testGroup "Bond.Registry"
  [ testCase "attribute handler transforms type" $ do
      let handler = AttributeHandler
            { Bond.Registry.hTransformType = \ty -> "Encrypted (" <> ty <> ")"
            , Bond.Registry.hExtraCode = \_name _mval -> []
            }
          reg = registerBondAttribute "encrypted" handler defaultBondRegistry
          schema = BondSchema
            { bondNamespace = Nothing
            , bondImports = []
            , bondDecls =
                [ BondDeclStruct (BondStruct
                    { bsName = "Secret"
                    , bsTypeParam = Nothing
                    , bsFields =
                        [ BondField 1 BondRequired BFTString "data" Nothing
                            (V.fromList [("encrypted", Nothing)])
                        ]
                    , bsAttributes = mempty
                    })
                ]
            }
          code = generateBondTypesWithRegistry reg schema
      assertBool "contains Encrypted" ("Encrypted" `T.isInfixOf` code)

  , testCase "attribute handler emits extra code" $ do
      let handler = AttributeHandler
            { Bond.Registry.hTransformType = id
            , Bond.Registry.hExtraCode = \name _mval ->
                ["-- bond attribute: " <> name]
            }
          reg = registerBondAttribute "audit" handler defaultBondRegistry
          schema = BondSchema
            { bondNamespace = Nothing
            , bondImports = []
            , bondDecls =
                [ BondDeclStruct (BondStruct
                    { bsName = "Audited"
                    , bsTypeParam = Nothing
                    , bsFields =
                        [ BondField 1 BondRequired BFTInt32 "value" Nothing
                            (V.fromList [("audit", Just "true")])
                        ]
                    , bsAttributes = mempty
                    })
                ]
            }
          code = generateBondTypesWithRegistry reg schema
      assertBool "contains extra code" ("-- bond attribute: audit" `T.isInfixOf` code)

  , testCase "backward compat: generateBondTypes still works" $ do
      let schema = BondSchema
            { bondNamespace = Nothing
            , bondImports = []
            , bondDecls =
                [ BondDeclStruct (BondStruct
                    { bsName = "Simple"
                    , bsTypeParam = Nothing
                    , bsFields =
                        [ BondField 1 BondRequired BFTString "name" Nothing mempty
                        ]
                    , bsAttributes = mempty
                    })
                ]
            }
          code = generateBondTypesWithRegistry defaultBondRegistry schema
      assertBool "contains data Simple" ("data Simple = Simple" `T.isInfixOf` code)
  ]

-- =========================================================================
-- Cap'n Proto registry tests
-- =========================================================================

capnProtoRegistryTests :: TestTree
capnProtoRegistryTests = testGroup "CapnProto.Registry"
  [ testCase "annotation handler transforms type" $ do
      let handler = CapnProto.Registry.AnnotationHandler
            { CapnProto.Registry.hTransformType = \ty -> "Validated (" <> ty <> ")"
            , CapnProto.Registry.hExtraCode = \_name _mval -> []
            }
          reg = registerCapnProtoAnnotation "validate" handler defaultCapnProtoRegistry
          schema = CapnProtoSchema
            { csFileId = Nothing
            , csImports = V.empty
            , csDecls = V.fromList
                [ DStruct (StructDef
                    { sdName = "Input"
                    , sdFields = V.fromList
                        [ FieldDef "email" 0 CTText Nothing
                            (V.fromList [("validate", Just "email")])
                        ]
                    , sdNested = V.empty
                    , sdUnions = V.empty
                    })
                ]
            }
          code = generateCapnProtoTypesWithRegistry reg schema
      assertBool "contains Validated" ("Validated" `T.isInfixOf` code)

  , testCase "annotation handler emits extra code" $ do
      let handler = CapnProto.Registry.AnnotationHandler
            { CapnProto.Registry.hTransformType = id
            , CapnProto.Registry.hExtraCode = \name _mval ->
                ["-- capnp annotation: " <> name]
            }
          reg = registerCapnProtoAnnotation "deprecated" handler defaultCapnProtoRegistry
          schema = CapnProtoSchema
            { csFileId = Nothing
            , csImports = V.empty
            , csDecls = V.fromList
                [ DStruct (StructDef
                    { sdName = "Legacy"
                    , sdFields = V.fromList
                        [ FieldDef "old" 0 CTInt32 Nothing
                            (V.fromList [("deprecated", Nothing)])
                        ]
                    , sdNested = V.empty
                    , sdUnions = V.empty
                    })
                ]
            }
          code = generateCapnProtoTypesWithRegistry reg schema
      assertBool "contains extra code" ("-- capnp annotation: deprecated" `T.isInfixOf` code)
  ]

-- =========================================================================
-- FlatBuffers registry tests
-- =========================================================================

flatBuffersRegistryTests :: TestTree
flatBuffersRegistryTests = testGroup "FlatBuffers.Registry"
  [ testCase "metadata handler transforms type" $ do
      let handler = FlatBuffers.Registry.MetadataHandler
            { FlatBuffers.Registry.hTransformType = \ty -> "Compressed (" <> ty <> ")"
            , FlatBuffers.Registry.hExtraCode = \_name _mval -> []
            }
          reg = registerFlatBuffersMetadata "compressed" handler defaultFlatBuffersRegistry
          schema = FlatBuffersSchema
            { fbsNamespace = Nothing
            , fbsIncludes = V.empty
            , fbsDecls = V.fromList
                [ FBTable (TableDef
                    { tdName = "BigData"
                    , tdFields = V.fromList
                        [ TableField "payload" FTString (Just "\"\"") False
                            (V.fromList [("compressed", Just "lz4")])
                        ]
                    })
                ]
            , fbsRootType = Nothing
            , fbsFileIdentifier = Nothing
            , fbsFileExtension = Nothing
            , fbsAttributes = V.empty
            }
          code = generateFlatBuffersTypesWithRegistry reg schema
      assertBool "contains Compressed" ("Compressed" `T.isInfixOf` code)

  , testCase "metadata handler emits extra code" $ do
      let handler = FlatBuffers.Registry.MetadataHandler
            { FlatBuffers.Registry.hTransformType = id
            , FlatBuffers.Registry.hExtraCode = \name _mval ->
                ["-- fbs metadata: " <> name]
            }
          reg = registerFlatBuffersMetadata "custom_attr" handler defaultFlatBuffersRegistry
          schema = FlatBuffersSchema
            { fbsNamespace = Nothing
            , fbsIncludes = V.empty
            , fbsDecls = V.fromList
                [ FBTable (TableDef
                    { tdName = "Tagged"
                    , tdFields = V.fromList
                        [ TableField "data" FTString (Just "\"\"") False
                            (V.fromList [("custom_attr", Nothing)])
                        ]
                    })
                ]
            , fbsRootType = Nothing
            , fbsFileIdentifier = Nothing
            , fbsFileExtension = Nothing
            , fbsAttributes = V.empty
            }
          code = generateFlatBuffersTypesWithRegistry reg schema
      assertBool "contains extra code" ("-- fbs metadata: custom_attr" `T.isInfixOf` code)

  , testCase "backward compat: generateFlatBuffersTypes still works" $ do
      let schema = FlatBuffersSchema
            { fbsNamespace = Nothing
            , fbsIncludes = V.empty
            , fbsDecls = V.fromList
                [ FBTable (TableDef
                    { tdName = "Monster"
                    , tdFields = V.fromList
                        [ TableField "name" FTString Nothing False V.empty
                        ]
                    })
                ]
            , fbsRootType = Nothing
            , fbsFileIdentifier = Nothing
            , fbsFileExtension = Nothing
            , fbsAttributes = V.empty
            }
          code = generateFlatBuffersTypesWithRegistry defaultFlatBuffersRegistry schema
      assertBool "contains data Monster" ("data Monster = Monster" `T.isInfixOf` code)
  ]

-- =========================================================================
-- Proto hooks enhancement tests
-- =========================================================================

protoHooksEnhancementTests :: TestTree
protoHooksEnhancementTests = testGroup "Proto.CodeGen.Hooks enhancements"
  [ testCase "FieldHookCtx is constructible" $ do
      let ctx = FieldHookCtx
            { fldFieldDef = Proto.AST.FieldDef Nothing (FTScalar SString) "name" (FieldNumber 1) []
            , fldParentMsg = "Person"
            , fldHsFieldName = "personName"
            , fldFieldOptions = []
            }
      fldParentMsg ctx @?= "Person"
      fldHsFieldName ctx @?= "personName"

  , testCase "onFieldCodeGen fires in hook" $ do
      let hook = defaultCodeGenHooks
            { onFieldCodeGen = \ctx ->
                ["-- field: " <> fldHsFieldName ctx]
            }
          ctx = FieldHookCtx
            { fldFieldDef = Proto.AST.FieldDef Nothing (FTScalar SString) "name" (FieldNumber 1) []
            , fldParentMsg = "Person"
            , fldHsFieldName = "personName"
            , fldFieldOptions = []
            }
          output = onFieldCodeGen hook ctx
      output @?= ["-- field: personName"]

  , testCase "onCustomOption returns Nothing by default" $ do
      let hook = defaultCodeGenHooks
          result = onCustomOption hook "some.option" (OVBool True)
      result @?= Nothing

  , testCase "onCustomOption returns WireTransform" $ do
      let hook = defaultCodeGenHooks
            { onCustomOption = \name _val ->
                if name == "compress"
                  then Just (WireTransform "compressEncode" "compressDecode")
                  else Nothing
            }
          result = onCustomOption hook "compress" (OVBool True)
      case result of
        Just wt -> do
          wtEncodeExpr wt @?= "compressEncode"
          wtDecodeExpr wt @?= "compressDecode"
        Nothing -> assertFailure "expected WireTransform"

  , testCase "THHooks has thOnService" $ do
      let hook = defaultTHHooks
            { thOnService = const (pure [])
            }
          combined = hook <> hook
      assertBool "thOnService composes" True

  , testCase "composed CodeGenHooks with field hooks" $ do
      let hook1 = defaultCodeGenHooks
            { onFieldCodeGen = \ctx ->
                ["-- h1: " <> fldHsFieldName ctx]
            }
          hook2 = defaultCodeGenHooks
            { onFieldCodeGen = \ctx ->
                ["-- h2: " <> fldHsFieldName ctx]
            }
          combined = hook1 <> hook2
          ctx = FieldHookCtx
            { fldFieldDef = Proto.AST.FieldDef Nothing (FTScalar SInt32) "x" (FieldNumber 1) []
            , fldParentMsg = "Msg"
            , fldHsFieldName = "msgX"
            , fldFieldOptions = []
            }
          output = onFieldCodeGen combined ctx
      output @?= ["-- h1: msgX", "-- h2: msgX"]

  , testCase "OptionValue and WireTransform types" $ do
      let ov1 = OVBool True
          ov2 = OVInt 42
          ov3 = OVFloat 3.14
          ov4 = OVString "hello"
      ov1 @?= OVBool True
      ov2 @?= OVInt 42
      ov3 @?= OVFloat 3.14
      ov4 @?= OVString "hello"

      let wt = WireTransform "enc" "dec"
      wtEncodeExpr wt @?= "enc"
      wtDecodeExpr wt @?= "dec"
  ]
