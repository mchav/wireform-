{- | Convert wireform's AST types to descriptor.proto types and back.

This enables bundling schema metadata with generated types:
the parsed 'ProtoFile' is converted to a 'FileDescriptorProto',
serialized, and embedded in the generated code.

The reverse direction ('fileDescriptorToAST') is used by the protoc
plugin, which receives 'FileDescriptorProto' from protoc and needs
wireform's 'ProtoFile' to drive code generation.
-}
module Proto.IDL.Descriptor (
  astToFileDescriptor,
  astToDescriptor,
  astToFieldDescriptor,
  astToEnumDescriptor,
  astToServiceDescriptor,
  serializeFileDescriptor,
  fileDescriptorToAST,
  descriptorToMessage,
  fieldDescriptorToField,
  enumDescriptorToEnum,
  serviceDescriptorToService,
) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Descriptor
import Proto.IDL.AST


-- | Convert a parsed ProtoFile to a FileDescriptorProto.
astToFileDescriptor :: FilePath -> ProtoFile -> FileDescriptorProto
astToFileDescriptor path pf =
  defaultFileDescriptorProto
    { fdpName = T.pack path
    , fdpPackage = fromMaybe "" (protoPackage pf)
    , fdpDependency = V.fromList (fmap importPath (protoImports pf))
    , fdpMessageType = V.fromList (concatMap topMessages (protoTopLevels pf))
    , fdpEnumType = V.fromList (concatMap topEnums (protoTopLevels pf))
    , fdpService = V.fromList (concatMap topServices (protoTopLevels pf))
    , fdpSyntax = syntaxStr (protoSyntax pf)
    , fdpEdition = editionStr (protoSyntax pf)
    }
  where
    topMessages (TLMessage msg) = [astToDescriptor msg]
    topMessages _ = []
    topEnums (TLEnum ed) = [astToEnumDescriptor ed]
    topEnums _ = []
    topServices (TLService svc) = [astToServiceDescriptor svc]
    topServices _ = []
    syntaxStr Proto2 = "proto2"
    syntaxStr Proto3 = "proto3"
    syntaxStr (Editions _) = "editions"
    editionStr (Editions ed) = editionName ed
    editionStr _ = ""


-- | Convert a MessageDef to a DescriptorProto.
astToDescriptor :: MessageDef -> DescriptorProto
astToDescriptor msg =
  defaultDescriptorProto
    { dpName = msgName msg
    , dpField = V.fromList (concatMap extractField (msgElements msg))
    , dpNestedType = V.fromList (concatMap extractNested (msgElements msg))
    , dpEnumType = V.fromList (concatMap extractEnum (msgElements msg))
    , dpOneofDecl = V.fromList (concatMap extractOneof (msgElements msg))
    }
  where
    extractField (MEField fd) = [astToFieldDescriptor fd]
    extractField (MEMapField mf) = [mapToFieldDescriptor mf]
    extractField (MEOneof od) = fmap oneofFieldToFDP (oneofFields od)
    extractField _ = []

    extractNested (MEMessage inner) = [astToDescriptor inner]
    extractNested _ = []

    extractEnum (MEEnum ed) = [astToEnumDescriptor ed]
    extractEnum _ = []

    extractOneof (MEOneof od) = [defaultOneofDescriptorProto {odpName = oneofName od}]
    extractOneof _ = []


-- | Convert a FieldDef to a FieldDescriptorProto.
astToFieldDescriptor :: FieldDef -> FieldDescriptorProto
astToFieldDescriptor fd =
  defaultFieldDescriptorProto
    { fdpFieldName = fieldName fd
    , fdpFieldNumber = fromIntegral (unFieldNumber (fieldNumber fd))
    , fdpFieldLabel = labelToInt (fieldLabel fd)
    , fdpFieldType = fieldTypeToInt (fieldType fd)
    , fdpFieldTypeName = case fieldType fd of
        FTNamed n -> n
        _ -> ""
    }


mapToFieldDescriptor :: MapField -> FieldDescriptorProto
mapToFieldDescriptor mf =
  defaultFieldDescriptorProto
    { fdpFieldName = mapFieldName mf
    , fdpFieldNumber = fromIntegral (unFieldNumber (mapFieldNum mf))
    , fdpFieldLabel = 3
    , fdpFieldType = 11
    , fdpFieldTypeName = mapFieldName mf <> "Entry"
    }


oneofFieldToFDP :: OneofField -> FieldDescriptorProto
oneofFieldToFDP of' =
  defaultFieldDescriptorProto
    { fdpFieldName = oneofFieldName of'
    , fdpFieldNumber = fromIntegral (unFieldNumber (oneofFieldNumber of'))
    , fdpFieldLabel = 1
    , fdpFieldType = fieldTypeToInt (oneofFieldType of')
    , fdpFieldTypeName = case oneofFieldType of' of
        FTNamed n -> n
        _ -> ""
    }


-- | Convert an EnumDef to an EnumDescriptorProto.
astToEnumDescriptor :: EnumDef -> EnumDescriptorProto
astToEnumDescriptor ed =
  defaultEnumDescriptorProto
    { edpName = enumName ed
    , edpValue = V.fromList (fmap toEVDP (enumValues ed))
    }
  where
    toEVDP ev = defaultEnumValueDescriptorProto {evdpName = evName ev, evdpNumber = fromIntegral (evNumber ev)}


-- | Convert a ServiceDef to a ServiceDescriptorProto.
astToServiceDescriptor :: ServiceDef -> ServiceDescriptorProto
astToServiceDescriptor svc =
  defaultServiceDescriptorProto
    { sdpName = svcName svc
    , sdpMethod = V.fromList (fmap toMDP (svcRpcs svc))
    }
  where
    toMDP rpc =
      defaultMethodDescriptorProto
        { mdpName = rpcName rpc
        , mdpInputType = rpcInput rpc
        , mdpOutputType = rpcOutput rpc
        , mdpClientStreaming = rpcInputStr rpc == Streaming
        , mdpServerStreaming = rpcOutputStr rpc == Streaming
        }


-- | Serialize a ProtoFile's schema to bytes (as a FileDescriptorProto).
serializeFileDescriptor :: FilePath -> ProtoFile -> ByteString
serializeFileDescriptor path pf = encodeMessage (astToFileDescriptor path pf)


labelToInt :: Maybe FieldLabel -> Int32
labelToInt Nothing = 1
labelToInt (Just Optional) = 1
labelToInt (Just Required) = 2
labelToInt (Just Repeated) = 3


fieldTypeToInt :: FieldType -> Int32
fieldTypeToInt (FTScalar s) = scalarTypeToInt s
fieldTypeToInt (FTNamed _) = 11


scalarTypeToInt :: ScalarType -> Int32
scalarTypeToInt = \case
  SDouble -> 1
  SFloat -> 2
  SInt64 -> 3
  SUInt64 -> 4
  SInt32 -> 5
  SFixed64 -> 6
  SFixed32 -> 7
  SBool -> 8
  SString -> 9
  SBytes -> 12
  SUInt32 -> 13
  SSFixed32 -> 15
  SSFixed64 -> 16
  SSInt32 -> 17
  SSInt64 -> 18


-- ---------------------------------------------------------------------------
-- Reverse conversion: FileDescriptorProto -> ProtoFile
-- ---------------------------------------------------------------------------

-- | Convert a 'FileDescriptorProto' (as received from protoc) to a 'ProtoFile'.
fileDescriptorToAST :: FileDescriptorProto -> ProtoFile
fileDescriptorToAST fdp =
  ProtoFile
    { protoSyntax = parseSyntax (fdpSyntax fdp) (fdpEdition fdp)
    , protoPackage = if T.null (fdpPackage fdp) then Nothing else Just (fdpPackage fdp)
    , protoImports = fmap (ImportDef () Nothing) (V.toList (fdpDependency fdp))
    , protoOptions = []
    , protoTopLevels =
        fmap (TLMessage . descriptorToMessage) (V.toList (fdpMessageType fdp))
          <> fmap (TLEnum . enumDescriptorToEnum) (V.toList (fdpEnumType fdp))
          <> fmap (TLService . serviceDescriptorToService) (V.toList (fdpService fdp))
    , protoSource = Nothing
    }


parseSyntax :: Text -> Text -> Syntax
parseSyntax syn ed
  | syn == "proto2" = Proto2
  | syn == "editions" = Editions (Edition ed)
  | otherwise = Proto3


-- | Convert a 'DescriptorProto' to a 'MessageDef'.
descriptorToMessage :: DescriptorProto -> MessageDef
descriptorToMessage dp =
  MessageDef
    { msgExt = ()
    , msgDoc = Nothing
    , msgName = dpName dp
    , msgElements = fieldElems <> nestedElems <> enumElems
    }
  where
    oneofNames = V.toList (fmap odpName (dpOneofDecl dp))
    allFields = V.toList (dpField dp)
    (oneofFields', regularFields) = partitionOneofFields oneofNames allFields
    fieldElems = fmap (MEField . fieldDescriptorToField) regularFields
    nestedElems = fmap (MEMessage . descriptorToMessage) (V.toList (dpNestedType dp))
    enumElems =
      fmap (MEEnum . enumDescriptorToEnum) (V.toList (dpEnumType dp))
        <> buildOneofDefs oneofNames oneofFields'


partitionOneofFields :: [Text] -> [FieldDescriptorProto] -> ([[FieldDescriptorProto]], [FieldDescriptorProto])
partitionOneofFields oneofNames fields =
  let regular = filter (\f -> fdpFieldOneofIdx f < 0) fields
      grouped =
        fmap
          (\(i, _) -> filter (\f -> fdpFieldOneofIdx f == fromIntegral i) fields)
          (zip [0 :: Int ..] oneofNames)
  in (grouped, regular)


buildOneofDefs :: [Text] -> [[FieldDescriptorProto]] -> [MessageElement]
buildOneofDefs = zipWith mkOneof
  where
    mkOneof name flds =
      MEOneof
        ( OneofDef
            { oneofExt = ()
            , oneofDoc = Nothing
            , oneofName = name
            , oneofFields = fmap mkOneofField flds
            , oneofOptions = []
            }
        )
    mkOneofField f =
      OneofField
        { oneofFieldExt = ()
        , oneofFieldDoc = Nothing
        , oneofFieldType = intToFieldType (fdpFieldType f) (fdpFieldTypeName f)
        , oneofFieldName = fdpFieldName f
        , oneofFieldNumber = FieldNumber (fromIntegral (fdpFieldNumber f))
        , oneofFieldOptions = []
        }


-- | Convert a 'FieldDescriptorProto' to a 'FieldDef'.
fieldDescriptorToField :: FieldDescriptorProto -> FieldDef
fieldDescriptorToField f =
  FieldDef
    { fieldExt = ()
    , fieldDoc = Nothing
    , fieldLabel = intToLabel (fdpFieldLabel f)
    , fieldType = intToFieldType (fdpFieldType f) (fdpFieldTypeName f)
    , fieldName = fdpFieldName f
    , fieldNumber = FieldNumber (fromIntegral (fdpFieldNumber f))
    , fieldOptions = jsonNameOpt (fdpFieldJsonName f)
    }


jsonNameOpt :: Text -> [OptionDef]
jsonNameOpt t
  | T.null t = []
  | otherwise = [OptionDef () (OptionName [SimpleOption "json_name"]) (CString t)]


-- | Convert an 'EnumDescriptorProto' to an 'EnumDef'.
enumDescriptorToEnum :: EnumDescriptorProto -> EnumDef
enumDescriptorToEnum e =
  EnumDef
    { enumExt = ()
    , enumDoc = Nothing
    , enumName = edpName e
    , enumValues =
        fmap
          (\v -> EnumValue () Nothing (evdpName v) (fromIntegral (evdpNumber v)) [])
          (V.toList (edpValue e))
    , enumOptions = []
    }


-- | Convert a 'ServiceDescriptorProto' to a 'ServiceDef'.
serviceDescriptorToService :: ServiceDescriptorProto -> ServiceDef
serviceDescriptorToService s =
  ServiceDef
    { svcExt = ()
    , svcDoc = Nothing
    , svcName = sdpName s
    , svcRpcs = fmap methodToRpc (V.toList (sdpMethod s))
    , svcOptions = []
    }
  where
    methodToRpc m =
      RpcDef
        { rpcExt = ()
        , rpcDoc = Nothing
        , rpcName = mdpName m
        , rpcInput = mdpInputType m
        , rpcInputStr = if mdpClientStreaming m then Streaming else NoStream
        , rpcOutput = mdpOutputType m
        , rpcOutputStr = if mdpServerStreaming m then Streaming else NoStream
        , rpcOptions = []
        }


intToLabel :: Int32 -> Maybe FieldLabel
intToLabel 1 = Just Optional
intToLabel 2 = Just Required
intToLabel 3 = Just Repeated
intToLabel _ = Nothing


intToFieldType :: Int32 -> Text -> FieldType
intToFieldType typeInt typeName = case intToScalar typeInt of
  Just s -> FTScalar s
  Nothing -> FTNamed (stripLeadingDot typeName)


stripLeadingDot :: Text -> Text
stripLeadingDot t = fromMaybe t (T.stripPrefix "." t)


intToScalar :: Int32 -> Maybe ScalarType
intToScalar = \case
  1 -> Just SDouble
  2 -> Just SFloat
  3 -> Just SInt64
  4 -> Just SUInt64
  5 -> Just SInt32
  6 -> Just SFixed64
  7 -> Just SFixed32
  8 -> Just SBool
  9 -> Just SString
  12 -> Just SBytes
  13 -> Just SUInt32
  15 -> Just SSFixed32
  16 -> Just SSFixed64
  17 -> Just SSInt32
  18 -> Just SSInt64
  _ -> Nothing
