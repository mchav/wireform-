-- | Convert wireform's AST types to descriptor.proto types.
--
-- This enables bundling schema metadata with generated types:
-- the parsed 'ProtoFile' is converted to a 'FileDescriptorProto',
-- serialized, and embedded in the generated code.
module Proto.Descriptor.Convert
  ( astToFileDescriptor
  , astToDescriptor
  , astToFieldDescriptor
  , astToEnumDescriptor
  , astToServiceDescriptor
  , serializeFileDescriptor
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Proto.AST
import Proto.Encode (encodeMessage)
import Proto.Google.Protobuf.Descriptor

-- | Convert a parsed ProtoFile to a FileDescriptorProto.
astToFileDescriptor :: FilePath -> ProtoFile -> FileDescriptorProto
astToFileDescriptor path pf = defaultFileDescriptorProto
  { fdpName       = T.pack path
  , fdpPackage    = fromMaybe "" (protoPackage pf)
  , fdpDependency = V.fromList (fmap importPath (protoImports pf))
  , fdpMessageType = V.fromList (concatMap topMessages (protoTopLevels pf))
  , fdpEnumType    = V.fromList (concatMap topEnums (protoTopLevels pf))
  , fdpService     = V.fromList (concatMap topServices (protoTopLevels pf))
  , fdpSyntax      = syntaxStr (protoSyntax pf)
  , fdpEdition     = editionStr (protoSyntax pf)
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
astToDescriptor msg = defaultDescriptorProto
  { dpName       = msgName msg
  , dpField      = V.fromList (concatMap extractField (msgElements msg))
  , dpNestedType = V.fromList (concatMap extractNested (msgElements msg))
  , dpEnumType   = V.fromList (concatMap extractEnum (msgElements msg))
  , dpOneofDecl  = V.fromList (concatMap extractOneof (msgElements msg))
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

    extractOneof (MEOneof od) = [OneofDescriptorProto (oneofName od)]
    extractOneof _ = []

-- | Convert a FieldDef to a FieldDescriptorProto.
astToFieldDescriptor :: FieldDef -> FieldDescriptorProto
astToFieldDescriptor fd = defaultFieldDescriptorProto
  { fdpFieldName   = fieldName fd
  , fdpFieldNumber = fromIntegral (unFieldNumber (fieldNumber fd))
  , fdpFieldLabel  = labelToInt (fieldLabel fd)
  , fdpFieldType   = fieldTypeToInt (fieldType fd)
  , fdpFieldTypeName = case fieldType fd of
      FTNamed n -> n
      _         -> ""
  }

mapToFieldDescriptor :: MapField -> FieldDescriptorProto
mapToFieldDescriptor mf = defaultFieldDescriptorProto
  { fdpFieldName   = mapFieldName mf
  , fdpFieldNumber = fromIntegral (unFieldNumber (mapFieldNum mf))
  , fdpFieldLabel  = 3
  , fdpFieldType   = 11
  , fdpFieldTypeName = mapFieldName mf <> "Entry"
  }

oneofFieldToFDP :: OneofField -> FieldDescriptorProto
oneofFieldToFDP of' = defaultFieldDescriptorProto
  { fdpFieldName   = oneofFieldName of'
  , fdpFieldNumber = fromIntegral (unFieldNumber (oneofFieldNumber of'))
  , fdpFieldLabel  = 1
  , fdpFieldType   = fieldTypeToInt (oneofFieldType of')
  , fdpFieldTypeName = case oneofFieldType of' of
      FTNamed n -> n
      _         -> ""
  }

-- | Convert an EnumDef to an EnumDescriptorProto.
astToEnumDescriptor :: EnumDef -> EnumDescriptorProto
astToEnumDescriptor ed = defaultEnumDescriptorProto
  { edpName  = enumName ed
  , edpValue = V.fromList (fmap toEVDP (enumValues ed))
  }
  where
    toEVDP ev = EnumValueDescriptorProto (evName ev) (fromIntegral (evNumber ev))

-- | Convert a ServiceDef to a ServiceDescriptorProto.
astToServiceDescriptor :: ServiceDef -> ServiceDescriptorProto
astToServiceDescriptor svc = defaultServiceDescriptorProto
  { sdpName   = svcName svc
  , sdpMethod = V.fromList (fmap toMDP (svcRpcs svc))
  }
  where
    toMDP rpc = MethodDescriptorProto
      { mdpName           = rpcName rpc
      , mdpInputType      = rpcInput rpc
      , mdpOutputType     = rpcOutput rpc
      , mdpClientStreaming = rpcInputStr rpc == Streaming
      , mdpServerStreaming = rpcOutputStr rpc == Streaming
      }

-- | Serialize a ProtoFile's schema to bytes (as a FileDescriptorProto).
serializeFileDescriptor :: FilePath -> ProtoFile -> ByteString
serializeFileDescriptor path pf = encodeMessage (astToFileDescriptor path pf)

labelToInt :: Maybe FieldLabel -> Int32
labelToInt Nothing         = 1
labelToInt (Just Optional) = 1
labelToInt (Just Required) = 2
labelToInt (Just Repeated) = 3

fieldTypeToInt :: FieldType -> Int32
fieldTypeToInt (FTScalar s) = scalarTypeToInt s
fieldTypeToInt (FTNamed _)  = 11

scalarTypeToInt :: ScalarType -> Int32
scalarTypeToInt = \case
  SDouble   -> 1
  SFloat    -> 2
  SInt64    -> 3
  SUInt64   -> 4
  SInt32    -> 5
  SFixed64  -> 6
  SFixed32  -> 7
  SBool     -> 8
  SString   -> 9
  SBytes    -> 12
  SUInt32   -> 13
  SSFixed32 -> 15
  SSFixed64 -> 16
  SSInt32   -> 17
  SSInt64   -> 18
