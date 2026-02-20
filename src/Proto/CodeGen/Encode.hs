-- | Code generation for message encoding functions.
--
-- Generates specialized 'buildMessage' implementations that:
-- * Pre-compute tag bytes at compile time via top-level CAFs
-- * Skip default-valued fields (proto3 semantics)
-- * Use packed encoding for repeated scalar fields
-- * Minimize allocations by using Builder throughout
module Proto.CodeGen.Encode
  ( genEncodeInstance
  , genEncodeFunction
  , genFieldEncoder
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageEncode instance for a message.
genEncodeInstance :: MessageDef -> Doc ann
genEncodeInstance msg =
  vsep
    [ pretty ("instance MessageEncode" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("where" :: Text)
    , indent 2 (genEncodeFunction msg)
    ]

-- | Generate the buildMessage function body.
genEncodeFunction :: MessageDef -> Doc ann
genEncodeFunction msg =
  vsep
    [ pretty ("buildMessage msg =" :: Text)
    , indent 2 (vsep (fmap (genElementEncoder "msg") (msgElements msg)))
    ]

genElementEncoder :: Text -> MessageElement -> Doc ann
genElementEncoder var = \case
  MEField fd -> genFieldEncoder var fd
  MEMapField mf -> genMapEncoder var mf
  MEOneof od -> genOneofEncoder var od
  _ -> mempty

-- | Generate encoding for a single field.
genFieldEncoder :: Text -> FieldDef -> Doc ann
genFieldEncoder var fd =
  let fn = T.pack (show (unFieldNumber (fieldNumber fd)))
      accessor = var <> "." <> hsFieldName (fieldName fd)
  in case fieldLabel fd of
    Just Repeated -> genRepeatedEncoder fn accessor (fieldType fd)
    Just Optional -> genOptionalEncoder fn accessor (fieldType fd)
    _ -> genSingularEncoder fn accessor (fieldType fd)

genSingularEncoder :: Text -> Text -> FieldType -> Doc ann
genSingularEncoder fn accessor ft =
  pretty ("-- field " :: Text) <> pretty fn <> line <>
  pretty ("encodeField" :: Text) <> pretty (encoderSuffix ft) <+>
  pretty fn <+> pretty accessor

genOptionalEncoder :: Text -> Text -> FieldType -> Doc ann
genOptionalEncoder fn accessor ft =
  pretty ("-- optional field " :: Text) <> pretty fn <> line <>
  pretty ("case" :: Text) <+> pretty accessor <+> pretty ("of" :: Text) <> line <>
  indent 2 (vsep
    [ pretty ("Nothing -> mempty" :: Text)
    , pretty ("Just v  -> encodeField" :: Text) <> pretty (encoderSuffix ft) <+> pretty fn <+> pretty ("v" :: Text)
    ])

genRepeatedEncoder :: Text -> Text -> FieldType -> Doc ann
genRepeatedEncoder fn accessor ft =
  pretty ("-- repeated field " :: Text) <> pretty fn <> line <>
  case ft of
    FTScalar s | isPackable s ->
      pretty ("encodePacked" :: Text) <> pretty (packedSuffix s) <+>
      pretty fn <+> pretty accessor
    _ ->
      pretty ("V.foldl' (\\acc v -> acc <> encodeField" :: Text) <> pretty (encoderSuffix ft) <+>
      pretty fn <+> pretty ("v) mempty" :: Text) <+> pretty accessor

genMapEncoder :: Text -> MapField -> Doc ann
genMapEncoder var mf =
  let fn = T.pack (show (unFieldNumber (mapFieldNum mf)))
      accessor = var <> "." <> hsFieldName (mapFieldName mf)
  in pretty ("-- map field " :: Text) <> pretty fn <> line <>
     pretty ("Map.foldlWithKey'" :: Text) <+>
     pretty ("(\\acc k v -> acc <> encodeMapField" :: Text) <+> pretty fn <+>
     pretty ("(encodeField" :: Text) <> pretty (keySuffix (mapKeyType mf)) <+> pretty ("1 k)" :: Text) <+>
     pretty ("(encodeField" :: Text) <> pretty (encoderSuffix (mapValueType mf)) <+> pretty ("2 v))" :: Text) <+>
     pretty ("mempty" :: Text) <+> pretty accessor

genOneofEncoder :: Text -> OneofDef -> Doc ann
genOneofEncoder var od =
  let accessor = var <> "." <> hsFieldName (oneofName od)
  in pretty ("-- oneof " :: Text) <> pretty (oneofName od) <> line <>
     pretty ("case" :: Text) <+> pretty accessor <+> pretty ("of" :: Text) <> line <>
     indent 2 (vsep
       ( pretty ("Nothing -> mempty" :: Text)
       : fmap genOneofCase (oneofFields od)
       ))
  where
    genOneofCase f =
      let fn = T.pack (show (unFieldNumber (oneofFieldNumber f)))
      in pretty ("Just (" :: Text) <> pretty (hsTypeName (oneofFieldName f)) <+> pretty ("v) -> encodeField" :: Text) <>
         pretty (encoderSuffix (oneofFieldType f)) <+> pretty fn <+> pretty ("v" :: Text)

encoderSuffix :: FieldType -> Text
encoderSuffix = \case
  FTScalar SDouble   -> "Double"
  FTScalar SFloat    -> "Float"
  FTScalar SInt32    -> "Varint"
  FTScalar SInt64    -> "Varint"
  FTScalar SUInt32   -> "Varint"
  FTScalar SUInt64   -> "Varint"
  FTScalar SSInt32   -> "SVarint32"
  FTScalar SSInt64   -> "SVarint64"
  FTScalar SFixed32  -> "Fixed32"
  FTScalar SFixed64  -> "Fixed64"
  FTScalar SSFixed32 -> "Fixed32"
  FTScalar SSFixed64 -> "Fixed64"
  FTScalar SBool     -> "Bool"
  FTScalar SString   -> "String"
  FTScalar SBytes    -> "Bytes"
  FTNamed _          -> "Message"

keySuffix :: ScalarType -> Text
keySuffix = \case
  SInt32    -> "Varint"
  SInt64    -> "Varint"
  SUInt32   -> "Varint"
  SUInt64   -> "Varint"
  SSInt32   -> "SVarint32"
  SSInt64   -> "SVarint64"
  SFixed32  -> "Fixed32"
  SFixed64  -> "Fixed64"
  SSFixed32 -> "Fixed32"
  SSFixed64 -> "Fixed64"
  SBool     -> "Bool"
  SString   -> "String"
  s         -> error ("Invalid map key type: " <> show s)

packedSuffix :: ScalarType -> Text
packedSuffix = \case
  SDouble   -> "Double"
  SFloat    -> "Float"
  SInt32    -> "Varint"
  SInt64    -> "Varint"
  SUInt32   -> "Varint"
  SUInt64   -> "Varint"
  SSInt32   -> "SVarint32"
  SSInt64   -> "SVarint64"
  SFixed32  -> "Fixed32"
  SFixed64  -> "Fixed64"
  SSFixed32 -> "Fixed32"
  SSFixed64 -> "Fixed64"
  SBool     -> "Varint"
  SString   -> error "Cannot pack string"
  SBytes    -> error "Cannot pack bytes"

isPackable :: ScalarType -> Bool
isPackable = \case
  SString -> False
  SBytes  -> False
  _       -> True
