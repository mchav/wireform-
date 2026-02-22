-- | Code generation for message encoding functions.
--
-- Generates specialized 'buildMessage' implementations using the two-pass
-- technique from Buf's performance guide:
-- * First pass: compute exact wire size via 'messageSize'
-- * Second pass: serialize via 'buildMessage' using pre-known sizes
-- * Skip default-valued fields (proto3 semantics)
-- * Packed encoding for repeated scalar fields
-- * Minimize allocations: no intermediate ByteString for submessages
module Proto.CodeGen.Encode
  ( genEncodeInstance
  , genSizeInstance
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageEncode instance for a message.
genEncodeInstance :: MessageDef -> Doc ann
genEncodeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
    [ pretty ("instance MessageEncode" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("where" :: Text)
    , indent 2 $ vsep
        [ pretty ("buildMessage msg =" :: Text)
        , indent 2 $ case fields of
            [] -> pretty ("mempty" :: Text)
            _  -> vsep (fmap (uncurry genFieldBuild) (zip [0..] fields))
        ]
    ]
  where
    genFieldBuild :: Int -> FieldInfo -> Doc ann
    genFieldBuild idx fi =
      let op = if idx == 0 then mempty else pretty ("<> " :: Text)
          accessor = "msg." <> hsFieldName (fiName fi)
          fn = T.pack (show (fiFieldNum fi))
      in op <> genBuildExpr fn accessor (fiLabel fi) (fiType fi)

genBuildExpr :: Text -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genBuildExpr fn accessor lbl ft = case lbl of
  Just Repeated -> genRepeatedBuild fn accessor ft
  Just Optional -> pretty ("(maybe mempty (\\v -> " :: Text) <> genSingleBuild fn "v" ft <> pretty (") " :: Text) <> pretty accessor <> pretty (")" :: Text)
  _ -> genProto3Build fn accessor ft

genProto3Build :: Text -> Text -> FieldType -> Doc ann
genProto3Build fn accessor ft =
  let cond = defaultCheck accessor ft
  in pretty ("(if " :: Text) <> cond <> pretty (" then mempty else " :: Text) <> genSingleBuild fn accessor ft <> pretty (")" :: Text)

genSingleBuild :: Text -> Text -> FieldType -> Doc ann
genSingleBuild fn accessor = \case
  FTScalar SDouble   -> pretty ("encodeFieldDouble " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SFloat    -> pretty ("encodeFieldFloat " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SInt32    -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SInt64    -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SUInt32   -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SUInt64   -> pretty ("encodeFieldVarint " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SSInt32   -> pretty ("encodeFieldSVarint32 " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SSInt64   -> pretty ("encodeFieldSVarint64 " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SFixed32  -> pretty ("encodeFieldFixed32 " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SFixed64  -> pretty ("encodeFieldFixed64 " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SSFixed32 -> pretty ("encodeFieldFixed32 " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SSFixed64 -> pretty ("encodeFieldFixed64 " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SBool     -> pretty ("encodeFieldBool " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SString   -> pretty ("encodeFieldString " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SBytes    -> pretty ("encodeFieldBytes " :: Text) <> pretty fn <+> pretty accessor
  FTNamed _          -> pretty ("encodeFieldMessageSized " :: Text) <> pretty fn <+> pretty accessor

genRepeatedBuild :: Text -> Text -> FieldType -> Doc ann
genRepeatedBuild fn accessor = \case
  FTScalar SString ->
    pretty ("V.foldl' (\\acc v -> acc <> encodeFieldString " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor
  FTScalar SBytes ->
    pretty ("V.foldl' (\\acc v -> acc <> encodeFieldBytes " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor
  FTScalar s ->
    pretty ("encode" :: Text) <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor
  FTNamed _ ->
    pretty ("V.foldl' (\\acc v -> acc <> encodeFieldMessageSized " :: Text) <> pretty fn <+> pretty ("v) mempty " :: Text) <> pretty accessor

defaultCheck :: Text -> FieldType -> Doc ann
defaultCheck accessor = \case
  FTScalar SBool   -> pretty accessor <+> pretty ("== False" :: Text)
  FTScalar SString -> pretty accessor <+> pretty ("== \"\"" :: Text)
  FTScalar SBytes  -> pretty ("BS.null " :: Text) <> pretty accessor
  FTScalar _       -> pretty accessor <+> pretty ("== 0" :: Text)
  FTNamed _        -> pretty accessor <+> pretty ("== Nothing" :: Text)

-- | Generate a MessageSize instance.
genSizeInstance :: MessageDef -> Doc ann
genSizeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
    [ pretty ("instance MessageSize" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("where" :: Text)
    , indent 2 $ vsep
        [ pretty ("messageSize msg =" :: Text)
        , indent 2 $ case fields of
            [] -> pretty ("0" :: Text)
            _  -> vsep (fmap (uncurry genFieldSize) (zip [0..] fields))
        ]
    ]
  where
    genFieldSize :: Int -> FieldInfo -> Doc ann
    genFieldSize idx fi =
      let op = if idx == 0 then mempty else pretty ("+ " :: Text)
          accessor = "msg." <> hsFieldName (fiName fi)
          fn = T.pack (show (fiFieldNum fi))
      in op <> genSizeExpr fn accessor (fiLabel fi) (fiType fi)

genSizeExpr :: Text -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genSizeExpr fn accessor lbl ft = case lbl of
  Just Repeated -> pretty ("(sizeRepeated " :: Text) <> pretty fn <+> pretty accessor <> pretty (")" :: Text)
  Just Optional -> pretty ("(maybe 0 (\\v -> " :: Text) <> genSingleSize fn "v" ft <> pretty (") " :: Text) <> pretty accessor <> pretty (")" :: Text)
  _ -> pretty ("(if " :: Text) <> defaultCheck accessor ft <> pretty (" then 0 else " :: Text) <> genSingleSize fn accessor ft <> pretty (")" :: Text)

genSingleSize :: Text -> Text -> FieldType -> Doc ann
genSingleSize fn accessor = \case
  FTScalar SDouble   -> pretty ("fieldDoubleSize " :: Text) <> pretty fn
  FTScalar SFloat    -> pretty ("fieldFloatSize " :: Text) <> pretty fn
  FTScalar SFixed32  -> pretty ("fieldFixed32Size " :: Text) <> pretty fn
  FTScalar SFixed64  -> pretty ("fieldFixed64Size " :: Text) <> pretty fn
  FTScalar SSFixed32 -> pretty ("fieldFixed32Size " :: Text) <> pretty fn
  FTScalar SSFixed64 -> pretty ("fieldFixed64Size " :: Text) <> pretty fn
  FTScalar SBool     -> pretty ("fieldBoolSize " :: Text) <> pretty fn
  FTScalar SInt32    -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SInt64    -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SUInt32   -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty ("(fromIntegral " :: Text) <> pretty accessor <> pretty (")" :: Text)
  FTScalar SUInt64   -> pretty ("fieldVarintSize " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SSInt32   -> pretty ("fieldSVarint32Size " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SSInt64   -> pretty ("fieldSVarint64Size " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SString   -> pretty ("fieldTextSize " :: Text) <> pretty fn <+> pretty accessor
  FTScalar SBytes    -> pretty ("fieldBytesSize " :: Text) <> pretty fn <+> pretty accessor
  FTNamed _          -> pretty ("fieldMessageSize " :: Text) <> pretty fn <+> pretty ("(messageSize " :: Text) <> pretty accessor <> pretty (")" :: Text)

packedFnName :: ScalarType -> Text
packedFnName = \case
  SDouble   -> "PackedDouble"
  SFloat    -> "PackedFloat"
  SInt32    -> "PackedVarint"
  SInt64    -> "PackedVarint"
  SUInt32   -> "PackedVarint"
  SUInt64   -> "PackedVarint"
  SSInt32   -> "PackedSVarint32"
  SSInt64   -> "PackedSVarint64"
  SFixed32  -> "PackedFixed32"
  SFixed64  -> "PackedFixed64"
  SSFixed32 -> "PackedFixed32"
  SSFixed64 -> "PackedFixed64"
  SBool     -> "PackedVarint"
  s         -> error ("Cannot pack: " <> show s)

data FieldInfo = FieldInfo
  { fiName     :: Text
  , fiFieldNum :: Int
  , fiLabel    :: Maybe FieldLabel
  , fiType     :: FieldType
  }

extractFields :: [MessageElement] -> [FieldInfo]
extractFields = concatMap go
  where
    go = \case
      MEField fd -> [FieldInfo
        { fiName     = fieldName fd
        , fiFieldNum = unFieldNumber (fieldNumber fd)
        , fiLabel    = fieldLabel fd
        , fiType     = fieldType fd
        }]
      _ -> []
