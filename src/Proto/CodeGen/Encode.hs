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
import Proto.CodeGen.Combinators (txt)
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageEncode instance for a message.
genEncodeInstance :: MessageDef -> Doc ann
genEncodeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
    [ txt "instance MessageEncode" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
    , indent 2 $ vsep
        [ txt "buildMessage msg ="
        , indent 2 $ case fields of
            [] -> txt "mempty"
            _  -> vsep (fmap (uncurry genFieldBuild) (zip [0..] fields))
        ]
    ]
  where
    genFieldBuild :: Int -> FieldInfo -> Doc ann
    genFieldBuild idx fi =
      let op = if idx == 0 then mempty else txt "<> "
          accessor = "msg." <> hsFieldName (fiName fi)
          fn = T.pack (show (fiFieldNum fi))
      in op <> genBuildExpr fn accessor (fiLabel fi) (fiType fi)

genBuildExpr :: Text -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genBuildExpr fn accessor lbl ft = case lbl of
  Just Repeated -> genRepeatedBuild fn accessor ft
  Just Optional -> txt "(maybe mempty (\\v -> " <> genSingleBuild fn "v" ft <> txt ") " <> pretty accessor <> txt ")"
  _ -> genProto3Build fn accessor ft

genProto3Build :: Text -> Text -> FieldType -> Doc ann
genProto3Build fn accessor ft =
  let cond = defaultCheck accessor ft
  in txt "(if " <> cond <> txt " then mempty else " <> genSingleBuild fn accessor ft <> txt ")"

genSingleBuild :: Text -> Text -> FieldType -> Doc ann
genSingleBuild fn accessor = \case
  FTScalar SDouble   -> txt "encodeFieldDouble " <> pretty fn <+> pretty accessor
  FTScalar SFloat    -> txt "encodeFieldFloat " <> pretty fn <+> pretty accessor
  FTScalar SInt32    -> txt "encodeFieldVarint " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SInt64    -> txt "encodeFieldVarint " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt32   -> txt "encodeFieldVarint " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt64   -> txt "encodeFieldVarint " <> pretty fn <+> pretty accessor
  FTScalar SSInt32   -> txt "encodeFieldSVarint32 " <> pretty fn <+> pretty accessor
  FTScalar SSInt64   -> txt "encodeFieldSVarint64 " <> pretty fn <+> pretty accessor
  FTScalar SFixed32  -> txt "encodeFieldFixed32 " <> pretty fn <+> pretty accessor
  FTScalar SFixed64  -> txt "encodeFieldFixed64 " <> pretty fn <+> pretty accessor
  FTScalar SSFixed32 -> txt "encodeFieldFixed32 " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SSFixed64 -> txt "encodeFieldFixed64 " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SBool     -> txt "encodeFieldBool " <> pretty fn <+> pretty accessor
  FTScalar SString   -> txt "encodeFieldString " <> pretty fn <+> pretty accessor
  FTScalar SBytes    -> txt "encodeFieldBytes " <> pretty fn <+> pretty accessor
  FTNamed _          -> txt "encodeFieldMessageSized " <> pretty fn <+> pretty accessor

genRepeatedBuild :: Text -> Text -> FieldType -> Doc ann
genRepeatedBuild fn accessor = \case
  FTScalar SString ->
    txt "V.foldl' (\\acc v -> acc <> encodeFieldString " <> pretty fn <+> txt "v) mempty " <> pretty accessor
  FTScalar SBytes ->
    txt "V.foldl' (\\acc v -> acc <> encodeFieldBytes " <> pretty fn <+> txt "v) mempty " <> pretty accessor
  FTScalar s ->
    txt "encode" <> pretty (packedFnName s) <+> pretty fn <+> pretty accessor
  FTNamed _ ->
    txt "V.foldl' (\\acc v -> acc <> encodeFieldMessageSized " <> pretty fn <+> txt "v) mempty " <> pretty accessor

defaultCheck :: Text -> FieldType -> Doc ann
defaultCheck accessor = \case
  FTScalar SBool   -> pretty accessor <+> txt "== False"
  FTScalar SString -> pretty accessor <+> pretty ("== \"\"" :: Text)
  FTScalar SBytes  -> txt "BS.null " <> pretty accessor
  FTScalar _       -> pretty accessor <+> txt "== 0"
  FTNamed _        -> pretty accessor <+> txt "== Nothing"

-- | Generate a MessageSize instance.
genSizeInstance :: MessageDef -> Doc ann
genSizeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
    [ txt "instance MessageSize" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
    , indent 2 $ vsep
        [ txt "messageSize msg ="
        , indent 2 $ case fields of
            [] -> txt "0"
            _  -> vsep (fmap (uncurry genFieldSize) (zip [0..] fields))
        ]
    ]
  where
    genFieldSize :: Int -> FieldInfo -> Doc ann
    genFieldSize idx fi =
      let op = if idx == 0 then mempty else txt "+ "
          accessor = "msg." <> hsFieldName (fiName fi)
          fn = T.pack (show (fiFieldNum fi))
      in op <> genSizeExpr fn accessor (fiLabel fi) (fiType fi)

genSizeExpr :: Text -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genSizeExpr fn accessor lbl ft = case lbl of
  Just Repeated -> txt "(sizeRepeated " <> pretty fn <+> pretty accessor <> txt ")"
  Just Optional -> txt "(maybe 0 (\\v -> " <> genSingleSize fn "v" ft <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> defaultCheck accessor ft <> txt " then 0 else " <> genSingleSize fn accessor ft <> txt ")"

genSingleSize :: Text -> Text -> FieldType -> Doc ann
genSingleSize fn accessor = \case
  FTScalar SDouble   -> txt "fieldDoubleSize " <> pretty fn
  FTScalar SFloat    -> txt "fieldFloatSize " <> pretty fn
  FTScalar SFixed32  -> txt "fieldFixed32Size " <> pretty fn
  FTScalar SFixed64  -> txt "fieldFixed64Size " <> pretty fn
  FTScalar SSFixed32 -> txt "fieldFixed32Size " <> pretty fn
  FTScalar SSFixed64 -> txt "fieldFixed64Size " <> pretty fn
  FTScalar SBool     -> txt "fieldBoolSize " <> pretty fn
  FTScalar SInt32    -> txt "fieldVarintSize " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SInt64    -> txt "fieldVarintSize " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt32   -> txt "fieldVarintSize " <> pretty fn <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt64   -> txt "fieldVarintSize " <> pretty fn <+> pretty accessor
  FTScalar SSInt32   -> txt "fieldSVarint32Size " <> pretty fn <+> pretty accessor
  FTScalar SSInt64   -> txt "fieldSVarint64Size " <> pretty fn <+> pretty accessor
  FTScalar SString   -> txt "fieldTextSize " <> pretty fn <+> pretty accessor
  FTScalar SBytes    -> txt "fieldBytesSize " <> pretty fn <+> pretty accessor
  FTNamed _          -> txt "fieldMessageSize " <> pretty fn <+> txt "(messageSize " <> pretty accessor <> txt ")"

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
