{- | Code generation for message encoding functions.

Generates specialized 'buildMessage' implementations using the archetype
encode functions from 'Proto.Internal.Encode.Archetype', with tag bytes pre-computed
at code generation time as compile-time constants.

* First pass: compute exact wire size via 'messageSize'
* Second pass: serialize via 'buildMessage' using pre-known sizes
* Skip default-valued fields (proto3 semantics)
* Packed encoding for repeated scalar fields
* Minimize allocations: no intermediate ByteString for submessages
-}
module Proto.CodeGen.Encode (
  genEncodeInstance,
  genSizeInstance,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Proto.CodeGen.Types (hsFieldName, hsTypeName)
import Proto.IDL.AST
import Proto.Internal.CodeGen.Combinators (txt)


wireVarint, wire64Bit, wireLengthDelimited, wire32Bit :: Int
wireVarint = 0
wire64Bit = 1
wireLengthDelimited = 2
wire32Bit = 5


computeTagByte :: Int -> Int -> Int
computeTagByte fieldNum wireType = fieldNum * 8 + wireType


-- | Generate a MessageEncode instance for a message.
genEncodeInstance :: MessageDef -> Doc ann
genEncodeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
      [ txt "instance MessageEncode" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
      , indent 2 $
          vsep
            [ txt "buildMessage msg ="
            , indent 2 $ case fields of
                [] -> txt "mempty"
                _ -> vsep (fmap (uncurry genFieldBuild) (zip [0 ..] fields))
            ]
      ]
  where
    genFieldBuild :: Int -> FieldInfo -> Doc ann
    genFieldBuild idx fi =
      let op = if idx == 0 then mempty else txt "<> "
          accessor = "msg." <> hsFieldName (fiName fi)
          fieldNum = fiFieldNum fi
      in op <> genBuildExpr fieldNum accessor (fiLabel fi) (fiType fi)


genBuildExpr :: Int -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genBuildExpr fieldNum accessor lbl ft = case lbl of
  Just Repeated -> genRepeatedBuild fieldNum accessor ft
  Just Optional -> txt "(maybe mempty (\\v -> " <> genSingleBuild fieldNum "v" ft <> txt ") " <> pretty accessor <> txt ")"
  _ -> genProto3Build fieldNum accessor ft


genProto3Build :: Int -> Text -> FieldType -> Doc ann
genProto3Build fieldNum accessor ft =
  let cond = defaultCheck accessor ft
  in txt "(if " <> cond <> txt " then mempty else " <> genSingleBuild fieldNum accessor ft <> txt ")"


genSingleBuild :: Int -> Text -> FieldType -> Doc ann
genSingleBuild fieldNum accessor = \case
  FTScalar SDouble -> txt "archDouble " <> pretty (computeTagByte fieldNum wire64Bit) <+> pretty accessor
  FTScalar SFloat -> txt "archFloat " <> pretty (computeTagByte fieldNum wire32Bit) <+> pretty accessor
  FTScalar SInt32 -> txt "archVarint " <> pretty (computeTagByte fieldNum wireVarint) <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SInt64 -> txt "archVarint " <> pretty (computeTagByte fieldNum wireVarint) <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt32 -> txt "archVarint " <> pretty (computeTagByte fieldNum wireVarint) <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt64 -> txt "archVarint " <> pretty (computeTagByte fieldNum wireVarint) <+> pretty accessor
  FTScalar SSInt32 -> txt "archSVarint32 " <> pretty (computeTagByte fieldNum wireVarint) <+> pretty accessor
  FTScalar SSInt64 -> txt "archSVarint64 " <> pretty (computeTagByte fieldNum wireVarint) <+> pretty accessor
  FTScalar SFixed32 -> txt "archFixed32 " <> pretty (computeTagByte fieldNum wire32Bit) <+> pretty accessor
  FTScalar SFixed64 -> txt "archFixed64 " <> pretty (computeTagByte fieldNum wire64Bit) <+> pretty accessor
  FTScalar SSFixed32 -> txt "archFixed32 " <> pretty (computeTagByte fieldNum wire32Bit) <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SSFixed64 -> txt "archFixed64 " <> pretty (computeTagByte fieldNum wire64Bit) <+> txt "(fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SBool -> txt "archBool " <> pretty (computeTagByte fieldNum wireVarint) <+> pretty accessor
  FTScalar SString -> txt "archString " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> pretty accessor
  FTScalar SBytes -> txt "archBytes " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> pretty accessor
  FTNamed _ -> txt "(let sz = messageSize " <> pretty accessor <> txt " in archSubmessage " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> txt "sz (buildMessage " <> pretty accessor <> txt "))"


genRepeatedBuild :: Int -> Text -> FieldType -> Doc ann
genRepeatedBuild fieldNum accessor = \case
  FTScalar SString ->
    txt "V.foldl' (\\acc v -> acc <> archString " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> txt "v) mempty " <> pretty accessor
  FTScalar SBytes ->
    txt "V.foldl' (\\acc v -> acc <> archBytes " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> txt "v) mempty " <> pretty accessor
  FTScalar s ->
    txt "encode" <> pretty (packedFnName s) <+> pretty (T.pack (show fieldNum)) <+> pretty accessor
  FTNamed _ ->
    txt "V.foldl' (\\acc v -> let sz = messageSize v in acc <> archSubmessage " <> pretty (computeTagByte fieldNum wireLengthDelimited) <+> txt "sz (buildMessage v)) mempty " <> pretty accessor


defaultCheck :: Text -> FieldType -> Doc ann
defaultCheck accessor = \case
  FTScalar SBool -> pretty accessor <+> txt "== False"
  FTScalar SString -> pretty accessor <+> pretty ("== \"\"" :: Text)
  FTScalar SBytes -> txt "BS.null " <> pretty accessor
  FTScalar _ -> pretty accessor <+> txt "== 0"
  FTNamed _ -> pretty accessor <+> txt "== Nothing"


-- | Generate a MessageSize instance.
genSizeInstance :: MessageDef -> Doc ann
genSizeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
      [ txt "instance MessageSize" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
      , indent 2 $
          vsep
            [ txt "messageSize msg ="
            , indent 2 $ case fields of
                [] -> txt "0"
                _ -> vsep (fmap (uncurry genFieldSize) (zip [0 ..] fields))
            ]
      ]
  where
    genFieldSize :: Int -> FieldInfo -> Doc ann
    genFieldSize idx fi =
      let op = if idx == 0 then mempty else txt "+ "
          accessor = "msg." <> hsFieldName (fiName fi)
          fieldNum = fiFieldNum fi
      in op <> genSizeExpr fieldNum accessor (fiLabel fi) (fiType fi)


genSizeExpr :: Int -> Text -> Maybe FieldLabel -> FieldType -> Doc ann
genSizeExpr fieldNum accessor lbl ft = case lbl of
  Just Repeated -> txt "(sizeRepeated " <> pretty (T.pack (show fieldNum)) <+> pretty accessor <> txt ")"
  Just Optional -> txt "(maybe 0 (\\v -> " <> genSingleSize "v" ft <> txt ") " <> pretty accessor <> txt ")"
  _ -> txt "(if " <> defaultCheck accessor ft <> txt " then 0 else " <> genSingleSize accessor ft <> txt ")"


genSingleSize :: Text -> FieldType -> Doc ann
genSingleSize accessor = \case
  FTScalar SDouble -> txt "archFixed64Size"
  FTScalar SFloat -> txt "archFixed32Size"
  FTScalar SFixed32 -> txt "archFixed32Size"
  FTScalar SFixed64 -> txt "archFixed64Size"
  FTScalar SSFixed32 -> txt "archFixed32Size"
  FTScalar SSFixed64 -> txt "archFixed64Size"
  FTScalar SBool -> txt "archBoolSize"
  FTScalar SInt32 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SInt64 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt32 -> txt "archVarintSize (fromIntegral " <> pretty accessor <> txt ")"
  FTScalar SUInt64 -> txt "archVarintSize " <> pretty accessor
  FTScalar SSInt32 -> txt "(1 + varintSize (fromIntegral (zigZag32 " <> pretty accessor <> txt ")))"
  FTScalar SSInt64 -> txt "(1 + varintSize (zigZag64 " <> pretty accessor <> txt "))"
  FTScalar SString -> txt "archStringSize " <> pretty accessor
  FTScalar SBytes -> txt "archBytesSize " <> pretty accessor
  FTNamed _ -> txt "archSubmessageSize (messageSize " <> pretty accessor <> txt ")"


packedFnName :: ScalarType -> Text
packedFnName = \case
  SDouble -> "PackedDouble"
  SFloat -> "PackedFloat"
  SInt32 -> "PackedVarint"
  SInt64 -> "PackedVarint"
  SUInt32 -> "PackedVarint"
  SUInt64 -> "PackedVarint"
  SSInt32 -> "PackedSVarint32"
  SSInt64 -> "PackedSVarint64"
  SFixed32 -> "PackedFixed32"
  SFixed64 -> "PackedFixed64"
  SSFixed32 -> "PackedFixed32"
  SSFixed64 -> "PackedFixed64"
  SBool -> "PackedVarint"
  s -> error ("Cannot pack: " <> show s)


data FieldInfo = FieldInfo
  { fiName :: Text
  , fiFieldNum :: Int
  , fiLabel :: Maybe FieldLabel
  , fiType :: FieldType
  }


extractFields :: [MessageElement] -> [FieldInfo]
extractFields = concatMap go
  where
    go = \case
      MEField fd ->
        [ FieldInfo
            { fiName = fieldName fd
            , fiFieldNum = unFieldNumber (fieldNumber fd)
            , fiLabel = fieldLabel fd
            , fiType = fieldType fd
            }
        ]
      _ -> []
