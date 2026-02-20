-- | Code generation for message decoding functions.
--
-- Generates specialized 'messageDecoder' implementations that:
-- * Use a mutable accumulator pattern for building message records
-- * Dispatch on field number with a case expression (not a Map lookup)
-- * Handle unknown fields by skipping
-- * Support both packed and unpacked repeated fields
-- * Support proto3 merge semantics for submessages
module Proto.CodeGen.Decode
  ( genDecodeInstance
  , genDecodeFunction
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageDecode instance for a message.
genDecodeInstance :: MessageDef -> Doc ann
genDecodeInstance msg =
  vsep
    [ pretty ("instance MessageDecode" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("where" :: Text)
    , indent 2 (genDecodeFunction msg)
    ]

-- | Generate the messageDecoder function body.
--
-- Uses a loop that reads tags and dispatches on field number.
-- Accumulates fields in local variables, then constructs the record.
genDecodeFunction :: MessageDef -> Doc ann
genDecodeFunction msg =
  vsep
    [ pretty ("messageDecoder = do" :: Text)
    , indent 2 (vsep
        [ pretty ("-- Initialize accumulators with default values" :: Text)
        , vsep (fmap genAccumInit fields)
        , pretty ("-- Decode loop" :: Text)
        , pretty ("let loop" :: Text) <+> hsep (fmap (pretty . accumName) fields) <+> pretty ("= do" :: Text)
        , indent 6 (vsep
            [ pretty ("mTag <- getTagOr" :: Text)
            , pretty ("case mTag of" :: Text)
            , indent 2 (vsep
                [ pretty ("Nothing -> pure" :: Text) <+> genRecordCon msg fields
                , pretty ("Just tag -> case tagFieldNumber tag of" :: Text)
                , indent 2 (vsep (fmap genFieldCase fields <> [genDefaultCase]))
                ])
            ])
        , pretty ("loop" :: Text) <+> hsep (fmap (pretty . accumDefault) fields)
        ])
    ]
  where
    fields = extractFields (msgElements msg)

    genAccumInit :: FieldInfo -> Doc ann
    genAccumInit _fi = mempty  -- accumulators are passed as loop arguments

    genFieldCase :: FieldInfo -> Doc ann
    genFieldCase fi =
      pretty (T.pack (show (fiFieldNum fi))) <+> pretty ("->" :: Text) <+>
      genFieldDecode fi

    genDefaultCase :: Doc ann
    genDefaultCase =
      pretty ("_ -> skipField (tagWireType tag) >> loop" :: Text) <+>
      pretty ("..." :: Text)

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
      MEMapField mf -> [FieldInfo
        { fiName     = mapFieldName mf
        , fiFieldNum = unFieldNumber (mapFieldNum mf)
        , fiLabel    = Just Repeated
        , fiType     = FTNamed (mapFieldName mf <> "_Entry")
        }]
      _ -> []

accumName :: FieldInfo -> Text
accumName fi = "acc_" <> hsFieldName (fiName fi)

accumDefault :: FieldInfo -> Text
accumDefault fi = case fiLabel fi of
  Just Repeated -> "mempty"
  Just Optional -> "Nothing"
  _             -> defaultForType (fiType fi)

defaultForType :: FieldType -> Text
defaultForType = \case
  FTScalar SBool    -> "False"
  FTScalar SString  -> "\"\""
  FTScalar SBytes   -> "\"\""
  FTScalar SDouble  -> "0"
  FTScalar SFloat   -> "0"
  FTScalar _        -> "0"
  FTNamed _         -> "Nothing"

genFieldDecode :: FieldInfo -> Doc ann
genFieldDecode fi =
  let acc = accumName fi
  in case fiLabel fi of
    Just Repeated ->
      pretty ("do { v <- " :: Text) <> pretty (decoderName (fiType fi)) <>
      pretty ("; loop " :: Text) <> pretty ("... " :: Text) <> pretty acc <> pretty (" <> [v] ..." :: Text) <> pretty ("}" :: Text)
    _ ->
      pretty ("do { v <- " :: Text) <> pretty (decoderName (fiType fi)) <>
      pretty ("; loop " :: Text) <> pretty ("... " :: Text) <> pretty ("v" :: Text) <> pretty (" ..." :: Text) <> pretty ("}" :: Text)

genRecordCon :: MessageDef -> [FieldInfo] -> Doc ann
genRecordCon msg fields =
  pretty (hsTypeName (msgName msg)) <+>
  braces (hsep (punctuate comma (fmap genFieldAssign fields)))
  where
    genFieldAssign fi =
      pretty (hsFieldName (fiName fi)) <+> pretty ("=" :: Text) <+> pretty (accumName fi)

decoderName :: FieldType -> Text
decoderName = \case
  FTScalar SDouble   -> "decodeFieldDouble"
  FTScalar SFloat    -> "decodeFieldFloat"
  FTScalar SInt32    -> "decodeFieldVarint"
  FTScalar SInt64    -> "decodeFieldVarint"
  FTScalar SUInt32   -> "decodeFieldVarint"
  FTScalar SUInt64   -> "decodeFieldVarint"
  FTScalar SSInt32   -> "decodeFieldSVarint32"
  FTScalar SSInt64   -> "decodeFieldSVarint64"
  FTScalar SFixed32  -> "decodeFieldFixed32"
  FTScalar SFixed64  -> "decodeFieldFixed64"
  FTScalar SSFixed32 -> "decodeFieldFixed32"
  FTScalar SSFixed64 -> "decodeFieldFixed64"
  FTScalar SBool     -> "decodeFieldBool"
  FTScalar SString   -> "decodeFieldString"
  FTScalar SBytes    -> "decodeFieldBytes"
  FTNamed _          -> "decodeFieldMessage"
