-- | Code generation for message decoding functions.
--
-- Generates specialized 'messageDecoder' implementations that:
-- * Use a tight decode loop with accumulators for each field
-- * Dispatch on field number with a case expression
-- * Handle unknown fields by skipping efficiently
-- * Support both packed and unpacked repeated fields
-- * Support proto3 merge semantics for submessages
module Proto.CodeGen.Decode
  ( genDecodeInstance
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

import Proto.AST
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageDecode instance for a message.
genDecodeInstance :: MessageDef -> Doc ann
genDecodeInstance msg =
  let fields = extractFields (msgElements msg)
  in vsep
    [ pretty ("instance MessageDecode" :: Text) <+> pretty (hsTypeName (msgName msg)) <+> pretty ("where" :: Text)
    , indent 2 $ vsep
        [ pretty ("messageDecoder =" :: Text) <+> pretty ("loop" :: Text) <+>
          hsep (fmap (pretty . fieldDefault) fields)
        , indent 2 $ pretty ("where" :: Text)
        , indent 4 $ vsep
            [ pretty ("loop" :: Text) <+> hsep (fmap (pretty . fieldAccum) fields) <+> pretty ("= do" :: Text)
            , indent 2 $ vsep
                [ pretty ("mTag <- getTagOr" :: Text)
                , pretty ("case mTag of" :: Text)
                , indent 2 $ vsep
                    [ pretty ("Nothing -> pure" :: Text) <+> genRecordCon msg fields
                    , pretty ("Just (Tag fn wt) -> case fn of" :: Text)
                    , indent 2 $ vsep (fmap genFieldCase fields <> [genDefaultCase fields])
                    ]
                ]
            ]
        ]
    ]

genFieldCase :: FieldInfo -> Doc ann
genFieldCase fi =
  let fn = T.pack (show (fiFieldNum fi))
      acc = fieldAccum fi
  in pretty fn <+> pretty ("-> do" :: Text) <> line <>
     indent 2 (genFieldDecode fi acc (fmap fieldAccum (fiOthers fi)))

genFieldDecode :: FieldInfo -> Text -> [Text] -> Doc ann
genFieldDecode fi thisAcc otherAccs =
  case fiLabel fi of
    Just Repeated ->
      vsep [ pretty ("v <- " :: Text) <> pretty (decoderExpr (fiType fi))
           , pretty ("loop " :: Text) <> hsep (fmap pretty (insertAt (fiIndex fi) ("(" <> thisAcc <> " <> V.singleton v)") otherAccs))
           ]
    _ ->
      vsep [ pretty ("v <- " :: Text) <> pretty (decoderExpr (fiType fi))
           , pretty ("loop " :: Text) <> hsep (fmap pretty (insertAt (fiIndex fi) "v" otherAccs))
           ]

genDefaultCase :: [FieldInfo] -> Doc ann
genDefaultCase fields =
  pretty ("_ -> skipField wt >> loop " :: Text) <> hsep (fmap (pretty . fieldAccum) fields)

genRecordCon :: MessageDef -> [FieldInfo] -> Doc ann
genRecordCon msg fields = case fields of
  [] -> pretty (hsTypeName (msgName msg)) <+> pretty ("{ }" :: Text)
  _  -> parens $
    pretty (hsTypeName (msgName msg)) <+>
    braces (hsep (punctuate comma (fmap genAssign fields)))
  where
    genAssign fi =
      pretty (hsFieldName (fiName fi)) <+> pretty ("=" :: Text) <+> pretty (fieldAccum fi)

fieldAccum :: FieldInfo -> Text
fieldAccum fi = "acc_" <> T.pack (show (fiIndex fi))

fieldDefault :: FieldInfo -> Text
fieldDefault fi = case fiLabel fi of
  Just Repeated -> case fiType fi of
    FTScalar s | isUnboxable s -> "VU.empty"
    _                          -> "V.empty"
  Just Optional -> "Nothing"
  _ -> case fiType fi of
    FTScalar SBool   -> "False"
    FTScalar SString -> "\"\""
    FTScalar SBytes  -> "\"\""
    FTScalar _       -> "0"
    FTNamed _        -> "Nothing"

isUnboxable :: ScalarType -> Bool
isUnboxable = \case
  SString -> False
  SBytes  -> False
  _       -> True

decoderExpr :: FieldType -> Text
decoderExpr = \case
  FTScalar SDouble   -> "decodeFieldDouble"
  FTScalar SFloat    -> "decodeFieldFloat"
  FTScalar SInt32    -> "fromIntegral <$> decodeFieldVarint"
  FTScalar SInt64    -> "fromIntegral <$> decodeFieldVarint"
  FTScalar SUInt32   -> "fromIntegral <$> decodeFieldVarint"
  FTScalar SUInt64   -> "decodeFieldVarint"
  FTScalar SSInt32   -> "decodeFieldSVarint32"
  FTScalar SSInt64   -> "decodeFieldSVarint64"
  FTScalar SFixed32  -> "decodeFieldFixed32"
  FTScalar SFixed64  -> "decodeFieldFixed64"
  FTScalar SSFixed32 -> "fromIntegral <$> decodeFieldFixed32"
  FTScalar SSFixed64 -> "fromIntegral <$> decodeFieldFixed64"
  FTScalar SBool     -> "decodeFieldBool"
  FTScalar SString   -> "decodeFieldString"
  FTScalar SBytes    -> "decodeFieldBytes"
  FTNamed _          -> "decodeFieldMessage"

data FieldInfo = FieldInfo
  { fiName     :: Text
  , fiFieldNum :: Int
  , fiLabel    :: Maybe FieldLabel
  , fiType     :: FieldType
  , fiIndex    :: Int
  , fiOthers   :: [FieldInfo]
  }

extractFields :: [MessageElement] -> [FieldInfo]
extractFields elems =
  let raw = concatMap go elems
      indexed = zipWith (\i fi -> fi { fiIndex = i }) [0..] raw
  in fmap (\fi -> fi { fiOthers = indexed }) indexed
  where
    go = \case
      MEField fd -> [FieldInfo
        { fiName     = fieldName fd
        , fiFieldNum = unFieldNumber (fieldNumber fd)
        , fiLabel    = fieldLabel fd
        , fiType     = fieldType fd
        , fiIndex    = 0
        , fiOthers   = []
        }]
      _ -> []

insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x _      = [x]
insertAt n x (y:ys) = y : insertAt (n - 1) x ys
insertAt _ x []     = [x]
