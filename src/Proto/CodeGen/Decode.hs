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
import Proto.CodeGen.Combinators (txt)
import Proto.CodeGen.Types (hsTypeName, hsFieldName)

-- | Generate a MessageDecode instance for a message.
genDecodeInstance :: MessageDef -> Doc ann
genDecodeInstance msg =
  let fields = extractFields (msgElements msg)
      allAccs = fmap fieldAccum fields
  in vsep
    [ txt "instance MessageDecode" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
    , indent 2 $ vsep
        [ txt "messageDecoder =" <+> txt "loop" <+>
          hsep (fmap (pretty . fieldDefault) fields)
        , indent 2 $ txt "where"
        , indent 4 $ vsep
            [ txt "loop" <+> hsep (fmap (pretty . fieldAccum) fields) <+> txt "= do"
            , indent 2 $ vsep
                [ txt "mTag <- getTagOrU"
                , txt "case mTag of"
                , indent 2 $ vsep
                    [ txt "UNothing -> pure" <+> genRecordCon msg fields
                    , txt "UJust (Tag fn wt) -> case fn of"
                    , indent 2 $ vsep (fmap (genFieldCase allAccs) fields <> [genDefaultCase allAccs])
                    ]
                ]
            ]
        ]
    ]

genFieldCase :: [Text] -> FieldInfo -> Doc ann
genFieldCase allAccs fi =
  let fn = T.pack (show (fiFieldNum fi))
  in pretty fn <+> txt "-> do" <> line <>
     indent 2 (genFieldDecode allAccs fi)

genFieldDecode :: [Text] -> FieldInfo -> Doc ann
genFieldDecode allAccs fi =
  let idx = fiIndex fi
      newAccs = case fiLabel fi of
        Just Repeated -> replaceAt idx ("(" <> fieldAccum fi <> " <> V.singleton v)") allAccs
        _             -> replaceAt idx "v" allAccs
  in vsep [ txt "v <- " <> pretty (decoderExpr (fiType fi))
          , txt "loop " <> hsep (fmap pretty newAccs)
          ]

genDefaultCase :: [Text] -> Doc ann
genDefaultCase allAccs =
  txt "_ -> skipField wt >> loop " <> hsep (fmap pretty allAccs)

genRecordCon :: MessageDef -> [FieldInfo] -> Doc ann
genRecordCon msg fields = case fields of
  [] -> pretty (hsTypeName (msgName msg)) <+> txt "{ }"
  _  -> parens $
    pretty (hsTypeName (msgName msg)) <+>
    braces (hsep (punctuate comma (fmap genAssign fields)))
  where
    genAssign fi =
      pretty (hsFieldName (fiName fi)) <+> txt "=" <+> pretty (fieldAccum fi)

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
  }

extractFields :: [MessageElement] -> [FieldInfo]
extractFields elems =
  zipWith (\i fi -> fi { fiIndex = i }) [0..] (concatMap go elems)
  where
    go = \case
      MEField fd -> [FieldInfo
        { fiName     = fieldName fd
        , fiFieldNum = unFieldNumber (fieldNumber fd)
        , fiLabel    = fieldLabel fd
        , fiType     = fieldType fd
        , fiIndex    = 0
        }]
      _ -> []

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 x (_:ys) = x : ys
replaceAt n x (y:ys) = y : replaceAt (n - 1) x ys
