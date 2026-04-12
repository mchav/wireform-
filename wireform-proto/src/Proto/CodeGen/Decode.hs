-- | Code generation for message decoding functions.
--
-- Generates specialized 'messageDecoder' implementations that:
-- * Use a tight decode loop with accumulators for each field
-- * Dispatch on field number with a case expression
-- * Handle unknown fields by skipping efficiently
-- * Support both packed and unpacked repeated fields
-- * Support proto3 merge semantics for submessages
--
-- Field-order scheduling (inspired by hyperpb):
-- After decoding field N, the generated code predicts that the next
-- field on the wire will be field N+1 (declaration order). This is
-- almost always correct since every protobuf encoder emits fields in
-- order. On a correct prediction, we skip the full case dispatch and
-- go straight to the right decoder — a single comparison instead of
-- a multi-way branch.
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
--
-- For messages with 2+ fields, generates per-field entry points that
-- predict the next field in declaration order (field-order scheduling).
-- For 0-1 field messages, uses the simpler flat dispatch.
genDecodeInstance :: MessageDef -> Doc ann
genDecodeInstance msg =
  let fields = extractFields (msgElements msg)
      allAccs = fmap fieldAccum fields
  in case fields of
    []  -> genDecodeInstanceSimple msg fields allAccs
    [_] -> genDecodeInstanceSimple msg fields allAccs
    _   -> genDecodeInstanceScheduled msg fields allAccs

-- | Simple flat dispatch (for 0-1 fields).
genDecodeInstanceSimple :: MessageDef -> [FieldInfo] -> [Text] -> Doc ann
genDecodeInstanceSimple msg fields allAccs =
  vsep
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
                    , indent 2 $ vsep (fmap (genFieldCaseSimple allAccs "loop") fields <> [genDefaultCase allAccs "loop"])
                    ]
                ]
            ]
        ]
    ]

-- | Field-order scheduled dispatch (for 2+ fields).
--
-- Generates:
--   loop_dispatch (the full case dispatch, used as fallback)
--   loop_after_0, loop_after_1, ... (per-field entry points)
--
-- Each loop_after_i first checks if fn == field[i+1].fieldNum;
-- on match, decodes and calls loop_after_{i+1}; on mismatch,
-- falls through to loop_dispatch.
genDecodeInstanceScheduled :: MessageDef -> [FieldInfo] -> [Text] -> Doc ann
genDecodeInstanceScheduled msg fields allAccs =
  let nFields = length fields
  in vsep
    [ txt "instance MessageDecode" <+> pretty (hsTypeName (msgName msg)) <+> txt "where"
    , indent 2 $ vsep
        [ txt "messageDecoder =" <+> txt "loop_dispatch" <+>
          hsep (fmap (pretty . fieldDefault) fields)
        , indent 2 $ txt "where"
        , indent 4 $ vsep $
            -- The full dispatch loop (fallback)
            [ genDispatchLoop msg fields allAccs nFields ] <>
            -- Per-field entry points with next-field prediction
            fmap (genAfterLoop msg fields allAccs nFields) [0 .. nFields - 1]
        ]
    ]

-- | Generate the full dispatch loop (fallback for mispredictions).
genDispatchLoop :: MessageDef -> [FieldInfo] -> [Text] -> Int -> Doc ann
genDispatchLoop msg fields allAccs nFields =
  vsep
    [ txt "loop_dispatch" <+> hsep (fmap (pretty . fieldAccum) fields) <+> txt "= do"
    , indent 2 $ vsep
        [ txt "mTag <- getTagOrU"
        , txt "case mTag of"
        , indent 2 $ vsep
            [ txt "UNothing -> pure" <+> genRecordCon msg fields
            , txt "UJust (Tag fn wt) -> case fn of"
            , indent 2 $ vsep (fmap (genFieldCaseScheduled allAccs nFields) fields
                              <> [genDefaultCase allAccs "loop_dispatch"])
            ]
        ]
    ]

-- | Generate a per-field "after" loop that predicts the next field.
genAfterLoop :: MessageDef -> [FieldInfo] -> [Text] -> Int -> Int -> Doc ann
genAfterLoop msg fields allAccs nFields currentIdx =
  let nextIdx = (currentIdx + 1) `mod` nFields
      nextField = fields !! nextIdx
      nextFn = T.pack (show (fiFieldNum nextField))
      afterName = afterLoopName nextIdx nFields
  in vsep
    [ txt (afterLoopNameForCurrent currentIdx) <+> hsep (fmap (pretty . fieldAccum) fields) <+> txt "= do"
    , indent 2 $ vsep
        [ txt "mTag <- getTagOrU"
        , txt "case mTag of"
        , indent 2 $ vsep
            [ txt "UNothing -> pure" <+> genRecordCon msg fields
            , txt "UJust (Tag fn wt)"
            , indent 2 $ vsep
                [ txt "| fn ==" <+> pretty nextFn <+> txt "-> do"
                , indent 4 $ genFieldDecodeScheduled allAccs nextField afterName nFields
                , txt "| otherwise -> case fn of"
                , indent 4 $ vsep (fmap (genFieldCaseScheduled allAccs nFields) fields
                                  <> [genDefaultCase allAccs "loop_dispatch"])
                ]
            ]
        ]
    ]

afterLoopNameForCurrent :: Int -> Text
afterLoopNameForCurrent idx = "loop_after_" <> T.pack (show idx)

afterLoopName :: Int -> Int -> Text
afterLoopName idx _nFields = "loop_after_" <> T.pack (show idx)

genFieldCaseSimple :: [Text] -> Text -> FieldInfo -> Doc ann
genFieldCaseSimple allAccs loopName fi =
  let fn = T.pack (show (fiFieldNum fi))
  in pretty fn <+> txt "-> do" <> line <>
     indent 2 (genFieldDecodeSimple allAccs fi loopName)

genFieldDecodeSimple :: [Text] -> FieldInfo -> Text -> Doc ann
genFieldDecodeSimple allAccs fi loopName =
  let idx = fiIndex fi
      newAccs = case fiLabel fi of
        Just Repeated -> replaceAt idx ("(" <> fieldAccum fi <> " <> V.singleton v)") allAccs
        _             -> replaceAt idx "v" allAccs
  in vsep [ txt "v <- " <> pretty (decoderExpr (fiType fi))
          , pretty loopName <+> hsep (fmap pretty newAccs)
          ]

genFieldCaseScheduled :: [Text] -> Int -> FieldInfo -> Doc ann
genFieldCaseScheduled allAccs nFields fi =
  let fn = T.pack (show (fiFieldNum fi))
      after = afterLoopName (fiIndex fi) nFields
  in pretty fn <+> txt "-> do" <> line <>
     indent 2 (genFieldDecodeScheduled allAccs fi after nFields)

genFieldDecodeScheduled :: [Text] -> FieldInfo -> Text -> Int -> Doc ann
genFieldDecodeScheduled allAccs fi loopName _nFields =
  let idx = fiIndex fi
      newAccs = case fiLabel fi of
        Just Repeated -> replaceAt idx ("(" <> fieldAccum fi <> " <> V.singleton v)") allAccs
        _             -> replaceAt idx "v" allAccs
  in vsep [ txt "v <- " <> pretty (decoderExpr (fiType fi))
          , pretty loopName <+> hsep (fmap pretty newAccs)
          ]

genDefaultCase :: [Text] -> Text -> Doc ann
genDefaultCase allAccs loopName =
  txt "_ -> skipField wt >> " <> pretty loopName <+> hsep (fmap pretty allAccs)

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
