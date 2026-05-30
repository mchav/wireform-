{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Read @buf.validate@ rules out of a /compiled/ protobuf descriptor
-- (@FileDescriptorProto@ / @DescriptorProto@), as produced by @protoc@ and
-- carried in a @FileDescriptorSet@.
--
-- protovalidate stores its rules as option /extensions/: extension #1159 on
-- @google.protobuf.FieldOptions@ (a @buf.validate.FieldConstraints@) and on
-- @google.protobuf.MessageOptions@ (a @buf.validate.MessageConstraints@).
-- Because "Proto.Google.Protobuf.Descriptor" now preserves unknown fields,
-- those extension bytes survive decoding; this module pulls them out and maps
-- them onto the validation 'MessageRules' model.
--
-- The universal parts — custom CEL constraints (@cel@), @required@, and
-- @ignore@ — are always read. The standard rule sets are mapped for the common
-- kinds (string / numeric / bool / bytes / repeated / map) using the
-- @buf.validate@ v1 field numbers; unrecognized rule fields are ignored.
module Protovalidate.Descriptor
  ( fileRulesFromDescriptor
  , messageRulesFromDescriptor
  , fieldConstraintExtension
  , messageConstraintExtension
  ) where

import qualified Data.ByteString as BS
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Int (Int32, Int64)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import CEL.Value (Value (..))
import Proto.Decode (UnknownField (..))
import Proto.Google.Protobuf.Descriptor
import Protovalidate.Constraint (Constraint, mkConstraint)
import Protovalidate.Rules

-- buf.validate extension number on FieldOptions / MessageOptions / OneofOptions.
bufValidateExtension :: Int
bufValidateExtension = 1159

----------------------------------------------------------------------
-- Extension byte extraction
----------------------------------------------------------------------

-- | The raw @buf.validate.FieldConstraints@ bytes attached to a field's
-- options, if present.
fieldConstraintExtension :: FieldOptions -> Maybe BS.ByteString
fieldConstraintExtension = lenDelimUnknown bufValidateExtension . fldoUnknownFields

-- | The raw @buf.validate.MessageConstraints@ bytes attached to a message's
-- options, if present.
messageConstraintExtension :: MessageOptions -> Maybe BS.ByteString
messageConstraintExtension = lenDelimUnknown bufValidateExtension . moUnknownFields

lenDelimUnknown :: Int -> [UnknownField] -> Maybe BS.ByteString
lenDelimUnknown fn ufs = case [bs | UnknownLenDelim n bs <- ufs, n == fn] of
  (bs : _) -> Just bs
  [] -> Nothing

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------

-- | Extract validation rules for every message defined in a file descriptor.
fileRulesFromDescriptor :: FileDescriptorProto -> Either Text [(Text, MessageRules)]
fileRulesFromDescriptor fdp =
  let msgs = collectMessages (fdpMessageType fdp)
   in traverse (\m -> (,) (dpName m) <$> extractMessage msgs Set.empty m) msgs

-- | Extract validation rules for a single (top-level or nested) message by
-- name.
messageRulesFromDescriptor :: FileDescriptorProto -> Text -> Either Text MessageRules
messageRulesFromDescriptor fdp name =
  let msgs = collectMessages (fdpMessageType fdp)
   in case [m | m <- msgs, dpName m == name] of
        (m : _) -> extractMessage msgs Set.empty m
        [] -> Left ("no such message: " <> name)

collectMessages :: V.Vector DescriptorProto -> [DescriptorProto]
collectMessages = concatMap go . V.toList
  where
    go m = m : collectMessages (dpNestedType m)

----------------------------------------------------------------------
-- Message / field extraction
----------------------------------------------------------------------

extractMessage :: [DescriptorProto] -> Set Text -> DescriptorProto -> Either Text MessageRules
extractMessage msgs visiting dp = do
  fields <- traverse (extractField msgs (Set.insert (dpName dp) visiting)) (V.toList (dpField dp))
  msgCustoms <- case dpOptions dp >>= messageConstraintExtension of
    Nothing -> Right []
    Just bytes -> messageConstraintsCel bytes
  Right (messageRules fields msgCustoms)

extractField :: [DescriptorProto] -> Set Text -> FieldDescriptorProto -> Either Text (Text, FieldRules)
extractField msgs visiting fdp = do
  let isRepeated = fdpFieldLabel fdp == 3
      namedMsg = resolveMessage msgs visiting (fdpFieldTypeName fdp) (fdpFieldType fdp)
  fr0 <- case fdpFieldOptions fdp >>= fieldConstraintExtension of
    Nothing -> Right emptyFieldRules
    Just bytes -> fieldConstraints bytes
  let fr =
        if isRepeated
          then case frKind fr0 of
            Just KRepeated -> fr0 {frItems = mergeItemMessage namedMsg (frItems fr0)}
            -- scalar rules on a repeated field actually describe the items
            _ ->
              emptyFieldRules
                { frRequired = frRequired fr0
                , frIgnoreEmpty = frIgnoreEmpty fr0
                , frCustom = frCustom fr0
                , frKind = Just KRepeated
                , frItems = mergeItemMessage namedMsg (itemsFromScalar fr0)
                }
          else fr0 {frMessage = namedMsg}
  Right (fdpFieldName fdp, fr)
  where
    itemsFromScalar fr0 =
      if frKind fr0 == Nothing && null (frRules fr0)
        then Nothing
        else Just emptyFieldRules {frKind = frKind fr0, frRules = frRules fr0}
    mergeItemMessage Nothing items = items
    mergeItemMessage msg Nothing = Just emptyFieldRules {frMessage = msg}
    mergeItemMessage msg (Just it) = Just it {frMessage = msg}

resolveMessage :: [DescriptorProto] -> Set Text -> Text -> Int32 -> Maybe MessageRules
resolveMessage msgs visiting typeName fieldType
  | fieldType /= 11 = Nothing -- not a message field (TYPE_MESSAGE == 11)
  | otherwise =
      let simple = last (T.splitOn "." typeName)
       in if Set.member simple visiting
            then Nothing
            else case [m | m <- msgs, dpName m == simple] of
              (m : _) -> either (const Nothing) Just (extractMessage msgs visiting m)
              [] -> Nothing

----------------------------------------------------------------------
-- buf.validate message decoding (via a self-contained wire scanner so that
-- repeated fields such as `cel` are preserved)
----------------------------------------------------------------------

-- Decode a FieldConstraints message into FieldRules.
fieldConstraints :: BS.ByteString -> Either Text FieldRules
fieldConstraints bytes = do
  let fs = scanFields bytes
  customs <- traverse constraintFromBytes [b | (23, WLen b) <- fs]
  let required = any (\(n, v) -> n == 25 && wvTrue v) fs
      ignored = any (\(n, v) -> n == 27 && wvNonZero v) fs
      ruleEntry = firstJust [(,) k b | (n, WLen b) <- fs, Just k <- [ruleKindOf n]]
  pure $ case ruleEntry of
    Just (kind, sub) ->
      let rules = decodeRuleMessage kind sub
       in emptyFieldRules
            { frRequired = required
            , frIgnoreEmpty = ignored
            , frCustom = customs
            , frKind = Just kind
            , frRules = rules
            }
    Nothing ->
      emptyFieldRules {frRequired = required, frIgnoreEmpty = ignored, frCustom = customs}
  where
    firstJust xs = case xs of (x : _) -> Just x; [] -> Nothing

-- Decode the `cel` repeated Constraint of a MessageConstraints message (field 3).
messageConstraintsCel :: BS.ByteString -> Either Text [Constraint]
messageConstraintsCel bytes =
  traverse constraintFromBytes [b | (3, WLen b) <- scanFields bytes]

-- Constraint { id=1 string, message=2 string, expression=3 string }
constraintFromBytes :: BS.ByteString -> Either Text Constraint
constraintFromBytes bytes =
  let fs = scanFields bytes
      str n = fromMaybe "" (firstStr n fs)
      cid = str 1
      msg = str 2
      expr = str 3
   in if T.null expr
        then Left "buf.validate constraint missing expression"
        else case mkConstraint cid msg expr of
          Left e -> Left ("invalid CEL in constraint '" <> cid <> "': " <> T.pack (show e))
          Right c -> Right c

firstStr :: Int -> [(Int, WV)] -> Maybe Text
firstStr n fs = case [b | (m, WLen b) <- fs, m == n] of
  (b : _) -> Just (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') b)
  [] -> Nothing

-- Map the FieldConstraints oneof field number to a RuleKind.
ruleKindOf :: Int -> Maybe RuleKind
ruleKindOf = \case
  1 -> Just KFloat
  2 -> Just KDouble
  3 -> Just KInt32
  4 -> Just KInt64
  5 -> Just KUint32
  6 -> Just KUint64
  7 -> Just KSint32
  8 -> Just KSint64
  9 -> Just KFixed32
  10 -> Just KFixed64
  11 -> Just KSfixed32
  12 -> Just KSfixed64
  13 -> Just KBool
  14 -> Just KString
  15 -> Just KBytes
  16 -> Just KEnum
  18 -> Just KRepeated
  19 -> Just KMap
  21 -> Just KDuration
  22 -> Just KTimestamp
  _ -> Nothing

-- Decode a rule submessage (e.g. StringRules) into (ruleName, value) pairs.
decodeRuleMessage :: RuleKind -> BS.ByteString -> [(Text, Value)]
decodeRuleMessage kind bytes =
  let fs = scanFields bytes
   in mergeIn [(name, ruleVal kind name wv) | (n, wv) <- fs, Just name <- [ruleName kind n]]
  where
    -- Merge repeated in/not_in entries into a single list value.
    mergeIn entries =
      let names = nubOrd (map fst entries)
       in [ (nm, combine nm [v | (k, v) <- entries, k == nm]) | nm <- names ]
    combine nm vs
      | nm == "in" || nm == "not_in" = VList (V.fromList vs)
      | otherwise = last vs
    nubOrd = go Set.empty
      where
        go _ [] = []
        go seen (x : xs) | Set.member x seen = go seen xs | otherwise = x : go (Set.insert x seen) xs

-- Field-number -> rule-name tables per kind (buf.validate v1).
ruleName :: RuleKind -> Int -> Maybe Text
ruleName kind n = case kind of
  KString -> stringRuleName n
  KBytes -> bytesRuleName n
  KBool -> if n == 1 then Just "const" else Nothing
  KRepeated -> case n of 1 -> Just "min_items"; 2 -> Just "max_items"; 3 -> Just "unique"; _ -> Nothing
  KMap -> case n of 1 -> Just "min_pairs"; 2 -> Just "max_pairs"; _ -> Nothing
  KEnum -> numericRuleName n
  _ -> numericRuleName n -- all numeric kinds share const/lt/lte/gt/gte/in/not_in

numericRuleName :: Int -> Maybe Text
numericRuleName = \case
  1 -> Just "const"; 2 -> Just "lt"; 3 -> Just "lte"; 4 -> Just "gt"; 5 -> Just "gte"
  6 -> Just "in"; 7 -> Just "not_in"; _ -> Nothing

stringRuleName :: Int -> Maybe Text
stringRuleName = \case
  1 -> Just "const"; 2 -> Just "min_len"; 3 -> Just "max_len"; 4 -> Just "min_bytes"
  5 -> Just "max_bytes"; 6 -> Just "pattern"; 7 -> Just "prefix"; 8 -> Just "suffix"
  9 -> Just "contains"; 10 -> Just "in"; 11 -> Just "not_in"; 12 -> Just "email"
  13 -> Just "hostname"; 14 -> Just "ip"; 15 -> Just "ipv4"; 16 -> Just "ipv6"
  17 -> Just "uri"; 18 -> Just "uri_ref"; 19 -> Just "len"; 21 -> Just "address"
  22 -> Just "uuid"; 23 -> Just "not_contains"; _ -> Nothing

bytesRuleName :: Int -> Maybe Text
bytesRuleName = \case
  1 -> Just "const"; 2 -> Just "min_len"; 3 -> Just "max_len"; 9 -> Just "in"
  10 -> Just "not_in"; 13 -> Just "len"; _ -> Nothing

countRuleNames :: [Text]
countRuleNames = ["min_len", "max_len", "len", "min_bytes", "max_bytes", "min_items", "max_items", "min_pairs", "max_pairs"]

stringValueRules :: [Text]
stringValueRules = ["const", "pattern", "prefix", "suffix", "contains", "not_contains", "in", "not_in"]

flagRules :: [Text]
flagRules = ["email", "hostname", "ip", "ipv4", "ipv6", "uri", "uri_ref", "address", "uuid", "unique"]

-- Convert a wire value to a CEL value, given the rule kind and rule name.
ruleVal :: RuleKind -> Text -> WV -> Value
ruleVal kind name wv
  | name `elem` countRuleNames = VUInt (wvWord wv)
  | name `elem` flagRules = VBool (wvTrue wv)
  | kind == KString && name `elem` stringValueRules = VString (wvText wv)
  | kind == KBytes && name `elem` ["const", "in", "not_in"] = VBytes (wvBytes wv)
  | otherwise = numericVal kind wv

numericVal :: RuleKind -> WV -> Value
numericVal kind wv = case kind of
  KBool -> VBool (wvTrue wv)
  KFloat -> VDouble (realToFrac (castWord32ToFloat (wvWord32 wv)))
  KDouble -> VDouble (castWord64ToDouble (wvWord wv))
  KUint32 -> VUInt (wvWord wv)
  KUint64 -> VUInt (wvWord wv)
  KFixed32 -> VUInt (wvWord wv)
  KFixed64 -> VUInt (wvWord wv)
  KSint32 -> VInt (zigzag (wvWord wv))
  KSint64 -> VInt (zigzag (wvWord wv))
  _ -> VInt (fromIntegral (wvWord wv) :: Int64) -- int32/int64/sfixed/enum

----------------------------------------------------------------------
-- Minimal protobuf wire scanner
----------------------------------------------------------------------

data WV = WVarint !Word64 | WI64 !Word64 | WI32 !Word32 | WLen !BS.ByteString
  deriving stock (Show)

wvWord :: WV -> Word64
wvWord (WVarint v) = v
wvWord (WI64 v) = v
wvWord (WI32 v) = fromIntegral v
wvWord (WLen _) = 0

wvWord32 :: WV -> Word32
wvWord32 (WI32 v) = v
wvWord32 wv = fromIntegral (wvWord wv)

wvTrue :: WV -> Bool
wvTrue = wvNonZero

wvNonZero :: WV -> Bool
wvNonZero wv = wvWord wv /= 0

wvText :: WV -> Text
wvText (WLen b) = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') b
wvText _ = ""

wvBytes :: WV -> BS.ByteString
wvBytes (WLen b) = b
wvBytes _ = BS.empty

zigzag :: Word64 -> Int64
zigzag w = fromIntegral ((w `shiftR` 1) `xor` negate (w .&. 1))

-- | Scan a protobuf message into @(fieldNumber, value)@ pairs, preserving
-- repeated fields. Malformed tails are ignored.
scanFields :: BS.ByteString -> [(Int, WV)]
scanFields = go
  where
    go bs
      | BS.null bs = []
      | otherwise = case readVarint bs of
          Nothing -> []
          Just (tag, rest) ->
            let fn = fromIntegral (tag `shiftR` 3)
                wt = tag .&. 7
             in case wt of
                  0 -> case readVarint rest of
                    Just (v, r) -> (fn, WVarint v) : go r
                    Nothing -> []
                  1 ->
                    if BS.length rest >= 8
                      then (fn, WI64 (leWord64 (BS.take 8 rest))) : go (BS.drop 8 rest)
                      else []
                  5 ->
                    if BS.length rest >= 4
                      then (fn, WI32 (leWord32 (BS.take 4 rest))) : go (BS.drop 4 rest)
                      else []
                  2 -> case readVarint rest of
                    Just (len, r) ->
                      let n = fromIntegral len
                       in if BS.length r >= n
                            then (fn, WLen (BS.take n r)) : go (BS.drop n r)
                            else []
                    Nothing -> []
                  _ -> [] -- groups (3/4) unsupported; stop

readVarint :: BS.ByteString -> Maybe (Word64, BS.ByteString)
readVarint = goV 0 0
  where
    goV !shift !acc bs = case BS.uncons bs of
      Nothing -> Nothing
      Just (b, rest) ->
        let acc' = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shift)
         in if b .&. 0x80 /= 0
              then if shift >= 63 then Nothing else goV (shift + 7) acc' rest
              else Just (acc', rest)

leWord64 :: BS.ByteString -> Word64
leWord64 = BS.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0

leWord32 :: BS.ByteString -> Word32
leWord32 = BS.foldr (\b acc -> acc `shiftL` 8 .|. fromIntegral b) 0
