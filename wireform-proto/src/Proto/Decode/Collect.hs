{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Error-/accumulating/ message decoding.

The primary 'Proto.Decode.decodeMessage' is fail-fast: it stops at the first
'DecodeError'. This module adds a diagnostic, schema-driven pass that, like
the validation library, collects /all/ recoverable problems in a message at
once — each with a field path — instead of surfacing only the first.

'decodeCollecting' returns the list of 'DecodeIssue's plus the strictly
decoded value when (and only when) the message is clean. It is built on the
message's 'Proto.Schema.ProtoMessage' field descriptors and a self-contained
wire scanner, so it never touches the hot-path decoder.

What it reports:

  * invalid UTF-8 in @string@ fields (each occurrence, with path/index);
  * structurally malformed (e.g. truncated) sub-messages and map entries;
  * malformed top-level wire data (bad varint, truncation, bad wire type).

Deep semantic checks inside sub-messages require the nested type's schema;
this pass validates each sub-message's framing one level down. Wire-type
mismatches on scalar fields are intentionally not flagged because packed
repeated fields make them ambiguous.
-}
module Proto.Decode.Collect (
  DecodeIssue (..),
  decodeCollecting,
  collectIssues,
) where

import Control.DeepSeq (NFData)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Proto.Decode (DecodeError (..), MessageDecode, decodeMessage)
import Proto.Schema (
  FieldDescriptor (..),
  FieldLabel' (..),
  FieldTypeDescriptor (..),
  ProtoMessage (..),
  ScalarFieldType (..),
  SomeFieldDescriptor (..),
 )


{- | A single problem found while decoding, with a dotted/indexed field path
(empty for message-level / framing problems).
-}
data DecodeIssue = DecodeIssue
  { issuePath :: ![Text]
  , issueError :: !DecodeError
  }
  deriving stock (Eq, Show, Generic)


instance NFData DecodeIssue


{- | Decode a message, collecting every recoverable problem. The typed value is
@'Just' a@ exactly when the strict decoder also succeeds (i.e. the message is
clean); otherwise it is 'Nothing' and 'fst' enumerates the problems.
-}
decodeCollecting :: forall a. (ProtoMessage a, MessageDecode a) => ByteString -> ([DecodeIssue], Maybe a)
decodeCollecting bytes =
  let issues = collectIssues (Proxy :: Proxy a) bytes
  in case decodeMessage bytes :: Either DecodeError a of
       Right a -> (issues, Just a)
       Left e -> (if null issues then [DecodeIssue [] e] else issues, Nothing)


-- | Collect issues for a message type's wire bytes using its field schema.
collectIssues :: ProtoMessage a => Proxy a -> ByteString -> [DecodeIssue]
collectIssues proxy bytes =
  let fds = protoFieldDescriptors proxy
      (entries, mErr) = scan bytes
      fieldIssues = checkEntries fds entries
      tailIssue = maybe [] (\e -> [DecodeIssue [] e]) mErr
  in fieldIssues ++ tailIssue


-- Assign per-field occurrence indices, then check each entry against its
-- descriptor.
checkEntries :: Map.Map Int (SomeFieldDescriptor a) -> [(Int, WV)] -> [DecodeIssue]
checkEntries fds = go Map.empty
  where
    go _ [] = []
    go counts ((fn, wv) : rest) =
      let idx = Map.findWithDefault 0 fn counts
          counts' = Map.insert fn (idx + 1) counts
      in checkOne fds fn idx wv ++ go counts' rest


checkOne :: Map.Map Int (SomeFieldDescriptor a) -> Int -> Int -> WV -> [DecodeIssue]
checkOne fds fn idx wv = case Map.lookup fn fds of
  Nothing -> [] -- unknown field: allowed, preserved elsewhere
  Just (SomeField fd) ->
    let seg =
          if fdLabel fd == LabelRepeated
            then fdName fd <> "[" <> T.pack (show idx) <> "]"
            else fdName fd
        issue e = [DecodeIssue [seg] e]
    in case fdTypeDesc fd of
         ScalarType StringField -> case wv of
           WLen bs -> case TE.decodeUtf8' bs of
             Left _ -> issue InvalidUtf8
             Right _ -> []
           _ -> []
         MessageType _ -> subMessageIssues issue wv
         MapType _ _ -> subMessageIssues issue wv
         _ -> []


subMessageIssues :: (DecodeError -> [DecodeIssue]) -> WV -> [DecodeIssue]
subMessageIssues issue wv = case wv of
  WLen bs -> case snd (scan bs) of
    Just e -> issue (SubMessageError e)
    Nothing -> []
  _ -> []


----------------------------------------------------------------------
-- Self-contained wire scanner
----------------------------------------------------------------------

data WV = WVarint !Word64 | WI64 !Word64 | WI32 !Word32 | WLen !ByteString


{- | Scan a message into @(fieldNumber, value)@ pairs, preserving repeats.
Returns any trailing structural error encountered.
-}
scan :: ByteString -> ([(Int, WV)], Maybe DecodeError)
scan = go id
  where
    go acc bs
      | BS.null bs = (acc [], Nothing)
      | otherwise = case readVarint bs of
          Nothing -> (acc [], Just InvalidVarint)
          Just (tag, r) ->
            let fn = fromIntegral (tag `shiftR` 3)
                wt = tag .&. 7
            in case wt of
                 0 -> case readVarint r of
                   Just (v, r2) -> go (acc . ((fn, WVarint v) :)) r2
                   Nothing -> (acc [], Just InvalidVarint)
                 1 ->
                   if BS.length r >= 8
                     then go (acc . ((fn, WI64 (le64 (BS.take 8 r))) :)) (BS.drop 8 r)
                     else (acc [], Just UnexpectedEnd)
                 5 ->
                   if BS.length r >= 4
                     then go (acc . ((fn, WI32 (le32 (BS.take 4 r))) :)) (BS.drop 4 r)
                     else (acc [], Just UnexpectedEnd)
                 2 -> case readVarint r of
                   Just (len, r2) ->
                     let n = fromIntegral len
                     in if n >= 0 && BS.length r2 >= n
                          then go (acc . ((fn, WLen (BS.take n r2)) :)) (BS.drop n r2)
                          else (acc [], Just UnexpectedEnd)
                   Nothing -> (acc [], Just InvalidVarint)
                 _ -> (acc [], Just (InvalidWireType (fromIntegral wt)))


readVarint :: ByteString -> Maybe (Word64, ByteString)
readVarint = goV 0 0
  where
    goV !shift !acc bs = case BS.uncons bs of
      Nothing -> Nothing
      Just (b, rest) ->
        let acc' = acc .|. (fromIntegral (b .&. 0x7F) `shiftL` shift)
        in if b .&. 0x80 /= 0
             then if shift >= 63 then Nothing else goV (shift + 7) acc' rest
             else Just (acc', rest)


le64 :: ByteString -> Word64
le64 = BS.foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0


le32 :: ByteString -> Word32
le32 = BS.foldr (\b a -> a `shiftL` 8 .|. fromIntegral b) 0
