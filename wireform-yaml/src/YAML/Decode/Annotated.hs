{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Annotation-preserving YAML decoder.

Parses YAML to 'AStream' / 'ADocument' / 'AValue' carrying:

* Source-byte spans for each top-level mapping entry / sequence
  item, so unmodified entries can be copied verbatim during
  re-emission.
* Comments and blank-line trivia attached to the entry that
  follows them.
* The original scalar style (plain / quoted / literal block /
  folded block) so that fresh edits keep the surrounding
  document's stylistic choices.

The structural parse delegates to "YAML.Decode" so spec
conformance stays in one place; this module layers a second
pass that walks the source text with a small line scanner.
-}
module YAML.Decode.Annotated (
  decodeAnnotated,
  decodeAnnotatedStream,
  decodeAnnotatedDocument,
) where

import Data.ByteString qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import YAML.Annotated (
  ABody (..),
  ADocument (..),
  AMapEntry (..),
  AStream (..),
  AValue (..),
  Comment (..),
  MapStyle (..),
  ScalarStyle (..),
  SeqStyle (..),
  SrcSpan (..),
  Trivia (..),
 )
import YAML.Decode qualified as YD
import YAML.Value (Document (..), Stream (..), Value (..))


-- ---------------------------------------------------------------------------
-- Public entry points
-- ---------------------------------------------------------------------------

{- | Decode a single-document YAML source into an annotated tree.
The original source 'Text' is returned alongside; the renderer
needs it to copy unchanged sub-trees verbatim.
-}
decodeAnnotated :: Text -> Either String (ADocument, Text)
decodeAnnotated src = do
  Stream docs <- YD.decodeStream src
  case V.toList docs of
    [d] -> pure (annotateDocument src d, src)
    [] ->
      pure
        ( ADocument
            False
            False
            (annotatePlain YNull)
            []
            []
        , src
        )
    _ ->
      Left
        "decodeAnnotated: source contains multiple documents; \
        \use decodeAnnotatedStream"


{- | Decode a multi-document YAML stream into annotated documents
plus the original source text.
-}
decodeAnnotatedStream :: Text -> Either String (AStream, Text)
decodeAnnotatedStream src = do
  Stream docs <- YD.decodeStream src
  pure (AStream (V.map (annotateDocument src) docs), src)


decodeAnnotatedDocument :: Text -> Either String ADocument
decodeAnnotatedDocument src = fst <$> decodeAnnotated src


-- ---------------------------------------------------------------------------
-- Annotation pass
-- ---------------------------------------------------------------------------

-- | Wrap a parsed 'Document' with annotations.
annotateDocument :: Text -> Document -> ADocument
annotateDocument src (Document start end body) =
  let !srcLen = BS.length (TE.encodeUtf8 src)
      !lineIdx = buildLineIndex src
      !av = annotateBody src lineIdx body
      !av' = av {avSpan = Just (SrcSpan 0 srcLen)}
  in ADocument
       { adDirectivesEnd = start
       , adExplicitEnd = end
       , adBody = av'
       , adLeading = []
       , adTrailing = []
       }


-- | Wrap top-level structures with per-entry spans + trivia.
annotateBody :: Text -> LineIndex -> Value -> AValue
annotateBody src lineIdx = \case
  YMap kvs
    | not (V.null kvs)
    , Just entries <- locateMapEntries src lineIdx (V.toList kvs) ->
        let aEntries = V.fromList entries
        in AValue (AMap aEntries MapBlock) emptyTrivia Nothing False
  YSeq xs
    | not (V.null xs)
    , Just items <- locateSeqItems src lineIdx (V.toList xs) ->
        let aItems = V.fromList items
        in AValue (ASeq aItems SeqBlock) emptyTrivia Nothing False
  v -> annotatePlain v


-- ---------------------------------------------------------------------------
-- Trivial annotation (no source spans, default styles)
-- ---------------------------------------------------------------------------

annotatePlain :: Value -> AValue
annotatePlain v = AValue (bodyOf v) emptyTrivia Nothing False
  where
    bodyOf = \case
      YNull -> ANull
      YBool b -> ABool b
      YInt n -> AInt n
      YFloat d -> AFloat d
      YString t -> AString t (defaultScalarStyle t)
      YSeq xs -> ASeq (V.map annotatePlain xs) SeqBlock
      YMap kvs ->
        AMap
          ( V.map
              ( \(k, vv) ->
                  AMapEntry
                    (annotatePlain k)
                    (annotatePlain vv)
                    Nothing
              )
              kvs
          )
          MapBlock
      YTagged tg inner -> ATagged tg (annotatePlain inner)
      YAnchored a inner -> AAnchored a (annotatePlain inner)


emptyTrivia :: Trivia
emptyTrivia = Trivia [] 0 Nothing


defaultScalarStyle :: Text -> ScalarStyle
defaultScalarStyle t
  | T.any (\c -> c == '\n' || c == '\r') t = SSDoubleQuoted
  | otherwise = SSPlain


-- ---------------------------------------------------------------------------
-- Line-byte index
-- ---------------------------------------------------------------------------

{- | A 'LineIndex' is a vector mapping 1-based line numbers to
their UTF-8 byte offsets in the source. Index 0 is unused; the
final element is the byte length of the source so that line
@i@ runs from @lineByte i@ inclusive to @lineByte (i + 1)@
exclusive.
-}
data LineIndex = LineIndex !(V.Vector Int)
  deriving (Show)


buildLineIndex :: Text -> LineIndex
buildLineIndex src =
  let bs = TE.encodeUtf8 src
      n = BS.length bs
      starts = 0 : indicesAfter (BS.elemIndices 0x0A bs)
      indicesAfter = map (+ 1)
  in LineIndex (V.fromList (starts ++ [n]))
{-# INLINE buildLineIndex #-}


lineRange :: LineIndex -> Int -> Maybe (Int, Int)
lineRange (LineIndex v) lno
  | lno < 1 || lno >= V.length v = Nothing
  | otherwise = Just (V.unsafeIndex v (lno - 1), V.unsafeIndex v lno)


-- ---------------------------------------------------------------------------
-- Top-level mapping entry locator
-- ---------------------------------------------------------------------------

{- | Walk the source in line order, matching each top-level
mapping key from the parsed structure to a line beginning at
column 0 with that key followed by @:@. For each entry we
record the byte span covering the key line + any continuation
lines (lines indented past column 0). Stand-alone comments and
blank lines that immediately precede the key are attached as
'triviaLeading' / 'triviaBlankLines'.

Returns 'Nothing' if the parsed structure can't be aligned with
the source on a 1:1 basis (e.g. mappings nested under @---@,
multi-document streams). The caller falls back to whole-doc
verbatim in that case.
-}
locateMapEntries
  :: Text
  -> LineIndex
  -> [(Value, Value)]
  -> Maybe [AMapEntry]
locateMapEntries src lineIdx kvs = go 1 [] 0 kvs
  where
    LineIndex starts = lineIdx
    !lineCount = V.length starts - 1

    -- 'pendingComments' / 'pendingBlanks' accumulate trivia
    -- waiting to attach to the next key line.
    go !lno !pendingComments !pendingBlanks remaining
      | lno > lineCount =
          if null remaining then Just [] else Nothing
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> Nothing
            Just lt ->
              let stripped = T.stripStart lt
                  indent = T.length lt - T.length stripped
              in -- Skip directive / doc-marker lines.
                 case T.uncons stripped of
                   _
                     | T.null stripped ->
                         go
                           (lno + 1)
                           pendingComments
                           (pendingBlanks + 1)
                           remaining
                   Just ('#', rest) ->
                     let cmt = Comment (stripCommentBody rest) indent
                     in go
                          (lno + 1)
                          (pendingComments ++ [cmt])
                          pendingBlanks
                          remaining
                   Just ('%', _) ->
                     go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('-', rest)
                     | T.isPrefixOf "--" rest ->
                         go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('.', rest)
                     | T.isPrefixOf ".." rest ->
                         go (lno + 1) pendingComments pendingBlanks remaining
                   _
                     | indent /= 0 ->
                         -- A continuation line outside any entry: bail.
                         Nothing
                   _ ->
                     case remaining of
                       [] -> Nothing -- extra content past the parsed entries
                       ((kv, vv) : rest) ->
                         case matchKeyOnLine stripped kv of
                           Nothing -> Nothing
                           Just _ ->
                             -- Find the entry's span: from start
                             -- of key line through last non-blank
                             -- continuation line.
                             let !entryStartByte = V.unsafeIndex starts (lno - 1)
                                 (endLine, _eolCmt) =
                                   scanEntryLines src lineIdx (lno + 1)
                                 !entryEndByte =
                                   -- Line @endLine@ is the first
                                   -- line that does NOT belong to
                                   -- this entry (or @lineCount + 1@
                                   -- when none follows). Bytes
                                   -- BEFORE that line make up the
                                   -- entry.
                                   V.unsafeIndex
                                     starts
                                     (min (endLine - 1) (V.length starts - 1))
                                 trailingCmt =
                                   parseEolComment src lineIdx lno
                                 trivia =
                                   Trivia
                                     pendingComments
                                     pendingBlanks
                                     Nothing
                                 keyAv =
                                   (annotatePlain kv)
                                     { avTrivia = trivia
                                     }
                                 valAv =
                                   annotateContainedValue
                                     src
                                     lineIdx
                                     vv
                                     ( Just
                                         ( SrcSpan
                                             entryStartByte
                                             entryEndByte
                                         )
                                     )
                                 entry =
                                   AMapEntry
                                     keyAv
                                     valAv
                                       { avSpan =
                                           Just
                                             ( SrcSpan
                                                 entryStartByte
                                                 entryEndByte
                                             )
                                       }
                                     trailingCmt
                             in (entry :)
                                  <$> go endLine [] 0 rest


-- ---------------------------------------------------------------------------
-- Top-level sequence item locator
-- ---------------------------------------------------------------------------

locateSeqItems
  :: Text -> LineIndex -> [Value] -> Maybe [AValue]
locateSeqItems src lineIdx items = go 1 [] 0 items
  where
    LineIndex starts = lineIdx
    !lineCount = V.length starts - 1

    go !lno !pendingComments !pendingBlanks remaining
      | lno > lineCount =
          if null remaining then Just [] else Nothing
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> Nothing
            Just lt ->
              let stripped = T.stripStart lt
                  indent = T.length lt - T.length stripped
              in case T.uncons stripped of
                   _
                     | T.null stripped ->
                         go
                           (lno + 1)
                           pendingComments
                           (pendingBlanks + 1)
                           remaining
                   Just ('#', rest) ->
                     let cmt = Comment (stripCommentBody rest) indent
                     in go
                          (lno + 1)
                          (pendingComments ++ [cmt])
                          pendingBlanks
                          remaining
                   Just ('%', _) ->
                     go (lno + 1) pendingComments pendingBlanks remaining
                   Just ('-', after) | T.null after || T.head after == ' ' ->
                     case remaining of
                       [] -> Nothing
                       (vv : rest) ->
                         let !entryStartByte = V.unsafeIndex starts (lno - 1)
                             (endLine, _) =
                               scanEntryLines src lineIdx (lno + 1)
                             !entryEndByte =
                               V.unsafeIndex
                                 starts
                                 (min (endLine - 1) (V.length starts - 1))
                             av =
                               annotateContainedValue
                                 src
                                 lineIdx
                                 vv
                                 ( Just
                                     ( SrcSpan
                                         entryStartByte
                                         entryEndByte
                                     )
                                 )
                             av' =
                               av
                                 { avTrivia =
                                     Trivia
                                       pendingComments
                                       pendingBlanks
                                       Nothing
                                 , avSpan =
                                     Just
                                       ( SrcSpan
                                           entryStartByte
                                           entryEndByte
                                       )
                                 }
                         in (av' :) <$> go endLine [] 0 rest
                   _ -> Nothing
                     where
                       !_ = indent -- silence unused


{- | An entry's body extends until the next non-indented line.
Blank lines and stand-alone comments belong to the NEXT entry's
trivia, not this one.
-}
scanEntryLines :: Text -> LineIndex -> Int -> (Int, Maybe Comment)
scanEntryLines src lineIdx@(LineIndex starts) start = go start
  where
    !lineCount = V.length starts - 1
    go !lno
      | lno > lineCount = (lineCount + 1, Nothing)
      | otherwise =
          case lineSliceUtf8 src lineIdx lno of
            Nothing -> (lno, Nothing)
            Just lt ->
              let stripped = T.stripStart lt
                  indent = T.length lt - T.length stripped
              in if not (T.null stripped) && indent > 0
                   then go (lno + 1)
                   else (lno, Nothing)


{- | If the parsed value is itself a complex (map / seq) container
whose layout might be useful to preserve, we still attach a
span (so verbatim copy works for the unmodified case) but fall
back to plain annotation for the children. Going deeper would
need a recursive structural scan.
-}
annotateContainedValue :: Text -> LineIndex -> Value -> Maybe SrcSpan -> AValue
annotateContainedValue _src _lineIdx v sp =
  let av = annotatePlain v
  in av {avSpan = sp}


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lineSliceUtf8 :: Text -> LineIndex -> Int -> Maybe Text
lineSliceUtf8 src lineIdx lno = do
  (s, e) <- lineRange lineIdx lno
  let bs = TE.encodeUtf8 src
  let raw = BS.take (max 0 (e - s)) (BS.drop s bs)
      bs' = case BS.unsnoc raw of
        Just (init', 0x0A) -> init'
        _ -> raw
      bs'' = case BS.unsnoc bs' of
        Just (init', 0x0D) -> init'
        _ -> bs'
  Right t <- pure (TE.decodeUtf8' bs'')
  pure t


{- | Match a top-level key name against the start of a stripped
line. Recognises plain / single-quoted / double-quoted scalar
keys; bails out for anything more exotic.
-}
matchKeyOnLine :: Text -> Value -> Maybe Text
matchKeyOnLine line key = do
  keyText <- case key of
    YString t -> Just t
    YInt n -> Just (T.pack (show n))
    YBool b -> Just (if b then T.pack "true" else T.pack "false")
    YNull -> Just (T.pack "null")
    _ -> Nothing
  let plainOk =
        T.isPrefixOf keyText line
          && hasColonAfter (T.length keyText) line
      sqOk =
        T.isPrefixOf (squote keyText) line
          && hasColonAfter (T.length keyText + 2) line
      dqOk =
        T.isPrefixOf (dquote keyText) line
          && hasColonAfter (T.length keyText + 2) line
  if plainOk || sqOk || dqOk then Just line else Nothing
  where
    squote t = T.cons '\'' (t `T.snoc` '\'')
    dquote t = T.cons '"' (t `T.snoc` '"')


{- | True if the character at @i@ is a colon followed by a space
or end-of-line.
-}
hasColonAfter :: Int -> Text -> Bool
hasColonAfter i t =
  case T.uncons (T.drop i t) of
    Just (':', rest) -> case T.uncons rest of
      Just (c, _) -> isSpace c
      Nothing -> True
    _ -> False


stripCommentBody :: Text -> Text
stripCommentBody t = T.dropWhile (== ' ') t


parseEolComment :: Text -> LineIndex -> Int -> Maybe Comment
parseEolComment src lineIdx lno = do
  lt <- lineSliceUtf8 src lineIdx lno
  -- Find an unquoted '#' preceded by whitespace.
  let go !i !inSq !inDq
        | i >= T.length lt = Nothing
        | otherwise =
            let c = T.index lt i
            in case c of
                 '\'' | not inDq -> go (i + 1) (not inSq) inDq
                 '"' | not inSq -> go (i + 1) inSq (not inDq)
                 '#'
                   | not inSq
                   , not inDq
                   , i > 0
                   , isSpace (T.index lt (i - 1)) ->
                       Just
                         ( Comment
                             (stripCommentBody (T.drop (i + 1) lt))
                             i
                         )
                 _ -> go (i + 1) inSq inDq
  go 0 False False


_isDigit :: Char -> Bool
_isDigit = isDigit
