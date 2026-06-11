{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Pretty-printing for 'YAML.Value.Value' and
'YAML.Annotated.AValue' with configurable layout.

The fast emitter in "YAML.Encode" is biased toward producing
compact, canonical output. 'YAML.Pretty.render' / 'renderAnnotated'
here lets you wrap long flow collections, force quoting on
ambiguous scalars, switch between block and flow styles, etc.
-}
module YAML.Pretty (
  -- * Options
  RenderOptions (..),
  defaultOptions,
  compactOptions,

  -- * Style preferences
  StringStyleHint (..),

  -- * Render entry points
  render,
  renderDocument,
  renderStream,
  renderBS,
  renderAnnotated,
  renderAnnotatedDocument,
  renderAnnotatedStream,
) where

import Control.DeepSeq (NFData)
import Data.ByteString qualified as BS
import Data.Char (isControl, isDigit, isPrint, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (
  Builder,
  fromString,
  fromText,
  singleton,
  toLazyText,
 )
import Data.Text.Lazy.Builder.Int qualified as BI
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Numeric (showHex)
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
import YAML.Value (
  Anchor (..),
  Document (..),
  Stream (..),
  Tag (..),
  Value (..),
 )
import YAML.Value qualified as YV


-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | What style to prefer for fresh strings.
data StringStyleHint
  = -- | Plain when safe, double-quoted otherwise.
    HintAuto
  | HintAlwaysDouble
  | HintAlwaysSingle
  | -- | For multi-line strings.
    HintLiteralBlock
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Tunables for the pretty-printer.
data RenderOptions = RenderOptions
  { roIndent :: {-# UNPACK #-} !Int
  -- ^ Spaces per indent level. Default @2@.
  , roMaxLineWidth :: {-# UNPACK #-} !Int
  {- ^ Soft maximum line width, used to decide when to wrap
  long flow collections onto multiple lines and when to
  promote a long block scalar to a literal block. Default
  @100@. Set to 'maxBound' to disable wrapping.
  -}
  , roDefaultMapStyle :: !MapStyle
  -- ^ How fresh mappings render. Default 'MapBlock'.
  , roDefaultSeqStyle :: !SeqStyle
  -- ^ How fresh sequences render. Default 'SeqBlock'.
  , roStringStyleHint :: !StringStyleHint
  -- ^ How fresh strings render. Default 'HintAuto'.
  , roCompactSeqOfMap :: !Bool
  {- ^ Use the @- key: value@ compact form for mappings as
  sequence items. Default 'True'.
  -}
  , roQuoteAmbiguousScalars :: !Bool
  {- ^ Quote a fresh plain string when its content would parse
  back as another core-schema literal (number, bool, null).
  Default 'True'.
  -}
  , roDocStartMarker :: !Bool
  {- ^ Emit a leading @---@ on every document. Default 'False'
  for single-document streams; 'True' for multi-document
  streams (forced by 'renderStream').
  -}
  , roDocEndMarker :: !Bool
  -- ^ Emit a trailing @...@ marker. Default 'False'.
  , roTrailingNewline :: !Bool
  -- ^ Emit a final @\n@ at end of stream. Default 'True'.
  , roPreserveOriginalText :: !Bool
  {- ^ When 'True' (the default), unchanged sub-trees in an
  annotated render reuse their original source bytes
  verbatim. Set to 'False' to force re-emission everywhere
  (useful for normalising / canonicalising a document).
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


-- | Sensible defaults for human-readable output.
defaultOptions :: RenderOptions
defaultOptions =
  RenderOptions
    { roIndent = 2
    , roMaxLineWidth = 100
    , roDefaultMapStyle = MapBlock
    , roDefaultSeqStyle = SeqBlock
    , roStringStyleHint = HintAuto
    , roCompactSeqOfMap = True
    , roQuoteAmbiguousScalars = True
    , roDocStartMarker = False
    , roDocEndMarker = False
    , roTrailingNewline = True
    , roPreserveOriginalText = True
    }


{- | Compact emission: short lines, no padding, prefer flow when
it's shorter.
-}
compactOptions :: RenderOptions
compactOptions =
  defaultOptions
    { roIndent = 2
    , roMaxLineWidth = 200
    , roDefaultMapStyle = MapFlow
    , roDefaultSeqStyle = SeqFlow
    , roStringStyleHint = HintAuto
    , roCompactSeqOfMap = True
    , roTrailingNewline = False
    }


-- ---------------------------------------------------------------------------
-- Render entry points
-- ---------------------------------------------------------------------------

-- | Render a 'Value' to 'Text' using the given options.
render :: RenderOptions -> Value -> Text
render opts v = TL.toStrict (toLazyText (build opts v))


renderBS :: RenderOptions -> Value -> BS.ByteString
renderBS opts = TE.encodeUtf8 . render opts


renderDocument :: RenderOptions -> Document -> Text
renderDocument opts (Document _ _ body) =
  TL.toStrict (toLazyText (build opts body))


renderStream :: RenderOptions -> Stream -> Text
renderStream opts (Stream docs)
  | V.null docs = T.empty
  | V.length docs == 1 = renderDocument opts (V.head docs)
  | otherwise =
      TL.toStrict
        ( toLazyText
            (V.foldl' step mempty (V.indexed docs))
        )
  where
    step acc (i, d) =
      let !marker =
            if i == 0 && not (roDocStartMarker opts)
              then mempty
              else fromText "---\n"
          !body = build opts (YV.docBody d)
          !end =
            if YV.docExplicitEnd d
              then fromText "...\n"
              else mempty
      in acc <> marker <> body <> end


{- | Render an 'AValue' tree. Uses the original source bytes for
sub-trees that haven't been modified (when
'roPreserveOriginalText' is on); falls back to the pretty-
printer otherwise.
-}
renderAnnotated :: RenderOptions -> AValue -> Maybe Text -> Text
renderAnnotated opts av msrc =
  TL.toStrict (toLazyText (buildAValue opts msrc 0 ChildOfDoc av))


renderAnnotatedDocument :: RenderOptions -> ADocument -> Maybe Text -> Text
renderAnnotatedDocument opts (ADocument start end body leading trailing) msrc =
  TL.toStrict
    ( toLazyText
        ( (if start || roDocStartMarker opts then fromText "---\n" else mempty)
            <> emitComments 0 leading
            <> buildAValue opts msrc 0 ChildOfDoc body
            <> emitComments 0 trailing
            <> (if end || roDocEndMarker opts then fromText "...\n" else mempty)
        )
    )


renderAnnotatedStream :: RenderOptions -> AStream -> Maybe Text -> Text
renderAnnotatedStream opts (AStream docs) msrc
  | V.null docs = T.empty
  | V.length docs == 1 =
      renderAnnotatedDocument opts (V.head docs) msrc
  | otherwise = TL.toStrict (toLazyText (V.foldl' step mempty (V.indexed docs)))
  where
    step acc (i, d) =
      let marker =
            if i == 0 && not (adDirectivesEnd d)
              then mempty
              else fromText "---\n"
          body =
            fromText
              (renderAnnotatedDocument opts d msrc)
      in acc <> marker <> body


-- ---------------------------------------------------------------------------
-- Common building blocks
-- ---------------------------------------------------------------------------

build :: RenderOptions -> Value -> Builder
build opts v0 =
  let body = buildValue opts 0 ChildOfDoc v0
      body' =
        body
          <> ( if roTrailingNewline opts && not (endsWithNL body)
                 then singleton '\n'
                 else mempty
             )
  in (if roDocStartMarker opts then fromText "---\n" else mempty)
       <> body'
       <> (if roDocEndMarker opts then fromText "...\n" else mempty)


-- A 'ChildContext' tells emitters how the value is being placed
-- (top-level, inside a block sequence item, inside a mapping
-- value at column N, etc.). It governs leading whitespace and
-- compact-form decisions.
data ChildContext
  = ChildOfDoc
  | {- | Same line as the mapping key's @:@; if the value fits
    inline we emit it after a single space, otherwise we go
    to the next line indented past the key.
    -}
    ChildOfMapValueInline
  | {- | Following a @-@ on the same line; treat similarly to
    'ChildOfMapValueInline'.
    -}
    ChildOfSeqItem


{- | Approximate the rendered length of a value to decide whether
it fits on one line. We don't need exactness — only enough to
make a flow / block / wrap decision.
-}
approxFlowLength :: Value -> Int
approxFlowLength = go
  where
    go = \case
      YNull -> 4
      YBool b -> if b then 4 else 5
      YInt n -> length (show n)
      YFloat d -> length (show d)
      YString t -> 2 + T.length t -- worst case: quoted
      YSeq xs
        | V.null xs -> 2
        | otherwise ->
            2
              + V.length xs * 2
              + V.sum (V.map go xs)
      YMap kvs
        | V.null kvs -> 2
        | otherwise ->
            2
              + V.length kvs * 4
              + V.sum (V.map (\(k, v) -> go k + go v) kvs)
      YTagged _ x -> 6 + go x
      YAnchored _ x -> 6 + go x
{-# INLINE approxFlowLength #-}


-- | Indent emission: @n@ spaces.
spaces :: Int -> Builder
spaces n
  | n <= 0 = mempty
  | otherwise = fromText (T.replicate n (T.singleton ' '))


{- | Does this builder, when forced, end with @\n@? We need only
a coarse approximation; if in doubt, return 'False' so the
caller emits a separator.
-}
endsWithNL :: Builder -> Bool
endsWithNL b = case TL.unsnoc (toLazyText b) of
  Just (_, '\n') -> True
  _ -> False


-- ---------------------------------------------------------------------------
-- Value rendering (un-annotated)
-- ---------------------------------------------------------------------------

buildValue :: RenderOptions -> Int -> ChildContext -> Value -> Builder
buildValue opts !ind ctx v = case v of
  YNull -> emitScalar opts ind ctx YNull
  YBool _ -> emitScalar opts ind ctx v
  YInt _ -> emitScalar opts ind ctx v
  YFloat _ -> emitScalar opts ind ctx v
  YString _ -> emitScalar opts ind ctx v
  YSeq xs
    | V.null xs -> contextSeparator ctx <> fromText "[]\n"
    | otherwise -> emitSeq opts ind ctx xs (roDefaultSeqStyle opts)
  YMap kvs
    | V.null kvs -> contextSeparator ctx <> fromText "{}\n"
    | otherwise -> emitMap opts ind ctx kvs (roDefaultMapStyle opts)
  YTagged tg inner ->
    contextSeparator ctx
      <> tagPrefix tg
      <> singleton ' '
      <> buildValueFlat opts ind inner
  YAnchored (Anchor n) inner ->
    contextSeparator ctx
      <> singleton '&'
      <> fromText n
      <> singleton ' '
      <> buildValueFlat opts ind inner


{- | Builder used inline (after the colon / dash separator) on the
same line as the parent's marker.
-}
buildValueFlat :: RenderOptions -> Int -> Value -> Builder
buildValueFlat opts ind v = case v of
  YNull -> fromText "null\n"
  _ -> buildValue opts ind ChildOfMapValueInline v


contextSeparator :: ChildContext -> Builder
contextSeparator = \case
  ChildOfDoc -> mempty
  ChildOfMapValueInline -> singleton ' '
  ChildOfSeqItem -> singleton ' '


-- ----- Sequences -----

emitSeq :: RenderOptions -> Int -> ChildContext -> V.Vector Value -> SeqStyle -> Builder
emitSeq opts ind ctx xs style
  | shouldFlowSeq opts ind xs style =
      contextSeparator ctx <> emitFlowSeq opts ind xs <> singleton '\n'
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty xs
  where
    step acc x =
      acc
        <> spaces ind
        <> fromText "- "
        <> buildSeqItem opts (ind + 2) x


shouldFlowSeq :: RenderOptions -> Int -> V.Vector Value -> SeqStyle -> Bool
shouldFlowSeq opts ind xs style =
  case style of
    SeqFlow -> True
    SeqBlock ->
      ind + V.sum (V.map approxFlowLength xs) + 2 * V.length xs + 2
        <= roMaxLineWidth opts
        && V.all isLeaf xs
        -- only auto-flow when items are leaf scalars, avoid
        -- nesting flow inside block by accident
        && False -- block style stays block by default
  where
    isLeaf = \case
      YSeq {} -> False
      YMap {} -> False
      YTagged _ x -> isLeaf (YV.unwrap x)
      YAnchored _ x -> isLeaf (YV.unwrap x)
      _ -> True


buildSeqItem :: RenderOptions -> Int -> Value -> Builder
buildSeqItem opts ind v
  | roCompactSeqOfMap opts
  , YMap kvs <- YV.unwrap v
  , not (V.null kvs) =
      emitCompactSeqMap opts ind kvs
  | YSeq xs <- YV.unwrap v
  , not (V.null xs) =
      emitSeq opts ind ChildOfSeqItem xs (roDefaultSeqStyle opts)
  | otherwise = buildValueFlat opts ind v


emitCompactSeqMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
emitCompactSeqMap opts ind kvs
  | V.null kvs = fromText "{}\n"
  | otherwise =
      let (k0, v0) = V.head kvs
          rest = V.tail kvs
      in emitMapEntry opts ind 0 k0 v0
           <> V.foldl' step mempty rest
  where
    step acc (k, v) = acc <> emitMapEntry opts ind ind k v


-- ----- Mappings -----

emitMap :: RenderOptions -> Int -> ChildContext -> V.Vector (Value, Value) -> MapStyle -> Builder
emitMap opts ind ctx kvs style
  | shouldFlowMap opts ind kvs style =
      contextSeparator ctx <> emitFlowMap opts ind kvs <> singleton '\n'
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty kvs
  where
    step acc (k, v) = acc <> emitMapEntry opts ind ind k v


shouldFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> MapStyle -> Bool
shouldFlowMap opts ind kvs = \case
  MapFlow -> True
  MapBlock ->
    ind
      + V.sum (V.map (\(k, v) -> approxFlowLength k + approxFlowLength v) kvs)
      + 4 * V.length kvs
      + 2
      <= roMaxLineWidth opts
      && False -- prefer block by default


emitMapEntry :: RenderOptions -> Int -> Int -> Value -> Value -> Builder
emitMapEntry opts childInd indentForLine k v =
  spaces indentForLine
    <> emitMapKey opts indentForLine k
    <> fromText ":"
    <> buildValueFlat opts childInd v


emitMapKey :: RenderOptions -> Int -> Value -> Builder
emitMapKey opts _ k = case YV.unwrap k of
  YString t -> emitPlainOrQuotedKey opts t
  YInt n -> BI.decimal n
  YBool b -> fromText (if b then "true" else "false")
  YNull -> fromText "null"
  YFloat d -> fromString (show d)
  other -> emitFlowValue opts other


emitPlainOrQuotedKey :: RenderOptions -> Text -> Builder
emitPlainOrQuotedKey opts t
  | needsQuoting opts t = doubleQuoted t
  | otherwise = fromText t


-- ----- Scalars -----

emitScalar :: RenderOptions -> Int -> ChildContext -> Value -> Builder
emitScalar opts _ind ctx v =
  contextSeparator ctx <> emitScalarBody opts v <> singleton '\n'


emitScalarBody :: RenderOptions -> Value -> Builder
emitScalarBody opts = \case
  YNull -> fromText "null"
  YBool b -> fromText (if b then "true" else "false")
  YInt n -> BI.decimal n
  YFloat d -> formatDouble d
  YString t -> emitString opts t
  v -> emitFlowValue opts v


emitString :: RenderOptions -> Text -> Builder
emitString opts t = case roStringStyleHint opts of
  HintAlwaysDouble -> doubleQuoted t
  HintAlwaysSingle -> singleQuoted t
  HintLiteralBlock | needsBlock -> literalBlock t
  _ -> case classifyString opts t of
    KsPlain -> fromText t
    KsDouble -> doubleQuoted t
    KsSingle -> singleQuoted t
    KsLiteral -> literalBlock t
  where
    needsBlock = T.any (== '\n') t


data KeyStr = KsPlain | KsDouble | KsSingle | KsLiteral


classifyString :: RenderOptions -> Text -> KeyStr
classifyString opts t
  | T.null t = KsDouble
  | T.any (\c -> c == '\n' || c == '\r') t = KsLiteral
  | hasUnsafeChar = KsDouble
  | roQuoteAmbiguousScalars opts && parsesAsLiteral = KsDouble
  | otherwise = KsPlain
  where
    hasUnsafeChar =
      T.any
        (\c -> c == '"' || c == '\\' || isControl c)
        t

    parsesAsLiteral = needsQuoting opts t


needsQuoting :: RenderOptions -> Text -> Bool
needsQuoting _opts t
  | T.null t = True
  | T.head t `elem` (":#{}[],&*!|>%@`'\"" :: String) = True
  | T.any (\c -> isControl c && c /= '\t') t = True
  | otherwise =
      let trimmed = T.strip t
      in trimmed == "null"
           || trimmed == "Null"
           || trimmed == "NULL"
           || trimmed == "true"
           || trimmed == "True"
           || trimmed == "TRUE"
           || trimmed == "false"
           || trimmed == "False"
           || trimmed == "FALSE"
           || trimmed == "~"
           || looksNumeric trimmed
           || T.any (\c -> c == ':' || c == '#') t


looksNumeric :: Text -> Bool
looksNumeric t = case T.uncons t of
  Just (c, _) -> c == '+' || c == '-' || c == '.' || isDigit c
  Nothing -> False


doubleQuoted :: Text -> Builder
doubleQuoted t = singleton '"' <> escapeDQ t <> singleton '"'


escapeDQ :: Text -> Builder
escapeDQ = T.foldr step mempty
  where
    step c acc = case c of
      '"' -> fromText "\\\"" <> acc
      '\\' -> fromText "\\\\" <> acc
      '\n' -> fromText "\\n" <> acc
      '\r' -> fromText "\\r" <> acc
      '\t' -> fromText "\\t" <> acc
      _
        | isControl c || not (isPrint c) ->
            fromString (printf "\\u%04x" (ord c)) <> acc
      _ -> singleton c <> acc


singleQuoted :: Text -> Builder
singleQuoted t = singleton '\'' <> fromText (T.replace "'" "''" t) <> singleton '\''


literalBlock :: Text -> Builder
literalBlock t =
  fromText "|\n"
    <> mconcat
      [ spaces 2 <> fromText line <> singleton '\n'
      | line <- T.splitOn (T.singleton '\n') t
      ]


-- ----- Flow style -----

emitFlowValue :: RenderOptions -> Value -> Builder
emitFlowValue opts = \case
  YNull -> fromText "null"
  YBool b -> fromText (if b then "true" else "false")
  YInt n -> BI.decimal n
  YFloat d -> formatDouble d
  YString t -> emitFlowString opts t
  YSeq xs -> emitFlowSeq opts 0 xs
  YMap kvs -> emitFlowMap opts 0 kvs
  YTagged tg x ->
    tagPrefix tg <> singleton ' ' <> emitFlowValue opts x
  YAnchored (Anchor n) x ->
    singleton '&'
      <> fromText n
      <> singleton ' '
      <> emitFlowValue opts x


emitFlowSeq :: RenderOptions -> Int -> V.Vector Value -> Builder
emitFlowSeq opts ind xs
  | V.null xs = fromText "[]"
  | otherwise =
      let approx =
            ind
              + 2
              + V.length xs * 2
              + V.sum (V.map approxFlowLength xs)
      in if approx <= roMaxLineWidth opts
           then
             singleton '['
               <> mconcat
                 (V.toList (V.imap (sep emitFlowValue opts) xs))
               <> singleton ']'
           else multiLineFlowSeq opts ind xs


multiLineFlowSeq :: RenderOptions -> Int -> V.Vector Value -> Builder
multiLineFlowSeq opts ind xs =
  fromText "[\n"
    <> V.foldl' step mempty (V.indexed xs)
    <> spaces ind
    <> singleton ']'
  where
    step acc (i, x) =
      acc
        <> spaces (ind + roIndent opts)
        <> emitFlowValue opts x
        <> (if i < V.length xs - 1 then singleton ',' else mempty)
        <> singleton '\n'


emitFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
emitFlowMap opts ind kvs
  | V.null kvs = fromText "{}"
  | otherwise =
      let approx =
            ind
              + 2
              + V.length kvs * 4
              + V.sum
                ( V.map
                    ( \(k, v) ->
                        approxFlowLength k + approxFlowLength v
                    )
                    kvs
                )
      in if approx <= roMaxLineWidth opts
           then
             singleton '{'
               <> mconcat
                 (V.toList (V.imap (flowKV opts) kvs))
               <> singleton '}'
           else multiLineFlowMap opts ind kvs


flowKV :: RenderOptions -> Int -> (Value, Value) -> Builder
flowKV opts i (k, v) =
  (if i == 0 then mempty else fromText ", ")
    <> emitFlowValue opts k
    <> fromText ": "
    <> emitFlowValue opts v


multiLineFlowMap :: RenderOptions -> Int -> V.Vector (Value, Value) -> Builder
multiLineFlowMap opts ind kvs =
  fromText "{\n"
    <> V.foldl' step mempty (V.indexed kvs)
    <> spaces ind
    <> singleton '}'
  where
    step acc (i, (k, v)) =
      acc
        <> spaces (ind + roIndent opts)
        <> emitFlowValue opts k
        <> fromText ": "
        <> emitFlowValue opts v
        <> (if i < V.length kvs - 1 then singleton ',' else mempty)
        <> singleton '\n'


sep
  :: (RenderOptions -> Value -> Builder)
  -> RenderOptions
  -> Int
  -> Value
  -> Builder
sep f opts i x = (if i == 0 then mempty else fromText ", ") <> f opts x


emitFlowString :: RenderOptions -> Text -> Builder
emitFlowString opts t = case classifyString opts t of
  KsPlain -> fromText t
  KsLiteral -> doubleQuoted t -- can't use literal in flow
  _ -> doubleQuoted t


formatDouble :: Double -> Builder
formatDouble d
  | isNaN d = fromText ".nan"
  | isInfinite d && d > 0 = fromText ".inf"
  | isInfinite d = fromText "-.inf"
  | otherwise = fromString (show d)


tagPrefix :: Tag -> Builder
tagPrefix (Tag t)
  | T.isPrefixOf "tag:yaml.org,2002:" t =
      fromText "!!" <> fromText (T.drop 18 t)
  | otherwise = fromText "!<" <> fromText t <> singleton '>'


-- ---------------------------------------------------------------------------
-- Annotated rendering (with verbatim fallback)
-- ---------------------------------------------------------------------------

buildAValue
  :: RenderOptions
  -> Maybe Text
  -> Int
  -> ChildContext
  -> AValue
  -> Builder
buildAValue opts msrc !ind ctx av
  | roPreserveOriginalText opts
  , not (avDirty av)
  , Just sp <- avSpan av
  , Just src <- msrc
  , Just slice <- spanSlice src sp =
      -- Verbatim copy of the original bytes (with the
      -- caller-side context separator).
      contextSeparator ctx <> fromText slice
  | otherwise =
      emitAComments ind (triviaLeading (avTrivia av))
        <> buildABody opts msrc ind ctx av


spanSlice :: Text -> SrcSpan -> Maybe Text
spanSlice src (SrcSpan s e)
  | s < 0 || e <= s = Nothing
  | otherwise =
      let bs = TE.encodeUtf8 src
          n = BS.length bs
      in if e > n
           then Nothing
           else Just (TE.decodeUtf8 (BS.take (e - s) (BS.drop s bs)))


buildABody
  :: RenderOptions
  -> Maybe Text
  -> Int
  -> ChildContext
  -> AValue
  -> Builder
buildABody opts msrc ind ctx av = case avBody av of
  ANull -> emitScalar opts ind ctx YNull
  ABool b -> emitScalar opts ind ctx (YBool b)
  AInt n -> emitScalar opts ind ctx (YInt n)
  AFloat d -> emitScalar opts ind ctx (YFloat d)
  AString t s -> emitAString opts ind ctx t s
  ASeq xs s -> emitASeq opts msrc ind ctx xs s
  AMap es s -> emitAMap opts msrc ind ctx es s
  ATagged tg inner ->
    contextSeparator ctx
      <> tagPrefix tg
      <> singleton ' '
      <> buildAValue opts msrc ind ChildOfMapValueInline inner
  AAnchored (Anchor n) inner ->
    contextSeparator ctx
      <> singleton '&'
      <> fromText n
      <> singleton ' '
      <> buildAValue opts msrc ind ChildOfMapValueInline inner
  AAlias (Anchor n) ->
    contextSeparator ctx
      <> singleton '*'
      <> fromText n
      <> singleton '\n'


emitAString
  :: RenderOptions
  -> Int
  -> ChildContext
  -> Text
  -> ScalarStyle
  -> Builder
emitAString opts _ind ctx t style =
  contextSeparator ctx
    <> renderScalarStyled opts t style
    <> singleton '\n'


renderScalarStyled :: RenderOptions -> Text -> ScalarStyle -> Builder
renderScalarStyled opts t = \case
  SSPlain
    | needsQuoting opts t -> doubleQuoted t
    | otherwise -> fromText t
  SSDoubleQuoted -> doubleQuoted t
  SSSingleQuoted -> singleQuoted t
  SSLiteral _ -> literalBlock t
  SSFolded _ -> literalBlock t -- folded round-trips as literal for now


emitASeq
  :: RenderOptions
  -> Maybe Text
  -> Int
  -> ChildContext
  -> V.Vector AValue
  -> SeqStyle
  -> Builder
emitASeq opts _msrc ind ctx xs SeqFlow =
  contextSeparator ctx
    <> emitFlowSeq opts ind (V.map astripAnnotationsForFlow xs)
    <> singleton '\n'
emitASeq opts msrc ind ctx xs SeqBlock
  | V.null xs =
      contextSeparator ctx <> fromText "[]\n"
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty xs
  where
    step !acc x = case verbatimEntry opts msrc x of
      Just slice ->
        acc
          <> emitTrivia ind (avTrivia x)
          <> fromText slice
      Nothing ->
        acc
          <> emitTrivia ind (avTrivia x)
          <> spaces ind
          <> fromText "- "
          <> buildAValueSeqItem opts msrc (ind + 2) x


buildAValueSeqItem
  :: RenderOptions -> Maybe Text -> Int -> AValue -> Builder
buildAValueSeqItem opts msrc ind av
  | roCompactSeqOfMap opts
  , AMap es s <- avBody av
  , not (V.null es) =
      emitACompactSeqMap opts msrc ind es s
  | otherwise = buildAValue opts msrc ind ChildOfSeqItem av


emitACompactSeqMap
  :: RenderOptions
  -> Maybe Text
  -> Int
  -> V.Vector AMapEntry
  -> MapStyle
  -> Builder
emitACompactSeqMap opts msrc ind es _ =
  let (e0, rest) = V.splitAt 1 es
  in V.foldl' (entryRow 0) mempty e0
       <> V.foldl' (entryRow ind) mempty rest
  where
    entryRow indForLine acc e =
      acc
        <> spaces indForLine
        <> emitAMapKey opts (amKey e)
        <> fromText ":"
        <> buildAValue opts msrc ind ChildOfMapValueInline (amValue e)
        <> emitTrailingComment (amTrailing e)


emitAMap
  :: RenderOptions
  -> Maybe Text
  -> Int
  -> ChildContext
  -> V.Vector AMapEntry
  -> MapStyle
  -> Builder
emitAMap opts _msrc ind ctx es MapFlow =
  contextSeparator ctx
    <> emitFlowMap opts ind (V.map entryToPair es)
    <> singleton '\n'
  where
    entryToPair e =
      ( astripAnnotationsForFlow (amKey e)
      , astripAnnotationsForFlow (amValue e)
      )
emitAMap opts msrc ind ctx es MapBlock
  | V.null es = contextSeparator ctx <> fromText "{}\n"
  | otherwise =
      ( case ctx of
          ChildOfMapValueInline -> singleton '\n'
          _ -> mempty
      )
        <> V.foldl' step mempty es
  where
    step !acc e =
      let kAv = amKey e
          vAv = amValue e
      in case verbatimEntry opts msrc vAv of
           Just slice ->
             acc
               <> emitTrivia ind (avTrivia kAv)
               <> fromText slice
           Nothing ->
             acc
               <> emitTrivia ind (avTrivia kAv)
               <> spaces ind
               <> emitAMapKey opts kAv
               <> fromText ":"
               <> buildAValue
                 opts
                 msrc
                 (ind + roIndent opts)
                 ChildOfMapValueInline
                 vAv
               <> emitTrailingComment (amTrailing e)


{- | Returns 'Just' the original entry slice (including its
trailing newline) when the entry's value has an unmodified
span and the renderer is configured to preserve original text.
-}
verbatimEntry :: RenderOptions -> Maybe Text -> AValue -> Maybe Text
verbatimEntry opts msrc av
  | roPreserveOriginalText opts
  , not (avDirty av)
  , Just sp <- avSpan av
  , Just src <- msrc
  , Just sl <- spanSlice src sp =
      Just sl
  | otherwise = Nothing


emitAMapKey :: RenderOptions -> AValue -> Builder
emitAMapKey opts av = case avBody av of
  AString t SSPlain -> emitPlainOrQuotedKey opts t
  AString t SSDoubleQuoted -> doubleQuoted t
  AString t SSSingleQuoted -> singleQuoted t
  AString t _ -> emitPlainOrQuotedKey opts t
  AInt n -> BI.decimal n
  ABool b -> fromText (if b then "true" else "false")
  ANull -> fromText "null"
  AFloat d -> formatDouble d
  _ -> emitFlowValue opts (astripAnnotationsForFlow av)


astripAnnotationsForFlow :: AValue -> Value
astripAnnotationsForFlow = astrip
  where
    astrip av = case avBody av of
      ANull -> YNull
      ABool b -> YBool b
      AInt n -> YInt n
      AFloat d -> YFloat d
      AString t _ -> YString t
      ASeq xs _ -> YSeq (V.map astrip xs)
      AMap es _ ->
        YMap
          ( V.map
              ( \e ->
                  ( astrip (amKey e)
                  , astrip (amValue e)
                  )
              )
              es
          )
      ATagged tg x -> YTagged tg (astrip x)
      AAnchored a x -> YAnchored a (astrip x)
      AAlias (Anchor n) -> YString (T.cons '*' n)


emitTrailingComment :: Maybe Comment -> Builder
emitTrailingComment Nothing = mempty
emitTrailingComment (Just c) =
  fromText "  # " <> fromText (commentText c) <> singleton '\n'


emitAComments :: Int -> [Comment] -> Builder
emitAComments _ [] = mempty
emitAComments ind cs =
  mconcat
    [ spaces (max ind (commentColumn c))
        <> singleton '#'
        <> singleton ' '
        <> fromText (commentText c)
        <> singleton '\n'
    | c <- cs
    ]


-- | Emit blank lines + leading comments before an entry.
emitTrivia :: Int -> Trivia -> Builder
emitTrivia ind tr =
  mconcat (replicate (triviaBlankLines tr) (singleton '\n'))
    <> emitAComments ind (triviaLeading tr)


emitComments :: Int -> [Comment] -> Builder
emitComments = emitAComments


-- printf-ish for hex escapes (avoid pulling in Text.Printf for one
-- call site).
printf :: String -> Int -> String
printf "\\u%04x" n = "\\u" <> pad4 (showHex n "")
  where
    pad4 s = replicate (4 - length s) '0' <> s
printf fmt _ = fmt
