-- | Annotated YAML representation that preserves enough source-level
-- information to round-trip a parsed document /verbatim/, while still
-- allowing programmatic modification.
--
-- The intended workflow is:
--
-- @
-- Right adoc <- 'YAML.Decode.Annotated.decodeAnnotated' yamlText
-- let adoc' = 'updateAt' [\"server\", \"port\"] ('aInt' 8081) adoc
-- let yamlText' = 'YAML.Encode.Annotated.render' 'YAML.Pretty.defaultOptions' adoc'
-- @
--
-- The renderer copies original source bytes for sub-trees that haven't
-- been modified (preserving comments, blank lines, quote style,
-- block-vs-flow choice, indentation, etc.) and falls back to the
-- pretty-printer's settings for fresh content.
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
module YAML.Annotated
  ( -- * Annotated value
    AValue (..)
  , ABody (..)
  , AMapEntry (..)
  , Trivia (..)
  , Comment (..)
  , SrcSpan (..)

    -- * Style
  , ScalarStyle (..)
  , Chomping (..)
  , SeqStyle (..)
  , MapStyle (..)

    -- * Document / stream
  , ADocument (..)
  , AStream (..)

    -- * Construction (un-annotated, ready for fresh emission)
  , aNull
  , aBool
  , aInt
  , aFloat
  , aString
  , aStringStyled
  , aSeq
  , aMap
  , aTagged
  , aAnchored

    -- * Querying
  , abody
  , astripAnnotations
  , isDirty
  , markDirty

    -- * Modification helpers
  , setKey
  , updateKey
  , deleteKey
  , appendKey
  , updateAt
  , setIndex
  , appendItem
  , deleteIndex
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import YAML.Value (Tag (..), Anchor (..))
import qualified YAML.Value as YV

-- ---------------------------------------------------------------------------
-- Source spans + trivia
-- ---------------------------------------------------------------------------

-- | A half-open byte range @[start, end)@ in the original source
-- 'Text' (encoded as UTF-8 and indexed by byte position so the
-- offsets are stable across text-encoding boundaries).
data SrcSpan = SrcSpan
  { srcStart :: {-# UNPACK #-} !Int
  , srcEnd   :: {-# UNPACK #-} !Int
  } deriving stock    (Show, Eq, Ord, Generic)
    deriving anyclass (NFData)

-- | A single comment token: a leading @#@ on its own line, or an
-- end-of-line comment trailing a value.
data Comment = Comment
  { commentText   :: !Text
    -- ^ The text after the @#@, without the @#@ itself or any
    -- indenting whitespace. May contain trailing whitespace.
  , commentColumn :: {-# UNPACK #-} !Int
    -- ^ Column where the @#@ sat in the source, useful for
    -- preserving indentation when re-emitting fresh content
    -- around the comment.
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Whitespace / comments / blank-lines that decorate a value.
data Trivia = Trivia
  { triviaLeading      :: ![Comment]
    -- ^ Stand-alone comments on lines /before/ this value
    -- (between the previous structural token and this one).
  , triviaBlankLines   :: {-# UNPACK #-} !Int
    -- ^ Number of blank lines immediately preceding the value
    -- (after any leading comments).
  , triviaTrailing     :: !(Maybe Comment)
    -- ^ End-of-line comment on the same line as the value, if
    -- any.
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | Trivia with no comments and no blank lines.
emptyTrivia :: Trivia
emptyTrivia = Trivia [] 0 Nothing

-- ---------------------------------------------------------------------------
-- Style
-- ---------------------------------------------------------------------------

-- | How a scalar was (or should be) emitted.
data ScalarStyle
  = SSPlain
    -- ^ @value@ — unquoted plain scalar.
  | SSDoubleQuoted
    -- ^ @\"value\"@ — double-quoted; supports YAML escapes.
  | SSSingleQuoted
    -- ^ @\'value\'@ — single-quoted.
  | SSLiteral !Chomping
    -- ^ @|@ — literal block scalar.
  | SSFolded !Chomping
    -- ^ @>@ — folded block scalar.
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | Chomping indicator for block scalars.
data Chomping = Clip | Strip | Keep
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | How a sequence was (or should be) emitted.
data SeqStyle = SeqBlock | SeqFlow
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | How a mapping was (or should be) emitted.
data MapStyle = MapBlock | MapFlow
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- ---------------------------------------------------------------------------
-- The annotated tree
-- ---------------------------------------------------------------------------

-- | Annotated YAML value.
--
-- Carries enough information for the renderer to reproduce the
-- original source byte-for-byte when nothing has been modified
-- ('avSpan' present and 'avDirty' False) and to fall back to the
-- 'YAML.Pretty.RenderOptions' pretty-printer otherwise.
data AValue = AValue
  { avBody       :: !ABody
  , avTrivia     :: !Trivia
  , avSpan       :: !(Maybe SrcSpan)
    -- ^ The half-open byte range in the original source that
    -- produced this value, if any. 'Nothing' means the value
    -- was constructed fresh and has no original-source backing.
  , avDirty      :: !Bool
    -- ^ Set to 'True' when a sub-tree has been modified; the
    -- renderer falls back to fresh emission for any node with
    -- @'avDirty' == True@ even if 'avSpan' is present.
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | The actual structural body of an 'AValue'.
data ABody
  = ANull
  | ABool   !Bool
  | AInt    !Int64
  | AFloat  !Double
  | AString !Text   !ScalarStyle
  | ASeq    !(Vector AValue)         !SeqStyle
  | AMap    !(Vector AMapEntry)      !MapStyle
  | ATagged    !Tag    !AValue
  | AAnchored  !Anchor !AValue
  | AAlias     !Anchor
    -- ^ A @*alias@ reference. The annotated layer keeps it
    -- explicit; the projection to 'YAML.Value.Value' resolves
    -- it.
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- | A mapping entry.
--
-- The @amTrailing@ comment is the eol comment on the value line
-- (if any); per-entry trivia for blank lines and stand-alone
-- comments lives on the @amKey@'s 'avTrivia'.
data AMapEntry = AMapEntry
  { amKey      :: !AValue
  , amValue    :: !AValue
  , amTrailing :: !(Maybe Comment)
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (NFData)

-- ---------------------------------------------------------------------------
-- Documents / streams
-- ---------------------------------------------------------------------------

-- | An annotated document.
data ADocument = ADocument
  { adDirectivesEnd :: !Bool
  , adExplicitEnd   :: !Bool
  , adBody          :: !AValue
  , adLeading       :: ![Comment]
    -- ^ Comments before the document body (after any preceding
    -- @---@ marker).
  , adTrailing      :: ![Comment]
    -- ^ Comments after the body (before any @...@ marker or the
    -- next document).
  } deriving stock    (Show, Eq, Generic)
    deriving anyclass (NFData)

-- | An annotated multi-document stream.
newtype AStream = AStream { aStreamDocs :: Vector ADocument }
  deriving stock    (Show, Eq, Generic)
  deriving newtype  (NFData)

-- ---------------------------------------------------------------------------
-- Construction (fresh, un-annotated)
-- ---------------------------------------------------------------------------

freshAValue :: ABody -> AValue
freshAValue b = AValue b emptyTrivia Nothing False

aNull :: AValue
aNull = freshAValue ANull

aBool :: Bool -> AValue
aBool b = freshAValue (ABool b)

aInt :: Int64 -> AValue
aInt n = freshAValue (AInt n)

aFloat :: Double -> AValue
aFloat d = freshAValue (AFloat d)

-- | Construct a string in the renderer's default scalar style
-- (typically plain unless the body would parse back as some other
-- core-schema literal, in which case the renderer will quote it).
aString :: Text -> AValue
aString t = freshAValue (AString t SSPlain)

-- | Construct a string with an explicit scalar style.
aStringStyled :: Text -> ScalarStyle -> AValue
aStringStyled t s = freshAValue (AString t s)

aSeq :: [AValue] -> AValue
aSeq xs = freshAValue (ASeq (V.fromList xs) SeqBlock)

aMap :: [(AValue, AValue)] -> AValue
aMap kvs = freshAValue
  (AMap (V.fromList [AMapEntry k v Nothing | (k, v) <- kvs]) MapBlock)

aTagged :: Tag -> AValue -> AValue
aTagged t v = freshAValue (ATagged t v)

aAnchored :: Anchor -> AValue -> AValue
aAnchored a v = freshAValue (AAnchored a v)

-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

abody :: AValue -> ABody
abody = avBody

isDirty :: AValue -> Bool
isDirty = avDirty

-- | Mark a value as dirty so the renderer will re-emit it from
-- scratch (using 'YAML.Pretty.RenderOptions') instead of copying
-- the original source bytes.
markDirty :: AValue -> AValue
markDirty v = v { avDirty = True }
{-# INLINE markDirty #-}

-- | Project an 'AValue' down to the unannotated 'YAML.Value.Value'
-- representation. Aliases project to 'YAML.Value.YString' bearing
-- the alias name; resolve them via the parser's anchor map if you
-- need full alias semantics.
astripAnnotations :: AValue -> YV.Value
astripAnnotations a = case avBody a of
  ANull          -> YV.YNull
  ABool b        -> YV.YBool b
  AInt n         -> YV.YInt n
  AFloat d       -> YV.YFloat d
  AString t _    -> YV.YString t
  ASeq xs _      -> YV.YSeq (V.map astripAnnotations xs)
  AMap es _      -> YV.YMap
                      (V.map (\(AMapEntry k v _) ->
                                (astripAnnotations k, astripAnnotations v))
                             es)
  ATagged tg v   -> YV.YTagged tg (astripAnnotations v)
  AAnchored an v -> YV.YAnchored an (astripAnnotations v)
  AAlias (Anchor n) -> YV.YString (T.pack "*" <> n)

-- ---------------------------------------------------------------------------
-- Modification helpers
-- ---------------------------------------------------------------------------

-- | Replace the value at the given mapping key. If the key isn't
-- present the original mapping is returned unchanged. Marks the
-- mapping (and only the mapping) as dirty.
setKey :: Text -> AValue -> AValue -> AValue
setKey k newV m = updateKey k (const (Just newV)) m

-- | Update the value at the given mapping key. Returning 'Nothing'
-- from the function deletes the entry; returning 'Just' replaces
-- it. If the key isn't present the original mapping is returned.
updateKey :: Text -> (AValue -> Maybe AValue) -> AValue -> AValue
updateKey k f m = case avBody m of
  AMap entries style ->
    let (changed, entries') = updateEntries entries
    in if changed
         then markDirty m { avBody = AMap entries' style }
         else m
  _ -> m
  where
    updateEntries es = V.foldr go (False, V.empty) es
      where
        go entry (cAcc, accVec) = case avBody (amKey entry) of
          AString kt _ | kt == k ->
            case f (amValue entry) of
              Just v' -> (True, V.cons (entry { amValue = markDirty v' }) accVec)
              Nothing -> (True, accVec)  -- delete
          _ -> (cAcc, V.cons entry accVec)

-- | Delete a key from a mapping. No-op if absent.
deleteKey :: Text -> AValue -> AValue
deleteKey k m = updateKey k (const Nothing) m

-- | Append a key-value pair to the end of a mapping. If the key
-- already exists the existing value is left in place; use
-- 'setKey' to replace.
appendKey :: Text -> AValue -> AValue -> AValue
appendKey k v m = case avBody m of
  AMap entries style
    | any (sameKey k) (V.toList entries) -> m
    | otherwise ->
        let entry = AMapEntry (markDirty (aString k))
                              (markDirty v)
                              Nothing
        in markDirty m
             { avBody = AMap (V.snoc entries entry) style }
  _ -> m
  where
    sameKey kk e = case avBody (amKey e) of
      AString s _ -> s == kk
      _           -> False

-- | Walk a path of mapping keys and apply 'setKey' at the leaf.
-- The intermediate mappings are also marked dirty so the renderer
-- knows their layout might need re-flowing.
updateAt :: [Text] -> AValue -> AValue -> AValue
updateAt []       _ m = m
updateAt [k]      v m = setKey k v m
updateAt (k : ks) v m = updateKey k (Just . updateAt ks v) m

-- | Replace an item in a sequence. If the index is out of bounds
-- the sequence is returned unchanged.
setIndex :: Int -> AValue -> AValue -> AValue
setIndex i x s = case avBody s of
  ASeq items style
    | i >= 0, i < V.length items ->
        markDirty s
          { avBody = ASeq (items V.// [(i, markDirty x)]) style }
  _ -> s

-- | Append an item to the end of a sequence.
appendItem :: AValue -> AValue -> AValue
appendItem x s = case avBody s of
  ASeq items style ->
    markDirty s
      { avBody = ASeq (V.snoc items (markDirty x)) style }
  _ -> s

-- | Remove an item from a sequence.
deleteIndex :: Int -> AValue -> AValue
deleteIndex i s = case avBody s of
  ASeq items style
    | i >= 0, i < V.length items ->
        let items' = V.ifilter (\j _ -> j /= i) items
        in markDirty s { avBody = ASeq items' style }
  _ -> s
