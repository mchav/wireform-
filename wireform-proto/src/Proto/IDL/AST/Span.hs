{-# LANGUAGE TypeFamilies #-}

{- | Source spans and trees-that-grow phase types for exactprint support.

The AST types in "Proto.IDL.AST" are parameterized by a phase @p@.
Type families map each phase to the appropriate annotation:

* 'Semantic' — no source information, used by codegen and analysis
* 'Parsed' — carries 'Span' for byte-accurate source reconstruction

Backward-compatible type aliases (@MessageDef = MessageDef' Semantic@, etc.)
keep existing consumer code unchanged.
-}
module Proto.IDL.AST.Span (
  -- * Phase types
  Parsed,
  Semantic,

  -- * Source spans
  SrcSpan (..),
  Span (..),
  noSpan,
  mkSpan,

  -- * Extension type families
  XNode,
) where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)


-- | Phase for parsed ASTs that carry source span information.
data Parsed


{- | Phase for semantic ASTs with no source location info.
This is the default used by codegen, analysis, and existing consumer code.
-}
data Semantic


-- | Byte-offset span into the original source text.
data SrcSpan = SrcSpan
  { spanStart :: {-# UNPACK #-} !Int
  -- ^ 0-based byte offset, inclusive
  , spanEnd :: {-# UNPACK #-} !Int
  -- ^ 0-based byte offset, exclusive
  }
  deriving stock (Show, Generic)


instance Eq SrcSpan where _ == _ = True


instance Ord SrcSpan where compare _ _ = EQ


instance NFData SrcSpan


{- | Optional source span, wrapping 'Maybe SrcSpan'.
'Eq' and 'Ord' are phantom (always equal) so derived instances
on AST types remain semantic.
-}
newtype Span = Span {unSpan :: Maybe SrcSpan}
  deriving stock (Show, Generic)


instance Eq Span where _ == _ = True


instance Ord Span where compare _ _ = EQ


instance NFData Span


-- | No source span (for programmatically constructed nodes).
noSpan :: Span
noSpan = Span Nothing


-- | Construct a span from start (inclusive) and end (exclusive) byte offsets.
mkSpan :: Int -> Int -> Span
mkSpan s e = Span (Just (SrcSpan s e))


{- | Extension type family: maps a phase to the node annotation type.

@XNode Parsed = Span@ — parsed nodes carry source spans.
@XNode Semantic = ()@ — semantic nodes carry no extra info.
-}
type family XNode (p :: *)


type instance XNode Parsed = Span


type instance XNode Semantic = ()
