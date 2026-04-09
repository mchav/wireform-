-- | HTML5 DOM types.
--
-- Unlike XML, HTML is case-insensitive — tag and attribute names are stored
-- lowercase. Void elements (br, hr, img, …) have no closing tag. Optional
-- closing tags and implicit element creation are handled by the parser.
module HTML.Value
  ( HTMLDocument(..)
  , HTMLNode(..)
  , HTMLAttribute(..)
  , Doctype(..)
  , textContent
  , getAttr
  , isVoidElement
  , isRawTextElement
  ) where

import Control.DeepSeq (NFData(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, toLazyText, fromText)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data HTMLDocument = HTMLDocument
  { htmlDoctype :: !(Maybe Doctype)
  , htmlRoot :: !HTMLNode
  } deriving stock (Show, Eq, Generic)

instance NFData HTMLDocument where
  rnf (HTMLDocument d r) = rnf d `seq` rnf r

data HTMLNode
  = HTMLElement !Text !(Vector HTMLAttribute) !(Vector HTMLNode)
  | HTMLText !Text
  | HTMLComment !Text
  | HTMLDoctype !Text !(Maybe Text) !(Maybe Text)
  deriving stock (Show, Eq, Generic)

instance NFData HTMLNode where
  rnf (HTMLElement t as cs) = rnf t `seq` rnf as `seq` rnf cs
  rnf (HTMLText t) = rnf t
  rnf (HTMLComment t) = rnf t
  rnf (HTMLDoctype t p s) = rnf t `seq` rnf p `seq` rnf s

data HTMLAttribute = HTMLAttribute !Text !Text
  deriving stock (Show, Eq, Generic)

instance NFData HTMLAttribute where
  rnf (HTMLAttribute n v) = rnf n `seq` rnf v

data Doctype = Doctype !(Maybe Text) !(Maybe Text) !(Maybe Text)
  deriving stock (Show, Eq, Generic)

instance NFData Doctype where
  rnf (Doctype a b c) = rnf a `seq` rnf b `seq` rnf c

textContent :: HTMLNode -> Text
textContent node = TL.toStrict (toLazyText (go node))
  where
    go :: HTMLNode -> Builder
    go (HTMLText t) = fromText t
    go (HTMLComment _) = mempty
    go (HTMLDoctype _ _ _) = mempty
    go (HTMLElement _ _ cs) = V.foldl' (\acc c -> acc <> go c) mempty cs

getAttr :: Text -> HTMLNode -> Maybe Text
getAttr name (HTMLElement _ attrs _) = go 0
  where
    !len = V.length attrs
    go !i
      | i >= len = Nothing
      | HTMLAttribute n v <- attrs V.! i, n == name = Just v
      | otherwise = go (i + 1)
getAttr _ _ = Nothing

{-# INLINE isVoidElement #-}
isVoidElement :: Text -> Bool
isVoidElement t = case t of
  "area" -> True; "base" -> True; "br" -> True; "col" -> True
  "embed" -> True; "hr" -> True; "img" -> True; "input" -> True
  "link" -> True; "meta" -> True; "source" -> True; "track" -> True
  "wbr" -> True
  _ -> False

{-# INLINE isRawTextElement #-}
isRawTextElement :: Text -> Bool
isRawTextElement t = t == "script" || t == "style"
