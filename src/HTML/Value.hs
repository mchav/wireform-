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
textContent (HTMLText t) = t
textContent (HTMLComment _) = T.empty
textContent (HTMLDoctype _ _ _) = T.empty
textContent (HTMLElement _ _ cs) = T.concat (V.toList (V.map textContent cs))

getAttr :: Text -> HTMLNode -> Maybe Text
getAttr name (HTMLElement _ attrs _) = go 0
  where
    !len = V.length attrs
    go !i
      | i >= len = Nothing
      | HTMLAttribute n v <- attrs V.! i, n == name = Just v
      | otherwise = go (i + 1)
getAttr _ _ = Nothing

isVoidElement :: Text -> Bool
isVoidElement t = t `elem` voidElements

voidElements :: [Text]
voidElements =
  [ "area", "base", "br", "col", "embed", "hr", "img", "input"
  , "link", "meta", "source", "track", "wbr"
  ]

isRawTextElement :: Text -> Bool
isRawTextElement t = t == "script" || t == "style"
