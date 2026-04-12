-- | CSS selector-style queries for HTML DOM.
--
-- Supports simple selectors: tag name, .class, #id, tag.class, tag#id,
-- and descendant combinators (space-separated).
module HTML.Query
  ( querySelector
  , querySelectorAll
  , getElementById
  , getElementsByClass
  , getElementsByTag
  ) where

import Data.Foldable (toList)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import HTML.Value

querySelector :: Text -> HTMLNode -> Maybe HTMLNode
querySelector sel node =
  let !results = querySelectorAll sel node
  in if V.null results then Nothing else Just (V.head results)

querySelectorAll :: Text -> HTMLNode -> Vector HTMLNode
querySelectorAll sel node =
  let !parts = T.words sel
  in case parts of
       [] -> V.empty
       [s] -> matchSelector (parseSimpleSelector s) node
       _ -> matchDescendantChain (map parseSimpleSelector parts) node

data SimpleSelector
  = SelTag !Text
  | SelClass !Text
  | SelId !Text
  | SelTagClass !Text !Text
  | SelTagId !Text !Text
  | SelAny
  deriving (Show)

parseSimpleSelector :: Text -> SimpleSelector
parseSimpleSelector s
  | T.null s = SelAny
  | T.isPrefixOf "." s = SelClass (T.drop 1 s)
  | T.isPrefixOf "#" s = SelId (T.drop 1 s)
  | T.any (== '.') s =
      let (tag, rest) = T.breakOn "." s
      in SelTagClass tag (T.drop 1 rest)
  | T.any (== '#') s =
      let (tag, rest) = T.breakOn "#" s
      in SelTagId tag (T.drop 1 rest)
  | s == "*" = SelAny
  | otherwise = SelTag s

matchSelector :: SimpleSelector -> HTMLNode -> Vector HTMLNode
matchSelector sel = collectDescendants (matchesSelector sel)

matchesSelector :: SimpleSelector -> HTMLNode -> Bool
matchesSelector sel (HTMLElement tag attrs _) = case sel of
  SelTag t -> tag == t
  SelClass c -> hasClass c attrs
  SelId i -> hasId i attrs
  SelTagClass t c -> tag == t && hasClass c attrs
  SelTagId t i -> tag == t && hasId i attrs
  SelAny -> True
matchesSelector _ _ = False

hasClass :: Text -> SmallArray HTMLAttribute -> Bool
hasClass cls attrs = case findAttr "class" attrs of
  Nothing -> False
  Just val -> cls `elem` T.words val

hasId :: Text -> SmallArray HTMLAttribute -> Bool
hasId i attrs = findAttr "id" attrs == Just i

findAttr :: Text -> SmallArray HTMLAttribute -> Maybe Text
findAttr name attrs = go 0
  where
    !len = sizeofSmallArray attrs
    go !i
      | i >= len = Nothing
      | HTMLAttribute n v <- indexSmallArray attrs i, n == name = Just v
      | otherwise = go (i + 1)

collectDescendants :: (HTMLNode -> Bool) -> HTMLNode -> Vector HTMLNode
collectDescendants pred' node = V.fromList (go node [])
  where
    go n@(HTMLElement _ _ children) acc =
      let acc' = foldr go acc (toList children)
      in if pred' n then n : acc' else acc'
    go _ acc = acc

matchDescendantChain :: [SimpleSelector] -> HTMLNode -> Vector HTMLNode
matchDescendantChain [] _ = V.empty
matchDescendantChain [s] node = matchSelector s node
matchDescendantChain (s:ss) node =
  let !ancestors = matchSelector s node
  in V.concatMap (matchDescendantChain ss) ancestors

getElementById :: Text -> HTMLNode -> Maybe HTMLNode
getElementById i = querySelector ("#" <> i)

getElementsByClass :: Text -> HTMLNode -> Vector HTMLNode
getElementsByClass c = querySelectorAll ("." <> c)

getElementsByTag :: Text -> HTMLNode -> Vector HTMLNode
getElementsByTag t = querySelectorAll t
