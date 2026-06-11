{-# LANGUAGE BangPatterns #-}

{- | Type-safe XPath-style query DSL for the XML DOM.

Compose queries using '(/>) for child chaining, '(//>) for descendant
chaining, and '(|>) for union. Filter with 'where_', 'whereAttr', etc.

@
import XML.DSL
import qualified XML.Path as XP

-- Find all \<item\> children of \<items\>, filtered by type attribute
let q = child \"items\" /> child \"item\" \`whereAttr\` (\"type\", \"book\")
select q root
@
-}
module XML.DSL (
  Query (..),

  -- * Composition
  (/>),
  (//>),
  (|>),

  -- * Axes
  child,
  anyChild,
  descendant,
  anyDescendant,
  parent,
  self,
  followingSibling,
  precedingSibling,

  -- * Content
  textContent,
  attribute,
  hasAttribute,
  textNodes,
  commentNodes,

  -- * Filtering
  where_,
  whereAttr,
  whereText,
  whereContains,

  -- * Indexing
  index,
  first,
  last,

  -- * Running queries
  select,
  selectOne,
  selectText,

  -- * Aggregation
  count,

  -- * User extension points
  liftQuery,
  liftPure,
  liftMaybe,
  liftFilter,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import XML.Path qualified as XP
import XML.Value
import Prelude hiding (last)


newtype Query a b = Query {runQuery :: a -> Vector b}


instance Functor (Query a) where
  fmap f (Query q) = Query (V.map f . q)


-- -----------------------------------------------------------------------
-- Composition
-- -----------------------------------------------------------------------

infixl 1 />


-- | Child-axis chaining: pipe output of one query into the next.
(/>) :: Query a b -> Query b c -> Query a c
(Query f) /> (Query g) = Query (\a -> V.concatMap g (f a))


infixl 1 //>


{- | Descendant-axis chaining: for each result of the left query, search
all descendants with the right query.
-}
(//>) :: Query a Node -> Query Node c -> Query a c
(Query f) //> (Query g) = Query $ \a ->
  let !nodes = f a
      allDesc n = V.cons n (XP.allDescendants n)
  in V.concatMap (\n -> V.concatMap g (allDesc n)) nodes


infixl 2 |>


-- | Union: concatenate results of two queries.
(|>) :: Query a b -> Query a b -> Query a b
(Query f) |> (Query g) = Query (\a -> f a V.++ g a)


-- -----------------------------------------------------------------------
-- Axes
-- -----------------------------------------------------------------------

-- | Select child elements by local name.
child :: Text -> Query Node Node
child name = Query $ \case
  Element _ _ cs -> V.filter (XP.isElementNamed name) cs
  _ -> V.empty


-- | Select all child elements (wildcard @*@).
anyChild :: Query Node Node
anyChild = Query $ \case
  Element _ _ cs -> V.filter XP.isElement cs
  _ -> V.empty


-- | Select descendant elements by local name.
descendant :: Text -> Query Node Node
descendant name = Query (XP.descendantsByName (simpleName name))


-- | Select all descendant elements.
anyDescendant :: Query Node Node
anyDescendant = Query XP.allDescendants


-- | Navigate to the parent cursor.
parent :: Query XP.Cursor XP.Cursor
parent = Query $ \c -> maybe V.empty V.singleton (XP.cursorParent c)


-- | Identity axis (self).
self :: Query a a
self = Query V.singleton


-- | Following siblings of a cursor.
followingSibling :: Query XP.Cursor XP.Cursor
followingSibling = Query XP.followingSiblings


-- | Preceding siblings of a cursor.
precedingSibling :: Query XP.Cursor XP.Cursor
precedingSibling = Query XP.precedingSiblings


-- -----------------------------------------------------------------------
-- Content
-- -----------------------------------------------------------------------

-- | Extract recursive text content of a node.
textContent :: Query Node Text
textContent = Query $ \n -> V.singleton (XP.textContent n)


-- | Look up an attribute by local name.
attribute :: Text -> Query Node (Maybe Text)
attribute name = Query $ \n -> V.singleton (XP.attr name n)


-- | Test whether an element has a given attribute (any value).
hasAttribute :: Text -> Query Node Node
hasAttribute name = Query $ \n ->
  case XP.attr name n of
    Just _ -> V.singleton n
    Nothing -> V.empty


-- | Direct text-node children.
textNodes :: Query Node Text
textNodes = Query XP.textNodes


-- | Direct comment-node children.
commentNodes :: Query Node Text
commentNodes = Query XP.commentNodes


-- -----------------------------------------------------------------------
-- Filtering
-- -----------------------------------------------------------------------

-- | Keep only results satisfying a predicate.
where_ :: (b -> Bool) -> Query a b -> Query a b
where_ p (Query q) = Query (V.filter p . q)


-- | Filter elements by @[\@attr='val']@.
whereAttr :: Text -> Text -> Query a Node -> Query a Node
whereAttr name val = where_ (\n -> XP.attr name n == Just val)


-- | Filter elements by @[text()='val']@.
whereText :: Text -> Query a Node -> Query a Node
whereText val = where_ (\n -> XP.textContent n == val)


-- | Filter elements where an attribute contains a substring.
whereContains :: Text -> Text -> Query a Node -> Query a Node
whereContains attrName substr =
  where_
    ( \n ->
        maybe False (T.isInfixOf substr) (XP.attr attrName n)
    )


-- -----------------------------------------------------------------------
-- Indexing (1-based like XPath)
-- -----------------------------------------------------------------------

-- | Select the @i@-th result (1-based).
index :: Int -> Query a b -> Query a b
index i (Query q) = Query $ \a ->
  let !r = q a
  in if i > 0 && i <= V.length r
       then V.singleton (r V.! (i - 1))
       else V.empty


-- | Select the first result.
first :: Query a b -> Query a b
first = index 1


-- | Select the last result.
last :: Query a b -> Query a b
last (Query q) = Query $ \a ->
  let !r = q a
  in if V.null r then V.empty else V.singleton (V.last r)


-- -----------------------------------------------------------------------
-- Running queries
-- -----------------------------------------------------------------------

-- | Run a query, returning all results.
select :: Query Node b -> Node -> Vector b
select (Query q) = q


-- | Run a query, returning the first result if any.
selectOne :: Query Node b -> Node -> Maybe b
selectOne q n =
  let !r = select q n
  in if V.null r then Nothing else Just (V.head r)


-- | Run a text-producing query and concatenate all results.
selectText :: Query Node Text -> Node -> Text
selectText q n = T.concat (V.toList (select q n))


-- -----------------------------------------------------------------------
-- Aggregation
-- -----------------------------------------------------------------------

-- | Count the number of results produced by a query.
count :: Query a b -> Query a Int
count (Query q) = Query (\a -> V.singleton (V.length (q a)))


-- -----------------------------------------------------------------------
-- User extension points
-- -----------------------------------------------------------------------

-- | Lift an arbitrary function into a 'Query'.
liftQuery :: (a -> Vector b) -> Query a b
liftQuery = Query


-- | Lift a pure function (always produces exactly one result).
liftPure :: (a -> b) -> Query a b
liftPure f = Query (V.singleton . f)


-- | Lift a partial function.
liftMaybe :: (a -> Maybe b) -> Query a b
liftMaybe f = Query (\a -> maybe V.empty V.singleton (f a))


-- | Lift a predicate as a pass-through filter.
liftFilter :: (a -> Bool) -> Query a a
liftFilter p = Query (\a -> if p a then V.singleton a else V.empty)
