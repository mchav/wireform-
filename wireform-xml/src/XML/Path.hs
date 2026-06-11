{-# LANGUAGE BangPatterns #-}

{- | XPath-lite query engine for the XML DOM.

Inspired by xml-conduit's cursor API, operating on 'Node' values
with 'Vector'-based children for fast lookup.
-}
module XML.Path (
  Cursor (..),
  Path (..),
  query,
  queryPath,
  attr,
  textContent,
  parsePath,
  fromNode,
  children,
  descendants,
  attributeValue,

  -- * Axes
  parentNode,
  selfNode,
  followingSiblings,
  precedingSiblings,
  ancestors,

  -- * Wildcards and node type selectors
  anyElement,
  textNodes,
  commentNodes,

  -- * Predicate helpers
  atIndex,
  withAttr,
  withText,
  containing,
  startsWith,

  -- * String functions
  normalizeSpace,

  -- * Helpers for DSL
  descendantsByName,
  allDescendants,
  isElement,
  isElementNamed,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import XML.Value


-- | A cursor pointing to a node in the DOM.
data Cursor = Cursor
  { cursorNode :: !Node
  , cursorParent :: !(Maybe Cursor)
  }
  deriving stock (Show)


instance Eq Cursor where
  a == b = cursorNode a == cursorNode b


-- | Path expression (simplified XPath subset).
data Path
  = Child !Name -- /name
  | Descendant !Name -- //name
  | AttrPath !Name -- @name
  | Index !Int -- [N]
  | Predicate Path Text -- [path='value']
  | Sequence [Path] -- chained paths
  | Self -- .
  | Parent -- ..
  | Wildcard
  deriving stock (Show, Eq)


-- | Create a cursor from a node.
fromNode :: Node -> Cursor
fromNode n = Cursor n Nothing


-- | Get child cursors.
children :: Cursor -> Vector Cursor
children (Cursor node parent) =
  case node of
    Element _ _ cs -> V.map (\c -> Cursor c (Just (Cursor node parent))) cs
    _ -> V.empty


-- | Get all descendant cursors.
descendants :: Cursor -> Vector Cursor
descendants cursor =
  let !cs = children cursor
      !descs = V.concatMap (\c -> V.cons c (descendants c)) cs
  in descs


-- | Get attribute value from a cursor node.
attributeValue :: Text -> Cursor -> Maybe Text
attributeValue name (Cursor (Element _ attrs _) _) =
  let go !i
        | i >= V.length attrs = Nothing
        | Attribute aname val <- attrs V.! i
        , nameLocal aname == name =
            Just val
        | otherwise = go (i + 1)
  in go 0
attributeValue _ _ = Nothing


-- | Query the DOM with a path.
query :: Path -> Node -> Vector Node
query path node = queryImpl path (V.singleton node)


queryImpl :: Path -> Vector Node -> Vector Node
queryImpl (Child name) nodes =
  V.concatMap (childrenByName name) nodes
queryImpl (Descendant name) nodes =
  V.concatMap (descendantsByName name) nodes
queryImpl (AttrPath name) nodes =
  V.concatMap (getAttrNode name) nodes
queryImpl (Index idx) nodes =
  if idx >= 0 && idx < V.length nodes
    then V.singleton (nodes V.! idx)
    else V.empty
queryImpl (Predicate path val) nodes =
  V.filter (matchPredicate path val) nodes
queryImpl (Sequence []) nodes = nodes
queryImpl (Sequence (p : ps)) nodes =
  queryImpl (Sequence ps) (queryImpl p nodes)
queryImpl Self nodes = nodes
queryImpl Parent _nodes = V.empty
queryImpl Wildcard nodes =
  V.concatMap anyElement nodes


childrenByName :: Name -> Node -> Vector Node
childrenByName name (Element _ _ cs) =
  V.filter (matchesName name) cs
childrenByName _ _ = V.empty


descendantsByName :: Name -> Node -> Vector Node
descendantsByName name node =
  let !cs = elementChildren node
      !matching = V.filter (matchesName name) cs
      !deeper = V.concatMap (descendantsByName name) cs
  in matching V.++ deeper


getAttrNode :: Name -> Node -> Vector Node
getAttrNode name (Element _ attrs _) =
  let matches = V.filter (\(Attribute aname _) -> nameLocal aname == nameLocal name) attrs
  in V.map (\(Attribute _ val) -> Text val) matches
getAttrNode _ _ = V.empty


matchesName :: Name -> Node -> Bool
matchesName target (Element name _ _) = nameLocal name == nameLocal target
matchesName _ _ = False


matchPredicate :: Path -> Text -> Node -> Bool
matchPredicate path val node =
  let !results = query path node
  in V.any (\n -> textContent n == val) results


{- | Convenience: query by simple element name path.
e.g., queryPath ["root", "items", "item"] node
-}
queryPath :: [Text] -> Node -> Vector Node
queryPath [] node = V.singleton node
queryPath names node = queryImpl (Sequence (map (Child . simpleName) names)) (V.singleton node)


-- | Get attribute value.
attr :: Text -> Node -> Maybe Text
attr name (Element _ attrs _) =
  let go !i
        | i >= V.length attrs = Nothing
        | Attribute aname val <- attrs V.! i
        , nameLocal aname == name =
            Just val
        | otherwise = go (i + 1)
  in go 0
attr _ _ = Nothing


-- | Get all text content (recursive).
textContent :: Node -> Text
textContent (Text t) = t
textContent (CData t) = t
textContent (Element _ _ cs) = T.concat (V.toList (V.map textContent cs))
textContent _ = T.empty


{- | Parse a simple path string: "root/items/item" or "root//item"
Extended to handle: ".", "..", "@attr", "*", "//name", "name[@attr='val']", "name[N]"
-}
parsePath :: Text -> Either String Path
parsePath t
  | T.null t = Left "Empty path"
  | T.isPrefixOf "@" t = Right (AttrPath (simpleName (T.drop 1 t)))
  | otherwise =
      let !parts = splitPath t
      in Right (Sequence (concatMap partToPath parts))
  where
    partToPath ("", name) = parseStep Descendant name
    partToPath (_, name) = parseStep Child name

    parseStep _ "." = [Self]
    parseStep _ ".." = [Parent]
    parseStep mkAxis raw =
      let (!base, !preds) = extractPredicates raw
          !axis = case base of
            "*" -> Wildcard
            _ -> mkAxis (simpleName base)
      in axis : preds

    extractPredicates :: Text -> (Text, [Path])
    extractPredicates s =
      let !base = T.takeWhile (/= '[') s
          !rest = T.drop (T.length base) s
      in (base, parsePreds rest)

    parsePreds :: Text -> [Path]
    parsePreds s
      | T.null s = []
      | T.isPrefixOf "[" s =
          let !inner = T.drop 1 (T.takeWhile (/= ']') s)
              !rest = T.drop 1 (T.dropWhile (/= ']') s)
              !p = parsePredInner inner
          in p : parsePreds rest
      | otherwise = []

    parsePredInner :: Text -> Path
    parsePredInner inner
      | T.isPrefixOf "@" inner
      , T.isInfixOf "='" inner =
          let !attrName = T.drop 1 (T.takeWhile (/= '=') inner)
              !val = T.dropEnd 1 (T.drop 1 (T.dropWhile (/= '\'') inner))
          in Predicate (AttrPath (simpleName attrName)) val
      | T.all (\c -> c >= '0' && c <= '9') inner =
          case reads (T.unpack inner) of
            [(n, "")] -> Index (n - 1)
            _ -> Child (simpleName inner)
      | otherwise = Child (simpleName inner)


splitPath :: Text -> [(Text, Text)]
splitPath t = go t []
  where
    go !s !acc
      | T.null s = reverse acc
      | T.isPrefixOf "//" s =
          let !rest = T.drop 2 s
              !name = takeStep rest
              !remaining = T.drop (T.length name) rest
          in go remaining (("", name) : acc)
      | T.isPrefixOf "/" s =
          let !rest = T.drop 1 s
              !name = takeStep rest
              !remaining = T.drop (T.length name) rest
          in go remaining (("/", name) : acc)
      | otherwise =
          let !name = takeStep s
              !remaining = T.drop (T.length name) s
          in go remaining (("/", name) : acc)

    takeStep :: Text -> Text
    takeStep s =
      let !base = T.takeWhile (\c -> c /= '/' && c /= '[') s
          !rest = T.drop (T.length base) s
      in if T.isPrefixOf "[" rest
           then
             let !bracket = takeBrackets rest
             in base <> bracket
           else base

    takeBrackets :: Text -> Text
    takeBrackets s
      | T.isPrefixOf "[" s =
          let !inner = T.takeWhile (/= ']') s <> "]"
              !rest = T.drop (T.length inner) s
          in inner <> takeBrackets rest
      | otherwise = T.empty


-- ---------------------------------------------------------------------------
-- Axes
-- ---------------------------------------------------------------------------

-- | Navigate to the parent cursor (if it exists).
parentNode :: Cursor -> Maybe Cursor
parentNode = cursorParent


-- | Identity axis: returns the cursor itself.
selfNode :: Cursor -> Cursor
selfNode = id


{- | All following sibling cursors (elements that come after this node
under the same parent).
-}
followingSiblings :: Cursor -> Vector Cursor
followingSiblings c = case cursorParent c of
  Nothing -> V.empty
  Just p ->
    let !cs = children p
        !idx = V.findIndex (\s -> cursorNode s == cursorNode c) cs
    in case idx of
         Nothing -> V.empty
         Just i -> V.drop (i + 1) cs


-- | All preceding sibling cursors.
precedingSiblings :: Cursor -> Vector Cursor
precedingSiblings c = case cursorParent c of
  Nothing -> V.empty
  Just p ->
    let !cs = children p
        !idx = V.findIndex (\s -> cursorNode s == cursorNode c) cs
    in case idx of
         Nothing -> V.empty
         Just i -> V.reverse (V.take i cs)


-- | All ancestor cursors from parent to root.
ancestors :: Cursor -> Vector Cursor
ancestors c = case cursorParent c of
  Nothing -> V.empty
  Just p -> V.cons p (ancestors p)


-- ---------------------------------------------------------------------------
-- Wildcards and node type selectors
-- ---------------------------------------------------------------------------

-- | @*@ wildcard: all element children.
anyElement :: Node -> Vector Node
anyElement (Element _ _ cs) = V.filter isElement cs
anyElement _ = V.empty


-- | @text()@ children: all direct text content nodes.
textNodes :: Node -> Vector Text
textNodes (Element _ _ cs) = V.concatMap go cs
  where
    go (Text t) = V.singleton t
    go (CData t) = V.singleton t
    go _ = V.empty
textNodes _ = V.empty


-- | @comment()@ children.
commentNodes :: Node -> Vector Text
commentNodes (Element _ _ cs) = V.concatMap go cs
  where
    go (Comment t) = V.singleton t
    go _ = V.empty
commentNodes _ = V.empty


-- ---------------------------------------------------------------------------
-- Predicate helpers
-- ---------------------------------------------------------------------------

-- | 1-based indexing (like XPath @[N]@).
atIndex :: Int -> Vector a -> Maybe a
atIndex i v
  | i > 0 && i <= V.length v = Just (v V.! (i - 1))
  | otherwise = Nothing


-- | Filter nodes by @[\@attr='val']@.
withAttr :: Text -> Text -> Vector Node -> Vector Node
withAttr name val = V.filter (\n -> attr name n == Just val)


-- | Filter nodes by @[text()='val']@.
withText :: Text -> Vector Node -> Vector Node
withText val = V.filter (\n -> textContent n == val)


-- | @contains(\@attr, 'str')@.
containing :: Text -> Text -> Node -> Bool
containing name substr n =
  maybe False (T.isInfixOf substr) (attr name n)


-- | @starts-with(\@attr, 'str')@.
startsWith :: Text -> Text -> Node -> Bool
startsWith name pfx n =
  maybe False (T.isPrefixOf pfx) (attr name n)


-- ---------------------------------------------------------------------------
-- String functions
-- ---------------------------------------------------------------------------

{- | XPath @normalize-space()@: collapse runs of whitespace to single
spaces and strip leading/trailing whitespace.
-}
normalizeSpace :: Text -> Text
normalizeSpace = T.unwords . T.words


-- ---------------------------------------------------------------------------
-- Internal helpers exported for DSL
-- ---------------------------------------------------------------------------

-- | Test if a node is an 'Element'.
isElement :: Node -> Bool
isElement (Element _ _ _) = True
isElement _ = False


-- | Test if a node is an element with the given local name.
isElementNamed :: Text -> Node -> Bool
isElementNamed name (Element n _ _) = nameLocal n == name
isElementNamed _ _ = False


-- | All descendant nodes (elements only).
allDescendants :: Node -> Vector Node
allDescendants node =
  let !cs = elementChildren node
      !elems = V.filter isElement cs
      !deeper = V.concatMap allDescendants cs
  in elems V.++ deeper
