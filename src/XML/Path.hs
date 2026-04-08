{-# LANGUAGE BangPatterns #-}
-- | XPath-lite query engine for the XML DOM.
--
-- Inspired by xml-conduit's cursor API, operating on 'Node' values
-- with 'Vector'-based children for fast lookup.
module XML.Path
  ( Cursor(..)
  , Path(..)
  , query
  , queryPath
  , attr
  , textContent
  , parsePath
  , fromNode
  , children
  , descendants
  , attributeValue
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import XML.Value

-- | A cursor pointing to a node in the DOM.
data Cursor = Cursor
  { cursorNode :: !Node
  , cursorParent :: !(Maybe Cursor)
  } deriving stock (Show)

instance Eq Cursor where
  a == b = cursorNode a == cursorNode b

-- | Path expression (simplified XPath subset).
data Path
  = Child !Name          -- /name
  | Descendant !Name     -- //name
  | AttrPath !Name       -- @name
  | Index !Int           -- [N]
  | Predicate Path Text  -- [path='value']
  | Sequence [Path]      -- chained paths
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
        , nameLocal aname == name = Just val
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
queryImpl (Sequence (p:ps)) nodes =
  queryImpl (Sequence ps) (queryImpl p nodes)

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

-- | Convenience: query by simple element name path.
-- e.g., queryPath ["root", "items", "item"] node
queryPath :: [Text] -> Node -> Vector Node
queryPath [] node = V.singleton node
queryPath names node = queryImpl (Sequence (map (Child . simpleName) names)) (V.singleton node)

-- | Get attribute value.
attr :: Text -> Node -> Maybe Text
attr name (Element _ attrs _) =
  let go !i
        | i >= V.length attrs = Nothing
        | Attribute aname val <- attrs V.! i
        , nameLocal aname == name = Just val
        | otherwise = go (i + 1)
  in go 0
attr _ _ = Nothing

-- | Get all text content (recursive).
textContent :: Node -> Text
textContent (Text t) = t
textContent (CData t) = t
textContent (Element _ _ cs) = T.concat (V.toList (V.map textContent cs))
textContent _ = T.empty

-- | Parse a simple path string: "root/items/item" or "root//item"
parsePath :: Text -> Either String Path
parsePath t
  | T.null t = Left "Empty path"
  | T.isPrefixOf "@" t = Right (AttrPath (simpleName (T.drop 1 t)))
  | otherwise =
      let !parts = splitPath t
      in Right (Sequence (map partToPath parts))
  where
    partToPath ("", name) = Descendant (simpleName name)
    partToPath (_, name) = Child (simpleName name)

splitPath :: Text -> [(Text, Text)]
splitPath t = go t []
  where
    go !s !acc
      | T.null s = reverse acc
      | T.isPrefixOf "//" s =
          let !rest = T.drop 2 s
              !name = T.takeWhile (/= '/') rest
              !remaining = T.drop (T.length name) rest
          in go remaining (("", name) : acc)
      | T.isPrefixOf "/" s =
          let !rest = T.drop 1 s
              !name = T.takeWhile (/= '/') rest
              !remaining = T.drop (T.length name) rest
          in go remaining (("/", name) : acc)
      | otherwise =
          let !name = T.takeWhile (/= '/') s
              !remaining = T.drop (T.length name) s
          in go remaining (("/", name) : acc)
