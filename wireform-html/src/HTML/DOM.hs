{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DOM-style API for parsed HTML documents.
--
-- Provides a zipper-based 'Node' type that supports parent\/sibling
-- navigation over the immutable 'HTMLNode' tree, an incremental parser,
-- CSS selector queries, and HTML serialization.
--
-- The underlying 'HTMLNode' tree is never copied or mutated — 'Node'
-- values carry a list of 'Crumb's (breadcrumbs) that record the path
-- from the root, enabling O(1) parent\/sibling access.
module HTML.DOM
  ( -- * Types
    Document(..)
  , Node
  , NodeType(..)
    -- * Parsing
  , parseDocument
    -- * Incremental parsing
  , Parser
  , newParser
  , feedParser
  , finishParser
    -- * Document access
  , documentDoctype
  , documentElement
    -- * Tree traversal
  , childNodes
  , firstChild
  , lastChild
  , nextSibling
  , prevSibling
  , parentNode
    -- * Node inspection
  , nodeName
  , nodeType
  , rawNode
  , textContent
  , tagName
  , getAttribute
  , getAttributes
  , hasAttribute
  , classList
    -- * Serialization
  , innerHTML
  , outerHTML
  , serialize
  , serializeDocument
    -- * CSS selectors
  , querySelector
  , querySelectorAll
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IORef
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray, indexSmallArray)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import HTML.Encode (buildNode, buildDocument)
import HTML.Parse (parseHTML)
import qualified HTML.Selector as Sel
import HTML.Value
  ( HTMLDocument(..), HTMLNode(..), HTMLAttribute(..), Doctype(..)
  )
import qualified HTML.Value as Value

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | A parsed HTML document.
newtype Document = Document HTMLDocument
  deriving (Show, Eq)

-- | Zipper context: one frame per ancestor on the path from root to current node.
data Crumb = Crumb
  !Text                        -- parent tag
  !(SmallArray HTMLAttribute)  -- parent attributes
  !(SmallArray HTMLNode)       -- parent's children
  {-# UNPACK #-} !Int          -- this node's index among siblings

-- | A node in the document tree with navigation context.
--
-- Two 'Node' values may refer to the same underlying 'HTMLNode'
-- but have different crumbs (i.e. were reached via different paths).
data Node = Node !HTMLNode ![Crumb]

instance Show Node where
  show (Node raw _) = "Node (" ++ show raw ++ ")"

instance Eq Node where
  Node a _ == Node b _ = a == b

data NodeType
  = ElementNode
  | TextNode
  | CommentNode
  | DocumentNode
  | DocumentTypeNode
  | DocumentFragmentNode
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | Parse a complete HTML document from a 'ByteString'.
parseDocument :: ByteString -> Document
parseDocument = Document . parseHTML

-- ---------------------------------------------------------------------------
-- Incremental parsing
-- ---------------------------------------------------------------------------

-- | Opaque incremental parser state.
--
-- Chunks fed via 'feedParser' are accumulated internally.
-- Call 'finishParser' to run the full tokenize+tree-build pipeline and
-- obtain the 'Document'.
--
-- The tokeniser hot path is completely untouched by this API.
-- True streaming tokenisation (processing partial input as it arrives)
-- is a planned future enhancement.
data Parser = Parser
  { _parserBuf :: !(IORef BB.Builder)
  , _parserLen :: !(IORef Int)
  }

-- | Create a fresh incremental parser.
newParser :: IO Parser
newParser = Parser <$> newIORef mempty <*> newIORef 0

-- | Feed a chunk of HTML bytes to the parser.
feedParser :: Parser -> ByteString -> IO ()
feedParser (Parser bRef lRef) chunk = do
  modifyIORef' bRef (<> BB.byteString chunk)
  modifyIORef' lRef (+ BS.length chunk)

-- | Finalize the parser and extract the document.
--
-- After calling this, the 'Parser' should not be reused.
finishParser :: Parser -> IO Document
finishParser (Parser bRef _) = do
  builder <- readIORef bRef
  let !bs = BL.toStrict (BB.toLazyByteString builder)
  pure $! Document (parseHTML bs)

-- ---------------------------------------------------------------------------
-- Document access
-- ---------------------------------------------------------------------------

-- | The document's doctype declaration, if present.
documentDoctype :: Document -> Maybe Doctype
documentDoctype (Document (HTMLDocument mdt _)) = mdt

-- | The root element of the document as a navigable 'Node'.
documentElement :: Document -> Node
documentElement (Document (HTMLDocument _ root)) = Node root []

-- ---------------------------------------------------------------------------
-- Tree traversal
-- ---------------------------------------------------------------------------

-- | All direct children of an element node, each carrying full
-- navigation context. Non-element nodes return @[]@.
childNodes :: Node -> [Node]
childNodes (Node (HTMLElement tag attrs children) crumbs) =
  let !n = sizeofSmallArray children
      go !i
        | i >= n    = []
        | otherwise =
            Node (indexSmallArray children i)
                 (Crumb tag attrs children i : crumbs)
              : go (i + 1)
  in go 0
childNodes _ = []

-- | The first child, if any.
firstChild :: Node -> Maybe Node
firstChild (Node (HTMLElement tag attrs children) crumbs)
  | sizeofSmallArray children > 0 =
      Just $! Node (indexSmallArray children 0)
                   (Crumb tag attrs children 0 : crumbs)
firstChild _ = Nothing

-- | The last child, if any.
lastChild :: Node -> Maybe Node
lastChild (Node (HTMLElement tag attrs children) crumbs)
  | let !n = sizeofSmallArray children, n > 0 =
      let !i = n - 1
      in Just $! Node (indexSmallArray children i)
                      (Crumb tag attrs children i : crumbs)
lastChild _ = Nothing

-- | The next sibling in the parent's child list.
nextSibling :: Node -> Maybe Node
nextSibling (Node _ []) = Nothing
nextSibling (Node _ (Crumb tag attrs children idx : rest)) =
  let !next = idx + 1
  in if next < sizeofSmallArray children
     then Just $! Node (indexSmallArray children next)
                       (Crumb tag attrs children next : rest)
     else Nothing

-- | The previous sibling in the parent's child list.
prevSibling :: Node -> Maybe Node
prevSibling (Node _ []) = Nothing
prevSibling (Node _ (Crumb _ _ _ idx : _))
  | idx <= 0 = Nothing
prevSibling (Node _ (Crumb tag attrs children idx : rest)) =
  let !prev = idx - 1
  in Just $! Node (indexSmallArray children prev)
                  (Crumb tag attrs children prev : rest)

-- | Navigate to the parent node. Returns 'Nothing' at the root.
parentNode :: Node -> Maybe Node
parentNode (Node _ []) = Nothing
parentNode (Node _ (Crumb tag attrs children _ : rest)) =
  Just $! Node (HTMLElement tag attrs children) rest

-- ---------------------------------------------------------------------------
-- Node inspection
-- ---------------------------------------------------------------------------

-- | The node name: tag name for elements, @\"#text\"@ for text, etc.
nodeName :: Node -> Text
nodeName (Node raw _) = case raw of
  HTMLElement tag _ _ -> tag
  HTMLText _          -> "#text"
  HTMLComment _       -> "#comment"
  HTMLDoctype n _ _   -> n

-- | The DOM node type.
nodeType :: Node -> NodeType
nodeType (Node raw _) = case raw of
  HTMLElement {}  -> ElementNode
  HTMLText {}     -> TextNode
  HTMLComment {}  -> CommentNode
  HTMLDoctype {}  -> DocumentTypeNode

-- | The underlying 'HTMLNode'. Useful for pattern matching on the raw tree.
rawNode :: Node -> HTMLNode
rawNode (Node raw _) = raw

-- | Recursive text content (concatenated text of all descendant text nodes).
textContent :: Node -> Text
textContent (Node raw _) = Value.textContent raw

-- | The tag name for element nodes, 'Nothing' otherwise.
tagName :: Node -> Maybe Text
tagName (Node (HTMLElement tag _ _) _) = Just tag
tagName _ = Nothing

-- | Look up an attribute value by name.
getAttribute :: Node -> Text -> Maybe Text
getAttribute (Node raw _) name = Value.getAttr name raw

-- | All attributes of an element node as a list.
getAttributes :: Node -> [HTMLAttribute]
getAttributes (Node (HTMLElement _ attrs _) _) = toList attrs
getAttributes _ = []

-- | Check whether an attribute exists on this node.
hasAttribute :: Node -> Text -> Bool
hasAttribute node name = case getAttribute node name of
  Just _  -> True
  Nothing -> False

-- | Split the @class@ attribute into individual class names.
classList :: Node -> [Text]
classList node = case getAttribute node "class" of
  Nothing  -> []
  Just val -> T.words val

-- ---------------------------------------------------------------------------
-- Serialization
-- ---------------------------------------------------------------------------

-- | Serialize a node and its descendants to an HTML 'BB.Builder'.
serialize :: Node -> BB.Builder
serialize (Node raw _) = buildNode raw

-- | Serialize a full document (including doctype) to an HTML 'BB.Builder'.
serializeDocument :: Document -> BB.Builder
serializeDocument (Document doc) = buildDocument doc

-- | Inner HTML of an element as 'Text' (serialized child nodes).
-- Returns 'T.empty' for non-element nodes.
innerHTML :: Node -> Text
innerHTML (Node (HTMLElement _ _ children) _) =
  TE.decodeUtf8 $! BL.toStrict $! BB.toLazyByteString $!
    foldMap buildNode children
innerHTML _ = T.empty

-- | Outer HTML of a node as 'Text' (the node itself, serialized).
outerHTML :: Node -> Text
outerHTML (Node raw _) =
  TE.decodeUtf8 $! BL.toStrict $! BB.toLazyByteString $! buildNode raw

-- ---------------------------------------------------------------------------
-- CSS selectors (backed by HTML.Selector)
-- ---------------------------------------------------------------------------

-- | Find the first descendant (or self) matching a CSS selector.
--
-- Uses the full CSS selector parser from 'HTML.Selector'.
-- Returns 'Nothing' if the selector is invalid or no match is found.
querySelector :: Node -> Text -> Maybe Node
querySelector root selText =
  case Sel.parseSelector selText of
    Left _  -> Nothing
    Right (Sel.Selector complexSels) ->
      firstJust (\cs -> findFirstComplex cs root) complexSels

-- | Find all descendants (and self) matching a CSS selector.
querySelectorAll :: Node -> Text -> [Node]
querySelectorAll root selText =
  case Sel.parseSelector selText of
    Left _  -> []
    Right (Sel.Selector complexSels) ->
      concatMap (\cs -> collectComplex cs root) complexSels

-- ---------------------------------------------------------------------------
-- Selector matching with zipper context
-- ---------------------------------------------------------------------------

matchesComplex :: Sel.ComplexSelector -> Node -> Bool
matchesComplex cs node =
  let (subject, context) = decomposeComplex cs
  in matchesCompound subject node && matchContext context node

-- | Decompose into (subject_compound, [(combinator, ancestor_compound)])
-- where the list goes outward from subject towards root.
decomposeComplex :: Sel.ComplexSelector -> (Sel.CompoundSelector, [(Sel.Combinator, Sel.CompoundSelector)])
decomposeComplex (Sel.ComplexSelector hd []) = (hd, [])
decomposeComplex (Sel.ComplexSelector hd tl) =
  let combs = map fst tl
      comps = hd : map snd (init tl)
      subject = snd (last tl)
  in (subject, zip (reverse combs) (reverse comps))

matchContext :: [(Sel.Combinator, Sel.CompoundSelector)] -> Node -> Bool
matchContext [] _ = True
matchContext ((Sel.Descendant, comp) : rest) n =
  anyAncestor (\anc -> matchesCompound comp anc && matchContext rest anc) n
matchContext ((Sel.Child, comp) : rest) n =
  case parentNode n of
    Just p  -> matchesCompound comp p && matchContext rest p
    Nothing -> False
matchContext ((Sel.AdjacentSibling, comp) : rest) n =
  case prevSibling n of
    Just ps -> matchesCompound comp ps && matchContext rest ps
    Nothing -> False
matchContext ((Sel.GeneralSibling, comp) : rest) n =
  anyPrevSibling (\sib -> matchesCompound comp sib && matchContext rest sib) n

matchesCompound :: Sel.CompoundSelector -> Node -> Bool
matchesCompound cs (Node raw _) = case raw of
  HTMLElement tag attrs _ -> Sel.matchCompound cs tag attrs
  _                       -> False

anyAncestor :: (Node -> Bool) -> Node -> Bool
anyAncestor f n = case parentNode n of
  Nothing -> False
  Just p  -> f p || anyAncestor f p

anyPrevSibling :: (Node -> Bool) -> Node -> Bool
anyPrevSibling f n = case prevSibling n of
  Nothing -> False
  Just s  -> f s || anyPrevSibling f s

findFirstComplex :: Sel.ComplexSelector -> Node -> Maybe Node
findFirstComplex cs node
  | matchesComplex cs node = Just node
  | otherwise = firstJust (findFirstComplex cs) (childNodes node)

collectComplex :: Sel.ComplexSelector -> Node -> [Node]
collectComplex cs node =
  let self = if matchesComplex cs node then (node :) else id
  in self (concatMap (collectComplex cs) (childNodes node))

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs
