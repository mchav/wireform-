{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
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
    Document
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
  , documentHTML
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
    -- * Construction
  , rootNode
    -- * CSS selectors
  , querySelector
  , querySelectorAll
  , querySelectorAllSel
  , querySelectorAllDoc
  , matches
  , closest
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList, foldl')
import Data.Int (Int32)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Primitive.PrimArray
  ( PrimArray, MutablePrimArray
  , newPrimArray, writePrimArray, unsafeFreezePrimArray, indexPrimArray, sizeofPrimArray
  , shrinkMutablePrimArray
  )
import Data.Primitive.SmallArray
  ( SmallArray, SmallMutableArray
  , sizeofSmallArray, indexSmallArray
  , newSmallArray, writeSmallArray, unsafeFreezeSmallArray
  )
import Data.STRef (newSTRef, readSTRef, writeSTRef)
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

-- | A parsed HTML document with a precomputed element index for fast
-- CSS selector queries.
data Document = Document !HTMLDocument !ElementIndex

instance Show Document where
  show (Document doc _) = "Document " ++ show doc

instance Eq Document where
  Document a _ == Document b _ = a == b

-- | Extract the raw 'HTMLDocument' from a 'Document'.
documentHTML :: Document -> HTMLDocument
documentHTML (Document doc _) = doc

-- | Pre-order element index for O(1) structural pseudo-class evaluation
-- and fast selector dispatch.
data ElementIndex = ElementIndex
  { eiCount    :: {-# UNPACK #-} !Int
  , eiNodes    :: !(SmallArray HTMLNode)
  , eiParent   :: !(PrimArray Int32)    -- parent flat index (-1 for root)
  , eiRawChild :: !(PrimArray Int32)    -- index in parent's SmallArray HTMLNode
  , eiElemPos  :: !(PrimArray Int32)    -- 1-based position among element siblings
  , eiElemCnt  :: !(PrimArray Int32)    -- total element children of parent
  , eiPrevElem :: !(PrimArray Int32)    -- previous element sibling flat index (-1)
  , eiNextElem :: !(PrimArray Int32)    -- next element sibling flat index (-1)
  , eiSubEnd   :: !(PrimArray Int32)    -- exclusive subtree end (first index outside subtree)
  , eiByTag    :: !(Map.Map Text (PrimArray Int32))
  , eiByClass  :: !(Map.Map Text (PrimArray Int32))
  }

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

-- | Wrap a raw 'HTMLNode' as a root 'Node' with no parent context.
rootNode :: HTMLNode -> Node
rootNode raw = Node raw []
{-# INLINE rootNode #-}

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
-- Builds a pre-order element index for fast CSS selector queries.
parseDocument :: ByteString -> Document
parseDocument bs =
  let !doc = parseHTML bs
      !idx = buildElementIndex (htmlRoot doc)
  in Document doc idx
  where
    htmlRoot (HTMLDocument _ root) = root

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
  pure $! parseDocument bs

-- ---------------------------------------------------------------------------
-- Document access
-- ---------------------------------------------------------------------------

-- | The document's doctype declaration, if present.
documentDoctype :: Document -> Maybe Doctype
documentDoctype (Document (HTMLDocument mdt _) _) = mdt

-- | The root element of the document as a navigable 'Node'.
documentElement :: Document -> Node
documentElement (Document (HTMLDocument _ root) _) = Node root []

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
serializeDocument (Document doc _) = buildDocument doc

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
-- Element index building
-- ---------------------------------------------------------------------------

countAllElements :: HTMLNode -> Int
countAllElements (HTMLElement _ _ children) =
  let !n = sizeofSmallArray children
  in 1 + countElemKids children 0 n 0
countAllElements _ = 0

countElemKids :: SmallArray HTMLNode -> Int -> Int -> Int -> Int
countElemKids !children !i !n !acc
  | i >= n = acc
  | otherwise =
      let !child = indexSmallArray children i
      in countElemKids children (i + 1) n (acc + countAllElements child)

countElemChildrenOnly :: SmallArray HTMLNode -> Int -> Int -> Int -> Int
countElemChildrenOnly !children !i !n !acc
  | i >= n = acc
  | HTMLElement {} <- indexSmallArray children i =
      countElemChildrenOnly children (i + 1) n (acc + 1)
  | otherwise = countElemChildrenOnly children (i + 1) n acc

buildElementIndex :: HTMLNode -> ElementIndex
buildElementIndex root = runST $ do
  let !count = countAllElements root
  mNodes   <- newSmallArray count (error "eiNodes: uninitialized")
  mParent  <- newPrimArray count
  mRawIdx  <- newPrimArray count
  mElemPos <- newPrimArray count
  mElemCnt <- newPrimArray count
  mPrevEl  <- newPrimArray count
  mNextEl  <- newPrimArray count
  mSubEnd  <- newPrimArray count
  nextRef  <- newSTRef (0 :: Int)
  tagRef   <- newSTRef (Map.empty :: Map.Map Text [Int])
  clsRef   <- newSTRef (Map.empty :: Map.Map Text [Int])

  let visit node !parentI !rawI !ePos !eCnt = do
        myI <- readSTRef nextRef
        writeSTRef nextRef (myI + 1)
        writeSmallArray mNodes myI node
        writePrimArray mParent myI (fromIntegral parentI)
        writePrimArray mRawIdx myI (fromIntegral rawI)
        writePrimArray mElemPos myI (fromIntegral ePos)
        writePrimArray mElemCnt myI (fromIntegral eCnt)

        case node of
          HTMLElement tag attrs children -> do
            tagMap <- readSTRef tagRef
            writeSTRef tagRef $! Map.insertWith (++) tag [myI] tagMap
            case Sel.findAttr "class" attrs of
              Just cv -> do
                let !ws = T.words cv
                clsMap <- readSTRef clsRef
                writeSTRef clsRef $! foldl' (\m w -> Map.insertWith (++) w [myI] m) clsMap ws
              Nothing -> pure ()
            let !cn = sizeofSmallArray children
                !ec = countElemChildrenOnly children 0 cn 0
            goKids children myI 0 cn 1 ec (-1 :: Int)
            afterI <- readSTRef nextRef
            writePrimArray mSubEnd myI (fromIntegral afterI)
          _ -> writePrimArray mSubEnd myI (fromIntegral (myI + 1))

      goKids !children !parentI !i !n !ePos !eCnt !prevFlat
        | i >= n = when (prevFlat >= 0) $
            writePrimArray mNextEl prevFlat (-1)
        | otherwise =
            let !child = indexSmallArray children i
            in case child of
              HTMLElement {} -> do
                childFlat <- readSTRef nextRef
                writePrimArray mPrevEl childFlat (fromIntegral prevFlat)
                when (prevFlat >= 0) $
                  writePrimArray mNextEl prevFlat (fromIntegral childFlat)
                visit child parentI i ePos eCnt
                goKids children parentI (i + 1) n (ePos + 1) eCnt childFlat
              _ ->
                goKids children parentI (i + 1) n ePos eCnt prevFlat

  writePrimArray mPrevEl 0 (-1 :: Int32)
  writePrimArray mNextEl 0 (-1 :: Int32)
  visit root (-1) 0 1 1

  nodes'   <- unsafeFreezeSmallArray mNodes
  parent'  <- unsafeFreezePrimArray mParent
  rawIdx'  <- unsafeFreezePrimArray mRawIdx
  elemPos' <- unsafeFreezePrimArray mElemPos
  elemCnt' <- unsafeFreezePrimArray mElemCnt
  prevEl'  <- unsafeFreezePrimArray mPrevEl
  nextEl'  <- unsafeFreezePrimArray mNextEl
  subEnd'  <- unsafeFreezePrimArray mSubEnd
  tagMap   <- readSTRef tagRef
  clsMap   <- readSTRef clsRef

  pure $! ElementIndex
    { eiCount    = count
    , eiNodes    = nodes'
    , eiParent   = parent'
    , eiRawChild = rawIdx'
    , eiElemPos  = elemPos'
    , eiElemCnt  = elemCnt'
    , eiPrevElem = prevEl'
    , eiNextElem = nextEl'
    , eiSubEnd   = subEnd'
    , eiByTag    = Map.map listToPrimArray tagMap
    , eiByClass  = Map.map listToPrimArray clsMap
    }

-- Convert a reversed list of Ints to a sorted PrimArray Int32.
listToPrimArray :: [Int] -> PrimArray Int32
listToPrimArray xs = runST $ do
  let !n = length xs
  ma <- newPrimArray n
  go ma (n - 1) xs
  unsafeFreezePrimArray ma
  where
    go _ _ [] = pure ()
    go ma !i (x : rest) = do
      writePrimArray ma i (fromIntegral x)
      go ma (i - 1) rest

-- Reconstruct crumb stack by walking parent pointers.
rebuildCrumbs :: ElementIndex -> Int -> [Crumb]
rebuildCrumbs idx = go []
  where
    go !acc !i =
      let !pi = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
      in if pi < 0
         then acc
         else case indexSmallArray (eiNodes idx) pi of
           HTMLElement ptag pattrs pchildren ->
             let !ri = fromIntegral (indexPrimArray (eiRawChild idx) i) :: Int
             in go (Crumb ptag pattrs pchildren ri : acc) pi
           _ -> acc

-- ---------------------------------------------------------------------------
-- CSS selectors (backed by HTML.Selector)
-- ---------------------------------------------------------------------------

-- | Find the first descendant matching a CSS selector (not self).
--
-- Per the DOM spec, 'querySelector' on an Element searches descendants
-- only. Returns 'Nothing' if the selector is invalid or no match.
querySelector :: Node -> Text -> Maybe Node
querySelector root selText =
  case Sel.parseSelector selText of
    Left _  -> Nothing
    Right sel -> case querySelectorAllSel sel root of
      []    -> Nothing
      (x:_) -> Just x

-- | Find all descendants matching a CSS selector string (not self).
-- Returns results in document order, deduplicated when multiple
-- comma-separated branches match the same node.
querySelectorAll :: Node -> Text -> [Node]
querySelectorAll root selText =
  case Sel.parseSelector selText of
    Left _  -> []
    Right sel -> querySelectorAllSel sel root


-- | Like 'querySelectorAll' but takes a pre-parsed 'Sel.Selector',
-- avoiding repeated parsing in hot loops. Searches descendants only
-- (not self), per the DOM spec for Element.querySelectorAll.
querySelectorAllSel :: Sel.Selector -> Node -> [Node]
querySelectorAllSel sel@(Sel.Selector complexSels) (Node raw crumbs) =
  case raw of
    HTMLElement tag attrs children ->
      forEachChild tag attrs children crumbs 0 (sizeofSmallArray children) []
    _ -> []
  where
    forEachChild !ptag !pattrs !pchildren !pcrumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray pchildren i
              !nextRest = forEachChild ptag pattrs pchildren pcrumbs (i + 1) n rest
          in case child of
               HTMLElement {} ->
                 let !crumbs' = Crumb ptag pattrs pchildren i : pcrumbs
                 in dispatchChild child crumbs' nextRest
               _ -> nextRest
    dispatchChild childRaw childCrumbs rest = case complexSels of
      [cs] -> dispatchSingle cs childRaw childCrumbs rest
      _ | all isFlatSingle complexSels ->
            collectFlatMulti (map extractCompound complexSels) childRaw childCrumbs rest
        | otherwise ->
            collectGeneral sel childRaw childCrumbs rest
    dispatchSingle (Sel.ComplexSelector compound []) r c rest
      | Sel.isFlatCompound compound = collectFlat compound r c rest
    dispatchSingle cs r c rest =
      let !(subject, ctx) = decomposeComplex cs
      in if Sel.isFlatCompound subject && canFastCtx ctx
         then collectFastCtx subject ctx r c rest
         else collectGeneral sel r c rest
    canFastCtx = all pairOk
      where
        pairOk (Sel.AdjacentSibling, _) = False
        pairOk (Sel.GeneralSibling, _) = False
        pairOk (_, comp) = Sel.isFlatCompound comp
    isFlatSingle (Sel.ComplexSelector c []) = Sel.isFlatCompound c
    isFlatSingle _ = False
    extractCompound (Sel.ComplexSelector c _) = c

-- | Like 'querySelectorAllSel' but uses the document's precomputed element
-- index for O(1) structural pseudo-class evaluation and optional tag-index
-- candidate reduction. Returns results in document order.
querySelectorAllDoc :: Sel.Selector -> Document -> [Node]
querySelectorAllDoc sel@(Sel.Selector complexSels) (Document _ idx) =
  case complexSels of
    [cs] -> dispatchIdx cs
    _    -> scanAllIdx sel idx
  where
    dispatchIdx cs =
      let !(subject, ctx) = decomposeComplex cs
      in case ctx of
        [] -> scanCompoundIdx subject idx
        _  -> scanComplexIdx subject ctx idx

-- Intersect two sorted PrimArray Int32s.
intersectSorted :: PrimArray Int32 -> PrimArray Int32 -> PrimArray Int32
intersectSorted a b = runST $ do
  let !na = sizeofPrimArray a
      !nb = sizeofPrimArray b
  out <- newPrimArray (min na nb)
  let go !ia !ib !k
        | ia >= na || ib >= nb = pure k
        | otherwise =
            let !va = indexPrimArray a ia
                !vb = indexPrimArray b ib
            in case compare va vb of
              LT -> go (ia + 1) ib k
              GT -> go ia (ib + 1) k
              EQ -> do
                writePrimArray out k va
                go (ia + 1) (ib + 1) (k + 1)
  !k <- go 0 0 0
  shrinkMutablePrimArray out k
  unsafeFreezePrimArray out

-- Resolve candidate set for a compound selector. When both tag and class
-- indices are available, intersects them so that type + class matching can
-- be skipped per-element.
data CandidateSet
  = PreMatched !(PrimArray Int32)
  | NeedFilter !(PrimArray Int32) !Sel.CompoundSelector

resolveCandidates :: Sel.CompoundSelector -> ElementIndex -> Maybe CandidateSet
resolveCandidates compound idx =
  let mTag = Sel.compoundType compound >>= \t -> Map.lookup t (eiByTag idx)
      mCls = Sel.compoundClass compound >>= \c -> Map.lookup c (eiByClass idx)
  in case (mTag, mCls) of
    (Just tagArr, Just clsArr) ->
      let !inter = intersectSorted tagArr clsArr
      in case Sel.compoundSubsWithoutClass compound of
        Just [] -> Just (PreMatched inter)
        Just rest -> Just (NeedFilter inter (Sel.CompoundSelector Nothing rest))
        Nothing -> Just (NeedFilter inter compound)
    (Just tagArr, Nothing) ->
      case stripType compound of
        Sel.CompoundSelector _ [] -> Just (PreMatched tagArr)
        rest -> Just (NeedFilter tagArr rest)
    (Nothing, Just clsArr) ->
      Just (NeedFilter clsArr (stripClass compound))
    (Nothing, Nothing) -> Nothing
  where
    stripType (Sel.CompoundSelector _ subs) = Sel.CompoundSelector Nothing subs
    stripClass c = case Sel.compoundSubsWithoutClass c of
      Just rest -> Sel.CompoundSelector (Sel.compoundType c >>= \t -> Just (Sel.TypeTag t)) rest
      Nothing -> c
{-# INLINE resolveCandidates #-}

-- Build nodes from an array of known-matching indices (iterate backwards for
-- correct document order without reverse).
buildNodesRev :: ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
buildNodesRev idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
          !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
      in buildNodesRev idx arr (j - 1) (node : acc)

scanCompoundIdx :: Sel.CompoundSelector -> ElementIndex -> [Node]
scanCompoundIdx compound idx =
  case resolveCandidates compound idx of
    Just (PreMatched arr) ->
      buildNodesRev idx arr (sizeofPrimArray arr - 1) []
    Just (NeedFilter arr rest) ->
      filterCandidatesRev rest idx arr (sizeofPrimArray arr - 1) []
    Nothing -> scanAllFwd compound idx 0 (eiCount idx) []

filterCandidatesRev :: Sel.CompoundSelector -> ElementIndex -> PrimArray Int32
                    -> Int -> [Node] -> [Node]
filterCandidatesRev compound idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchesCompoundIdx idx i compound
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterCandidatesRev compound idx arr (j - 1) (node : acc)
         else filterCandidatesRev compound idx arr (j - 1) acc

scanAllFwd :: Sel.CompoundSelector -> ElementIndex -> Int -> Int -> [Node] -> [Node]
scanAllFwd compound idx !i !n acc
  | i >= n = reverse acc
  | matchesCompoundIdx idx i compound =
      let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
      in scanAllFwd compound idx (i + 1) n (node : acc)
  | otherwise = scanAllFwd compound idx (i + 1) n acc

-- Eagerly resolve a CandidateSet to a PrimArray of definite matches.
materializeCandidates :: CandidateSet -> ElementIndex -> PrimArray Int32
materializeCandidates (PreMatched arr) _ = arr
materializeCandidates (NeedFilter arr compound) idx = runST $ do
  let !n = sizeofPrimArray arr
  out <- newPrimArray n
  let go !j !k
        | j >= n = pure k
        | otherwise =
            let !i = fromIntegral (indexPrimArray arr j) :: Int
            in if matchesCompoundIdx idx i compound
               then writePrimArray out k (indexPrimArray arr j) >> go (j + 1) (k + 1)
               else go (j + 1) k
  !k <- go 0 0
  shrinkMutablePrimArray out k
  unsafeFreezePrimArray out

scanComplexIdx :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
               -> ElementIndex -> [Node]
scanComplexIdx subject ctx idx =
  case ctx of
    -- Single descendant combinator: use merge scan
    [(Sel.Descendant, ctxCompound)]
      | Just subCS  <- resolveCandidates subject idx
      , Just ctxCS  <- resolveCandidates ctxCompound idx ->
          let !ctxArr = materializeCandidates ctxCS idx
          in mergeDescendantNodes subCS ctxArr idx
    -- General case: per-element context matching
    _ -> case resolveCandidates subject idx of
      Just (PreMatched arr) ->
        filterContextRev ctx idx arr (sizeofPrimArray arr - 1) []
      Just (NeedFilter arr rest) ->
        filterComplexRev rest ctx idx arr (sizeofPrimArray arr - 1) []
      Nothing -> scanAllComplexFwd subject ctx idx 0 (eiCount idx) []

-- O(m+k) merge scan for descendant combinator. Subject candidates and context
-- candidates are both sorted in document order. Uses eiSubEnd for subtree
-- containment: context c is an ancestor of subject s iff c < s < eiSubEnd[c].
mergeDescendantNodes :: CandidateSet -> PrimArray Int32 -> ElementIndex -> [Node]
mergeDescendantNodes subjectCS contextArr idx =
  let (!subArr, !mCompound) = case subjectCS of
        PreMatched arr   -> (arr, Nothing)
        NeedFilter arr c -> (arr, Just c)
      !nSub = sizeofPrimArray subArr
      !nCtx = sizeofPrimArray contextArr
      !subEnd = eiSubEnd idx
      -- Forward sweep collecting matching indices into a reversed list,
      -- then build Nodes by iterating the reversed list (gives doc order).
      matchedRev = goFwd 0 0 [] []
      goFwd !si !ci !stk !acc
        | si >= nSub = acc
        | otherwise =
            let !s = indexPrimArray subArr si
                -- Push context candidates that come before this subject
                (!ci', !stk') = pushCtx ci stk s
                -- Pop contexts whose subtrees have ended
                !stk'' = popExpired stk' s
            in case stk'' of
              [] -> goFwd (si + 1) ci' stk'' acc
              _  ->
                let !sI = fromIntegral s :: Int
                    keep = case mCompound of
                      Nothing -> True
                      Just c  -> matchesCompoundIdx idx sI c
                in if keep
                   then goFwd (si + 1) ci' stk'' (sI : acc)
                   else goFwd (si + 1) ci' stk''  acc
      pushCtx !ci !stk !s
        | ci >= nCtx = (ci, stk)
        | otherwise =
            let !c = indexPrimArray contextArr ci
            in if c < s
               then pushCtx (ci + 1) ((c, indexPrimArray subEnd (fromIntegral c)) : stk) s
               else (ci, stk)
      popExpired [] _ = []
      popExpired stk@((_, end) : rest) !s
        | end <= s  = popExpired rest s
        | otherwise = stk
  in buildFromRevIndices matchedRev []
  where
    buildFromRevIndices [] acc = acc
    buildFromRevIndices (i : is) acc =
      let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
      in buildFromRevIndices is (node : acc)

-- Subject is pre-matched; only check context combinators.
filterContextRev :: [(Sel.Combinator, Sel.CompoundSelector)]
                 -> ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
filterContextRev ctx idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchContextIdx idx i ctx
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterContextRev ctx idx arr (j - 1) (node : acc)
         else filterContextRev ctx idx arr (j - 1) acc

filterComplexRev :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
                 -> ElementIndex -> PrimArray Int32 -> Int -> [Node] -> [Node]
filterComplexRev subject ctx idx arr !j acc
  | j < 0 = acc
  | otherwise =
      let !i = fromIntegral (indexPrimArray arr j) :: Int
      in if matchesCompoundIdx idx i subject && matchContextIdx idx i ctx
         then let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
              in filterComplexRev subject ctx idx arr (j - 1) (node : acc)
         else filterComplexRev subject ctx idx arr (j - 1) acc

scanAllComplexFwd :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
                  -> ElementIndex -> Int -> Int -> [Node] -> [Node]
scanAllComplexFwd subject ctx idx !i !n acc
  | i >= n = reverse acc
  | matchesCompoundIdx idx i subject && matchContextIdx idx i ctx =
      let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
      in scanAllComplexFwd subject ctx idx (i + 1) n (node : acc)
  | otherwise = scanAllComplexFwd subject ctx idx (i + 1) n acc

-- Full scan fallback for multi-selector (comma-separated).
scanAllIdx :: Sel.Selector -> ElementIndex -> [Node]
scanAllIdx sel idx = go 0 (eiCount idx) []
  where
    go !i !n acc
      | i >= n = reverse acc
      | matchesSelectorIdx sel idx i =
          let !node = Node (indexSmallArray (eiNodes idx) i) (rebuildCrumbs idx i)
          in go (i + 1) n (node : acc)
      | otherwise = go (i + 1) n acc

-- ---------------------------------------------------------------------------
-- Index-based matching
-- ---------------------------------------------------------------------------

matchesSelectorIdx :: Sel.Selector -> ElementIndex -> Int -> Bool
matchesSelectorIdx (Sel.Selector [cs]) idx i = matchesComplexIdx idx i cs
matchesSelectorIdx (Sel.Selector complexSels) idx i =
  any (\cs -> matchesComplexIdx idx i cs) complexSels

matchesComplexIdx :: ElementIndex -> Int -> Sel.ComplexSelector -> Bool
matchesComplexIdx idx i (Sel.ComplexSelector compound []) =
  matchesCompoundIdx idx i compound
matchesComplexIdx idx i cs =
  let !(subject, ctx) = decomposeComplex cs
  in matchesCompoundIdx idx i subject && matchContextIdx idx i ctx
{-# INLINE matchesComplexIdx #-}

matchesCompoundIdx :: ElementIndex -> Int -> Sel.CompoundSelector -> Bool
matchesCompoundIdx idx i (Sel.CompoundSelector mtype subs) =
  case indexSmallArray (eiNodes idx) i of
    HTMLElement tag attrs _ ->
      Sel.matchType mtype tag && allSubsIdx idx i tag attrs subs
    _ -> False
{-# INLINE matchesCompoundIdx #-}

allSubsIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> [Sel.SubSel] -> Bool
allSubsIdx _ _ !_ !_ [] = True
allSubsIdx idx i tag attrs (s : rest) =
  matchSubIdx idx i tag attrs s && allSubsIdx idx i tag attrs rest
{-# INLINE allSubsIdx #-}

matchSubIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubIdx idx i tag attrs = \case
  -- Structural pseudo-classes — O(1) from precomputed index
  Sel.SelFirstChild ->
    indexPrimArray (eiElemPos idx) i == 1
  Sel.SelLastChild ->
    indexPrimArray (eiElemPos idx) i == indexPrimArray (eiElemCnt idx) i
  Sel.SelOnlyChild ->
    indexPrimArray (eiElemCnt idx) i == 1
  Sel.SelNthChild a b ->
    Sel.nthMatch a b (fromIntegral (indexPrimArray (eiElemPos idx) i))
  Sel.SelNthLastChild a b ->
    let !pos = indexPrimArray (eiElemPos idx) i
        !cnt = indexPrimArray (eiElemCnt idx) i
    in Sel.nthMatch a b (fromIntegral (cnt - pos + 1))

  -- Type-based structural pseudos — walk sibling chain in index
  Sel.SelFirstOfType -> typeIndexIdx idx i tag == 1
  Sel.SelLastOfType -> typeIndexFromEndIdx idx i tag == 1
  Sel.SelOnlyOfType -> typeIndexIdx idx i tag == 1 && typeIndexFromEndIdx idx i tag == 1
  Sel.SelNthOfType a b -> Sel.nthMatch a b (typeIndexIdx idx i tag)
  Sel.SelNthLastOfType a b -> Sel.nthMatch a b (typeIndexFromEndIdx idx i tag)

  Sel.SelEmpty ->
    case indexSmallArray (eiNodes idx) i of
      HTMLElement _ _ children -> allCommentsOnly children 0 (sizeofSmallArray children)
      _ -> False
  Sel.SelRoot ->
    indexPrimArray (eiParent idx) i < 0

  -- Logical pseudo-classes
  Sel.SelNot (Sel.Selector sels) ->
    not (any (matchesComplexIdx idx i) sels)
  Sel.SelIs (Sel.Selector sels) ->
    any (matchesComplexIdx idx i) sels
  Sel.SelWhere (Sel.Selector sels) ->
    any (matchesComplexIdx idx i) sels
  Sel.SelHas rels ->
    any (\(comb, cs) -> matchHasRelIdx idx i comb cs) rels

  -- :nth-child(An+B of S) — element must itself match S
  Sel.SelNthChildOf a b sel@(Sel.Selector sels) ->
    any (matchesComplexIdx idx i) sels
    && Sel.nthMatch a b (nthChildOfIdx idx i sel True)
  Sel.SelNthLastChildOf a b sel@(Sel.Selector sels) ->
    any (matchesComplexIdx idx i) sels
    && Sel.nthMatch a b (nthChildOfIdx idx i sel False)

  -- Additional structural
  Sel.SelScope ->
    indexPrimArray (eiParent idx) i < 0
  Sel.SelDefined -> True
  Sel.SelBlank ->
    case indexSmallArray (eiNodes idx) i of
      HTMLElement _ _ children -> allBlankChildren children 0 (sizeofSmallArray children)
      _ -> False
  Sel.SelDir dir' -> matchDirIdx idx i dir'
  Sel.SelTarget -> False

  -- Form/link pseudo-classes (with fieldset disabled inheritance)
  Sel.SelEnabled -> isFormElement tag && not (isActuallyDisabledIdx idx i tag attrs)
  Sel.SelDisabled -> isFormElement tag && isActuallyDisabledIdx idx i tag attrs
  Sel.SelChecked ->
    (tag == "input" && Sel.attrExists "checked" attrs)
    || (tag == "option" && Sel.attrExists "selected" attrs)
  Sel.SelRequired ->
    isFormInputElement tag && Sel.attrExists "required" attrs
  Sel.SelOptional ->
    isFormInputElement tag && not (Sel.attrExists "required" attrs)
  Sel.SelReadOnly ->
    not (isFormInputElement tag)
    || Sel.attrExists "readonly" attrs
    || isActuallyDisabledIdx idx i tag attrs
  Sel.SelReadWrite ->
    isFormInputElement tag
    && not (Sel.attrExists "readonly" attrs)
    && not (isActuallyDisabledIdx idx i tag attrs)
  Sel.SelDefault -> matchDefaultIdx tag attrs
  Sel.SelPlaceholderShown ->
    (tag == "input" || tag == "textarea")
    && Sel.attrExists "placeholder" attrs
    && not (hasNonEmptyValue attrs)
  Sel.SelIndeterminate ->
    tag == "input" && matchIndeterminate attrs
  Sel.SelLink ->
    (tag == "a" || tag == "area" || tag == "link") && Sel.attrExists "href" attrs

  Sel.SelLang langTags -> matchLangIdx idx i langTags

  Sel.SelNeverMatch -> False

  -- Attribute selectors
  s -> Sel.matchSub attrs s

allCommentsOnly :: SmallArray HTMLNode -> Int -> Int -> Bool
allCommentsOnly !children !i !n
  | i >= n = True
  | HTMLComment {} <- indexSmallArray children i = allCommentsOnly children (i + 1) n
  | otherwise = False

-- Walk sibling chain for :nth-of-type
typeIndexIdx :: ElementIndex -> Int -> Text -> Int
typeIndexIdx idx i tag = go 1 (fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int)
  where
    go !n prev
      | prev < 0 = n
      | HTMLElement t _ _ <- indexSmallArray (eiNodes idx) prev
      , t == tag = go (n + 1) (fromIntegral (indexPrimArray (eiPrevElem idx) prev))
      | otherwise = go n (fromIntegral (indexPrimArray (eiPrevElem idx) prev))

typeIndexFromEndIdx :: ElementIndex -> Int -> Text -> Int
typeIndexFromEndIdx idx i tag = go 1 (fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int)
  where
    go !n nxt
      | nxt < 0 = n
      | HTMLElement t _ _ <- indexSmallArray (eiNodes idx) nxt
      , t == tag = go (n + 1) (fromIntegral (indexPrimArray (eiNextElem idx) nxt))
      | otherwise = go n (fromIntegral (indexPrimArray (eiNextElem idx) nxt))

matchContextIdx :: ElementIndex -> Int -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchContextIdx _ _ [] = True
matchContextIdx idx i ((Sel.Descendant, comp) : rest) =
  anyAncestorIdx idx i (\j -> matchesCompoundIdx idx j comp && matchContextIdx idx j rest)
matchContextIdx idx i ((Sel.Child, comp) : rest) =
  let !pi = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
  in pi >= 0 && matchesCompoundIdx idx pi comp && matchContextIdx idx pi rest
matchContextIdx idx i ((Sel.AdjacentSibling, comp) : rest) =
  let !prev = fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int
  in prev >= 0 && matchesCompoundIdx idx prev comp && matchContextIdx idx prev rest
matchContextIdx idx i ((Sel.GeneralSibling, comp) : rest) =
  anyPrevSibIdx idx i (\j -> matchesCompoundIdx idx j comp && matchContextIdx idx j rest)

anyAncestorIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyAncestorIdx idx !i f =
  let !pi = fromIntegral (indexPrimArray (eiParent idx) i) :: Int
  in pi >= 0 && (f pi || anyAncestorIdx idx pi f)

anyPrevSibIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyPrevSibIdx idx !i f =
  let !prev = fromIntegral (indexPrimArray (eiPrevElem idx) i) :: Int
  in prev >= 0 && (f prev || anyPrevSibIdx idx prev f)

anyDescendantIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyDescendantIdx idx !i f = go (i + 1)
  where
    !n = eiCount idx
    go !j
      | j >= n = False
      | not (isDescendantOf idx j i) = False
      | f j = True
      | otherwise = go (j + 1)

-- In pre-order, j is a descendant of i iff j > i and j's ancestor chain
-- includes i. We walk parent pointers (bounded by depth).
isDescendantOf :: ElementIndex -> Int -> Int -> Bool
isDescendantOf idx !j !i = go j
  where
    go !k
      | k <= i = False
      | otherwise =
          let !pk = fromIntegral (indexPrimArray (eiParent idx) k) :: Int
          in pk == i || (pk > i && go pk)

matchLangIdx :: ElementIndex -> Int -> [Text] -> Bool
matchLangIdx idx !i langTags = go i
  where
    go !j
      | j < 0 = False
      | HTMLElement _ attrs _ <- indexSmallArray (eiNodes idx) j
      , Just val <- Sel.findAttr "lang" attrs =
          let !lval = T.toLower val
          in any (\lt -> lval == lt || T.isPrefixOf (lt <> "-") lval) langTags
      | otherwise = go (fromIntegral (indexPrimArray (eiParent idx) j))

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
  case prevElementSibling n of
    Just ps -> matchesCompound comp ps && matchContext rest ps
    Nothing -> False
matchContext ((Sel.GeneralSibling, comp) : rest) n =
  anyPrevElementSibling (\sib -> matchesCompound comp sib && matchContext rest sib) n


prevElementSibling :: Node -> Maybe Node
prevElementSibling n = case prevSibling n of
  Nothing -> Nothing
  Just s | isElement s -> Just s
         | otherwise -> prevElementSibling s

matchesCompound :: Sel.CompoundSelector -> Node -> Bool
matchesCompound (Sel.CompoundSelector mtype subs) node@(Node raw _) = case raw of
  HTMLElement tag attrs _ ->
    Sel.matchType mtype tag && all (matchSubDOM node tag attrs) subs
  _ -> False


matchSubDOM :: Node -> Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubDOM node tag attrs = \case
  -- Attribute selectors delegate to flat matching
  s@(Sel.SelId _) -> Sel.matchSub attrs s
  s@(Sel.SelClass _) -> Sel.matchSub attrs s
  s@(Sel.SelAttrExists _) -> Sel.matchSub attrs s
  s@(Sel.SelAttrExact {}) -> Sel.matchSub attrs s
  s@(Sel.SelAttrPrefix {}) -> Sel.matchSub attrs s
  s@(Sel.SelAttrSuffix {}) -> Sel.matchSub attrs s
  s@(Sel.SelAttrContains {}) -> Sel.matchSub attrs s
  s@(Sel.SelAttrWord {}) -> Sel.matchSub attrs s
  s@(Sel.SelAttrHyphen {}) -> Sel.matchSub attrs s

  -- Structural pseudo-classes
  Sel.SelFirstChild -> isFirstElementChild node
  Sel.SelLastChild -> isLastElementChild node
  Sel.SelOnlyChild -> isFirstElementChild node && isLastElementChild node
  Sel.SelFirstOfType -> isFirstOfType tag node
  Sel.SelLastOfType -> isLastOfType tag node
  Sel.SelOnlyOfType -> isFirstOfType tag node && isLastOfType tag node
  Sel.SelNthChild a b -> Sel.nthMatch a b (elementIndex node)
  Sel.SelNthLastChild a b -> Sel.nthMatch a b (elementIndexFromEnd node)
  Sel.SelNthOfType a b -> Sel.nthMatch a b (typeIndex tag node)
  Sel.SelNthLastOfType a b -> Sel.nthMatch a b (typeIndexFromEnd tag node)
  Sel.SelEmpty -> isEmptyElement node
  Sel.SelRoot -> isRootNode node

  -- Logical pseudo-classes
  Sel.SelNot (Sel.Selector sels) ->
    not (any (\cs -> matchesComplex cs node) sels)
  Sel.SelIs (Sel.Selector sels) ->
    any (\cs -> matchesComplex cs node) sels
  Sel.SelWhere (Sel.Selector sels) ->
    any (\cs -> matchesComplex cs node) sels
  Sel.SelHas rels ->
    any (\(comb, cs) -> matchHasRel node comb cs) rels

  -- :nth-child(An+B of S) — element must itself match S
  Sel.SelNthChildOf a b sel@(Sel.Selector sels) ->
    any (\cs -> matchesComplex cs node) sels
    && Sel.nthMatch a b (nthChildOfNode node sel True)
  Sel.SelNthLastChildOf a b sel@(Sel.Selector sels) ->
    any (\cs -> matchesComplex cs node) sels
    && Sel.nthMatch a b (nthChildOfNode node sel False)

  -- Additional structural
  Sel.SelScope -> isRootNode node
  Sel.SelDefined -> True
  Sel.SelBlank -> isBlankElement node
  Sel.SelDir dir' -> matchDir dir' node
  Sel.SelTarget -> False

  -- Form/link pseudo-classes (with fieldset disabled inheritance)
  Sel.SelEnabled -> isFormElement tag && not (isActuallyDisabledDOM node tag attrs)
  Sel.SelDisabled -> isFormElement tag && isActuallyDisabledDOM node tag attrs
  Sel.SelChecked ->
    (tag == "input" && Sel.attrExists "checked" attrs)
    || (tag == "option" && Sel.attrExists "selected" attrs)
  Sel.SelRequired ->
    isFormInputElement tag && Sel.attrExists "required" attrs
  Sel.SelOptional ->
    isFormInputElement tag && not (Sel.attrExists "required" attrs)
  Sel.SelReadOnly ->
    not (isFormInputElement tag)
    || Sel.attrExists "readonly" attrs
    || isActuallyDisabledDOM node tag attrs
  Sel.SelReadWrite ->
    isFormInputElement tag
    && not (Sel.attrExists "readonly" attrs)
    && not (isActuallyDisabledDOM node tag attrs)
  Sel.SelDefault -> matchDefaultIdx tag attrs
  Sel.SelPlaceholderShown ->
    (tag == "input" || tag == "textarea")
    && Sel.attrExists "placeholder" attrs
    && not (hasNonEmptyValue attrs)
  Sel.SelIndeterminate ->
    tag == "input" && matchIndeterminate attrs
  Sel.SelLink ->
    (tag == "a" || tag == "area" || tag == "link") && Sel.attrExists "href" attrs

  Sel.SelLang langTags -> matchLang langTags node

  Sel.SelNeverMatch -> False


-- Sibling-index helpers (1-based, counting only element nodes)

elementIndex :: Node -> Int
elementIndex n = go 1 n
  where
    go !i node = case prevSibling node of
      Nothing -> i
      Just s -> if isElement s then go (i + 1) s else go i s

elementIndexFromEnd :: Node -> Int
elementIndexFromEnd n = go 1 n
  where
    go !i node = case nextSibling node of
      Nothing -> i
      Just s -> if isElement s then go (i + 1) s else go i s

typeIndex :: Text -> Node -> Int
typeIndex t n = go 1 n
  where
    go !i node = case prevSibling node of
      Nothing -> i
      Just s -> if nodeTagName s == Just t then go (i + 1) s else go i s

typeIndexFromEnd :: Text -> Node -> Int
typeIndexFromEnd t n = go 1 n
  where
    go !i node = case nextSibling node of
      Nothing -> i
      Just s -> if nodeTagName s == Just t then go (i + 1) s else go i s

isFirstElementChild :: Node -> Bool
isFirstElementChild n = case prevSibling n of
  Nothing -> True
  Just s -> not (isElement s) && isFirstElementChild s

isLastElementChild :: Node -> Bool
isLastElementChild n = case nextSibling n of
  Nothing -> True
  Just s -> not (isElement s) && isLastElementChild s

isFirstOfType :: Text -> Node -> Bool
isFirstOfType t n = case prevSibling n of
  Nothing -> True
  Just s -> nodeTagName s /= Just t && isFirstOfType t s

isLastOfType :: Text -> Node -> Bool
isLastOfType t n = case nextSibling n of
  Nothing -> True
  Just s -> nodeTagName s /= Just t && isLastOfType t s

isEmptyElement :: Node -> Bool
isEmptyElement node = all isNotContentChild (childNodes node)
  where
    isNotContentChild (Node (HTMLComment _) _) = True
    isNotContentChild _ = False

isElement :: Node -> Bool
isElement (Node (HTMLElement {}) _) = True
isElement _ = False

nodeTagName :: Node -> Maybe Text
nodeTagName (Node (HTMLElement t _ _) _) = Just t
nodeTagName _ = Nothing

-- | The root element has no element ancestor (its parent, if any, is
-- the implicit document node represented by an empty crumb list or a
-- non-element crumb).
isRootNode :: Node -> Bool
isRootNode (Node _ []) = True
isRootNode (Node _ (Crumb {} : _)) = False

isFormElement :: Text -> Bool
isFormElement t =
  t == "input" || t == "select" || t == "textarea"
  || t == "button" || t == "fieldset"

isFormInputElement :: Text -> Bool
isFormInputElement t = t == "input" || t == "select" || t == "textarea"

-- | Check if a form element is actually disabled, including fieldset
-- inheritance per HTML spec. A form element is disabled if:
-- 1. It has a `disabled` attribute, OR
-- 2. It is a descendant of a disabled `<fieldset>` and NOT inside that
--    fieldset's first `<legend>` child.
isActuallyDisabledIdx :: ElementIndex -> Int -> Text -> SmallArray HTMLAttribute -> Bool
isActuallyDisabledIdx idx i tag attrs
  | tag == "fieldset" = Sel.attrExists "disabled" attrs
  | Sel.attrExists "disabled" attrs = True
  | otherwise = hasDisabledFieldsetAncestorIdx idx i

hasDisabledFieldsetAncestorIdx :: ElementIndex -> Int -> Bool
hasDisabledFieldsetAncestorIdx idx !i = go i
  where
    go !j =
      let !pj = fromIntegral (indexPrimArray (eiParent idx) j) :: Int
      in if pj < 0 then False
         else case indexSmallArray (eiNodes idx) pj of
           HTMLElement "fieldset" pattrs _ ->
             if Sel.attrExists "disabled" pattrs
             then not (isInsideFirstLegendIdx idx j pj)
             else go pj
           HTMLElement {} -> go pj
           _ -> False

-- Check if element j is inside the first <legend> child of fieldset at fsi.
isInsideFirstLegendIdx :: ElementIndex -> Int -> Int -> Bool
isInsideFirstLegendIdx idx !j !fsi = findFirstLegend (fsi + 1)
  where
    !fsEnd = fromIntegral (indexPrimArray (eiSubEnd idx) fsi) :: Int
    findFirstLegend !k
      | k >= fsEnd = False
      | HTMLElement "legend" _ _ <- indexSmallArray (eiNodes idx) k
      , fromIntegral (indexPrimArray (eiParent idx) k) == (fsi :: Int) =
          let !legEnd = fromIntegral (indexPrimArray (eiSubEnd idx) k) :: Int
          in j >= k && j < legEnd
      | HTMLElement {} <- indexSmallArray (eiNodes idx) k
      , fromIntegral (indexPrimArray (eiParent idx) k) == (fsi :: Int) =
          findFirstLegend (fromIntegral (indexPrimArray (eiSubEnd idx) k))
      | otherwise = findFirstLegend (k + 1)

isActuallyDisabledDOM :: Node -> Text -> SmallArray HTMLAttribute -> Bool
isActuallyDisabledDOM _ tag attrs
  | tag == "fieldset" = Sel.attrExists "disabled" attrs
  | Sel.attrExists "disabled" attrs = True
isActuallyDisabledDOM (Node _ crumbs) _ _ = checkFieldsetCrumbs crumbs False

-- Walk crumb stack looking for a disabled fieldset ancestor.
-- The Bool tracks whether the previous hop entered through a <legend>.
checkFieldsetCrumbs :: [Crumb] -> Bool -> Bool
checkFieldsetCrumbs [] _ = False
checkFieldsetCrumbs (Crumb ptag pattrs pchildren _ : rest) wasInLegend
  | ptag == "fieldset" && Sel.attrExists "disabled" pattrs =
      if wasInLegend && isFirstLegendChild pchildren
      then checkFieldsetCrumbs rest False
      else True
  | ptag == "legend" = checkFieldsetCrumbs rest True
  | otherwise = checkFieldsetCrumbs rest False

-- Check if the first element child of this children array is a <legend>.
isFirstLegendChild :: SmallArray HTMLNode -> Bool
isFirstLegendChild children = go 0
  where
    !n = sizeofSmallArray children
    go !i
      | i >= n = False
      | HTMLElement "legend" _ _ <- indexSmallArray children i = True
      | HTMLElement {} <- indexSmallArray children i = False
      | otherwise = go (i + 1)

hasNonEmptyValue :: SmallArray HTMLAttribute -> Bool
hasNonEmptyValue attrs = case Sel.findAttr "value" attrs of
  Just v -> not (T.null v)
  Nothing -> False

-- | :default matches the default button in a form or the first selected option.
matchDefaultIdx :: Text -> SmallArray HTMLAttribute -> Bool
matchDefaultIdx tag attrs
  | tag == "button" || (tag == "input" && isSubmitType attrs) =
      True
  | tag == "option" = Sel.attrExists "selected" attrs
  | tag == "input" && isCheckable attrs = Sel.attrExists "checked" attrs
  | otherwise = False

isSubmitType :: SmallArray HTMLAttribute -> Bool
isSubmitType attrs = case Sel.findAttr "type" attrs of
  Nothing -> True
  Just v -> T.toLower v == "submit"

isCheckable :: SmallArray HTMLAttribute -> Bool
isCheckable attrs = case Sel.findAttr "type" attrs of
  Nothing -> False
  Just v -> let !t = T.toLower v in t == "checkbox" || t == "radio"

-- | :indeterminate matches checkboxes without checked and type=radio without checked
matchIndeterminate :: SmallArray HTMLAttribute -> Bool
matchIndeterminate attrs = case Sel.findAttr "type" attrs of
  Nothing -> False
  Just v ->
    let !t = T.toLower v
    in (t == "checkbox" || t == "radio") && not (Sel.attrExists "checked" attrs)

-- | :blank matches elements with no children or only whitespace text children.
allBlankChildren :: SmallArray HTMLNode -> Int -> Int -> Bool
allBlankChildren !children !i !n
  | i >= n = True
  | HTMLComment {} <- indexSmallArray children i = allBlankChildren children (i + 1) n
  | HTMLText t <- indexSmallArray children i = T.all isWSChar t && allBlankChildren children (i + 1) n
  | otherwise = False

isWSChar :: Char -> Bool
isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'
{-# INLINE isWSChar #-}

isBlankElement :: Node -> Bool
isBlankElement node = all isBlankChild (childNodes node)
  where
    isBlankChild (Node (HTMLComment _) _) = True
    isBlankChild (Node (HTMLText t) _) = T.all isWSChar t
    isBlankChild _ = False

-- | :dir() matches based on the inherited dir attribute (ltr/rtl).
-- Only ltr, rtl, and auto are valid dir values; others are ignored.
matchDirIdx :: ElementIndex -> Int -> Text -> Bool
matchDirIdx idx !i dir' = go i
  where
    go !j
      | j < 0 = dir' == "ltr"
      | HTMLElement _ attrs _ <- indexSmallArray (eiNodes idx) j
      , Just val <- Sel.findAttr "dir" attrs
      , isValidDir val = T.toLower val == dir'
      | otherwise = go (fromIntegral (indexPrimArray (eiParent idx) j))

matchDir :: Text -> Node -> Bool
matchDir dir' node = case getInheritedDir node of
  Nothing -> dir' == "ltr"
  Just val -> T.toLower val == dir'

isValidDir :: Text -> Bool
isValidDir v = let !lv = T.toLower v in lv == "ltr" || lv == "rtl" || lv == "auto"

getInheritedDir :: Node -> Maybe Text
getInheritedDir (Node raw crumbs) = case raw of
  HTMLElement _ attrs _ ->
    case Sel.findAttr "dir" attrs of
      Just v | isValidDir v -> Just v
      _ -> getInheritedDir' crumbs
  _ -> getInheritedDir' crumbs
  where
    getInheritedDir' [] = Nothing
    getInheritedDir' (Crumb _ attrs _ _ : rest) =
      case Sel.findAttr "dir" attrs of
        Just v | isValidDir v -> Just v
        _ -> getInheritedDir' rest

-- | :has() with a relative selector — forward-match from subject.
-- Unlike normal CSS matching (right-to-left), :has() evaluates the
-- relative selector left-to-right starting from the subject element.
matchHasRelIdx :: ElementIndex -> Int -> Sel.Combinator -> Sel.ComplexSelector -> Bool
matchHasRelIdx idx i comb (Sel.ComplexSelector compound chain) =
  hasCandidateIdx idx i comb (\j ->
    matchesCompoundIdx idx j compound && matchHasChainIdx idx j chain)

-- Follow the remaining combinator chain forward from a matched element.
matchHasChainIdx :: ElementIndex -> Int -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchHasChainIdx _ _ [] = True
matchHasChainIdx idx i ((comb, compound) : rest) =
  hasCandidateIdx idx i comb (\j ->
    matchesCompoundIdx idx j compound && matchHasChainIdx idx j rest)

-- Find candidates in the given combinator direction from element i.
hasCandidateIdx :: ElementIndex -> Int -> Sel.Combinator -> (Int -> Bool) -> Bool
hasCandidateIdx idx i comb f = case comb of
  Sel.Descendant -> anyDescendantIdx idx i f
  Sel.Child -> anyChildIdx idx i f
  Sel.AdjacentSibling ->
    let !nxt = fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int
    in nxt >= 0 && f nxt
  Sel.GeneralSibling -> anyNextSiblingIdx idx i f

anyChildIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyChildIdx idx i f =
  let !subEnd = fromIntegral (indexPrimArray (eiSubEnd idx) i) :: Int
  in go (i + 1) subEnd
  where
    go !j !end
      | j >= end = False
      | HTMLElement {} <- indexSmallArray (eiNodes idx) j
      , fromIntegral (indexPrimArray (eiParent idx) j) == (i :: Int) =
          f j || go (fromIntegral (indexPrimArray (eiSubEnd idx) j)) end
      | otherwise = go (j + 1) end

anyNextSiblingIdx :: ElementIndex -> Int -> (Int -> Bool) -> Bool
anyNextSiblingIdx idx i f = go (fromIntegral (indexPrimArray (eiNextElem idx) i) :: Int)
  where
    go !j
      | j < 0 = False
      | f j = True
      | otherwise = go (fromIntegral (indexPrimArray (eiNextElem idx) j))

matchHasRel :: Node -> Sel.Combinator -> Sel.ComplexSelector -> Bool
matchHasRel node comb (Sel.ComplexSelector compound chain) =
  hasCandidateDOM node comb (\n ->
    matchesCompound compound n && matchHasChainDOM n chain)

matchHasChainDOM :: Node -> [(Sel.Combinator, Sel.CompoundSelector)] -> Bool
matchHasChainDOM _ [] = True
matchHasChainDOM node ((comb, compound) : rest) =
  hasCandidateDOM node comb (\n ->
    matchesCompound compound n && matchHasChainDOM n rest)

hasCandidateDOM :: Node -> Sel.Combinator -> (Node -> Bool) -> Bool
hasCandidateDOM node comb f = case comb of
  Sel.Descendant -> anyDescendant f node
  Sel.Child -> any (\c -> isElement c && f c) (childNodes node)
  Sel.AdjacentSibling -> case nextElementSibling node of
    Nothing -> False
    Just s -> f s
  Sel.GeneralSibling -> anyNextElementSibling f node

nextElementSibling :: Node -> Maybe Node
nextElementSibling n = case nextSibling n of
  Nothing -> Nothing
  Just s | isElement s -> Just s
         | otherwise -> nextElementSibling s

anyNextElementSibling :: (Node -> Bool) -> Node -> Bool
anyNextElementSibling f n = case nextSibling n of
  Nothing -> False
  Just s -> (isElement s && f s) || anyNextElementSibling f s

-- | :nth-child(An+B of S) — count only siblings matching the selector.
nthChildOfIdx :: ElementIndex -> Int -> Sel.Selector -> Bool -> Int
nthChildOfIdx idx i (Sel.Selector sels) forward =
  let !prevOrNext = if forward then eiPrevElem idx else eiNextElem idx
  in go 1 (fromIntegral (indexPrimArray prevOrNext i) :: Int)
  where
    go !n !j
      | j < 0 = n
      | HTMLElement {} <- indexSmallArray (eiNodes idx) j
      , any (matchesComplexIdx idx j) sels =
          go (n + 1) (fromIntegral (indexPrimArray prevOrNext j))
      | otherwise =
          go n (fromIntegral (indexPrimArray prevOrNext j))
    prevOrNext = if forward then eiPrevElem idx else eiNextElem idx

nthChildOfNode :: Node -> Sel.Selector -> Bool -> Int
nthChildOfNode n (Sel.Selector sels) forward = go 1 (step n)
  where
    step = if forward then prevSibling else nextSibling
    go !i Nothing = i
    go !i (Just s)
      | isElement s && any (\cs -> matchesComplex cs s) sels = go (i + 1) (step s)
      | otherwise = go i (step s)


-- | :lang() matches when the element or any ancestor has a lang attribute
-- whose value is a case-insensitive match for the given language tag (with
-- hyphen-prefix matching per BCP 47).
matchLang :: [Text] -> Node -> Bool
matchLang langTags node = case getInheritedLang node of
  Nothing -> False
  Just val ->
    let !lval = T.toLower val
    in any (\lt -> lval == lt || T.isPrefixOf (lt <> "-") lval) langTags

getInheritedLang :: Node -> Maybe Text
getInheritedLang (Node raw crumbs) = case raw of
  HTMLElement _ attrs _ ->
    case Sel.findAttr "lang" attrs of
      Just v -> Just v
      Nothing -> getInheritedLang' crumbs
  _ -> getInheritedLang' crumbs
  where
    getInheritedLang' [] = Nothing
    getInheritedLang' (Crumb _ attrs _ _ : rest) =
      case Sel.findAttr "lang" attrs of
        Just v -> Just v
        Nothing -> getInheritedLang' rest

anyDescendant :: (Node -> Bool) -> Node -> Bool
anyDescendant f node = any (\c -> f c || anyDescendant f c) (childNodes node)

anyAncestor :: (Node -> Bool) -> Node -> Bool
anyAncestor f n = case parentNode n of
  Nothing -> False
  Just p  -> f p || anyAncestor f p

anyPrevSibling :: (Node -> Bool) -> Node -> Bool
anyPrevSibling f n = case prevSibling n of
  Nothing -> False
  Just s  -> f s || anyPrevSibling f s

anyPrevElementSibling :: (Node -> Bool) -> Node -> Bool
anyPrevElementSibling f n = case prevSibling n of
  Nothing -> False
  Just s | isElement s -> f s || anyPrevElementSibling f s
         | otherwise -> anyPrevElementSibling f s

findFirstComplex :: Sel.ComplexSelector -> Node -> Maybe Node
findFirstComplex cs node
  | matchesComplex cs node = Just node
  | otherwise = firstJust (findFirstComplex cs) (childNodes node)

matchesSelector :: Sel.Selector -> Node -> Bool
matchesSelector (Sel.Selector [cs]) node = matchesComplex cs node
matchesSelector (Sel.Selector complexSels) node =
  any (\cs -> matchesComplex cs node) complexSels
{-# INLINE matchesSelector #-}

-- | Test whether a node matches a CSS selector string (Element.matches()).
matches :: Node -> Text -> Bool
matches node selText =
  case Sel.parseSelector selText of
    Left _  -> False
    Right sel -> matchesSelector sel node

-- | Return the closest ancestor (or self) matching a CSS selector
-- string (Element.closest()).
closest :: Node -> Text -> Maybe Node
closest node selText =
  case Sel.parseSelector selText of
    Left _  -> Nothing
    Right sel -> go sel node
  where
    go sel n
      | matchesSelector sel n = Just n
      | otherwise = case parentNode n of
          Nothing -> Nothing
          Just p  -> go sel p


-- ---------------------------------------------------------------------------
-- Flat matching (no tree context needed)
-- ---------------------------------------------------------------------------

matchCompoundFlat :: Sel.CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompoundFlat (Sel.CompoundSelector mtype subs) tag attrs =
  Sel.matchType mtype tag && allSubsFlat tag attrs subs
{-# INLINE matchCompoundFlat #-}

allSubsFlat :: Text -> SmallArray HTMLAttribute -> [Sel.SubSel] -> Bool
allSubsFlat !_ !_ [] = True
allSubsFlat tag attrs (s : rest) = matchSubFlat tag attrs s && allSubsFlat tag attrs rest
{-# INLINE allSubsFlat #-}

matchSubFlat :: Text -> SmallArray HTMLAttribute -> Sel.SubSel -> Bool
matchSubFlat _ _ Sel.SelNeverMatch = False
matchSubFlat tag attrs (Sel.SelNot (Sel.Selector sels)) =
  not (any (matchComplexFlat tag attrs) sels)
matchSubFlat tag attrs (Sel.SelIs (Sel.Selector sels)) =
  any (matchComplexFlat tag attrs) sels
matchSubFlat tag attrs (Sel.SelWhere (Sel.Selector sels)) =
  any (matchComplexFlat tag attrs) sels
matchSubFlat tag attrs Sel.SelChecked =
  (tag == "input" && Sel.attrExists "checked" attrs)
  || (tag == "option" && Sel.attrExists "selected" attrs)
matchSubFlat tag attrs Sel.SelRequired =
  isFormInputElement tag && Sel.attrExists "required" attrs
matchSubFlat tag attrs Sel.SelOptional =
  isFormInputElement tag && not (Sel.attrExists "required" attrs)
matchSubFlat tag attrs Sel.SelDefault = matchDefaultIdx tag attrs
matchSubFlat tag attrs Sel.SelPlaceholderShown =
  (tag == "input" || tag == "textarea")
  && Sel.attrExists "placeholder" attrs
  && not (hasNonEmptyValue attrs)
matchSubFlat tag attrs Sel.SelIndeterminate =
  tag == "input" && matchIndeterminate attrs
matchSubFlat tag attrs Sel.SelLink =
  (tag == "a" || tag == "area" || tag == "link") && Sel.attrExists "href" attrs
matchSubFlat _ _ Sel.SelDefined = True
matchSubFlat _ _ Sel.SelTarget = False
matchSubFlat _ attrs s = Sel.matchSub attrs s
{-# INLINE matchSubFlat #-}

matchComplexFlat :: Text -> SmallArray HTMLAttribute -> Sel.ComplexSelector -> Bool
matchComplexFlat tag attrs (Sel.ComplexSelector compound []) =
  matchCompoundFlat compound tag attrs
matchComplexFlat _ _ _ = False
{-# INLINE matchComplexFlat #-}


-- ---------------------------------------------------------------------------
-- Optimized traversals
-- ---------------------------------------------------------------------------

-- Flat traversal for selectors that are a single compound with no
-- combinators and no structural pseudo-classes. Matches tag + attrs
-- directly, only constructing Node for results. Text nodes are skipped
-- entirely (no Crumb allocation).
collectFlat :: Sel.CompoundSelector -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFlat compound = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchCompoundFlat compound tag attrs
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest


-- Flat traversal for multiple flat compound selectors (comma-separated).
collectFlatMulti :: [Sel.CompoundSelector] -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFlatMulti compounds = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if anyMatch tag attrs compounds
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest
    anyMatch !_ !_ [] = False
    anyMatch tag attrs (c:cs) = matchCompoundFlat c tag attrs || anyMatch tag attrs cs


-- Fast-context traversal: subject and ancestor compounds are all flat
-- (no structural pseudos), combinators are Descendant/Child only.
-- Matches subject against tag+attrs, ancestor compounds against crumbs
-- directly — never reconstructing Node for context matching.
collectFastCtx :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)]
               -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectFastCtx subject ctx = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchCompoundFlat subject tag attrs && matchCtxCrumbs ctx crumbs
         then Node raw crumbs : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest

matchCtxCrumbs :: [(Sel.Combinator, Sel.CompoundSelector)] -> [Crumb] -> Bool
matchCtxCrumbs [] _ = True
matchCtxCrumbs _ [] = False
matchCtxCrumbs ((Sel.Descendant, comp) : rest) crumbs = anyCrumb comp rest crumbs
matchCtxCrumbs ((Sel.Child, comp) : rest) (Crumb tag attrs _ _ : parentCrumbs) =
  matchCompoundFlat comp tag attrs && matchCtxCrumbs rest parentCrumbs
matchCtxCrumbs _ _ = False

anyCrumb :: Sel.CompoundSelector -> [(Sel.Combinator, Sel.CompoundSelector)] -> [Crumb] -> Bool
anyCrumb _ _ [] = False
anyCrumb comp rest (Crumb tag attrs _ _ : parentCrumbs) =
  (matchCompoundFlat comp tag attrs && matchCtxCrumbs rest parentCrumbs)
  || anyCrumb comp rest parentCrumbs


-- General traversal using difference lists and direct SmallArray indexing.
-- Used when the selector needs full tree context (structural pseudos,
-- sibling combinators). Text nodes are still skipped.
collectGeneral :: Sel.Selector -> HTMLNode -> [Crumb] -> [Node] -> [Node]
collectGeneral sel = go
  where
    go raw@(HTMLElement tag attrs children) crumbs rest =
      let !node = Node raw crumbs
          !n = sizeofSmallArray children
          kidRest = goKids tag attrs children crumbs 0 n rest
      in if matchesSelector sel node
         then node : kidRest
         else kidRest
    go _ _ rest = rest
    goKids !tag !attrs !children !crumbs !i !n rest
      | i >= n = rest
      | otherwise =
          let !child = indexSmallArray children i
          in case child of
              HTMLElement {} ->
                let !crumbs' = Crumb tag attrs children i : crumbs
                in go child crumbs' (goKids tag attrs children crumbs (i + 1) n rest)
              _ -> goKids tag attrs children crumbs (i + 1) n rest

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just y  -> Just y
  Nothing -> firstJust f xs
