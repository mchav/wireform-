{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

{- | Streaming HTML rewriter (lol-html equivalent).

Processes HTML in a single streaming pass, firing user-registered
callbacks when CSS selectors match, and emitting rewritten output
without ever building a full DOM tree.

The rewriter tracks only a stack of open element names (for
descendant\/child combinators) and a set of active selector automata.
Memory usage is O(nesting depth × number of selectors), not
O(document size).
-}
module HTML.Rewriter (
  -- * Configuration types
  Rewriter,
  ContentType (..),

  -- * Builder DSL
  RewriterBuilder,
  onElement,
  onText,
  onComment,
  onDoctype,
  onEndTag,
  buildRewriter,

  -- * Mutable handles
  ElementRef,
  TextChunkRef,
  CommentRef,
  DoctypeRef,
  EndTagRef,

  -- * Element mutation
  getTagName,
  setTagName,
  getElemAttr,
  setElemAttr,
  removeElemAttr,
  hasElemAttr,
  getElemAttrs,
  beforeElement,
  prependToElement,
  appendToElement,
  afterElement,
  replaceElement,
  removeElement,
  removeChildren,
  setInnerContent,
  onElementEndTag,

  -- * Text chunk mutation
  getTextContent,
  replaceTextChunk,
  beforeTextChunk,
  afterTextChunk,
  removeTextChunk,
  isLastInTextNode,

  -- * Comment mutation
  getCommentText,
  setCommentText,
  replaceComment,
  beforeComment,
  afterComment,
  removeComment,

  -- * End tag mutation
  getEndTagName,
  setEndTagName,
  beforeEndTag,
  afterEndTag,

  -- * Doctype access
  getDoctypeName,
  getDoctypePublicId,
  getDoctypeSystemId,

  -- * Running
  rewrite,
  RewriterState,
  newRewriterState,
  feedRewriter,
  finishRewriter,
  feedRewriter',
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (forM_, unless, when)
import Data.List (foldl')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Builder.Extra qualified as BBE
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Unsafe qualified as BSU
import Data.Maybe (isJust)
import Data.IORef
import Data.Primitive.ByteArray (MutableByteArray (..), newPinnedByteArray, copyMutableByteArray, mutableByteArrayContents)
import Data.Primitive.PrimArray (MutablePrimArray, newPrimArray, readPrimArray, writePrimArray, setPrimArray)
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, indexSmallArray, sizeofSmallArray, runSmallArray, newSmallArray, writeSmallArray, thawSmallArray, copySmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Array.Byte (ByteArray (ByteArray))
import Data.ByteString.Internal (ByteString (BS))
import Data.Text.Internal (Text (..))
import GHC.Exts (Addr#, ByteArray#, Int (..), Int#, RealWorld, copyAddrToByteArray#, copyByteArray#, indexWord8Array#, newByteArray#, plusAddr#, runRW#, unsafeFreezeByteArray#, writeWord8Array#, (+#))
import GHC.ForeignPtr (ForeignPtr (ForeignPtr), ForeignPtrContents (MallocPtr, PlainPtr))
import GHC.Ptr (Ptr (..))
import GHC.IO (IO (..))
import GHC.Word (Word8 (W8#))
import HTML.Parse
  ( Token (..)
  , ScanTextResult (..)
  , decodeTextSlice
  , decodeTextSliceKnown
  , isAlphaByte
  , isWSByte
  , parseEntityRef
  , readByteOff
  , readTagAttrsBS
  , scanClassAndSkip
  , scanTagNameFast
  , scanTextAscii
  , skipTagBS
  , skipToGtBS
  , tokenizeCallbackIOWith
  )
import qualified HTML.Parse as P (isRawTextTag)
import HTML.TagId (TagId (..), internTagAddr, internTagAddrU, fastTagIdAddr, tagIdFromText, tagIdIsVoid)
import HTML.Selector
import HTML.Value (HTMLAttribute (..))


-- ---------------------------------------------------------------------------
-- Content type
-- ---------------------------------------------------------------------------

data ContentType = AsText | AsHTML
  deriving (Show, Eq)


-- ---------------------------------------------------------------------------
-- Mutations – intrusive sum, no extra Maybe box
-- ---------------------------------------------------------------------------

-- | Shared mutation state for before/after/replace/remove.
-- 'MutNone' is the unmodified state; checking is a single pattern match.
data Mutations
  = MutNone
  | Mut !BB.Builder !BB.Builder !(Maybe BB.Builder) !Bool
  -- ^ before, after, replacement (builder), removed
  | MutText !(Maybe BB.Builder) !(Maybe BB.Builder) !Text !ContentType !Bool
  -- ^ before, after, replacement text, content type, removed

-- | Apply f to the before/after/repl/removed fields. Allocates on first use.
withMut :: IORef Mutations -> (BB.Builder -> BB.Builder -> Maybe BB.Builder -> Bool -> Mutations) -> IO ()
withMut ref f = do
  m <- readIORef ref
  case m of
    MutNone -> writeIORef ref (f mempty mempty Nothing False)
    Mut b a r d -> writeIORef ref (f b a r d)
    MutText mb ma _ _ d -> writeIORef ref (f (maybe mempty id mb) (maybe mempty id ma) Nothing d)
{-# INLINE withMut #-}


-- ---------------------------------------------------------------------------
-- Mutable handles
-- ---------------------------------------------------------------------------

-- | Element-specific mutations beyond base before/after/replace/remove.
data ElemMut = ElemMut
  { emTag :: !(Maybe Text)
  , emAttrs :: !(Maybe (SmallArray HTMLAttribute))
  , emNewAttrs :: ![(Text, Text)]
  , emPrepend :: !(Maybe BB.Builder)
  , emAppend :: !(Maybe BB.Builder)
  , emRmChildren :: !Bool
  , emInnerContent :: !(Maybe BB.Builder)
  , emEndTagHandler :: !(Maybe (EndTagRef -> IO ()))
  }

emptyElemMut :: ElemMut
emptyElemMut = ElemMut Nothing Nothing [] Nothing Nothing False Nothing Nothing
{-# INLINE emptyElemMut #-}

-- | Compact representation of element modifications.
-- Common cases (tag rename, new attrs) avoid the 72-byte ElemMut allocation.
data ElemMod
  = EMNone
  | EMTag !Text
  | EMNewAttrs ![(Text, Text)]
  | EMTagAndAttrs !Text ![(Text, Text)]
  | EMFull !ElemMut

elemModToMut :: ElemMod -> Maybe ElemMut
elemModToMut EMNone = Nothing
elemModToMut (EMTag t) = Just (emptyElemMut {emTag = Just t})
elemModToMut (EMNewAttrs as) = Just (emptyElemMut {emNewAttrs = as})
elemModToMut (EMTagAndAttrs t as) = Just (emptyElemMut {emTag = Just t, emNewAttrs = as})
elemModToMut (EMFull m) = Just m
{-# INLINE elemModToMut #-}

elemModTag :: ElemMod -> Maybe Text
elemModTag (EMTag t) = Just t
elemModTag (EMTagAndAttrs t _) = Just t
elemModTag (EMFull m) = emTag m
elemModTag _ = Nothing
{-# INLINE elemModTag #-}

modifyElemMut :: ElementRef -> (ElemMut -> ElemMut) -> IO ()
modifyElemMut er f = do
  m <- readIORef (_erElem er)
  let !base = case m of
        EMNone -> emptyElemMut
        EMTag t -> emptyElemMut {emTag = Just t}
        EMNewAttrs as -> emptyElemMut {emNewAttrs = as}
        EMTagAndAttrs t as -> emptyElemMut {emTag = Just t, emNewAttrs = as}
        EMFull em -> em
  writeIORef (_erElem er) $! EMFull $! f base
{-# INLINE modifyElemMut #-}

data ElementRef = ElementRef
  { _erOrigTag :: !(IORef Text)
  , _erOrigAttrs :: !(IORef (SmallArray HTMLAttribute))
  , _erSelfClose :: !Bool
  , _erMut :: !(IORef Mutations)
  , _erElem :: !(IORef ElemMod)
  , _erInts :: !(MutablePrimArray RealWorld Int)
    -- ^ slot 0 = valid (0/1), slot 1 = attrOff, slot 2 = srcLen
  , _erSharedBA :: !(IORef ByteArray)
  , _erSrcBS :: !(IORef ByteString)
  }


data TextChunkRef = TextChunkRef
  { _trContent :: !(IORef Text)
  , _trMut :: !(IORef Mutations)
  , _trIsLast :: !Bool
  , _trValid :: !(IORef Bool)
  }


data CommentRef = CommentRef
  { _crText :: !(IORef Text)
  , _crMut :: !(IORef Mutations)
  , _crValid :: !(IORef Bool)
  }


data DoctypeRef = DoctypeRef
  { _drName :: !Text
  , _drPubId :: !(Maybe Text)
  , _drSysId :: !(Maybe Text)
  , _drValid :: !(IORef Bool)
  }


data EndTagRef = EndTagRef
  { _etrTag :: !(IORef Text)
  , _etrMut :: !(IORef Mutations)
  , _etrValid :: !(IORef Bool)
  }


data ExpiredRefError = ExpiredRefError !Text
  deriving (Show)


instance Exception ExpiredRefError


checkValidER :: ElementRef -> IO ()
checkValidER er = do
  v <- readPrimArray (_erInts er) 0
  when ((v :: Int) == 0) $ throwIO (ExpiredRefError ("ElementRef used outside its callback scope"))
{-# INLINE checkValidER #-}

checkValid :: IORef Bool -> Text -> IO ()
checkValid ref what = do
  v <- readIORef ref
  when (not v) $ throwIO (ExpiredRefError (what <> " used outside its callback scope"))
{-# INLINE checkValid #-}


-- ---------------------------------------------------------------------------
-- Builder DSL
-- ---------------------------------------------------------------------------

data HandlerEntry
  = HElement !Selector !(ElementRef -> IO ())
  | HText !Selector !(TextChunkRef -> IO ())
  | HEndTag !Selector !(EndTagRef -> IO ())


data GlobalHandlers = GlobalHandlers
  { ghComment :: ![CommentRef -> IO ()]
  , ghDoctype :: ![DoctypeRef -> IO ()]
  }


data RewriterConfig = RewriterConfig
  { rcHandlers :: ![HandlerEntry]
  , rcGlobal :: !GlobalHandlers
  }


newtype RewriterBuilder a = RewriterBuilder
  {unRB :: RewriterConfig -> (a, RewriterConfig)}


instance Functor RewriterBuilder where
  fmap f (RewriterBuilder g) = RewriterBuilder $ \c ->
    let (a, c') = g c in (f a, c')


instance Applicative RewriterBuilder where
  pure a = RewriterBuilder $ \c -> (a, c)
  RewriterBuilder f <*> RewriterBuilder g = RewriterBuilder $ \c ->
    let (fab, c1) = f c
        (a, c2) = g c1
    in (fab a, c2)


instance Monad RewriterBuilder where
  RewriterBuilder m >>= k = RewriterBuilder $ \c ->
    let (a, c1) = m c
    in unRB (k a) c1


onElement :: Selector -> (ElementRef -> IO ()) -> RewriterBuilder ()
onElement sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HElement sel handler]})


onText :: Selector -> (TextChunkRef -> IO ()) -> RewriterBuilder ()
onText sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HText sel handler]})


onComment :: (CommentRef -> IO ()) -> RewriterBuilder ()
onComment handler = RewriterBuilder $ \c ->
  let g = rcGlobal c
  in ((), c {rcGlobal = g {ghComment = ghComment g ++ [handler]}})


onDoctype :: (DoctypeRef -> IO ()) -> RewriterBuilder ()
onDoctype handler = RewriterBuilder $ \c ->
  let g = rcGlobal c
  in ((), c {rcGlobal = g {ghDoctype = ghDoctype g ++ [handler]}})


onEndTag :: Selector -> (EndTagRef -> IO ()) -> RewriterBuilder ()
onEndTag sel handler = RewriterBuilder $ \c ->
  ((), c {rcHandlers = rcHandlers c ++ [HEndTag sel handler]})


-- ---------------------------------------------------------------------------
-- Rewriter (compiled configuration)
-- ---------------------------------------------------------------------------

-- | Pre-decomposed selector: subject + ancestor context chain.
-- Computed once at build time to avoid per-match allocation.
data DecomposedSel = DecomposedSel
  { dsSubject :: !CompoundSelector
  , dsContext :: ![(Combinator, CompoundSelector)]
  }


data CompiledHandler
  = CHElement !(SmallArray DecomposedSel) !(ElementRef -> IO ())
  | CHText !(SmallArray DecomposedSel) !(TextChunkRef -> IO ())
  | CHEndTag !(SmallArray DecomposedSel) !(EndTagRef -> IO ())


data Rewriter = Rewriter
  { rwHandlers :: !(SmallArray CompiledHandler)
  , rwComment :: !(SmallArray (CommentRef -> IO ()))
  , rwDoctype :: !(SmallArray (DoctypeRef -> IO ()))
  , rwHasText :: !Bool
  , rwHasElement :: !Bool
  , rwHasEndTag :: !Bool
  , rwTagFilter :: !(TagId -> Bool)
  , rwContextNeedsAttrs :: !Bool
  , rwNeedsStack :: !Bool
  , rwNeedsContextStack :: !Bool
  , rwTextSelectors :: !(SmallArray DecomposedSel)
  , rwClassOnly :: !Bool
  }


isNoopRewriter :: Rewriter -> Bool
isNoopRewriter rw = sizeofSmallArray (rwHandlers rw) == 0
                 && sizeofSmallArray (rwComment rw) == 0
                 && sizeofSmallArray (rwDoctype rw) == 0
{-# INLINE isNoopRewriter #-}


hasTextHandlers :: Rewriter -> Bool
hasTextHandlers = rwHasText
{-# INLINE hasTextHandlers #-}


hasElementHandlers :: Rewriter -> Bool
hasElementHandlers = rwHasElement
{-# INLINE hasElementHandlers #-}


buildRewriter :: RewriterBuilder () -> Either SelectorError Rewriter
buildRewriter builder =
  let emptyConfig = RewriterConfig [] (GlobalHandlers [] [])
      ((), cfg) = unRB builder emptyConfig
  in compileConfig cfg


compileConfig :: RewriterConfig -> Either SelectorError Rewriter
compileConfig cfg = do
  handlersList <- mapM compileHandler (rcHandlers cfg)
  let !handlers = smallArrayFromList handlersList
      !hasUniversal = anyHasUniversal handlersList
      !tagTids = collectTagIds handlersList
      !tagFilter = if hasUniversal then const True
                   else if null tagTids then const False
                   else \tid -> tid `elem` tagTids
      !ctxAttrs = contextNeedsAttrs handlersList
      !hasText = any isTextHandler handlersList
      !hasEndTag = any isEndTagHandler handlersList
      !hasContext = any handlerHasContext handlersList
      !needsStack = hasText || hasContext || hasEndTag
      !textSels = concatTextSelectors handlersList
  Right
    Rewriter
      { rwHandlers = handlers
      , rwComment = smallArrayFromList (ghComment (rcGlobal cfg))
      , rwDoctype = smallArrayFromList (ghDoctype (rcGlobal cfg))
      , rwHasText = hasText
      , rwHasElement = any isElementHandler handlersList
      , rwHasEndTag = hasEndTag
      , rwTagFilter = tagFilter
      , rwContextNeedsAttrs = ctxAttrs
      , rwNeedsStack = needsStack
      , rwNeedsContextStack = hasContext || hasEndTag
      , rwTextSelectors = textSels
      , rwClassOnly = allClassOnly handlersList textSels && not ctxAttrs
      }
  where
    isTextHandler (CHText _ _) = True
    isTextHandler _ = False
    isElementHandler (CHElement _ _) = True
    isElementHandler _ = False
    isEndTagHandler (CHEndTag _ _) = True
    isEndTagHandler _ = False
    compileHandler (HElement sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHElement (smallArrayFromList (map decomposeComplex cs)) handler)
    compileHandler (HText sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHText (smallArrayFromList (map decomposeComplex cs)) handler)
    compileHandler (HEndTag sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHEndTag (smallArrayFromList (map decomposeComplex cs)) handler)

    allClassOnly :: [CompiledHandler] -> SmallArray DecomposedSel -> Bool
    allClassOnly hs textSelsArr =
      all handlerClassOnly hs && allDecomposedClassOnly textSelsArr
      where
        handlerClassOnly (CHElement ds _) = allDecomposedClassOnly ds
        handlerClassOnly (CHText ds _) = allDecomposedClassOnly ds
        handlerClassOnly (CHEndTag _ _) = False
        allDecomposedClassOnly :: SmallArray DecomposedSel -> Bool
        allDecomposedClassOnly ds = go2 0
          where
            !n = sizeofSmallArray ds
            go2 !i
              | i >= n = True
              | DecomposedSel subj ctx <- indexSmallArray ds i =
                  isClassOnlyCompound subj && null ctx && go2 (i + 1)

    anyHasUniversal :: [CompiledHandler] -> Bool
    anyHasUniversal = any handlerUniversal
      where
        handlerUniversal (CHElement ds _) = any decomposedUniversal ds
        handlerUniversal (CHText ds _) = any decomposedUniversal ds
        handlerUniversal (CHEndTag ds _) = any decomposedUniversal ds
        decomposedUniversal (DecomposedSel subj _) = compoundUniversal subj
        compoundUniversal (CompoundSelector Nothing _) = True
        compoundUniversal (CompoundSelector (Just TypeUniversal) _) = True
        compoundUniversal _ = False

    collectTagIds :: [CompiledHandler] -> [TagId]
    collectTagIds = concatMap handlerTagIds
      where
        handlerTagIds (CHElement ds _) = concatMap decomposedTids ds
        handlerTagIds (CHText ds _) = concatMap decomposedTids ds
        handlerTagIds (CHEndTag ds _) = concatMap decomposedTids ds
        decomposedTids (DecomposedSel subj ctx) =
          compoundTids subj ++ concatMap (compoundTids . snd) ctx
        compoundTids (CompoundSelector (Just (TypeTag t)) _) =
          [tagIdFromText t]
        compoundTids _ = []

    contextNeedsAttrs :: [CompiledHandler] -> Bool
    contextNeedsAttrs = any handlerCtxAttrs
      where
        handlerCtxAttrs (CHElement ds _) = any dsCtxAttrs ds
        handlerCtxAttrs (CHText ds _) = any dsCtxAttrs ds
        handlerCtxAttrs (CHEndTag ds _) = any dsCtxAttrs ds
        dsCtxAttrs (DecomposedSel _ ctx) = any (compoundHasSubs . snd) ctx
        compoundHasSubs (CompoundSelector _ subs) = not (null subs)

    handlerHasContext :: CompiledHandler -> Bool
    handlerHasContext (CHElement ds _) = any dsHasCtx ds
    handlerHasContext (CHText ds _) = any dsHasCtx ds
    handlerHasContext (CHEndTag ds _) = any dsHasCtx ds

    dsHasCtx :: DecomposedSel -> Bool
    dsHasCtx (DecomposedSel _ ctx) = not (null ctx)

    concatTextSelectors :: [CompiledHandler] -> SmallArray DecomposedSel
    concatTextSelectors hs =
      let !totalLen = foldl' (\n h -> case h of CHText ds _ -> n + sizeofSmallArray ds; _ -> n) 0 hs
      in if totalLen == 0
        then emptySmallArray
        else runSmallArray $ do
          ma <- newSmallArray totalLen (error "unreachable")
          let fill !_ [] = pure ()
              fill !off (CHText ds _ : rest) = do
                let !dsLen = sizeofSmallArray ds
                copySmallArray ma off ds 0 dsLen
                fill (off + dsLen) rest
              fill !off (_ : rest) = fill off rest
          fill 0 hs
          pure ma

    validateRewriter sel =
      if isRewriterCompatible sel
        then Right ()
        else Left (UnsupportedSelector "selector uses DOM-only features (pseudo-classes or sibling combinators)")


-- ---------------------------------------------------------------------------
-- Selector automaton state
-- ---------------------------------------------------------------------------

data StackFrame = StackFrame
  { sfTag :: !Text
  , sfAttrs :: !(SmallArray HTMLAttribute)
  , sfDepth :: {-# UNPACK #-} !Int
  , sfTextMatch :: !Bool
  }


data PartialMatch = PartialMatch
  { pmSteps :: ![(Combinator, CompoundSelector)]
  , pmDepth :: !Int
  }


-- | Mutable state for the scanner automaton.
-- asCounters is a MutablePrimArray with 3 Int slots:
--   [0] = depth (element nesting depth)
--   [1] = suppressUntil (-1 = inactive)
--   [2] = removeChildrenUntil (-1 = inactive)
--
-- IORef Int was benchmarked and is WORSE: writeIORef stores thunks for
-- computed values (e.g. d+1), adding ~32 bytes/write, while
-- writePrimArray forces via Prim and GHC eliminates the boxing via
-- case-of-case. Net: PrimArray ~76K vs IORef ~96K for scan-only.
--
-- asTextMask is a depth-indexed array: slot d stores 1 if any text
-- handler has a matching ancestor at or above depth d, 0 otherwise.
data AutoState = AutoState
  { asStack :: !(IORef [StackFrame])
  , asCounters :: !(MutablePrimArray RealWorld Int)
  , asTextMask :: !(MutablePrimArray RealWorld Int)
  , asPartials :: !(IORef [(Int, PartialMatch)])
  , asEndTagHandlers :: !(IORef [(Int, EndTagRef -> IO ())])
  }

{-# INLINE readDepth #-}
readDepth :: AutoState -> IO Int
readDepth st = readPrimArray (asCounters st) 0

{-# INLINE writeDepth #-}
writeDepth :: AutoState -> Int -> IO ()
writeDepth st = writePrimArray (asCounters st) 0

{-# INLINE readSuppressUntil #-}
readSuppressUntil :: AutoState -> IO Int
readSuppressUntil st = readPrimArray (asCounters st) 1

{-# INLINE writeSuppressUntil #-}
writeSuppressUntil :: AutoState -> Int -> IO ()
writeSuppressUntil st = writePrimArray (asCounters st) 1

{-# INLINE readRemoveChildrenUntil #-}
readRemoveChildrenUntil :: AutoState -> IO Int
readRemoveChildrenUntil st = readPrimArray (asCounters st) 2

{-# INLINE writeRemoveChildrenUntil #-}
writeRemoveChildrenUntil :: AutoState -> Int -> IO ()
writeRemoveChildrenUntil st = writePrimArray (asCounters st) 2

{-# INLINE checkSuppressed #-}
checkSuppressed :: Bool -> AutoState -> IO Bool
checkSuppressed False _st = pure False
checkSuppressed True st = do
  suppress <- readPrimArray (asCounters st) 1
  if (suppress :: Int) >= 0 then pure True
  else do
    removeCh <- readPrimArray (asCounters st) 2
    pure ((removeCh :: Int) >= 0)


newAutoState :: IO AutoState
newAutoState = do
  counters <- newPrimArray 3
  writePrimArray counters 0 (0 :: Int)
  writePrimArray counters 1 (-1 :: Int)
  writePrimArray counters 2 (-1 :: Int)
  textMask <- newPrimArray 256
  writePrimArray textMask 0 (0 :: Int)
  AutoState
    <$> newIORef []
    <*> pure counters
    <*> pure textMask
    <*> newIORef []
    <*> newIORef []

{-# INLINE readTextMask #-}
readTextMask :: AutoState -> Int -> IO Int
readTextMask st d = readPrimArray (asTextMask st) d

{-# INLINE writeTextMask #-}
writeTextMask :: AutoState -> Int -> Int -> IO ()
writeTextMask st = writePrimArray (asTextMask st)

{-# INLINE textMaskActive #-}
textMaskActive :: AutoState -> Int -> IO Bool
textMaskActive st d
  | d <= 0 = pure False
  | otherwise = do m <- readPrimArray (asTextMask st) (d - 1); pure (m /= 0)



{- | Check if a complex selector matches at the current position.
The parser stores selectors left-to-right: "div > span" becomes
@ComplexSelector div [(Child, span)]@. CSS matching is right-to-left:
the subject (rightmost compound) must match the current element, then
ancestors are checked working backwards through the chain.
-}
matchAtPosition :: DecomposedSel -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> Bool
matchAtPosition ds stack tag attrs =
  matchCompound (dsSubject ds) tag attrs && matchContext (dsContext ds) stack
{-# INLINE matchAtPosition #-}


{- | Extract the subject (rightmost compound) and the ancestor chain.
"div > span" → DecomposedSel span [(Child, div)]
"a b > c" → DecomposedSel c [(Child, b), (Descendant, a)]
Computed once at build time.
-}
decomposeComplex :: ComplexSelector -> DecomposedSel
decomposeComplex (ComplexSelector hd []) = DecomposedSel hd []
decomposeComplex (ComplexSelector hd tl) =
  let subject = snd (last tl)
      allCompounds = hd : map snd (init tl)
      allCombs = map fst tl
      context = reverse (zip allCombs allCompounds)
  in DecomposedSel subject context


-- | Walk the ancestor chain against the element stack.
matchContext :: [(Combinator, CompoundSelector)] -> [StackFrame] -> Bool
matchContext [] _ = True
matchContext ((Child, comp) : rest) frames =
  case frames of
    (StackFrame t a _ _ : frames') ->
      matchCompound comp t a && matchContext rest frames'
    [] -> False
matchContext ((Descendant, comp) : rest) frames =
  scanAncestors frames
  where
    scanAncestors [] = False
    scanAncestors (StackFrame t a _ _ : frames') =
      (matchCompound comp t a && matchContext rest frames') || scanAncestors frames'
matchContext ((AdjacentSibling, _) : _) _ = False
matchContext ((GeneralSibling, _) : _) _ = False


-- ---------------------------------------------------------------------------
-- Element mutation API
-- ---------------------------------------------------------------------------

getTagName :: ElementRef -> IO Text
getTagName er = do
  checkValidER er
  em <- readIORef (_erElem er)
  case elemModTag em of
    Just t -> pure t
    Nothing -> readIORef (_erOrigTag er)


setTagName :: ElementRef -> Text -> IO ()
setTagName er t = do
  checkValidER er
  em <- readIORef (_erElem er)
  writeIORef (_erElem er) $! case em of
    EMNone -> EMTag t
    EMTag _ -> EMTag t
    EMNewAttrs as -> EMTagAndAttrs t as
    EMTagAndAttrs _ as -> EMTagAndAttrs t as
    EMFull m -> EMFull (m {emTag = Just t})


getElemAttr :: ElementRef -> Text -> IO (Maybe Text)
getElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMNewAttrs pending
      | Just v <- lookupPending name pending -> pure (Just v)
    EMTagAndAttrs _ pending
      | Just v <- lookupPending name pending -> pure (Just v)
    EMFull (ElemMut {emNewAttrs = pending})
      | Just v <- lookupPending name pending -> pure (Just v)
    EMFull (ElemMut {emAttrs = Just attrs}) -> pure $ lookupAttrArr name attrs
    _ -> do
      attrs <- forceAttrs er
      pure $ lookupAttrArr name attrs


setElemAttr :: ElementRef -> Text -> Text -> IO ()
setElemAttr er name val = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull e@(ElemMut {emAttrs = Just attrs}) ->
      writeIORef (_erElem er) $! EMFull $! e {emAttrs = Just (setAttrArr name val attrs)}
    EMFull e@(ElemMut {emNewAttrs = pending}) | not (null pending) ->
      writeIORef (_erElem er) $! EMFull $! e {emNewAttrs = (name, val) : pending}
    _ -> do
      attrOff <- readPrimArray (_erInts er) 1
      if (attrOff :: Int) >= 0
        then do
          srcBS <- readIORef (_erSrcBS er)
          srcLen <- readPrimArray (_erInts er) 2
          if hasAttrNameInTag srcBS attrOff (srcLen :: Int) name
            then do
              origArr <- forceAttrs er
              modifyElemMut er (\e -> e {emAttrs = Just (setAttrArr name val origArr)})
            else do
              writeIORef (_erElem er) $! case em of
                EMNone -> EMNewAttrs [(name, val)]
                EMTag t -> EMTagAndAttrs t [(name, val)]
                EMNewAttrs as -> EMNewAttrs ((name, val) : as)
                EMTagAndAttrs t as -> EMTagAndAttrs t ((name, val) : as)
                EMFull m -> EMFull (m {emNewAttrs = (name, val) : emNewAttrs m})
        else do
          origArr <- forceAttrs er
          modifyElemMut er (\e -> e {emAttrs = Just (setAttrArr name val origArr)})


removeElemAttr :: ElementRef -> Text -> IO ()
removeElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull e@(ElemMut {emAttrs = Just attrs}) ->
      writeIORef (_erElem er) $! EMFull $! e {emAttrs = Just (removeAttrArr name attrs)}
    _ -> do
      origArr <- forceAttrs er
      modifyElemMut er (\e -> e {emAttrs = Just (removeAttrArr name origArr)})


hasElemAttr :: ElementRef -> Text -> IO Bool
hasElemAttr er name = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMNewAttrs pending | any (\(n, _) -> n == name) pending -> pure True
    EMTagAndAttrs _ pending | any (\(n, _) -> n == name) pending -> pure True
    EMFull (ElemMut {emNewAttrs = pending}) | any (\(n, _) -> n == name) pending -> pure True
    EMFull (ElemMut {emAttrs = Just attrs}) -> pure $ hasAttrArr name attrs
    _ -> do
      origArr <- forceAttrs er
      pure $ hasAttrArr name origArr


getElemAttrs :: ElementRef -> IO [(Text, Text)]
getElemAttrs er = do
  checkValidER er
  em <- readIORef (_erElem er)
  case em of
    EMFull (ElemMut {emAttrs = Just a}) -> pure $
      let !sz = sizeofSmallArray a
          go !i acc
            | i < 0 = acc
            | otherwise = let !(HTMLAttribute n v) = indexSmallArray a i in go (i - 1) ((n, v) : acc)
      in go (sz - 1) []
    _ -> do
      origArr <- forceAttrs er
      let !sz = sizeofSmallArray origArr
          go2 !i acc
            | i < 0 = acc
            | otherwise = let !(HTMLAttribute n v) = indexSmallArray origArr i in go2 (i - 1) ((n, v) : acc)
      pure $ go2 (sz - 1) []


beforeElement :: ElementRef -> Text -> ContentType -> IO ()
beforeElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


prependToElement :: ElementRef -> Text -> ContentType -> IO ()
prependToElement er content ct = do
  checkValidER er
  let !b = encodeContent content ct
  modifyElemMut er (\em -> em {emPrepend = Just $ maybe b (<> b) (emPrepend em)})


appendToElement :: ElementRef -> Text -> ContentType -> IO ()
appendToElement er content ct = do
  checkValidER er
  let !b = encodeContent content ct
  modifyElemMut er (\em -> em {emAppend = Just $ maybe b (<> b) (emAppend em)})


afterElement :: ElementRef -> Text -> ContentType -> IO ()
afterElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


replaceElement :: ElementRef -> Text -> ContentType -> IO ()
replaceElement er content ct = do
  checkValidER er
  withMut (_erMut er) (\b a _ _ -> Mut b a (Just (encodeContent content ct)) True)


removeElement :: ElementRef -> IO ()
removeElement er = do
  checkValidER er
  withMut (_erMut er) (\b a r _ -> Mut b a r True)


removeChildren :: ElementRef -> IO ()
removeChildren er = do
  checkValidER er
  modifyElemMut er (\em -> em {emRmChildren = True})


setInnerContent :: ElementRef -> Text -> ContentType -> IO ()
setInnerContent er content ct = do
  checkValidER er
  modifyElemMut er (\em -> em {emInnerContent = Just (encodeContent content ct)})


onElementEndTag :: ElementRef -> (EndTagRef -> IO ()) -> IO ()
onElementEndTag er handler = do
  checkValidER er
  modifyElemMut er (\em -> em {emEndTagHandler = Just handler})


-- ---------------------------------------------------------------------------
-- Text chunk mutation API
-- ---------------------------------------------------------------------------

getTextContent :: TextChunkRef -> IO Text
getTextContent tr = checkValid (_trValid tr) "TextChunkRef" >> readIORef (_trContent tr)


replaceTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
replaceTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  m <- readIORef (_trMut tr)
  case m of
    MutNone -> writeIORef (_trMut tr) $! MutText Nothing Nothing content ct True
    Mut b a _ _ -> writeIORef (_trMut tr) $! MutText (Just b) (Just a) content ct True
    MutText b a _ _ _ -> writeIORef (_trMut tr) $! MutText b a content ct True


beforeTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
beforeTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
afterTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


removeTextChunk :: TextChunkRef -> IO ()
removeTextChunk tr = do
  checkValid (_trValid tr) "TextChunkRef"
  withMut (_trMut tr) (\b a r _ -> Mut b a r True)


isLastInTextNode :: TextChunkRef -> IO Bool
isLastInTextNode tr = checkValid (_trValid tr) "TextChunkRef" >> pure (_trIsLast tr)


-- ---------------------------------------------------------------------------
-- Comment mutation API
-- ---------------------------------------------------------------------------

getCommentText :: CommentRef -> IO Text
getCommentText cr = checkValid (_crValid cr) "CommentRef" >> readIORef (_crText cr)


setCommentText :: CommentRef -> Text -> IO ()
setCommentText cr t = checkValid (_crValid cr) "CommentRef" >> writeIORef (_crText cr) t


replaceComment :: CommentRef -> Text -> ContentType -> IO ()
replaceComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a _ _ -> Mut b a (Just (encodeContent content ct)) True)


beforeComment :: CommentRef -> Text -> ContentType -> IO ()
beforeComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterComment :: CommentRef -> Text -> ContentType -> IO ()
afterComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


removeComment :: CommentRef -> IO ()
removeComment cr = do
  checkValid (_crValid cr) "CommentRef"
  withMut (_crMut cr) (\b a r _ -> Mut b a r True)


-- ---------------------------------------------------------------------------
-- Doctype access
-- ---------------------------------------------------------------------------

getDoctypeName :: DoctypeRef -> IO Text
getDoctypeName dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drName dr)


getDoctypePublicId :: DoctypeRef -> IO (Maybe Text)
getDoctypePublicId dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drPubId dr)


getDoctypeSystemId :: DoctypeRef -> IO (Maybe Text)
getDoctypeSystemId dr = checkValid (_drValid dr) "DoctypeRef" >> pure (_drSysId dr)


-- ---------------------------------------------------------------------------
-- End tag mutation
-- ---------------------------------------------------------------------------

getEndTagName :: EndTagRef -> IO Text
getEndTagName etr = checkValid (_etrValid etr) "EndTagRef" >> readIORef (_etrTag etr)


setEndTagName :: EndTagRef -> Text -> IO ()
setEndTagName etr t = checkValid (_etrValid etr) "EndTagRef" >> writeIORef (_etrTag etr) t


beforeEndTag :: EndTagRef -> Text -> ContentType -> IO ()
beforeEndTag etr content ct = do
  checkValid (_etrValid etr) "EndTagRef"
  withMut (_etrMut etr) (\b a r d -> Mut (b <> encodeContent content ct) a r d)


afterEndTag :: EndTagRef -> Text -> ContentType -> IO ()
afterEndTag etr content ct = do
  checkValid (_etrValid etr) "EndTagRef"
  withMut (_etrMut etr) (\b a r d -> Mut b (a <> encodeContent content ct) r d)


-- ---------------------------------------------------------------------------
-- Running the rewriter
-- ---------------------------------------------------------------------------

data RewriterState = RewriterState
  { rsRewriter :: !Rewriter
  , rsAuto :: !AutoState
  , rsLeftover :: !(IORef ByteString)
  , rsElementPool :: !ElementRef
  , rsTextPool :: !TextChunkRef
  , rsEndTagPool :: !EndTagRef
  , rsCommentPool :: !CommentRef
  }


-- ---------------------------------------------------------------------------
-- COW output: only build output when mutations occur
-- ---------------------------------------------------------------------------

-- | Copy-on-write output buffer. Int fields stored in a MutablePrimArray
-- to avoid IORef thunk allocations on every position update.
--
-- Layout of cowInts: [pos, cap, flushed, dirty]
data CowOutput = CowOutput
  { cowBuf     :: !(IORef (MutableByteArray RealWorld))
  , cowInts    :: !(MutablePrimArray RealWorld Int)
  , cowHint    :: !Int
  }

cowPosIdx, cowCapIdx, cowFlushedIdx, cowDirtyIdx :: Int
cowPosIdx = 0
cowCapIdx = 1
cowFlushedIdx = 2
cowDirtyIdx = 3

{-# INLINE cowReadPos #-}
cowReadPos :: CowOutput -> IO Int
cowReadPos cow = readPrimArray (cowInts cow) cowPosIdx

{-# INLINE cowWritePos #-}
cowWritePos :: CowOutput -> Int -> IO ()
cowWritePos cow !v = writePrimArray (cowInts cow) cowPosIdx v

{-# INLINE cowReadCap #-}
cowReadCap :: CowOutput -> IO Int
cowReadCap cow = readPrimArray (cowInts cow) cowCapIdx

{-# INLINE cowWriteCap #-}
cowWriteCap :: CowOutput -> Int -> IO ()
cowWriteCap cow !v = writePrimArray (cowInts cow) cowCapIdx v

{-# INLINE cowReadFlushed #-}
cowReadFlushed :: CowOutput -> IO Int
cowReadFlushed cow = readPrimArray (cowInts cow) cowFlushedIdx

{-# INLINE cowWriteFlushed #-}
cowWriteFlushed :: CowOutput -> Int -> IO ()
cowWriteFlushed cow !v = writePrimArray (cowInts cow) cowFlushedIdx v

{-# INLINE cowReadDirty #-}
cowReadDirty :: CowOutput -> IO Bool
cowReadDirty cow = do
  v <- readPrimArray (cowInts cow) cowDirtyIdx
  pure (v /= (0 :: Int))

{-# INLINE cowSetDirty #-}
cowSetDirty :: CowOutput -> IO ()
cowSetDirty cow = writePrimArray (cowInts cow) cowDirtyIdx (1 :: Int)

{-# INLINE newCowOutput #-}
newCowOutput :: Int -> IO CowOutput
newCowOutput hint = do
  dummy <- newPinnedByteArray 0
  ints <- newPrimArray 4
  setPrimArray ints 0 4 (0 :: Int)
  CowOutput <$> newIORef dummy <*> pure ints <*> pure hint

cowEnsure :: CowOutput -> Int -> IO ()
cowEnsure cow needed = do
  pos <- cowReadPos cow
  cap <- cowReadCap cow
  when (pos + needed > cap) $ do
    let !newCap
          | cap == 0  = max (cowHint cow) needed
          | otherwise = max (cap + cap `div` 2) (pos + needed)
    oldBuf <- readIORef (cowBuf cow)
    newBuf <- newPinnedByteArray newCap
    when (pos > 0) $ copyMutableByteArray newBuf 0 oldBuf 0 pos
    writeIORef (cowBuf cow) newBuf
    cowWriteCap cow newCap
{-# INLINE cowEnsure #-}

cowWriteBS :: CowOutput -> ByteString -> IO ()
cowWriteBS cow (BS (ForeignPtr bsAddr# _) bsLen) = do
  cowEnsure cow bsLen
  pos <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  copyAddrToMBA buf pos bsAddr# bsLen
  cowWritePos cow (pos + bsLen)
{-# INLINE cowWriteBS #-}

cowWriteSlice :: CowOutput -> ByteString -> Int -> Int -> IO ()
cowWriteSlice cow src from to = do
  let !n = to - from
  when (n > 0) $ do
    cowEnsure cow n
    pos <- cowReadPos cow
    buf <- readIORef (cowBuf cow)
    let !(BS (ForeignPtr srcAddr# _) _) = src
        !(I# from#) = from
    copyAddrToMBA buf pos (srcAddr# `plusAddr#` from#) n
    cowWritePos cow (pos + n)
{-# INLINE cowWriteSlice #-}

cowWriteBuilder :: CowOutput -> BB.Builder -> IO ()
cowWriteBuilder cow builder =
  BL.foldrChunks (\chunk rest -> cowWriteBS cow chunk >> rest) (pure ())
    (BBE.toLazyByteStringWith (BBE.safeStrategy 128 1024) BL.empty builder)
{-# INLINE cowWriteBuilder #-}

copyAddrToMBA :: MutableByteArray RealWorld -> Int -> Addr# -> Int -> IO ()
copyAddrToMBA (MutableByteArray mba#) (I# dstOff#) srcAddr# (I# n#) =
  IO (\s -> case copyAddrToByteArray# srcAddr# mba# dstOff# n# s of s' -> (# s', () #))
{-# INLINE copyAddrToMBA #-}

copyBAToMBA :: MutableByteArray RealWorld -> Int -> ByteArray# -> Int -> Int -> IO ()
copyBAToMBA (MutableByteArray mba#) (I# dstOff#) ba# (I# srcOff#) (I# n#) =
  IO (\s -> case copyByteArray# ba# srcOff# mba# dstOff# n# s of s' -> (# s', () #))
{-# INLINE copyBAToMBA #-}

cowWriteByte :: CowOutput -> Word8 -> IO ()
cowWriteByte cow !b = do
  cowEnsure cow 1
  pos <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  writeBA buf pos b
  cowWritePos cow (pos + 1)
{-# INLINE cowWriteByte #-}

cowWriteTextBytes :: CowOutput -> Text -> IO ()
cowWriteTextBytes cow (Text (ByteArray ba#) off len) = do
  cowEnsure cow len
  pos <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  copyBAToMBA buf pos ba# off len
  cowWritePos cow (pos + len)
{-# INLINE cowWriteTextBytes #-}

cowWriteEndTag :: CowOutput -> Text -> IO ()
cowWriteEndTag cow tag = do
  let !(Text (ByteArray ba#) off len) = tag
      !total = len + 3
  cowEnsure cow total
  pos <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  writeBA buf pos 0x3C   -- '<'
  writeBA buf (pos + 1) 0x2F  -- '/'
  copyBAToMBA buf (pos + 2) ba# off len
  writeBA buf (pos + 2 + len) 0x3E  -- '>'
  cowWritePos cow (pos + total)
{-# INLINE cowWriteEndTag #-}

writeBA :: MutableByteArray RealWorld -> Int -> Word8 -> IO ()
writeBA (MutableByteArray mba#) (I# off#) (W8# w#) =
  IO (\s -> case writeWord8Array# mba# off# w# s of s' -> (# s', () #))
{-# INLINE writeBA #-}

cowWriteStartTag :: CowOutput -> Text -> SmallArray HTMLAttribute -> Bool -> IO ()
cowWriteStartTag cow tag attrs selfClose = do
  let !(Text (ByteArray tagBA#) tagOff tagLen) = tag
      !n = sizeofSmallArray attrs
  cowEnsure cow (tagLen + 3 + n * 40)
  pos0 <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  writeBA buf pos0 0x3C  -- '<'
  copyBAToMBA buf (pos0 + 1) tagBA# tagOff tagLen
  cowWritePos cow (pos0 + 1 + tagLen)
  let go !i
        | i >= n = pure ()
        | otherwise = do
            let !(HTMLAttribute aName aVal) = indexSmallArray attrs i
                !(Text (ByteArray nameBA#) nameOff nameLen) = aName
            cowEnsure cow (4 + nameLen + 64)
            p <- cowReadPos cow
            b <- readIORef (cowBuf cow)
            writeBA b p 0x20      -- ' '
            copyBAToMBA b (p + 1) nameBA# nameOff nameLen
            writeBA b (p + 1 + nameLen) 0x3D  -- '='
            writeBA b (p + 2 + nameLen) 0x22  -- '"'
            cowWritePos cow (p + 3 + nameLen)
            cowEscapeAttrVal cow aVal
            cowWriteByte cow 0x22  -- '"'
            go (i + 1)
  go 0
  if selfClose
    then do
      cowEnsure cow 3
      p <- cowReadPos cow
      b <- readIORef (cowBuf cow)
      writeBA b p 0x20      -- ' '
      writeBA b (p + 1) 0x2F  -- '/'
      writeBA b (p + 2) 0x3E  -- '>'
      cowWritePos cow (p + 3)
    else cowWriteByte cow 0x3E -- '>'
{-# NOINLINE cowWriteStartTag #-}

cowWriteStartTagList :: CowOutput -> Text -> [HTMLAttribute] -> Bool -> IO ()
cowWriteStartTagList cow tag attrs selfClose = do
  let !(Text (ByteArray tagBA#) tagOff tagLen) = tag
  cowEnsure cow (tagLen + 3)
  pos0 <- cowReadPos cow
  buf <- readIORef (cowBuf cow)
  writeBA buf pos0 0x3C  -- '<'
  copyBAToMBA buf (pos0 + 1) tagBA# tagOff tagLen
  cowWritePos cow (pos0 + 1 + tagLen)
  let go [] = pure ()
      go (HTMLAttribute aName aVal : rest) = do
        let !(Text (ByteArray nameBA#) nameOff nameLen) = aName
        cowEnsure cow (4 + nameLen + 64)
        p <- cowReadPos cow
        b <- readIORef (cowBuf cow)
        writeBA b p 0x20      -- ' '
        copyBAToMBA b (p + 1) nameBA# nameOff nameLen
        writeBA b (p + 1 + nameLen) 0x3D  -- '='
        writeBA b (p + 2 + nameLen) 0x22  -- '"'
        cowWritePos cow (p + 3 + nameLen)
        cowEscapeAttrVal cow aVal
        cowWriteByte cow 0x22  -- '"'
        go rest
  go attrs
  if selfClose
    then do
      cowEnsure cow 3
      p <- cowReadPos cow
      b <- readIORef (cowBuf cow)
      writeBA b p 0x20      -- ' '
      writeBA b (p + 1) 0x2F  -- '/'
      writeBA b (p + 2) 0x3E  -- '>'
      cowWritePos cow (p + 3)
    else cowWriteByte cow 0x3E -- '>'
{-# NOINLINE cowWriteStartTagList #-}

cowEscapeAttrVal :: CowOutput -> Text -> IO ()
cowEscapeAttrVal cow (Text (ByteArray ba#) off len) = go off off
  where
    !end = off + len
    flushSeg !segStart !segEnd = do
      let !segLen = segEnd - segStart
      when (segLen > 0) $ do
        cowEnsure cow segLen
        p <- cowReadPos cow
        buf <- readIORef (cowBuf cow)
        copyBAToMBA buf p ba# segStart segLen
        cowWritePos cow (p + segLen)
    go !segStart !i
      | i >= end = flushSeg segStart end
      | otherwise =
          let !b = indexBA ba# i
          in case b of
            0x22 -> flushSeg segStart i >> cowWriteBS cow "&quot;" >> go (i + 1) (i + 1)
            0x26 -> flushSeg segStart i >> cowWriteBS cow "&amp;" >> go (i + 1) (i + 1)
            _    -> go segStart (i + 1)
{-# INLINE cowEscapeAttrVal #-}

cowEscapeText :: CowOutput -> Text -> IO ()
cowEscapeText cow (Text (ByteArray ba#) off len) = go off off
  where
    !end = off + len
    flushSeg !segStart !segEnd = do
      let !segLen = segEnd - segStart
      when (segLen > 0) $ do
        cowEnsure cow segLen
        p <- cowReadPos cow
        buf <- readIORef (cowBuf cow)
        copyBAToMBA buf p ba# segStart segLen
        cowWritePos cow (p + segLen)
    go !segStart !i
      | i >= end = flushSeg segStart end
      | otherwise =
          let !b = indexBA ba# i
          in case b of
            0x3C -> flushSeg segStart i >> cowWriteBS cow "&lt;" >> go (i + 1) (i + 1)
            0x3E -> flushSeg segStart i >> cowWriteBS cow "&gt;" >> go (i + 1) (i + 1)
            0x26 -> flushSeg segStart i >> cowWriteBS cow "&amp;" >> go (i + 1) (i + 1)
            _    -> go segStart (i + 1)
{-# INLINE cowEscapeText #-}

indexBA :: ByteArray# -> Int -> Word8
indexBA ba# (I# i#) = W8# (indexWord8Array# ba# i#)
{-# INLINE indexBA #-}

cowFlushTo :: CowOutput -> ByteString -> Int -> IO ()
cowFlushTo cow src off = do
  flushed <- cowReadFlushed cow
  when (off > flushed) $ cowWriteSlice cow src flushed off
{-# INLINE cowFlushTo #-}

{-# INLINE cowEmitMod #-}
cowEmitMod :: CowOutput -> ByteString -> Int -> Int -> BB.Builder -> IO ()
cowEmitMod cow src startOff endOff builder = do
  flushed <- cowReadFlushed cow
  when (startOff > flushed) $
    cowWriteSlice cow src flushed startOff
  cowWriteBuilder cow builder
  cowWriteFlushed cow endOff
  cowSetDirty cow

{-# INLINE cowEmitModAppend #-}
cowEmitModAppend :: CowOutput -> BB.Builder -> IO ()
cowEmitModAppend cow builder = do
  cowSetDirty cow
  cowWriteBuilder cow builder

{-# INLINE cowSkipTo #-}
cowSkipTo :: CowOutput -> Int -> IO ()
cowSkipTo cow endOff = do
  flushed <- cowReadFlushed cow
  when (endOff > flushed) $ do
    cowWriteFlushed cow endOff
    cowSetDirty cow

{-# INLINE cowFinalize #-}
cowFinalize :: CowOutput -> ByteString -> IO ByteString
cowFinalize cow src = do
  dirty <- cowReadDirty cow
  if not dirty
    then pure src
    else do
      flushed <- cowReadFlushed cow
      let !remaining = BS.length src - flushed
      when (remaining > 0) $
        cowWriteSlice cow src flushed (BS.length src)
      pos <- cowReadPos cow
      buf <- readIORef (cowBuf cow)
      let !(MutableByteArray mba#) = buf
          !(Ptr addr#) = mutableByteArrayContents buf
      pure $! BS (ForeignPtr addr# (PlainPtr mba#)) pos


-- | One-shot: rewrite a complete document.
--
-- Uses COW (copy-on-write) output: if no handler mutates anything,
-- the original ByteString is returned with zero output allocation.
-- Scans input bytes directly, avoiding Token constructor and Text
-- allocation for non-matching content.
rewrite :: Rewriter -> ByteString -> IO ByteString
rewrite rw bs
  | isNoopRewriter rw = pure bs
  | otherwise = do
      cow <- newCowOutput (BS.length bs + 4096)
      let !needsText = rwHasText rw
          !needsContextStack = rwNeedsContextStack rw
      st <- newAutoState
      ePool <- newElementRef "" mempty False
      tPool <- newTextChunkRef "" True
      etPool <- newEndTagRef ""
      cPool <- newCommentRef ""
      sharedBA <- freezeByteStringBA bs
      writeIORef (_erSharedBA ePool) sharedBA
      writeIORef (_erSrcBS ePool) bs
      writePrimArray (_erInts ePool) 2 (BS.length bs)
      let !(BS (ForeignPtr addr# _) _) = bs
          !len = BS.length bs
          !tagFilter = rwTagFilter rw
          !ctxNeedsAttrs = rwContextNeedsAttrs rw
          !needsStack = rwNeedsStack rw
          !textSels = rwTextSelectors rw
          !classOnly = rwClassOnly rw
          !canSuppress = rwHasElement rw

          goScan !off
            | off >= len = pure ()
            | otherwise =
                let !(ScanTextResult end allAscii) = scanTextAscii addr# off len
                in if end > off
                    then do
                      suppress <- readSuppressUntil st
                      if suppress >= 0
                        then cowSkipTo cow end
                        else do
                          removeCh <- readRemoveChildrenUntil st
                          if removeCh >= 0
                            then cowSkipTo cow end
                            else goTextRun off end allAscii
                      goScan end
                    else case readByteOff addr# off of
                      0x3C -> goTag (off + 1) off
                      0x26 -> do
                        let !windowEnd = min len (off + 65)
                            !input = toStringFrom bs (off + 1) windowEnd
                            (ent, rest) = parseEntityRef input
                            !consumed = length input - length rest
                            !entEnd = off + 1 + consumed
                        suppressed <- if canSuppress
                          then do
                            suppressE <- readSuppressUntil st
                            removeChE <- readRemoveChildrenUntil st
                            pure (suppressE >= 0 || removeChE >= 0)
                          else pure False
                        if suppressed
                          then do
                            cowSkipTo cow entEnd
                            goScan entEnd
                          else case ent of
                            [] -> goScan (off + 1)
                            [c] -> do
                              goCharToken off entEnd c
                              goScan entEnd
                            _ -> do
                              mapM_ (\c -> goCharToken off entEnd c) ent
                              goScan entEnd
                      0x00 -> do
                        suppressed0 <- if canSuppress
                          then do
                            suppress0 <- readSuppressUntil st
                            removeCh0 <- readRemoveChildrenUntil st
                            pure (suppress0 >= 0 || removeCh0 >= 0)
                          else pure False
                        if suppressed0
                          then do cowSkipTo cow (off + 1); goScan (off + 1)
                          else do goCharToken off (off + 1) '\0'; goScan (off + 1)
                      0x0D -> do
                        let !next = off + 1
                            !crEnd = if next < len && readByteOff addr# next == 0x0A then next + 1 else next
                        suppressed0 <- if canSuppress
                          then do
                            suppressR <- readSuppressUntil st
                            removeChR <- readRemoveChildrenUntil st
                            pure (suppressR >= 0 || removeChR >= 0)
                          else pure False
                        if suppressed0
                          then do cowSkipTo cow crEnd; goScan crEnd
                          else do goCharToken off crEnd '\n'; goScan crEnd
                      _ -> goScan (off + 1)

          goTextRun !tOff !tEnd !tAscii
            | needsText = do
                d <- readDepth st
                hasMatch <- if needsContextStack
                  then do
                    stk <- readIORef (asStack st)
                    pure (anyTextAncestorMatches stk)
                  else textMaskActive st d
                when hasMatch $ do
                  let !text = decodeTextSliceKnown sharedBA tOff (tEnd - tOff) bs tAscii
                  tr <- resetTextChunkRef tPool text True
                  anyMatched <- if needsContextStack
                    then do stk <- readIORef (asStack st); runTextHandlers rw stk tr
                    else runTextHandlersAll rw tr
                  writeIORef (_trValid tr) False
                  when anyMatched $ do
                    stk <- if needsContextStack then readIORef (asStack st) else pure []
                    emitTextResult cow bs tOff tEnd tr stk
            | otherwise = pure ()

          goTag !off !ltOff
            | off >= len = goCharToken ltOff len '<'
            | otherwise = case readByteOff addr# off of
                0x21 -> goMarkupDecl (off + 1) ltOff
                0x2F -> goEndTag (off + 1) ltOff
                0x3F -> goPI (off + 1) ltOff
                b | isAlphaByte b -> goStartTag off ltOff
                _ -> do
                  goCharToken ltOff (ltOff + 1) '<'
                  goScan off

          goStartTag !off !ltOff = do
            let !nameEnd = scanTagNameFast addr# off len
                !tagLen = nameEnd - off
            let !tid = fastTagIdAddr addr# off tagLen bs
                !matchesSel = tagFilter tid
            let !isVoid = tagIdIsVoid tid
            suppressedST <- checkSuppressed canSuppress st
            if suppressedST
              then case skipTagBS addr# nameEnd len of
                (# selfClose, afterTag #) ->
                  when (afterTag <= len) $ do
                    cowSkipTo cow afterTag
                    when (not selfClose && not isVoid) $ do
                      dIncr <- readDepth st
                      writeDepth st (dIncr + 1)
                    goScan afterTag
              else if not matchesSel && not ctxNeedsAttrs
                    then case skipTagBS addr# nameEnd len of
                      (# selfClose, afterTag #) ->
                        when (afterTag <= len) $ do
                          when (not selfClose && not isVoid) $ do
                            d0 <- readDepth st
                            if needsStack
                              then do
                                let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                                if needsContextStack
                                  then do
                                    stk0 <- readIORef (asStack st)
                                    let !parentTM = case stk0 of (sf : _) -> sfTextMatch sf; [] -> False
                                        !frame = StackFrame lcName emptySmallArray d0 parentTM
                                    writeIORef (asStack st) (frame : stk0)
                                  else do
                                    pm <- if d0 > 0 then readTextMask st (d0 - 1) else pure 0
                                    writeTextMask st d0 pm
                              else pure ()
                            writeDepth st (d0 + 1)
                          if P.isRawTextTag tid && not selfClose
                            then do
                              let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                              goRawText lcName afterTag
                            else goScan afterTag
                  else if matchesSel && classOnly && not ctxNeedsAttrs
                    then let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                    in case scanClassAndSkip addr# nameEnd len of
                      (# classOff, classLen, selfClose, afterTag #) ->
                        when (afterTag <= len) $ do
                          let !selfTM = matchAnyDecomposedClass textSels lcName addr# classOff classLen
                          resetElementRefDeferred ePool lcName selfClose nameEnd
                          anyMatched <- runElementHandlersClass rw lcName addr# classOff classLen ePool
                          writePrimArray (_erInts ePool) 0 (0 :: Int)
                          if not anyMatched
                            then do
                              when (not selfClose && not isVoid) $ do
                                d1 <- readDepth st
                                when needsStack $ do
                                  pm <- if d1 > 0 then readTextMask st (d1 - 1) else pure 0
                                  let !mask = if pm /= 0 || selfTM then 1 else 0
                                  writeTextMask st d1 mask
                                writeDepth st (d1 + 1)
                              if P.isRawTextTag tid && not selfClose
                                then goRawText lcName afterTag
                                else goScan afterTag
                            else do
                              mut <- readIORef (_erMut ePool)
                              mElem <- readIORef (_erElem ePool)
                              let !isDirty = case mut of MutNone -> False; _ -> True
                                  !isElemDirty = case mElem of EMNone -> False; _ -> True
                              if not isDirty && not isElemDirty
                                then do
                                  when (not selfClose && not isVoid) $ do
                                    d2 <- readDepth st
                                    when needsStack $ do
                                      pm <- if d2 > 0 then readTextMask st (d2 - 1) else pure 0
                                      let !mask = if pm /= 0 || selfTM then 1 else 0
                                      writeTextMask st d2 mask
                                    writeDepth st (d2 + 1)
                                  if P.isRawTextTag tid && not selfClose
                                    then goRawText lcName afterTag
                                    else goScan afterTag
                                else case (mut, mElem) of
                                  (MutNone, EMTag tag') -> do
                                    cowFlushTo cow bs ltOff
                                    cowWriteByte cow 0x3C
                                    cowWriteTextBytes cow tag'
                                    cowWriteFlushed cow nameEnd
                                    cowSetDirty cow
                                    when (not selfClose && not isVoid) $ do
                                      depth <- readDepth st
                                      when needsStack $ do
                                        pm <- if depth > 0 then readTextMask st (depth - 1) else pure 0
                                        let !mask = if pm /= 0 || selfTM then 1 else 0
                                        writeTextMask st depth mask
                                      ehs0 <- readIORef (asEndTagHandlers st)
                                      let deferredRename etr = writeIORef (_etrTag etr) tag'
                                      writeIORef (asEndTagHandlers st) ((depth, deferredRename) : ehs0)
                                      writeDepth st (depth + 1)
                                    if P.isRawTextTag tid && not selfClose
                                      then goRawText lcName afterTag
                                      else goScan afterTag
                                  (MutNone, EMNewAttrs newAs) -> do
                                    let !gtPos = if selfClose then afterTag - 2 else afterTag - 1
                                    cowFlushTo cow bs gtPos
                                    let emitNew [] = pure ()
                                        emitNew ((n, v) : rest) = emitNew rest >> cowWriteOneAttr cow n v
                                    emitNew newAs
                                    if selfClose
                                      then do cowWriteByte cow 0x2F; cowWriteByte cow 0x3E
                                      else cowWriteByte cow 0x3E
                                    cowWriteFlushed cow afterTag
                                    cowSetDirty cow
                                    when (not selfClose && not isVoid) $ do
                                      depth <- readDepth st
                                      when needsStack $ do
                                        pm <- if depth > 0 then readTextMask st (depth - 1) else pure 0
                                        let !mask = if pm /= 0 || selfTM then 1 else 0
                                        writeTextMask st depth mask
                                      writeDepth st (depth + 1)
                                    if P.isRawTextTag tid && not selfClose
                                      then goRawText lcName afterTag
                                      else goScan afterTag
                                  (MutNone, EMTagAndAttrs tag' newAs) -> do
                                    cowFlushTo cow bs ltOff
                                    cowWriteByte cow 0x3C
                                    cowWriteTextBytes cow tag'
                                    let !(Text _ _ lcNameLen) = lcName
                                        !attrStart = ltOff + 1 + lcNameLen
                                        !gtPos = if selfClose then afterTag - 2 else afterTag - 1
                                    cowWriteSlice cow bs attrStart gtPos
                                    let emitNew [] = pure ()
                                        emitNew ((n, v) : rest) = emitNew rest >> cowWriteOneAttr cow n v
                                    emitNew newAs
                                    if selfClose
                                      then do cowWriteByte cow 0x2F; cowWriteByte cow 0x3E
                                      else cowWriteByte cow 0x3E
                                    cowWriteFlushed cow afterTag
                                    cowSetDirty cow
                                    when (not selfClose && not isVoid) $ do
                                      depth <- readDepth st
                                      when needsStack $ do
                                        pm <- if depth > 0 then readTextMask st (depth - 1) else pure 0
                                        let !mask = if pm /= 0 || selfTM then 1 else 0
                                        writeTextMask st depth mask
                                      ehs0 <- readIORef (asEndTagHandlers st)
                                      let deferredRename etr = writeIORef (_etrTag etr) tag'
                                      writeIORef (asEndTagHandlers st) ((depth, deferredRename) : ehs0)
                                      writeDepth st (depth + 1)
                                    if P.isRawTextTag tid && not selfClose
                                      then goRawText lcName afterTag
                                      else goScan afterTag
                                  _ -> do
                                    attrs <- forceAttrs ePool
                                    emitModifiedStartTag cow bs rw ePool lcName attrs selfClose isVoid ltOff afterTag mut mElem st needsContextStack textSels
                                    if P.isRawTextTag tid && not selfClose
                                      then goRawText lcName afterTag
                                      else goScan afterTag
                    else do
                      let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                      let (!attrs, !selfClose, !afterTag) = readTagAttrsBS sharedBA bs nameEnd len
                      when (afterTag <= len) $ if not matchesSel
                        then do
                          when (not selfClose && not isVoid) $ do
                            d0 <- readDepth st
                            when needsStack $
                              if needsContextStack
                                then do
                                  stk0 <- readIORef (asStack st)
                                  let !parentTM = case stk0 of (sf : _) -> sfTextMatch sf; [] -> False
                                      !selfTM = matchAnyDecomposed textSels stk0 lcName attrs
                                      !textMatch = parentTM || selfTM
                                      !frame = StackFrame lcName attrs d0 textMatch
                                  writeIORef (asStack st) (frame : stk0)
                                else do
                                  pm <- if d0 > 0 then readTextMask st (d0 - 1) else pure 0
                                  let !selfTM = matchAnyDecomposed textSels [] lcName attrs
                                      !mask = if pm /= 0 || selfTM then 1 else 0
                                  writeTextMask st d0 mask
                            writeDepth st (d0 + 1)
                          if P.isRawTextTag tid && not selfClose
                            then goRawText lcName afterTag
                            else goScan afterTag
                        else do
                          stack <- if needsContextStack then readIORef (asStack st) else pure []
                          let !parentTM = case stack of (sf : _) -> sfTextMatch sf; [] -> False
                              !selfTM = matchAnyDecomposed textSels stack lcName attrs
                              !textMatch = parentTM || selfTM
                          resetElementRef ePool lcName attrs selfClose
                          anyMatched <- runElementHandlers rw stack lcName attrs ePool
                          writePrimArray (_erInts ePool) 0 (0 :: Int)
                          if not anyMatched
                            then do
                              when (not selfClose && not isVoid) $ do
                                d1 <- readDepth st
                                when needsStack $
                                  if needsContextStack
                                    then do
                                      let !frame = StackFrame lcName attrs d1 textMatch
                                      writeIORef (asStack st) (frame : stack)
                                    else do
                                      pm <- if d1 > 0 then readTextMask st (d1 - 1) else pure 0
                                      let !mask = if pm /= 0 || selfTM then 1 else 0
                                      writeTextMask st d1 mask
                                writeDepth st (d1 + 1)
                              if P.isRawTextTag tid && not selfClose
                                then goRawText lcName afterTag
                                else goScan afterTag
                            else do
                              mut <- readIORef (_erMut ePool)
                              mElem <- readIORef (_erElem ePool)
                              let !isDirty = case mut of MutNone -> False; _ -> True
                                  !isElemDirty = case mElem of EMNone -> False; _ -> True
                              if not isDirty && not isElemDirty
                                then do
                                  when (not selfClose && not isVoid) $ do
                                    d2 <- readDepth st
                                    when needsStack $
                                      if needsContextStack
                                        then do
                                          let !frame = StackFrame lcName attrs d2 textMatch
                                          writeIORef (asStack st) (frame : stack)
                                        else do
                                          pm <- if d2 > 0 then readTextMask st (d2 - 1) else pure 0
                                          let !mask = if pm /= 0 || selfTM then 1 else 0
                                          writeTextMask st d2 mask
                                    writeDepth st (d2 + 1)
                                  if P.isRawTextTag tid && not selfClose
                                    then goRawText lcName afterTag
                                    else goScan afterTag
                                else do
                                  emitModifiedStartTag cow bs rw ePool lcName attrs selfClose isVoid ltOff afterTag mut mElem st needsContextStack textSels
                                  if P.isRawTextTag tid && not selfClose
                                    then goRawText lcName afterTag
                                    else goScan afterTag

          goEndTag !off !ltOff
            | off >= len = do
              goCharToken ltOff (ltOff + 1) '<'
              goCharToken (ltOff + 1) (ltOff + 2) '/'
            | isAlphaByte (readByteOff addr# off) = do
              let !nameEnd = scanTagNameFast addr# off len
                  !tagLen = nameEnd - off
                  !afterGt = skipToGtBS bs nameEnd len
              when (nameEnd < len) $
                if canSuppress then do
                  suppressET <- readSuppressUntil st
                  if suppressET >= 0
                    then do
                      let !suppDepth = suppressET
                      dET <- readDepth st
                      let !newD = dET - 1
                      cowSkipTo cow afterGt
                      if newD <= suppDepth
                        then do
                          writeSuppressUntil st (-1)
                          writeDepth st newD
                          when (needsStack && needsContextStack) $ do
                            stkET <- readIORef (asStack st)
                            writeIORef (asStack st) (case stkET of (_ : xs) -> xs; [] -> [])
                        else writeDepth st newD
                      goScan afterGt
                    else do
                      removeChET <- readRemoveChildrenUntil st
                      if removeChET >= 0
                        then do
                          let !rcDepth = removeChET
                          dRC <- readDepth st
                          let !newD = dRC - 1
                          if newD <= rcDepth
                            then do
                              cowSkipTo cow afterGt
                              writeRemoveChildrenUntil st (-1)
                              let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                              runEndTagFull cow bs st rw etPool lcName ltOff afterGt
                              goScan afterGt
                            else do
                              cowSkipTo cow afterGt
                              writeDepth st newD
                              goScan afterGt
                        else goEndTagDispatch off tagLen ltOff afterGt
                else goEndTagDispatch off tagLen ltOff afterGt
            | readByteOff addr# off == 0x3E = goScan (off + 1)
            | otherwise = goScan (off + 1)

          goEndTagDispatch !off !tagLen !ltOff !afterGt = do
            d <- readDepth st
            let !newD = d - 1
            ehs <- readIORef (asEndTagHandlers st)
            let !noEndTagSel = not (rwHasEndTag rw)
                !canSkip = case ehs of
                  [] -> noEndTagSel
                  ((ehDepth, _) : _) -> ehDepth < newD && noEndTagSel
            if canSkip
              then do
                when (needsStack && needsContextStack) $ do
                  stk <- readIORef (asStack st)
                  writeIORef (asStack st) (case stk of (_ : xs) -> xs; [] -> [])
                writeDepth st newD
                goScan afterGt
              else do
                let !(# lcName, _ #) = internTagAddrU addr# off tagLen bs
                runEndTagFull cow bs st rw etPool lcName ltOff afterGt
                goScan afterGt

          goCharToken !startOff !endOff !c = do
            suppressedCT <- checkSuppressed canSuppress st
            if suppressedCT
              then pure ()
              else
                if needsText
                      then do
                        d <- readDepth st
                        hasMatch <- if needsContextStack
                          then do stk <- readIORef (asStack st); pure (anyTextAncestorMatches stk)
                          else textMaskActive st d
                        when hasMatch $ do
                          let !text = T.singleton c
                          tr <- resetTextChunkRef tPool text True
                          anyMatched <- if needsContextStack
                            then do stk <- readIORef (asStack st); runTextHandlers rw stk tr
                            else runTextHandlersAll rw tr
                          writeIORef (_trValid tr) False
                          when anyMatched $ do
                            stk <- if needsContextStack then readIORef (asStack st) else pure []
                            emitTextResult cow bs startOff endOff tr stk
                      else pure ()

          goMarkupDecl !off !ltOff
            | off >= len = pure ()
            | readByteOff addr# off == 0x2D && off + 1 < len && readByteOff addr# (off + 1) == 0x2D =
                goComment (off + 2) ltOff
            | otherwise = do
                let !afterGt = skipToGtBS bs off len
                when (afterGt <= len) $ do
                  suppressedMD <- checkSuppressed canSuppress st
                  unless suppressedMD $ do
                    let !nameBS = sliceBS bs (ltOff + 2) (min (ltOff + 10) len)
                    case () of
                      _ | BS.isPrefixOf "DOCTYPE" (bsToUpper nameBS) || BS.isPrefixOf "doctype" nameBS -> do
                            let (!name, !pub, !sys) = parseDoctypeBS bs (ltOff + 10) afterGt
                            if sizeofSmallArray (rwDoctype rw) == 0
                              then pure ()
                              else do
                                dr <- newDoctypeRef name pub sys
                                forM_ (rwDoctype rw) $ \handler -> handler dr
                                writeIORef (_drValid dr) False
                                cowEmitMod cow bs ltOff afterGt (emitDoctypeRaw name)
                        | otherwise -> pure ()
                goScan afterGt

          goComment !off !ltOff = do
            let !endPos = scanCommentEnd addr# off len
                !commentEnd = endPos + 3
                !text = decodeTextSlice sharedBA addr# off (endPos - off) bs
            suppressedCM <- checkSuppressed canSuppress st
            if suppressedCM
              then goScan (min commentEnd len)
              else if sizeofSmallArray (rwComment rw) == 0
                      then goScan (min commentEnd len)
                      else do
                        resetCommentRef cPool text
                        forM_ (rwComment rw) $ \handler -> handler cPool
                        writeIORef (_crValid cPool) False
                        mut <- readIORef (_crMut cPool)
                        t <- readIORef (_crText cPool)
                        case mut of
                          MutNone
                            | t /= text ->
                                cowEmitMod cow bs ltOff (min commentEnd len)
                                  (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->")
                            | otherwise -> pure ()
                          Mut bef aft mRepl removed
                            | Just repl <- mRepl ->
                                cowEmitMod cow bs ltOff (min commentEnd len) (bef <> repl <> aft)
                            | removed ->
                                cowEmitMod cow bs ltOff (min commentEnd len) mempty
                            | otherwise ->
                                cowEmitMod cow bs ltOff (min commentEnd len)
                                  (bef <> BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->" <> aft)
                        goScan (min commentEnd len)

          goPI !off !_ltOff = do
            let !endPos = scanPIEnd addr# off len
                !piEnd = endPos + 2
            goScan (min piEnd len)

          goRawText !tagName !off = do
            let !endPos = scanRawTextEnd addr# off len tagName bs
            if needsText
              then do
                d <- readDepth st
                hasMatch <- if needsContextStack
                  then do stk <- readIORef (asStack st); pure (anyTextAncestorMatches stk)
                  else textMaskActive st d
                when (hasMatch && endPos > off) $ do
                  let !text = decodeTextSlice sharedBA addr# off (endPos - off) bs
                  tr <- resetTextChunkRef tPool text True
                  anyMatched <- if needsContextStack
                    then do stk <- readIORef (asStack st); runTextHandlers rw stk tr
                    else runTextHandlersAll rw tr
                  writeIORef (_trValid tr) False
                  when anyMatched $ do
                    stk <- if needsContextStack then readIORef (asStack st) else pure []
                    emitTextResult cow bs off endPos tr stk
              else pure ()
            let !closeEnd = skipToGtBS bs (endPos + 2) len
            runEndTagFull cow bs st rw etPool tagName endPos closeEnd
            goScan closeEnd

      goScan 0
      cowFinalize cow bs


-- | Emit a modified start tag to the COW output.
emitModifiedStartTag :: CowOutput -> ByteString -> Rewriter -> ElementRef -> Text -> SmallArray HTMLAttribute -> Bool -> Bool -> Int -> Int -> Mutations -> ElemMod -> AutoState -> Bool -> SmallArray DecomposedSel -> IO ()
emitModifiedStartTag cow src rw _ePool lcName attrs selfClose isVoid ltOff afterTag mut mElem st needsContextStack textSels = do
  stack <- if needsContextStack then readIORef (asStack st) else pure []
  depth <- readDepth st
  ehs0 <- readIORef (asEndTagHandlers st)
  let bef = case mut of Mut b _ _ _ -> b; _ -> mempty
      aft = case mut of Mut _ a _ _ -> a; _ -> mempty
      mRepl = case mut of Mut _ _ r _ -> r; _ -> Nothing
      removed = case mut of Mut _ _ _ r -> r; _ -> False
  case mRepl of
    Just repl -> do
      cowEmitMod cow src ltOff afterTag (bef <> repl <> aft)
      when (not selfClose && not isVoid) $ do
        writeSuppressUntil st depth
        writeDepth st (depth + 1)
    _ | removed -> do
      cowEmitMod cow src ltOff afterTag mempty
      when (not selfClose && not isVoid) $ do
        writeSuppressUntil st depth
        writeDepth st (depth + 1)
    _ -> do
      let !mFull = elemModToMut mElem
          tag' = maybe lcName id (elemModTag mElem)
          modAttrs = mFull >>= emAttrs
          rmChildren = maybe False emRmChildren mFull
          innerContent = mFull >>= emInnerContent
          userEndHandler = mFull >>= emEndTagHandler
      cowFlushTo cow src ltOff
      case mut of
        MutNone -> pure ()
        _ -> cowWriteBuilder cow bef
      let !pendingNew = case mElem of
            EMNewAttrs as -> as
            EMTagAndAttrs _ as -> as
            EMFull m -> emNewAttrs m
            _ -> []
      case modAttrs of
        Nothing | null pendingNew -> cowWriteStartTag cow tag' attrs selfClose
        Nothing -> do
          let !gtPos = if selfClose then afterTag - 2 else afterTag - 1
          cowWriteSlice cow src ltOff gtPos
          let emitNew [] = pure ()
              emitNew ((n, v) : rest) = emitNew rest >> cowWriteOneAttr cow n v
          emitNew pendingNew
          if selfClose
            then do cowWriteByte cow 0x2F; cowWriteByte cow 0x3E
            else cowWriteByte cow 0x3E
        Just modArr -> cowWriteStartTag cow tag' modArr selfClose
      case mFull of
        Just em | Just p <- emPrepend em -> cowWriteBuilder cow p
        _ -> pure ()
      cowWriteFlushed cow afterTag
      cowSetDirty cow
      if selfClose || isVoid
        then case mut of
          MutNone -> pure ()
          _ -> cowEmitModAppend cow aft
        else do
          let mAppnd = mFull >>= emAppend
              needsSuppress = case innerContent of Just _ -> True; Nothing -> rmChildren
              !tagRenamed = tag' /= lcName
              !needsDeferred = tagRenamed
                            || isJust innerContent
                            || isJust mAppnd
                            || case mut of MutNone -> False; _ -> True
                            || isJust userEndHandler
              deferredHandler etr = do
                when tagRenamed $ writeIORef (_etrTag etr) tag'
                let !hasBefore = isJust innerContent || isJust mAppnd
                    !hasAft = case mut of MutNone -> False; _ -> True
                when (hasBefore || hasAft) $ do
                  m <- readIORef (_etrMut etr)
                  let bld = case m of
                        MutNone -> Mut bldBef bldAft Nothing False
                        Mut b0 a0 r0 d0 -> Mut (b0 <> bldBef) (a0 <> bldAft) r0 d0
                        MutText mb0 ma0 _ _ d0 -> Mut (maybe bldBef (<> bldBef) mb0) (maybe bldAft (<> bldAft) ma0) Nothing d0
                      bldBef = case innerContent of
                        Just ic -> case mAppnd of
                          Just ap -> ic <> ap
                          Nothing -> ic
                        Nothing -> case mAppnd of
                          Just ap -> ap
                          Nothing -> mempty
                      bldAft = aft
                  writeIORef (_etrMut etr) bld
                case userEndHandler of
                  Just h -> h etr
                  Nothing -> pure ()
          if needsContextStack
            then do
              let !parentTM = case stack of (sf : _) -> sfTextMatch sf; [] -> False
                  !selfTM = matchAnyDecomposed (rwTextSelectors rw) stack lcName attrs
                  !textMatch = parentTM || selfTM
                  !frame = StackFrame lcName attrs depth textMatch
              writeIORef (asStack st) (frame : stack)
            else do
              pm <- if depth > 0 then readTextMask st (depth - 1) else pure 0
              let !selfTM = matchAnyDecomposed textSels [] lcName attrs
                  !mask = if pm /= 0 || selfTM then 1 else 0
              writeTextMask st depth mask
          writeDepth st (depth + 1)
          when needsDeferred $
            writeIORef (asEndTagHandlers st) ((depth, deferredHandler) : ehs0)
          when needsSuppress $
            writeRemoveChildrenUntil st depth


spanEndHandlers :: Int -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
spanEndHandlers !threshold = go
  where
    go [] = ([], [])
    go xs@(x : rest)
      | fst x >= threshold =
          case go rest of (matched, remaining) -> (x : matched, remaining)
      | otherwise = ([], xs)

-- | Run end tag handling with full mutation support.
runEndTagFull :: CowOutput -> ByteString -> AutoState -> Rewriter -> EndTagRef -> Text -> Int -> Int -> IO ()
runEndTagFull cow src st rw etPool lcName ltOff afterGt = do
  d <- readDepth st
  let !newD = d - 1
      !wantsStack = rwNeedsStack rw
      !wantsContextStack = rwNeedsContextStack rw
  ehs <- readIORef (asEndTagHandlers st)
  case ehs of
    [] | not (rwHasEndTag rw) -> do
      when (wantsStack && wantsContextStack) $ do
        stack <- readIORef (asStack st)
        writeIORef (asStack st) (case stack of (_ : xs) -> xs; [] -> [])
      writeDepth st newD
    ehs' -> do
      stack <- if wantsContextStack then readIORef (asStack st) else pure []
      let !newStack = case stack of (_ : xs) -> xs; [] -> []
          (endHandlers, remainingEH) = spanEndHandlers newD ehs'
      resetEndTagRef etPool lcName
      forM_ endHandlers $ \(_, handler) -> handler etPool
      _ <- runEndTagHandlers rw stack lcName etPool
      writeIORef (_etrValid etPool) False
      mut <- readIORef (_etrMut etPool)
      tag' <- readIORef (_etrTag etPool)
      case mut of
        MutNone
          | tag' /= lcName -> do
              cowFlushTo cow src ltOff
              cowWriteEndTag cow tag'
              cowWriteFlushed cow afterGt
              cowSetDirty cow
          | otherwise -> pure ()
        Mut bef aft _repl _removed -> do
          cowFlushTo cow src ltOff
          cowWriteBuilder cow bef
          cowWriteEndTag cow tag'
          cowWriteBuilder cow aft
          cowWriteFlushed cow afterGt
          cowSetDirty cow
      when wantsContextStack $ writeIORef (asStack st) newStack
      writeDepth st newD
      writeIORef (asEndTagHandlers st) remainingEH


-- | Emit modified text to COW output.
-- MutNone is a passthrough: the original bytes are preserved by COW.
emitTextResult :: CowOutput -> ByteString -> Int -> Int -> TextChunkRef -> [StackFrame] -> IO ()
emitTextResult cow src startOff endOff tr stk = do
  mut <- readIORef (_trMut tr)
  case mut of
    MutNone -> pure ()
    MutText mbef maft content ct _removed -> do
      cowFlushTo cow src startOff
      case mbef of Just bef -> cowWriteBuilder cow bef; Nothing -> pure ()
      case ct of
        AsText -> cowEscapeText cow content
        AsHTML -> cowWriteTextBytes cow content
      case maft of Just aft -> cowWriteBuilder cow aft; Nothing -> pure ()
      cowWriteFlushed cow endOff
      cowSetDirty cow
    Mut bef aft mRepl removed
      | Just repl <- mRepl -> do
          cowFlushTo cow src startOff
          cowWriteBuilder cow bef
          cowWriteBuilder cow repl
          cowWriteBuilder cow aft
          cowWriteFlushed cow endOff
          cowSetDirty cow
      | removed -> cowEmitMod cow src startOff endOff mempty
      | otherwise -> do
          content <- readIORef (_trContent tr)
          let !inRaw = case stk of
                (StackFrame t _ _ _ : _) -> isRawTextTag t
                [] -> False
          cowFlushTo cow src startOff
          cowWriteBuilder cow bef
          if inRaw
            then cowWriteTextBytes cow content
            else cowEscapeText cow content
          cowWriteBuilder cow aft
          cowWriteFlushed cow endOff
          cowSetDirty cow


-- | Scan forward to find end of comment (-->) from current position.
scanCommentEnd :: Addr# -> Int -> Int -> Int
scanCommentEnd addr# !off !len
  | off + 2 >= len = len
  | readByteOff addr# off == 0x2D
    && readByteOff addr# (off + 1) == 0x2D
    && readByteOff addr# (off + 2) == 0x3E = off
  | otherwise = scanCommentEnd addr# (off + 1) len


-- | Scan forward to find end of PI (?>) from current position.
scanPIEnd :: Addr# -> Int -> Int -> Int
scanPIEnd addr# !off !len
  | off + 1 >= len = len
  | readByteOff addr# off == 0x3F
    && readByteOff addr# (off + 1) == 0x3E = off
  | otherwise = scanPIEnd addr# (off + 1) len


-- | Scan for the closing tag of a raw text element (e.g. </script>).
-- Returns the offset of the '<' in the closing tag, or len if not found.
scanRawTextEnd :: Addr# -> Int -> Int -> Text -> ByteString -> Int
scanRawTextEnd addr# !off !len !tagName !_src = go off
  where
    !tagBS = TE.encodeUtf8 tagName
    !tagLen = BS.length tagBS
    go !i
      | i + 2 + tagLen > len = len
      | readByteOff addr# i == 0x3C
        && readByteOff addr# (i + 1) == 0x2F
        && matchesTag (i + 2) = i
      | otherwise = go (i + 1)
    matchesTag !start = go' 0
      where
        go' !j
          | j >= tagLen = True
          | otherwise =
              let !b = readByteOff addr# (start + j)
                  !expected = BSU.unsafeIndex tagBS j
                  !bLower = if b >= 0x41 && b <= 0x5A then b + 32 else b
              in bLower == expected && go' (j + 1)


-- | Parse DOCTYPE from raw bytes. Minimal: just extract the name.
parseDoctypeBS :: ByteString -> Int -> Int -> (Text, Maybe Text, Maybe Text)
parseDoctypeBS !bs !off !endGt =
  let !nameStart = skipWSBS bs off endGt
      !nameEnd = scanWordBS bs nameStart endGt
      !name = if nameEnd > nameStart
              then TE.decodeUtf8Lenient (sliceBS bs nameStart nameEnd)
              else "html"
  in (name, Nothing, Nothing)
  where
    skipWSBS b !i !e
      | i >= e = e
      | otherwise = case BS.index b i of
          w | w == 0x20 || w == 0x09 || w == 0x0A || w == 0x0D -> skipWSBS b (i + 1) e
          _ -> i
    scanWordBS b !i !e
      | i >= e = e
      | otherwise = case BS.index b i of
          w | w == 0x20 || w == 0x09 || w == 0x0A || w == 0x0D || w == 0x3E -> i
          _ -> scanWordBS b (i + 1) e


-- | Uppercase a short ByteString (for DOCTYPE matching).
bsToUpper :: ByteString -> ByteString
bsToUpper = BS.map (\w -> if w >= 0x61 && w <= 0x7A then w - 32 else w)


-- | Create an incremental rewriter state.
newRewriterState :: Rewriter -> IO RewriterState
newRewriterState rw = do
  auto <- newAutoState
  lo <- newIORef BS.empty
  ePool <- newElementRef "" mempty False
  tPool <- newTextChunkRef "" True
  etPool <- newEndTagRef ""
  cPool <- newCommentRef ""
  pure (RewriterState rw auto lo ePool tPool etPool cPool)


{- | Feed a chunk. Tokenizes as much complete content as possible,
carrying forward any incomplete tag across the chunk boundary.
Returns rewritten output for all fully processed tokens.
-}
feedRewriter :: RewriterState -> ByteString -> IO BB.Builder
feedRewriter rs chunk = do
  prev <- readIORef (rsLeftover rs)
  let !combined = if BS.null prev then chunk else prev <> chunk
      !splitAt = findSafeBreak combined
      !toProcess = BS.take splitAt combined
      !remainder = BS.drop splitAt combined
      !needsAttrs = hasElementHandlers (rsRewriter rs)
  writeIORef (rsLeftover rs) remainder
  if BS.null toProcess
    then pure mempty
    else do
      outRef <- newIORef mempty
      let emit !b = modifyIORef' outRef (<> b)
          !rw = rsRewriter rs
      tokenizeCallbackIOWith needsAttrs toProcess $ \tok startOff endOff ->
        processOneToken rw (rsAuto rs) emit toProcess (rsElementPool rs) (rsTextPool rs) (rsEndTagPool rs) (rsCommentPool rs) tok startOff endOff
      readIORef outRef


-- | Finalize. Flushes any remaining buffered content.
finishRewriter :: RewriterState -> IO BB.Builder
finishRewriter rs = do
  leftover <- readIORef (rsLeftover rs)
  let !needsAttrs = hasElementHandlers (rsRewriter rs)
      !rw = rsRewriter rs
  if BS.null leftover
    then pure mempty
    else do
      outRef <- newIORef mempty
      let emit !b = modifyIORef' outRef (<> b)
      tokenizeCallbackIOWith needsAttrs leftover $ \tok startOff endOff ->
        processOneToken rw (rsAuto rs) emit leftover (rsElementPool rs) (rsTextPool rs) (rsEndTagPool rs) (rsCommentPool rs) tok startOff endOff
      readIORef outRef


-- | Streaming with output callback.
feedRewriter' :: RewriterState -> ByteString -> (ByteString -> IO ()) -> IO ()
feedRewriter' rs chunk sink = do
  out <- feedRewriter rs chunk
  let !bs = BL.toStrict (BB.toLazyByteString out)
  when (not (BS.null bs)) $ sink bs


{- | Find the byte offset where it's safe to break for tokenization.
Scans backwards to find the last @\<@ that doesn't have a matching
@\>@ after it (an incomplete tag). Everything before that point is
safe to tokenize independently.
-}
findSafeBreak :: ByteString -> Int
findSafeBreak !bs = go (BS.length bs - 1)
  where
    go !i
      | i < 0 = BS.length bs
      | otherwise = case BS.index bs i of
          0x3C -> i
          0x3E -> BS.length bs
          _ -> go (i - 1)


-- ---------------------------------------------------------------------------
-- Token processing core
-- ---------------------------------------------------------------------------

processOneToken :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> ByteString -> ElementRef -> TextChunkRef -> EndTagRef -> CommentRef -> Token -> Int -> Int -> IO ()
processOneToken rw st emit src ePool tPool etPool cPool tok startOff endOff = do
  suppress <- readSuppressUntil st
  if suppress >= 0
    then do
      let !suppDepth = suppress
      case tok of
        TEndTag _ _ -> do
          d <- readDepth st
          let !newD = d - 1
          if newD <= suppDepth
            then do
              stk <- readIORef (asStack st)
              let !newStack = case stk of (_ : xs) -> xs; [] -> []
              writeSuppressUntil st (-1)
              writeDepth st newD
              writeIORef (asStack st) newStack
            else writeDepth st newD
        TStartTag _ _ sc tid ->
          when (not sc && not (tagIdIsVoid tid)) $ do
            d <- readDepth st
            writeDepth st (d + 1)
        _ -> pure ()
    else do
      removeCh <- readRemoveChildrenUntil st
      if removeCh >= 0
        then do
          let !rcDepth = removeCh
          case tok of
            TEndTag _ _ -> do
              d <- readDepth st
              let !newD = d - 1
              if newD <= rcDepth
                then do
                  writeRemoveChildrenUntil st (-1)
                  handleEndTag rw st emit src etPool tok startOff endOff
                else writeDepth st newD
            TStartTag _ _ sc tid ->
              when (not sc && not (tagIdIsVoid tid)) $ do
                d <- readDepth st
                writeDepth st (d + 1)
            _ -> pure ()
        else case tok of
          TStartTag name attrs selfClose tid -> handleStartTag rw st emit src ePool name attrs selfClose tid startOff endOff
          TEndTag _ _ -> handleEndTag rw st emit src etPool tok startOff endOff
          TString _ -> do
            stk <- readIORef (asStack st)
            emitRawOrFallback stk emit src startOff endOff (handleText rw st emit tPool)
          TChar _c -> do
            stk <- readIORef (asStack st)
            emitRawOrFallback stk emit src startOff endOff (handleText rw st emit tPool)
          TComment text -> handleComment rw st emit cPool text
          TDoctype name pub sys _ -> handleDoctype rw st emit name pub sys
          TEOF -> pure ()
  where
    emitRawOrFallback stk emitF srcBS !s !e fallback =
      if s >= 0 && canBypassText stk
        then emitF (BB.byteString (BS.take (e - s) (BS.drop s srcBS)))
        else case tok of
          TString text -> fallback text True
          TChar c -> fallback (T.singleton c) True
          _ -> pure ()

    canBypassText stk
      | not (hasTextHandlers rw) = True
      | otherwise = not (anyTextAncestorMatches stk)
    {-# INLINE canBypassText #-}


handleStartTag :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> ByteString -> ElementRef -> Text -> SmallArray HTMLAttribute -> Bool -> TagId -> Int -> Int -> IO ()
handleStartTag rw st emit src ePool name attrs selfClose tid startOff endOff = do
  let !isVoid = tagIdIsVoid tid
  stack <- readIORef (asStack st)
  let !parentTM = case stack of (sf : _) -> sfTextMatch sf; [] -> False
      !selfTM = matchAnyDecomposed (rwTextSelectors rw) stack name attrs
      !textMatch = parentTM || selfTM

  resetElementRef ePool name attrs selfClose
  anyMatched <- runElementHandlers rw stack name attrs ePool
  writePrimArray (_erInts ePool) 0 (0 :: Int)

  if not anyMatched
    then do
      if startOff >= 0
        then emit (BB.byteString (BS.take (endOff - startOff) (BS.drop startOff src)))
        else emit (emitStartTagRaw name attrs selfClose)
      when (not selfClose && not isVoid) $ do
        d <- readDepth st
        let !frame = StackFrame name attrs d textMatch
        writeIORef (asStack st) (frame : stack)
        writeDepth st (d + 1)
    else do

      mut <- readIORef (_erMut ePool)
      mElem <- readIORef (_erElem ePool)
      let !dirty = case mut of MutNone -> False; _ -> True
          !elemDirty = case mElem of EMNone -> False; _ -> True

      if not dirty && not elemDirty
        then do
          if startOff >= 0
            then emit (BB.byteString (BS.take (endOff - startOff) (BS.drop startOff src)))
            else emit (emitStartTagRaw name attrs selfClose)
          when (not selfClose && not isVoid) $ do
            d <- readDepth st
            let !frame = StackFrame name attrs d textMatch
            writeIORef (asStack st) (frame : stack)
            writeDepth st (d + 1)
        else do
          let bef = case mut of Mut b _ _ _ -> b; _ -> mempty
              aft = case mut of Mut _ a _ _ -> a; _ -> mempty
              mRepl = case mut of Mut _ _ r _ -> r; _ -> Nothing
              removed = case mut of Mut _ _ _ r -> r; _ -> False
          case mRepl of
            Just repl -> do
              emit bef >> emit repl >> emit aft
              when (not selfClose && not isVoid) $ do
                d <- readDepth st
                writeSuppressUntil st d
                writeDepth st (d + 1)
            _ | removed -> do
              when (not selfClose && not isVoid) $ do
                d <- readDepth st
                writeSuppressUntil st d
                writeDepth st (d + 1)
            _ -> do
              let !mFull = elemModToMut mElem
                  tag' = maybe name id (elemModTag mElem)
                  attrs' = maybe attrs id (mFull >>= emAttrs)
                  mPrep = mFull >>= emPrepend
                  mAppnd = mFull >>= emAppend
                  rmChildren = maybe False emRmChildren mFull
                  innerContent = mFull >>= emInnerContent
                  userEndHandler = mFull >>= emEndTagHandler

              emit bef
              emit (emitStartTagFromArr tag' attrs' selfClose)
              case mPrep of
                Just p -> emit p
                Nothing -> pure ()

              if selfClose || isVoid
                then emit aft
                else do
                  depthNow <- readDepth st
                  ehs <- readIORef (asEndTagHandlers st)
                  let needsSuppress = case innerContent of Just _ -> True; Nothing -> rmChildren
                      !tagRenamed = tag' /= name
                      !needsDeferred = tagRenamed
                                    || isJust innerContent
                                    || isJust mAppnd
                                    || case mut of MutNone -> False; _ -> True
                                    || isJust userEndHandler
                      deferredHandler etr = do
                        when tagRenamed $ writeIORef (_etrTag etr) tag'
                        let !hasBefore = isJust innerContent || isJust mAppnd
                            !hasAft = case mut of MutNone -> False; _ -> True
                        when (hasBefore || hasAft) $ do
                          m <- readIORef (_etrMut etr)
                          let bld = case m of
                                MutNone -> Mut bldBef bldAft Nothing False
                                Mut b0 a0 r0 d0 -> Mut (b0 <> bldBef) (a0 <> bldAft) r0 d0
                                MutText mb0 ma0 _ _ d0 -> Mut (maybe bldBef (<> bldBef) mb0) (maybe bldAft (<> bldAft) ma0) Nothing d0
                              bldBef = case innerContent of
                                Just ic -> case mAppnd of
                                  Just ap -> ic <> ap
                                  Nothing -> ic
                                Nothing -> case mAppnd of
                                  Just ap -> ap
                                  Nothing -> mempty
                              bldAft = aft
                          writeIORef (_etrMut etr) bld
                        case userEndHandler of
                          Just h -> h etr
                          Nothing -> pure ()
                  let !parentTM2 = case stack of (sf : _) -> sfTextMatch sf; [] -> False
                      !selfTM2 = matchAnyDecomposed (rwTextSelectors rw) stack name attrs
                      !textMatch2 = parentTM2 || selfTM2
                      !frame = StackFrame name attrs depthNow textMatch2
                  writeIORef (asStack st) (frame : stack)
                  writeDepth st (depthNow + 1)
                  when needsDeferred $
                    writeIORef (asEndTagHandlers st) ((depthNow, deferredHandler) : ehs)
                  when needsSuppress $
                    writeRemoveChildrenUntil st depthNow


handleEndTag :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> ByteString -> EndTagRef -> Token -> Int -> Int -> IO ()
handleEndTag rw st emit src etPool tok startOff endOff = do
  d <- readDepth st
  stack <- readIORef (asStack st)
  ehandlers <- readIORef (asEndTagHandlers st)
  let !name = tokenTag tok
      !newD = d - 1
      (endHandlers, remainingEH) = span (\(depth, _) -> depth >= newD) ehandlers
      !newStack = case stack of (_ : xs) -> xs; [] -> []

  if null endHandlers && not (rwHasElement rw) && not (rwHasEndTag rw)
    then do
      if startOff >= 0
        then emit (BB.byteString (BS.take (endOff - startOff) (BS.drop startOff src)))
        else emit (emitEndTagRaw name)
      writeIORef (asStack st) newStack
      writeDepth st newD
      writeIORef (asEndTagHandlers st) remainingEH
    else do
      stack <- readIORef (asStack st)
      let !newStack = case stack of (_ : xs) -> xs; [] -> []
      resetEndTagRef etPool name
      forM_ endHandlers $ \(_, handler) -> handler etPool
      _ <- runEndTagHandlers rw stack name etPool
      writeIORef (_etrValid etPool) False

      mut <- readIORef (_etrMut etPool)
      tag' <- readIORef (_etrTag etPool)
      case mut of
        MutNone ->
          emit (emitEndTagRaw tag')
        Mut bef aft _repl _removed -> do
          emit bef
          emit (emitEndTagRaw tag')
          emit aft
        MutText {} -> pure ()
      writeIORef (asStack st) newStack
      writeDepth st newD
      writeIORef (asEndTagHandlers st) remainingEH


handleText :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> TextChunkRef -> Text -> Bool -> IO ()
handleText rw st emit tPool text isLast = do
  stack <- readIORef (asStack st)
  let !inRaw = case stack of
        (StackFrame t _ _ _ : _) -> isRawTextTag t
        [] -> False
      !emitText = if inRaw then BB.byteString . TE.encodeUtf8 else escapeTextBuilder
  tr <- resetTextChunkRef tPool text isLast
  anyMatched <- runTextHandlers rw stack tr
  writeIORef (_trValid tr) False

  if not anyMatched
    then emit (emitText text)
    else do
      mut <- readIORef (_trMut tr)
      content <- readIORef (_trContent tr)
      case mut of
        MutNone -> emit (emitText content)
        MutText mbef maft replContent ct _ -> do
          case mbef of Just bef -> emit bef; Nothing -> pure ()
          emit (encodeContent replContent ct)
          case maft of Just aft2 -> emit aft2; Nothing -> pure ()
        Mut bef aft mRepl removed
          | Just repl <- mRepl -> emit bef >> emit repl >> emit aft
          | removed -> pure ()
          | otherwise -> emit bef >> emit (emitText content) >> emit aft


handleComment :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> CommentRef -> Text -> IO ()
handleComment rw _st emit cPool text = do
  if sizeofSmallArray (rwComment rw) == 0
    then emit (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 text) <> BB.byteString "-->")
    else do
      resetCommentRef cPool text
      let !cr = cPool
      forM_ (rwComment rw) $ \handler -> handler cr
      writeIORef (_crValid cr) False

      mut <- readIORef (_crMut cr)
      t <- readIORef (_crText cr)
      case mut of
        MutNone -> do
          emit (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->")
        Mut bef aft mRepl removed
          | Just repl <- mRepl -> emit bef >> emit repl >> emit aft
          | removed -> pure ()
          | otherwise -> do
              emit bef
              emit (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->")
              emit aft
        MutText {} -> pure ()


handleDoctype :: Rewriter -> AutoState -> (BB.Builder -> IO ()) -> Text -> Maybe Text -> Maybe Text -> IO ()
handleDoctype rw _st emit name pub sys = do
  if sizeofSmallArray (rwDoctype rw) == 0
    then emit (emitDoctypeRaw name)
    else do
      dr <- newDoctypeRef name pub sys
      forM_ (rwDoctype rw) $ \handler -> handler dr
      writeIORef (_drValid dr) False
      emit (emitDoctypeRaw name)


-- ---------------------------------------------------------------------------
-- Selector matching against element stack
-- ---------------------------------------------------------------------------

-- | Single-pass: run all matching element handlers, return True if any matched.
runElementHandlers :: Rewriter -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> ElementRef -> IO Bool
runElementHandlers rw stack tag attrs er = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHElement decomposed handler ->
            if matchAnyDecomposed decomposed stack tag attrs
              then handler er >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runElementHandlers #-}


-- | Single-pass: run all matching end-tag handlers, return True if any matched.
runEndTagHandlers :: Rewriter -> [StackFrame] -> Text -> EndTagRef -> IO Bool
runEndTagHandlers rw stack name etr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHEndTag decomposed handler ->
            if matchAnyEndTag decomposed stack name
              then handler etr >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runEndTagHandlers #-}


-- | O(1) check: does any text handler have an ancestor match at the current stack?
-- Uses the cached sfTextMatch flag computed when each frame was pushed.
anyTextAncestorMatches :: [StackFrame] -> Bool
anyTextAncestorMatches [] = False
anyTextAncestorMatches (sf : _) = sfTextMatch sf
{-# INLINE anyTextAncestorMatches #-}


-- | Single-pass: run all matching text handlers, return True if any matched.
-- Walks the ancestor stack to determine which text handlers apply.
runTextHandlers :: Rewriter -> [StackFrame] -> TextChunkRef -> IO Bool
runTextHandlers rw stack tr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHText decomposed handler ->
            if anyAncestorMatches decomposed stack
              then handler tr >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)

    anyAncestorMatches _ [] = False
    anyAncestorMatches !sels (StackFrame tag attrs _ _ : frames) =
      matchAnyDecomposed sels frames tag attrs || anyAncestorMatches sels frames
{-# INLINE runTextHandlers #-}

-- | Fire all text handlers unconditionally. Used when
-- rwNeedsContextStack=False and the depth-indexed text mask already
-- confirmed that a matching ancestor exists.
runTextHandlersAll :: Rewriter -> TextChunkRef -> IO Bool
runTextHandlersAll rw tr = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHText _ handler -> handler tr >> go True (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runTextHandlersAll #-}


matchAnyDecomposed :: SmallArray DecomposedSel -> [StackFrame] -> Text -> SmallArray HTMLAttribute -> Bool
matchAnyDecomposed !arr stack tag attrs = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchAtPosition (indexSmallArray arr i) stack tag attrs = True
      | otherwise = go (i + 1)
{-# INLINE matchAnyDecomposed #-}


-- | Run element handlers using class-only byte matching (no SmallArray attrs).
runElementHandlersClass :: Rewriter -> Text -> Addr# -> Int -> Int -> ElementRef -> IO Bool
runElementHandlersClass rw tag addr# classOff classLen er = go False 0
  where
    !handlers = rwHandlers rw
    !hLen = sizeofSmallArray handlers
    go !matched !i
      | i >= hLen = pure matched
      | otherwise = case indexSmallArray handlers i of
          CHElement decomposed handler ->
            if matchAnyDecomposedClass decomposed tag addr# classOff classLen
              then handler er >> go True (i + 1)
              else go matched (i + 1)
          _ -> go matched (i + 1)
{-# INLINE runElementHandlersClass #-}




-- | Class-only decomposed selector matching at byte level.
matchAnyDecomposedClass :: SmallArray DecomposedSel -> Text -> Addr# -> Int -> Int -> Bool
matchAnyDecomposedClass !arr tag addr# classOff classLen = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchDecomposedClass (indexSmallArray arr i) = True
      | otherwise = go (i + 1)
    matchDecomposedClass (DecomposedSel (CompoundSelector mtype subs) _ctx) =
      matchType mtype tag && allSubsClass subs
    allSubsClass [] = True
    allSubsClass (SelClass c : rest) = hasClassWord c addr# classOff classLen && allSubsClass rest
    allSubsClass _ = False
{-# INLINE matchAnyDecomposedClass #-}


matchAnyEndTag :: SmallArray DecomposedSel -> [StackFrame] -> Text -> Bool
matchAnyEndTag !arr stack name = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | matchEndPositionD (indexSmallArray arr i) stack name = True
      | otherwise = go (i + 1)
{-# INLINE matchAnyEndTag #-}


matchEndPositionD :: DecomposedSel -> [StackFrame] -> Text -> Bool
matchEndPositionD ds stack name =
  let currentAttrs = case stack of
        (StackFrame _ a _ _ : _) -> a
        [] -> mempty
  in matchCompoundForEnd (dsSubject ds) name currentAttrs && matchContext (dsContext ds) stack


matchCompoundForEnd :: CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompoundForEnd (CompoundSelector mtype _subs) name _attrs =
  case mtype of
    Nothing -> True
    Just TypeUniversal -> True
    Just (TypeTag t) -> t == name


-- ---------------------------------------------------------------------------
-- Ref constructors
-- ---------------------------------------------------------------------------

newElementRef :: Text -> SmallArray HTMLAttribute -> Bool -> IO ElementRef
newElementRef tag attrs selfClose = do
  tRef <- newIORef tag
  aRef <- newIORef attrs
  mut <- newIORef MutNone
  em <- newIORef EMNone
  ints <- newPrimArray 3
  writePrimArray ints 0 (1 :: Int)   -- valid
  writePrimArray ints 1 (-1 :: Int)  -- attrOff
  writePrimArray ints 2 (0 :: Int)   -- srcLen
  baRef <- newIORef (ByteArray ba0#)
  bsRef <- newIORef BS.empty
  pure (ElementRef tRef aRef selfClose mut em ints baRef bsRef)
  where
    !(ByteArray ba0#) = emptyBA
{-# NOINLINE newElementRef #-}

emptyBA :: ByteArray
emptyBA = case runRW# (\s0 -> case newByteArray# 0# s0 of
    (# s1, mba# #) -> unsafeFreezeByteArray# mba# s1) of
  (# _, ba# #) -> ByteArray ba#
{-# NOINLINE emptyBA #-}


resetElementRef :: ElementRef -> Text -> SmallArray HTMLAttribute -> Bool -> IO ()
resetElementRef er tag attrs _selfClose = do
  writeIORef (_erOrigTag er) tag
  writeIORef (_erOrigAttrs er) attrs
  writeIORef (_erMut er) MutNone
  writeIORef (_erElem er) EMNone
  let !ints = _erInts er
  writePrimArray ints 0 (1 :: Int)   -- valid
  writePrimArray ints 1 (-1 :: Int)  -- attrOff (computed)
{-# INLINE resetElementRef #-}

resetElementRefDeferred :: ElementRef -> Text -> Bool -> Int -> IO ()
resetElementRefDeferred er tag _selfClose !nameEnd = do
  writeIORef (_erOrigTag er) tag
  writeIORef (_erOrigAttrs er) emptySmallArray
  writeIORef (_erMut er) MutNone
  writeIORef (_erElem er) EMNone
  let !ints = _erInts er
  writePrimArray ints 0 (1 :: Int)     -- valid
  writePrimArray ints 1 nameEnd        -- attrOff
{-# INLINE resetElementRefDeferred #-}

forceAttrs :: ElementRef -> IO (SmallArray HTMLAttribute)
forceAttrs er = do
  let !ints = _erInts er
  off <- readPrimArray ints 1
  if (off :: Int) < 0
    then readIORef (_erOrigAttrs er)
    else do
      ba <- readIORef (_erSharedBA er)
      srcBS <- readIORef (_erSrcBS er)
      srcLen <- readPrimArray ints 2
      let (!attrs, !_, !_) = readTagAttrsBS ba srcBS off (srcLen :: Int)
      writeIORef (_erOrigAttrs er) attrs
      writePrimArray ints 1 (-1 :: Int)
      pure attrs
{-# INLINE forceAttrs #-}


lookupPending :: Text -> [(Text, Text)] -> Maybe Text
lookupPending _ [] = Nothing
lookupPending name ((n, v) : rest)
  | n == name = Just v
  | otherwise = lookupPending name rest
{-# INLINE lookupPending #-}


hasAttrNameInTag :: ByteString -> Int -> Int -> Text -> Bool
hasAttrNameInTag (BS (ForeignPtr addr# _) _) !off !end name = go off
  where
    !(Text (ByteArray nameBA#) nameOff nameLen) = name
    rd :: Int -> Word8
    rd i = readByteOff addr# i
    {-# INLINE rd #-}

    go !i
      | i >= end = False
      | otherwise = case rd i of
          0x3E -> False
          0x2F | i + 1 < end, rd (i + 1) == 0x3E -> False
          b | isWSByte b -> go (i + 1)
          _ -> checkName i

    checkName !nameStart =
      let !nameEnd' = scanToDelim nameStart
          !attrNameLen = nameEnd' - nameStart
      in if attrNameLen == nameLen && matchBytes nameStart
           then True
           else skipValue nameEnd'

    scanToDelim !j
      | j >= end = j
      | otherwise = case rd j of
          0x3D -> j
          0x3E -> j
          0x2F -> j
          b | isWSByte b -> j
          _ -> scanToDelim (j + 1)

    matchBytes !start = matchLoop 0
      where
        matchLoop !k
          | k >= nameLen = True
          | otherwise =
              let !a = toLowerByte (rd (start + k))
                  !b = indexBA nameBA# (nameOff + k)
              in a == b && matchLoop (k + 1)

    toLowerByte :: Word8 -> Word8
    toLowerByte w = if w >= 0x41 && w <= 0x5A then w + 32 else w

    skipValue !j
      | j >= end = go j
      | otherwise =
          let !j1 = skipWS j
          in if j1 >= end || rd j1 /= 0x3D
               then go j1
               else
                 let !j2 = skipWS (j1 + 1)
                 in if j2 >= end then go j2
                    else case rd j2 of
                      0x22 -> go (scanPast 0x22 (j2 + 1))
                      0x27 -> go (scanPast 0x27 (j2 + 1))
                      _    -> go (scanUnq j2)

    scanPast !delim !j
      | j >= end = j
      | rd j == delim = j + 1
      | otherwise = scanPast delim (j + 1)

    scanUnq !j
      | j >= end = j
      | isWSByte (rd j) || rd j == 0x3E = j
      | otherwise = scanUnq (j + 1)

    skipWS !j
      | j >= end = j
      | isWSByte (rd j) = skipWS (j + 1)
      | otherwise = j
{-# INLINE hasAttrNameInTag #-}


cowWriteOneAttr :: CowOutput -> Text -> Text -> IO ()
cowWriteOneAttr cow name val = do
  let !(Text (ByteArray nameBA#) nameOff nameLen) = name
  cowEnsure cow (4 + nameLen + 64)
  p <- cowReadPos cow
  b <- readIORef (cowBuf cow)
  writeBA b p 0x20
  copyBAToMBA b (p + 1) nameBA# nameOff nameLen
  writeBA b (p + 1 + nameLen) 0x3D
  writeBA b (p + 2 + nameLen) 0x22
  cowWritePos cow (p + 3 + nameLen)
  cowEscapeAttrVal cow val
  cowWriteByte cow 0x22
{-# INLINE cowWriteOneAttr #-}


newTextChunkRef :: Text -> Bool -> IO TextChunkRef
newTextChunkRef text isLast = do
  cRef <- newIORef text
  mut <- newIORef MutNone
  valid <- newIORef True
  pure (TextChunkRef cRef mut isLast valid)


resetTextChunkRef :: TextChunkRef -> Text -> Bool -> IO TextChunkRef
resetTextChunkRef tr text _isLast = do
  writeIORef (_trContent tr) text
  writeIORef (_trMut tr) MutNone
  writeIORef (_trValid tr) True
  pure tr
{-# INLINE resetTextChunkRef #-}


newCommentRef :: Text -> IO CommentRef
newCommentRef text = do
  tRef <- newIORef text
  mut <- newIORef MutNone
  valid <- newIORef True
  pure (CommentRef tRef mut valid)


resetCommentRef :: CommentRef -> Text -> IO ()
resetCommentRef cr text = do
  writeIORef (_crText cr) text
  writeIORef (_crMut cr) MutNone
  writeIORef (_crValid cr) True
{-# INLINE resetCommentRef #-}


newDoctypeRef :: Text -> Maybe Text -> Maybe Text -> IO DoctypeRef
newDoctypeRef name pub sys = do
  valid <- newIORef True
  pure (DoctypeRef name pub sys valid)


newEndTagRef :: Text -> IO EndTagRef
newEndTagRef tag = do
  tRef <- newIORef tag
  mut <- newIORef MutNone
  valid <- newIORef True
  pure (EndTagRef tRef mut valid)


resetEndTagRef :: EndTagRef -> Text -> IO ()
resetEndTagRef etr tag = do
  writeIORef (_etrTag etr) tag
  writeIORef (_etrMut etr) MutNone
  writeIORef (_etrValid etr) True
{-# INLINE resetEndTagRef #-}


-- ---------------------------------------------------------------------------
-- Output helpers
-- ---------------------------------------------------------------------------

encodeContent :: Text -> ContentType -> BB.Builder
encodeContent text AsHTML = BB.byteString (TE.encodeUtf8 text)
encodeContent text AsText = escapeTextBuilder text


escapeTextBuilder :: Text -> BB.Builder
escapeTextBuilder t =
  let !bs = TE.encodeUtf8 t
  in escapeBS bs 0 (BS.length bs)
  where
    escapeBS !bs !off !len
      | off >= len = mempty
      | otherwise =
          let !b = BS.index bs off
          in case b of
              0x3C -> BB.byteString (BS.take (off - 0) BS.empty) <> BB.byteString "&lt;" <> escapeBS bs (off + 1) len
              _ -> scanClean bs off off len

    scanClean !bs !start !off !len
      | off >= len = BB.byteString (BS.take (off - start) (BS.drop start bs))
      | otherwise =
          let !b = BS.index bs off
          in case b of
              0x3C ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&lt;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x3E ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&gt;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x26 ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&amp;"
                  <> scanClean bs (off + 1) (off + 1) len
              _ -> scanClean bs start (off + 1) len


escapeAttrBuilder :: Text -> BB.Builder
escapeAttrBuilder t =
  let !bs = TE.encodeUtf8 t
  in scanClean bs 0 0 (BS.length bs)
  where
    scanClean !bs !start !off !len
      | off >= len = BB.byteString (BS.take (off - start) (BS.drop start bs))
      | otherwise =
          let !b = BS.index bs off
          in case b of
              0x22 ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&quot;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x26 ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&amp;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x3C ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&lt;"
                  <> scanClean bs (off + 1) (off + 1) len
              0x3E ->
                BB.byteString (BS.take (off - start) (BS.drop start bs))
                  <> BB.byteString "&gt;"
                  <> scanClean bs (off + 1) (off + 1) len
              _ -> scanClean bs start (off + 1) len


emitStartTagRaw :: Text -> SmallArray HTMLAttribute -> Bool -> BB.Builder
emitStartTagRaw tag attrs selfClose =
  BB.char7 '<'
    <> BB.byteString (TE.encodeUtf8 tag)
    <> emitAttrsRaw attrs
    <> (if selfClose then BB.byteString " />" else BB.char7 '>')


emitStartTagFromArr :: Text -> SmallArray HTMLAttribute -> Bool -> BB.Builder
emitStartTagFromArr tag attrs selfClose =
  BB.char7 '<'
    <> BB.byteString (TE.encodeUtf8 tag)
    <> emitAttrsRaw attrs
    <> (if selfClose then BB.byteString " />" else BB.char7 '>')


emitAttrsRaw :: SmallArray HTMLAttribute -> BB.Builder
emitAttrsRaw attrs = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = mempty
      | otherwise =
          let !(HTMLAttribute name val) = indexSmallArray attrs i
          in emitOneAttr name val <> go (i + 1)


emitAttrsFromList :: [HTMLAttribute] -> BB.Builder
emitAttrsFromList = foldMap (\(HTMLAttribute name val) -> emitOneAttr name val)


emitOneAttr :: Text -> Text -> BB.Builder
emitOneAttr name val =
  BB.char7 ' '
    <> BB.byteString (TE.encodeUtf8 name)
    <> BB.byteString "=\""
    <> escapeAttrBuilder val
    <> BB.char7 '"'


emitEndTagRaw :: Text -> BB.Builder
emitEndTagRaw tag = BB.byteString "</" <> BB.byteString (TE.encodeUtf8 tag) <> BB.char7 '>'


emitDoctypeRaw :: Text -> BB.Builder
emitDoctypeRaw name =
  BB.byteString "<!DOCTYPE " <> BB.byteString (TE.encodeUtf8 (if T.null name then "html" else name)) <> BB.char7 '>'


-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

tokenTag :: Token -> Text
tokenTag (TStartTag name _ _ _) = name
tokenTag (TEndTag name _) = name
tokenTag _ = T.empty


isRawTextTag :: Text -> Bool
isRawTextTag t = t == "style" || t == "script" || t == "xmp"
{-# INLINE isRawTextTag #-}


lookupAttrList :: Text -> [HTMLAttribute] -> Maybe Text
lookupAttrList _ [] = Nothing
lookupAttrList name (HTMLAttribute n v : rest)
  | n == name = Just v
  | otherwise = lookupAttrList name rest


lookupAttrArr :: Text -> SmallArray HTMLAttribute -> Maybe Text
lookupAttrArr name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = Nothing
      | otherwise =
          let !(HTMLAttribute k v) = indexSmallArray arr i
          in if k == name then Just v else go (i + 1)
{-# INLINE lookupAttrArr #-}


hasAttrArr :: Text -> SmallArray HTMLAttribute -> Bool
hasAttrArr name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = False
      | otherwise =
          let !(HTMLAttribute k _) = indexSmallArray arr i
          in k == name || go (i + 1)
{-# INLINE hasAttrArr #-}


setAttrArr :: Text -> Text -> SmallArray HTMLAttribute -> SmallArray HTMLAttribute
setAttrArr name val arr =
  let !n = sizeofSmallArray arr
      !idx = findAttrIdx name arr
  in if idx >= 0
    then runSmallArray $ do
      ma <- thawSmallArray arr 0 n
      writeSmallArray ma idx (HTMLAttribute name val)
      pure ma
    else runSmallArray $ do
      ma <- newSmallArray (n + 1) (HTMLAttribute name val)
      copySmallArray ma 0 arr 0 n
      pure ma
{-# INLINE setAttrArr #-}


removeAttrArr :: Text -> SmallArray HTMLAttribute -> SmallArray HTMLAttribute
removeAttrArr name arr =
  let !n = sizeofSmallArray arr
      !idx = findAttrIdx name arr
  in if idx < 0
    then arr
    else runSmallArray $ do
      ma <- newSmallArray (n - 1) (HTMLAttribute "" "")
      copySmallArray ma 0 arr 0 idx
      copySmallArray ma idx arr (idx + 1) (n - idx - 1)
      pure ma
{-# INLINE removeAttrArr #-}


findAttrIdx :: Text -> SmallArray HTMLAttribute -> Int
findAttrIdx name arr = go 0
  where
    !n = sizeofSmallArray arr
    go !i
      | i >= n = -1
      | otherwise = let !(HTMLAttribute k _) = indexSmallArray arr i
                    in if k == name then i else go (i + 1)
{-# INLINE findAttrIdx #-}


-- ---------------------------------------------------------------------------
-- Direct scan helpers
-- ---------------------------------------------------------------------------

-- | Extract or copy a ByteArray from a ByteString for Text slice creation.
-- For PlainPtr ByteStrings (the common case), this freezes the underlying
-- MutableByteArray# in-place — zero allocation.  The ByteString must
-- remain alive for the duration (the caller holds 'bs').
freezeByteStringBA :: ByteString -> IO ByteArray
freezeByteStringBA (BS (ForeignPtr _ (PlainPtr mba#)) _) =
  IO (\s -> case unsafeFreezeByteArray# mba# s of (# s', ba# #) -> (# s', ByteArray ba# #))
freezeByteStringBA (BS (ForeignPtr _ (MallocPtr mba# _)) _) =
  IO (\s -> case unsafeFreezeByteArray# mba# s of (# s', ba# #) -> (# s', ByteArray ba# #))
freezeByteStringBA (BS (ForeignPtr addr# _) len) =
  pure (makeSharedBACopy addr# len)
{-# INLINE freezeByteStringBA #-}

makeSharedBACopy :: Addr# -> Int -> ByteArray
makeSharedBACopy addr# len =
  case runRW#
    ( \s0 ->
        case newByteArray# len# s0 of
          (# s1, mba# #) ->
            case copyAddrToByteArray# addr# mba# 0# len# s1 of
              s2 ->
                case unsafeFreezeByteArray# mba# s2 of
                  (# s3, ba# #) -> (# s3, ba# #)
    ) of
    (# _, ba# #) -> ByteArray ba#
  where
    !(I# len#) = len
{-# NOINLINE makeSharedBACopy #-}


-- | Zero-copy slice of a ByteString.
sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs off end = BS.take (end - off) (BS.drop off bs)
{-# INLINE sliceBS #-}


-- | Decode a byte range to String for entity parsing.
toStringFrom :: ByteString -> Int -> Int -> String
toStringFrom bsS offS lenS =
  T.unpack (TE.decodeUtf8Lenient (BSU.unsafeTake (lenS - offS) (BSU.unsafeDrop offS bsS)))
