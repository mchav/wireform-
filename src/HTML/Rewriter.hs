{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Control.Monad (forM_, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (toList)
import Data.IORef
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import HTML.Parse (Token (..), tokenizeCallbackIOWith)
import HTML.Selector
import HTML.Value (HTMLAttribute (..), isVoidElement)


-- ---------------------------------------------------------------------------
-- Content type
-- ---------------------------------------------------------------------------

data ContentType = AsText | AsHTML
  deriving (Show, Eq)


-- ---------------------------------------------------------------------------
-- Mutable handles
-- ---------------------------------------------------------------------------

data ElementRef = ElementRef
  { _erTag :: !(IORef Text)
  , _erAttrs :: !(IORef [HTMLAttribute])
  , _erSelfClose :: !Bool
  , _erBefore :: !(IORef BB.Builder)
  , _erPrepend :: !(IORef BB.Builder)
  , _erAppend :: !(IORef BB.Builder)
  , _erAfter :: !(IORef BB.Builder)
  , _erRemoved :: !(IORef Bool)
  , _erReplaced :: !(IORef (Maybe BB.Builder))
  , _erRemoveChildren :: !(IORef Bool)
  , _erInnerContent :: !(IORef (Maybe BB.Builder))
  , _erEndTagHandler :: !(IORef (Maybe (EndTagRef -> IO ())))
  , _erValid :: !(IORef Bool)
  }


data TextChunkRef = TextChunkRef
  { _trContent :: !(IORef Text)
  , _trBefore :: !(IORef BB.Builder)
  , _trAfter :: !(IORef BB.Builder)
  , _trRemoved :: !(IORef Bool)
  , _trReplaced :: !(IORef (Maybe BB.Builder))
  , _trIsLast :: !Bool
  , _trValid :: !(IORef Bool)
  }


data CommentRef = CommentRef
  { _crText :: !(IORef Text)
  , _crBefore :: !(IORef BB.Builder)
  , _crAfter :: !(IORef BB.Builder)
  , _crRemoved :: !(IORef Bool)
  , _crReplaced :: !(IORef (Maybe BB.Builder))
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
  , _etrBefore :: !(IORef BB.Builder)
  , _etrAfter :: !(IORef BB.Builder)
  , _etrValid :: !(IORef Bool)
  }


data ExpiredRefError = ExpiredRefError !Text
  deriving (Show)


instance Exception ExpiredRefError


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

data CompiledHandler
  = CHElement ![ComplexSelector] !(ElementRef -> IO ())
  | CHText ![ComplexSelector] !(TextChunkRef -> IO ())
  | CHEndTag ![ComplexSelector] !(EndTagRef -> IO ())


data Rewriter = Rewriter
  { rwHandlers :: ![CompiledHandler]
  , rwComment :: ![CommentRef -> IO ()]
  , rwDoctype :: ![DoctypeRef -> IO ()]
  , rwHasText :: !Bool
  , rwHasElement :: !Bool
  }


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
  handlers <- mapM compileHandler (rcHandlers cfg)
  Right
    Rewriter
      { rwHandlers = handlers
      , rwComment = ghComment (rcGlobal cfg)
      , rwDoctype = ghDoctype (rcGlobal cfg)
      , rwHasText = any isTextHandler handlers
      , rwHasElement = any isElementHandler handlers
      }
  where
    isTextHandler (CHText _ _) = True
    isTextHandler _ = False
    isElementHandler (CHElement _ _) = True
    isElementHandler _ = False
    compileHandler (HElement sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHElement cs handler)
    compileHandler (HText sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHText cs handler)
    compileHandler (HEndTag sel@(Selector cs) handler) = do
      validateRewriter sel
      Right (CHEndTag cs handler)

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
  , sfDepth :: !Int
  }


data PartialMatch = PartialMatch
  { pmSteps :: ![(Combinator, CompoundSelector)]
  , pmDepth :: !Int
  }


data AutoState = AutoState
  { asStack :: ![StackFrame]
  , asDepth :: !Int
  , asPartials :: ![(Int, PartialMatch)]
  , asSuppressUntil :: !(Maybe Int)
  , asRemoveChildrenUntil :: !(Maybe Int)
  , asEndTagHandlers :: ![(Int, EndTagRef -> IO ())]
  }


newAutoState :: AutoState
newAutoState = AutoState [] 0 [] Nothing Nothing []


{- | Check if a complex selector matches at the current position.
The parser stores selectors left-to-right: "div > span" becomes
@ComplexSelector div [(Child, span)]@. CSS matching is right-to-left:
the subject (rightmost compound) must match the current element, then
ancestors are checked working backwards through the chain.
-}
matchAtPosition :: ComplexSelector -> AutoState -> Text -> SmallArray HTMLAttribute -> Bool
matchAtPosition cs st tag attrs =
  let (subject, context) = decomposeComplex cs
  in matchCompound subject tag attrs && matchContext context (asStack st)


{- | Extract the subject (rightmost compound) and the ancestor chain.
"div > span" → (span, [(Child, div)])
"a b > c" → (c, [(Child, b), (Descendant, a)])
-}
decomposeComplex :: ComplexSelector -> (CompoundSelector, [(Combinator, CompoundSelector)])
decomposeComplex (ComplexSelector hd []) = (hd, [])
decomposeComplex (ComplexSelector hd tl) =
  let subject = snd (last tl)
      allCompounds = hd : map snd (init tl)
      allCombs = map fst tl
      context = reverse (zip allCombs allCompounds)
  in (subject, context)


-- | Walk the ancestor chain against the element stack.
matchContext :: [(Combinator, CompoundSelector)] -> [StackFrame] -> Bool
matchContext [] _ = True
matchContext ((Child, comp) : rest) frames =
  case frames of
    (StackFrame t a _ : frames') ->
      matchCompound comp t a && matchContext rest frames'
    [] -> False
matchContext ((Descendant, comp) : rest) frames =
  scanAncestors frames
  where
    scanAncestors [] = False
    scanAncestors (StackFrame t a _ : frames') =
      (matchCompound comp t a && matchContext rest frames') || scanAncestors frames'
matchContext ((AdjacentSibling, _) : _) _ = False
matchContext ((GeneralSibling, _) : _) _ = False


-- ---------------------------------------------------------------------------
-- Element mutation API
-- ---------------------------------------------------------------------------

getTagName :: ElementRef -> IO Text
getTagName er = checkValid (_erValid er) "ElementRef" >> readIORef (_erTag er)


setTagName :: ElementRef -> Text -> IO ()
setTagName er t = checkValid (_erValid er) "ElementRef" >> writeIORef (_erTag er) t


getElemAttr :: ElementRef -> Text -> IO (Maybe Text)
getElemAttr er name = do
  checkValid (_erValid er) "ElementRef"
  attrs <- readIORef (_erAttrs er)
  pure $ lookupAttr name attrs


setElemAttr :: ElementRef -> Text -> Text -> IO ()
setElemAttr er name val = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erAttrs er) (setAttrList name val)


removeElemAttr :: ElementRef -> Text -> IO ()
removeElemAttr er name = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erAttrs er) (filter (\(HTMLAttribute n _) -> n /= name))


hasElemAttr :: ElementRef -> Text -> IO Bool
hasElemAttr er name = do
  checkValid (_erValid er) "ElementRef"
  attrs <- readIORef (_erAttrs er)
  pure $ any (\(HTMLAttribute n _) -> n == name) attrs


getElemAttrs :: ElementRef -> IO [(Text, Text)]
getElemAttrs er = do
  checkValid (_erValid er) "ElementRef"
  attrs <- readIORef (_erAttrs er)
  pure $ map (\(HTMLAttribute n v) -> (n, v)) attrs


beforeElement :: ElementRef -> Text -> ContentType -> IO ()
beforeElement er content ct = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erBefore er) (<> encodeContent content ct)


prependToElement :: ElementRef -> Text -> ContentType -> IO ()
prependToElement er content ct = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erPrepend er) (<> encodeContent content ct)


appendToElement :: ElementRef -> Text -> ContentType -> IO ()
appendToElement er content ct = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erAppend er) (<> encodeContent content ct)


afterElement :: ElementRef -> Text -> ContentType -> IO ()
afterElement er content ct = do
  checkValid (_erValid er) "ElementRef"
  modifyIORef' (_erAfter er) (<> encodeContent content ct)


replaceElement :: ElementRef -> Text -> ContentType -> IO ()
replaceElement er content ct = do
  checkValid (_erValid er) "ElementRef"
  writeIORef (_erReplaced er) (Just (encodeContent content ct))


removeElement :: ElementRef -> IO ()
removeElement er = do
  checkValid (_erValid er) "ElementRef"
  writeIORef (_erRemoved er) True


removeChildren :: ElementRef -> IO ()
removeChildren er = do
  checkValid (_erValid er) "ElementRef"
  writeIORef (_erRemoveChildren er) True


setInnerContent :: ElementRef -> Text -> ContentType -> IO ()
setInnerContent er content ct = do
  checkValid (_erValid er) "ElementRef"
  writeIORef (_erInnerContent er) (Just (encodeContent content ct))


onElementEndTag :: ElementRef -> (EndTagRef -> IO ()) -> IO ()
onElementEndTag er handler = do
  checkValid (_erValid er) "ElementRef"
  writeIORef (_erEndTagHandler er) (Just handler)


-- ---------------------------------------------------------------------------
-- Text chunk mutation API
-- ---------------------------------------------------------------------------

getTextContent :: TextChunkRef -> IO Text
getTextContent tr = checkValid (_trValid tr) "TextChunkRef" >> readIORef (_trContent tr)


replaceTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
replaceTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  writeIORef (_trReplaced tr) (Just (encodeContent content ct))


beforeTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
beforeTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  modifyIORef' (_trBefore tr) (<> encodeContent content ct)


afterTextChunk :: TextChunkRef -> Text -> ContentType -> IO ()
afterTextChunk tr content ct = do
  checkValid (_trValid tr) "TextChunkRef"
  modifyIORef' (_trAfter tr) (<> encodeContent content ct)


removeTextChunk :: TextChunkRef -> IO ()
removeTextChunk tr = do
  checkValid (_trValid tr) "TextChunkRef"
  writeIORef (_trRemoved tr) True


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
  writeIORef (_crReplaced cr) (Just (encodeContent content ct))


beforeComment :: CommentRef -> Text -> ContentType -> IO ()
beforeComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  modifyIORef' (_crBefore cr) (<> encodeContent content ct)


afterComment :: CommentRef -> Text -> ContentType -> IO ()
afterComment cr content ct = do
  checkValid (_crValid cr) "CommentRef"
  modifyIORef' (_crAfter cr) (<> encodeContent content ct)


removeComment :: CommentRef -> IO ()
removeComment cr = do
  checkValid (_crValid cr) "CommentRef"
  writeIORef (_crRemoved cr) True


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
  modifyIORef' (_etrBefore etr) (<> encodeContent content ct)


afterEndTag :: EndTagRef -> Text -> ContentType -> IO ()
afterEndTag etr content ct = do
  checkValid (_etrValid etr) "EndTagRef"
  modifyIORef' (_etrAfter etr) (<> encodeContent content ct)


-- ---------------------------------------------------------------------------
-- Running the rewriter
-- ---------------------------------------------------------------------------

data RewriterState = RewriterState
  { rsRewriter :: !Rewriter
  , rsAuto :: !(IORef AutoState)
  , rsLeftover :: !(IORef ByteString)
  }


-- | One-shot: rewrite a complete document.
rewrite :: Rewriter -> ByteString -> IO ByteString
rewrite rw bs = do
  outRef <- newIORef mempty
  stRef <- newIORef newAutoState
  let emit !b = modifyIORef' outRef (<> b)
      !needsAttrs = hasElementHandlers rw
  tokenizeCallbackIOWith needsAttrs bs $ \tok startOff endOff ->
    processOneToken rw stRef emit bs tok startOff endOff
  out <- readIORef outRef
  pure $! BL.toStrict $! BB.toLazyByteString out


-- | Create an incremental rewriter state.
newRewriterState :: Rewriter -> IO RewriterState
newRewriterState rw = do
  auto <- newIORef newAutoState
  lo <- newIORef BS.empty
  pure (RewriterState rw auto lo)


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
      tokenizeCallbackIOWith needsAttrs toProcess $ \tok startOff endOff ->
        processOneToken (rsRewriter rs) (rsAuto rs) emit toProcess tok startOff endOff
      readIORef outRef


-- | Finalize. Flushes any remaining buffered content.
finishRewriter :: RewriterState -> IO BB.Builder
finishRewriter rs = do
  leftover <- readIORef (rsLeftover rs)
  let !needsAttrs = hasElementHandlers (rsRewriter rs)
  if BS.null leftover
    then pure mempty
    else do
      outRef <- newIORef mempty
      let emit !b = modifyIORef' outRef (<> b)
      tokenizeCallbackIOWith needsAttrs leftover $ \tok startOff endOff ->
        processOneToken (rsRewriter rs) (rsAuto rs) emit leftover tok startOff endOff
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

processOneToken :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> ByteString -> Token -> Int -> Int -> IO ()
processOneToken rw stRef emit src tok startOff endOff = do
  st <- readIORef stRef
  case asSuppressUntil st of
    Just suppDepth -> case tok of
      TEndTag _ _ -> do
        let !newD = asDepth st - 1
        if newD <= suppDepth
          then do
            let !newStack = case asStack st of (_ : xs) -> xs; [] -> []
            writeIORef stRef st {asSuppressUntil = Nothing, asDepth = newD, asStack = newStack}
          else writeIORef stRef st {asDepth = newD}
      TStartTag _ _ sc _ ->
        when (not sc && not (isVoidTag (tokenTag tok))) $
          writeIORef stRef st {asDepth = asDepth st + 1}
      _ -> pure ()
    Nothing -> case asRemoveChildrenUntil st of
      Just rcDepth -> case tok of
        TEndTag _ _ -> do
          let !newD = asDepth st - 1
          if newD <= rcDepth
            then do
              writeIORef stRef st {asRemoveChildrenUntil = Nothing, asDepth = newD}
              handleEndTag rw stRef emit src tok startOff endOff
            else writeIORef stRef st {asDepth = newD}
        TStartTag _ _ sc _ ->
          when (not sc && not (isVoidTag (tokenTag tok))) $
            writeIORef stRef st {asDepth = asDepth st + 1}
        _ -> pure ()
      Nothing -> case tok of
        TStartTag name attrs selfClose _ -> handleStartTag rw stRef emit src name attrs selfClose startOff endOff
        TEndTag _ _ -> handleEndTag rw stRef emit src tok startOff endOff
        TString _ -> emitRawOrFallback emit src startOff endOff (handleText rw stRef emit)
        TChar c -> emitRawOrFallback emit src startOff endOff (handleText rw stRef emit)
        TComment text -> handleComment rw stRef emit text
        TDoctype name pub sys _ -> handleDoctype rw stRef emit name pub sys
        TEOF -> pure ()
  where
    emitRawOrFallback emitF srcBS !s !e fallback
      | not (hasTextHandlers rw) && s >= 0 =
          emitF (BB.byteString (BS.take (e - s) (BS.drop s srcBS)))
      | otherwise = case tok of
          TString text -> fallback text True
          TChar c -> fallback (T.singleton c) True
          _ -> pure ()


handleStartTag :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> ByteString -> Text -> SmallArray HTMLAttribute -> Bool -> Int -> Int -> IO ()
handleStartTag rw stRef emit src name attrs selfClose startOff endOff = do
  st <- readIORef stRef

  let !matchingElement = collectElementHandlers rw st name attrs

  if null matchingElement
    then do
      if startOff >= 0
        then emit (BB.byteString (BS.take (endOff - startOff) (BS.drop startOff src)))
        else emit (emitStartTagRaw name attrs selfClose)
      when (not selfClose && not (isVoidTag name)) $ do
        let !frame = StackFrame name attrs (asDepth st)
            !st' = st {asStack = frame : asStack st, asDepth = asDepth st + 1}
        writeIORef stRef st'
    else do
      er <- newElementRef name attrs selfClose
      forM_ matchingElement $ \handler -> handler er
      writeIORef (_erValid er) False

      removed <- readIORef (_erRemoved er)
      replaced <- readIORef (_erReplaced er)
      case replaced of
        Just repl | not removed -> do
          bef <- readIORef (_erBefore er)
          aft <- readIORef (_erAfter er)
          emit bef
          emit repl
          emit aft
          when (not selfClose && not (isVoidTag name)) $
            writeIORef stRef st {asSuppressUntil = Just (asDepth st), asDepth = asDepth st + 1}
        _ | removed -> do
          when (not selfClose && not (isVoidTag name)) $
            writeIORef stRef st {asSuppressUntil = Just (asDepth st), asDepth = asDepth st + 1}
        _ -> do
          tag' <- readIORef (_erTag er)
          attrs' <- readIORef (_erAttrs er)
          bef <- readIORef (_erBefore er)
          prep <- readIORef (_erPrepend er)
          rmChildren <- readIORef (_erRemoveChildren er)
          innerContent <- readIORef (_erInnerContent er)
          userEndHandler <- readIORef (_erEndTagHandler er)
          appnd <- readIORef (_erAppend er)
          aft <- readIORef (_erAfter er)

          emit bef
          emit (emitStartTagFromList tag' attrs' selfClose)
          emit prep

          if selfClose || isVoidTag name
            then emit aft
            else do
              let needsSuppress = case innerContent of Just _ -> True; Nothing -> rmChildren
                  deferredHandler etr = do
                    when (tag' /= name) $ writeIORef (_etrTag etr) tag'
                    case innerContent of
                      Just ic -> modifyIORef' (_etrBefore etr) (<> ic)
                      Nothing -> pure ()
                    modifyIORef' (_etrBefore etr) (<> appnd)
                    modifyIORef' (_etrAfter etr) (<> aft)
                    case userEndHandler of
                      Just h -> h etr
                      Nothing -> pure ()
                  !frame = StackFrame name attrs (asDepth st)
                  !st' =
                    st
                      { asStack = frame : asStack st
                      , asDepth = asDepth st + 1
                      , asEndTagHandlers = (asDepth st, deferredHandler) : asEndTagHandlers st
                      }
              if needsSuppress
                then writeIORef stRef st' {asRemoveChildrenUntil = Just (asDepth st)}
                else writeIORef stRef st'


handleEndTag :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> ByteString -> Token -> Int -> Int -> IO ()
handleEndTag rw stRef emit src tok startOff endOff = do
  st <- readIORef stRef
  let !d = asDepth st
      !name = tokenTag tok
      !newD = d - 1
      !matchingEndTag = collectEndTagHandlers rw st name
      (endHandlers, remainingEH) = span (\(depth, _) -> depth >= newD) (asEndTagHandlers st)
      !newStack = case asStack st of
        (_ : xs) -> xs
        [] -> []

  if null matchingEndTag && null endHandlers
    then do
      if startOff >= 0
        then emit (BB.byteString (BS.take (endOff - startOff) (BS.drop startOff src)))
        else emit (emitEndTagRaw name)
      writeIORef stRef st {asStack = newStack, asDepth = newD, asEndTagHandlers = remainingEH}
    else do
      etr <- newEndTagRef name
      forM_ endHandlers $ \(_, handler) -> handler etr
      forM_ matchingEndTag $ \handler -> handler etr
      writeIORef (_etrValid etr) False

      bef <- readIORef (_etrBefore etr)
      aft <- readIORef (_etrAfter etr)
      tag' <- readIORef (_etrTag etr)

      emit bef
      emit (emitEndTagRaw tag')
      emit aft
      writeIORef stRef st {asStack = newStack, asDepth = newD, asEndTagHandlers = remainingEH}


handleText :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> Text -> Bool -> IO ()
handleText rw stRef emit text isLast = do
  st <- readIORef stRef
  let !inRaw = case asStack st of
        (StackFrame t _ _ : _) -> isRawTextTag t
        [] -> False
      !emitText = if inRaw then BB.byteString . TE.encodeUtf8 else escapeTextBuilder
      !matchingText = collectTextHandlersFromStack rw st
  if null matchingText
    then emit (emitText text)
    else do
      tr <- newTextChunkRef text isLast
      forM_ matchingText $ \handler -> handler tr
      writeIORef (_trValid tr) False

      removed <- readIORef (_trRemoved tr)
      replaced <- readIORef (_trReplaced tr)
      bef <- readIORef (_trBefore tr)
      aft <- readIORef (_trAfter tr)

      case replaced of
        Just repl | not removed -> emit bef >> emit repl >> emit aft
        _ | removed -> pure ()
        _ -> do
          content <- readIORef (_trContent tr)
          emit bef >> emit (emitText content) >> emit aft


handleComment :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> Text -> IO ()
handleComment rw _stRef emit text = do
  if null (rwComment rw)
    then emit (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 text) <> BB.byteString "-->")
    else do
      cr <- newCommentRef text
      forM_ (rwComment rw) $ \handler -> handler cr
      writeIORef (_crValid cr) False

      removed <- readIORef (_crRemoved cr)
      replaced <- readIORef (_crReplaced cr)
      bef <- readIORef (_crBefore cr)
      aft <- readIORef (_crAfter cr)

      case replaced of
        Just repl | not removed -> emit bef >> emit repl >> emit aft
        _ | removed -> pure ()
        _ -> do
          t <- readIORef (_crText cr)
          emit bef
          emit (BB.byteString "<!--" <> BB.byteString (TE.encodeUtf8 t) <> BB.byteString "-->")
          emit aft


handleDoctype :: Rewriter -> IORef AutoState -> (BB.Builder -> IO ()) -> Text -> Maybe Text -> Maybe Text -> IO ()
handleDoctype rw _stRef emit name pub sys = do
  if null (rwDoctype rw)
    then emit (emitDoctypeRaw name)
    else do
      dr <- newDoctypeRef name pub sys
      forM_ (rwDoctype rw) $ \handler -> handler dr
      writeIORef (_drValid dr) False
      emit (emitDoctypeRaw name)


-- ---------------------------------------------------------------------------
-- Selector matching against element stack
-- ---------------------------------------------------------------------------

collectElementHandlers :: Rewriter -> AutoState -> Text -> SmallArray HTMLAttribute -> [ElementRef -> IO ()]
collectElementHandlers rw st tag attrs = do
  ch <- rwHandlers rw
  case ch of
    CHElement complexSels handler ->
      if any (\cs -> matchAtPosition cs st tag attrs) complexSels
        then [handler]
        else []
    _ -> []


collectEndTagHandlers :: Rewriter -> AutoState -> Text -> [EndTagRef -> IO ()]
collectEndTagHandlers rw st name = do
  ch <- rwHandlers rw
  case ch of
    CHEndTag complexSels handler ->
      if any (\cs -> matchEndPosition cs st name) complexSels
        then [handler]
        else []
    _ -> []


collectTextHandlersFromStack :: Rewriter -> AutoState -> [TextChunkRef -> IO ()]
collectTextHandlersFromStack rw st = do
  ch <- rwHandlers rw
  case ch of
    CHText complexSels handler ->
      if anyAncestorMatches complexSels (asStack st)
        then [handler]
        else []
    _ -> []
  where
    anyAncestorMatches _ [] = False
    anyAncestorMatches sels (StackFrame tag attrs _ : rest) =
      let ancestorSt = st {asStack = rest}
      in any (\cs -> matchAtPosition cs ancestorSt tag attrs) sels
          || anyAncestorMatches sels rest


matchEndPosition :: ComplexSelector -> AutoState -> Text -> Bool
matchEndPosition cs st name =
  let (subject, context) = decomposeComplex cs
      currentAttrs = case asStack st of
        (StackFrame _ a _ : _) -> a
        [] -> mempty
  in matchCompoundForEnd subject name currentAttrs && matchContext context (asStack st)


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
  aRef <- newIORef (toList attrs)
  bef <- newIORef mempty
  prep <- newIORef mempty
  appnd <- newIORef mempty
  aft <- newIORef mempty
  rem' <- newIORef False
  repl <- newIORef Nothing
  rmCh <- newIORef False
  inner <- newIORef Nothing
  endH <- newIORef Nothing
  valid <- newIORef True
  pure (ElementRef tRef aRef selfClose bef prep appnd aft rem' repl rmCh inner endH valid)


newTextChunkRef :: Text -> Bool -> IO TextChunkRef
newTextChunkRef text isLast = do
  cRef <- newIORef text
  bef <- newIORef mempty
  aft <- newIORef mempty
  rem' <- newIORef False
  repl <- newIORef Nothing
  valid <- newIORef True
  pure (TextChunkRef cRef bef aft rem' repl isLast valid)


newCommentRef :: Text -> IO CommentRef
newCommentRef text = do
  tRef <- newIORef text
  bef <- newIORef mempty
  aft <- newIORef mempty
  rem' <- newIORef False
  repl <- newIORef Nothing
  valid <- newIORef True
  pure (CommentRef tRef bef aft rem' repl valid)


newDoctypeRef :: Text -> Maybe Text -> Maybe Text -> IO DoctypeRef
newDoctypeRef name pub sys = do
  valid <- newIORef True
  pure (DoctypeRef name pub sys valid)


newEndTagRef :: Text -> IO EndTagRef
newEndTagRef tag = do
  tRef <- newIORef tag
  bef <- newIORef mempty
  aft <- newIORef mempty
  valid <- newIORef True
  pure (EndTagRef tRef bef aft valid)


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


emitStartTagFromList :: Text -> [HTMLAttribute] -> Bool -> BB.Builder
emitStartTagFromList tag attrs selfClose =
  BB.char7 '<'
    <> BB.byteString (TE.encodeUtf8 tag)
    <> emitAttrsFromList attrs
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


isVoidTag :: Text -> Bool
isVoidTag = isVoidElement


isRawTextTag :: Text -> Bool
isRawTextTag t = t == "style" || t == "script" || t == "xmp"
{-# INLINE isRawTextTag #-}


lookupAttr :: Text -> [HTMLAttribute] -> Maybe Text
lookupAttr _ [] = Nothing
lookupAttr name (HTMLAttribute n v : rest)
  | n == name = Just v
  | otherwise = lookupAttr name rest


setAttrList :: Text -> Text -> [HTMLAttribute] -> [HTMLAttribute]
setAttrList name val = go False
  where
    go replaced [] = if replaced then [] else [HTMLAttribute name val]
    go replaced (a@(HTMLAttribute n _) : rest)
      | n == name = HTMLAttribute name val : go True rest
      | otherwise = a : go replaced rest
