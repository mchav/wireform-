{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Full HTML5 tree construction algorithm with mutable tree nodes.
module HTML.Parse
  ( parseHTML
  , parseHTMLFragment
  , parseHTMLNodes
  ) where

import Data.ByteString (ByteString)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, toLower, isAlpha, isAlphaNum)
import Data.IORef
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as S
import System.IO.Unsafe (unsafePerformIO)

import HTML.Value

------------------------------------------------------------------------
-- Token types
------------------------------------------------------------------------

data Token
  = TDoctype !Text !(Maybe Text) !(Maybe Text) !Bool
  | TStartTag !Text ![(Text,Text)] !Bool
  | TEndTag !Text
  | TChar !Char
  | TComment !Text
  | TEOF
  deriving (Show)

------------------------------------------------------------------------
-- Insertion modes
------------------------------------------------------------------------

data InsertionMode
  = MInitial | MBeforeHtml | MBeforeHead | MInHead | MInHeadNoscript
  | MAfterHead | MInBody | MText | MInTable | MInTableText
  | MInCaption | MInColumnGroup | MInTableBody | MInRow | MInCell
  | MInSelect | MInSelectInTable | MInTemplate | MAfterBody
  | MInFrameset | MAfterFrameset | MAfterAfterBody | MAfterAfterFrameset
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Mutable tree nodes
------------------------------------------------------------------------

data TBNode = TBNode
  { nodeId       :: !Int
  , nodeName     :: !Text
  , nodeAttrsRef :: !(IORef [(Text,Text)])
  , nodeNs       :: !(Maybe Text)
  , nodeChildren :: !(IORef [TBNode])
  , nodeParent   :: !(IORef (Maybe TBNode))
  , nodeIsTemplate :: !Bool
  , nodeTemplateContents :: !(IORef [TBNode])
  }

instance Eq TBNode where
  a == b = nodeId a == nodeId b

data TBText = TBText !Text
data TBComment = TBComment !Text
data TBDoctype = TBDoctype !Text !(Maybe Text) !(Maybe Text)

data ChildNode
  = CElement !TBNode
  | CText !Text
  | CComment !Text
  | CDoctype !Text !(Maybe Text) !(Maybe Text)

------------------------------------------------------------------------
-- Tree builder state
------------------------------------------------------------------------

data TreeBuilder = TreeBuilder
  { tbMode           :: !(IORef InsertionMode)
  , tbOriginalMode   :: !(IORef InsertionMode)
  , tbOpenElements   :: !(IORef [TBNode])
  , tbActiveFormatting :: !(IORef [AFEntry])
  , tbHeadElement    :: !(IORef (Maybe TBNode))
  , tbFormElement    :: !(IORef (Maybe TBNode))
  , tbFramesetOk     :: !(IORef Bool)
  , tbInsertFromTable :: !(IORef Bool)
  , tbPendingTableText :: !(IORef [Char])
  , tbTemplateModes  :: !(IORef [InsertionMode])
  , tbIgnoreLF       :: !(IORef Bool)
  , tbDocument       :: !(IORef [ChildNode])
  , tbQuirksMode     :: !(IORef Text)
  , tbNextId         :: !(IORef Int)
  , tbScriptingEnabled :: !Bool
  , tbFragmentContext :: !(Maybe (Text, Maybe Text))
  }

data AFEntry
  = AFMarker
  | AFEntry !Text ![(Text,Text)] !TBNode
  deriving (Eq)

------------------------------------------------------------------------
-- Public API
------------------------------------------------------------------------

parseHTML :: ByteString -> HTMLDocument
parseHTML bs = unsafePerformIO $ do
  let txt = TE.decodeUtf8Lenient bs
      tokens = tokenize txt
  tb <- newTreeBuilder Nothing
  mapM_ (processToken tb) tokens
  processToken tb TEOF
  buildDocument tb

parseHTMLNodes :: ByteString -> [HTMLNode]
parseHTMLNodes bs = unsafePerformIO $ do
  let txt = TE.decodeUtf8Lenient bs
      tokens = tokenize txt
  tb <- newTreeBuilder Nothing
  mapM_ (processToken tb) tokens
  processToken tb TEOF
  buildAllNodes tb

parseHTMLFragment :: Text -> Maybe Text -> ByteString -> [HTMLNode]
parseHTMLFragment contextTag contextNs bs = unsafePerformIO $ do
  let txt = TE.decodeUtf8Lenient bs
      tokens = fragmentTokenize contextTag txt
  tb <- newTreeBuilder (Just (contextTag, contextNs))
  mapM_ (processToken tb) tokens
  processToken tb TEOF
  buildFragmentResult tb

fragmentTokenize :: Text -> Text -> [Token]
fragmentTokenize ctx txt
  | ctx `elem` ["title", "textarea"] = tokenizeRCData (T.unpack txt) ctx
  | ctx `elem` ["style", "xmp", "iframe", "noembed", "noframes", "noscript", "script"] =
      tokenizeRawText (T.unpack txt) ctx
  | ctx == "plaintext" = map TChar (T.unpack txt)
  | otherwise = tokenize txt

------------------------------------------------------------------------
-- Initialize tree builder
------------------------------------------------------------------------

newTreeBuilder :: Maybe (Text, Maybe Text) -> IO TreeBuilder
newTreeBuilder mCtx = do
  modeRef <- newIORef MInitial
  origRef <- newIORef MInitial
  openRef <- newIORef []
  afRef <- newIORef []
  headRef <- newIORef Nothing
  formRef <- newIORef Nothing
  frameRef <- newIORef True
  insertRef <- newIORef False
  pendRef <- newIORef []
  tmRef <- newIORef []
  lfRef <- newIORef False
  docRef <- newIORef []
  qmRef <- newIORef "no-quirks"
  idRef <- newIORef 1
  let tb = TreeBuilder modeRef origRef openRef afRef headRef formRef
            frameRef insertRef pendRef tmRef lfRef docRef qmRef idRef True mCtx
  case mCtx of
    Nothing -> pure tb
    Just (ctxTag, ctxNs) -> do
      htmlNode <- newTBNode tb "html" [] Nothing False
      writeIORef openRef [htmlNode]
      case ctxNs of
        Just ns | ns == "svg" || ns == "math" -> do
          ctxNode <- newTBNode tb ctxTag [] ctxNs False
          appendChild htmlNode ctxNode
          writeIORef openRef [ctxNode, htmlNode]
          writeIORef modeRef MInBody
        _ -> do
          let mode0 = resetInsertionModeForContext ctxTag ctxNs
          writeIORef modeRef mode0
      writeIORef frameRef False
      pure tb

resetInsertionModeForContext :: Text -> Maybe Text -> InsertionMode
resetInsertionModeForContext name _ns
  | name == "td" || name == "th" = MInCell
  | name == "tr" = MInRow
  | name == "tbody" || name == "thead" || name == "tfoot" = MInTableBody
  | name == "caption" = MInCaption
  | name == "colgroup" = MInColumnGroup
  | name == "table" = MInTable
  | name == "template" = MInTemplate
  | name == "head" || name == "body" = MInBody
  | name == "frameset" = MInFrameset
  | name == "html" = MBeforeHead
  | name == "select" = MInSelect
  | otherwise = MInBody

newTBNode :: TreeBuilder -> Text -> [(Text,Text)] -> Maybe Text -> Bool -> IO TBNode
newTBNode tb name attrs ns isTmpl = do
  nid <- readIORef (tbNextId tb)
  writeIORef (tbNextId tb) (nid + 1)
  attrRef <- newIORef attrs
  childRef <- newIORef []
  parentRef <- newIORef Nothing
  tmplRef <- newIORef []
  pure TBNode
    { nodeId = nid
    , nodeName = name
    , nodeAttrsRef = attrRef
    , nodeNs = ns
    , nodeChildren = childRef
    , nodeParent = parentRef
    , nodeIsTemplate = isTmpl
    , nodeTemplateContents = tmplRef
    }

nodeAttrs :: TBNode -> IO [(Text,Text)]
nodeAttrs = readIORef . nodeAttrsRef

------------------------------------------------------------------------
-- Build final document
------------------------------------------------------------------------

buildDocument :: TreeBuilder -> IO HTMLDocument
buildDocument tb = do
  allNodes <- buildAllNodes tb
  let mdt = extractDoctype allNodes
      root = findOrCreateRoot allNodes
  pure (HTMLDocument mdt root)

buildAllNodes :: TreeBuilder -> IO [HTMLNode]
buildAllNodes tb = do
  docNodes <- readIORef (tbDocument tb)
  openElems <- readIORef (tbOpenElements tb)
  let rootFromStack = case reverse openElems of
        (root:_) -> Just root
        [] -> Nothing
  result <- mapM (buildDocChild rootFromStack) docNodes
  let flatResult = concat result
  case rootFromStack of
    Just root | not (any isRootElement docNodes) -> do
      r <- tbNodeToHTMLNode root
      pure (flatResult ++ [r])
    _ -> pure flatResult
  where
    isRootElement (CElement _) = True
    isRootElement _ = False
    buildDocChild mRoot (CElement node) = case mRoot of
      Just root | node == root -> do
        r <- tbNodeToHTMLNode root
        pure [r]
      _ -> do
        r <- tbNodeToHTMLNode node
        pure [r]
    buildDocChild _ cn = do
      r <- childToHTMLNode cn
      pure [r]

buildFragmentResult :: TreeBuilder -> IO [HTMLNode]
buildFragmentResult tb = do
  openElems <- readIORef (tbOpenElements tb)
  case tbFragmentContext tb of
    Just (_, Just ns) | ns == "svg" || ns == "math" -> do
      case reverse openElems of
        (htmlElem:_) -> do
          htmlChildren <- readIORef (nodeChildren htmlElem)
          allNodes <- fmap concat $ mapM getNodeChildren (reverse htmlChildren)
          mapM tbNodeToHTMLNode allNodes
        [] -> do
          docNodes <- readIORef (tbDocument tb)
          mapM childToHTMLNode docNodes
    _ -> case reverse openElems of
      [] -> do
        docNodes <- readIORef (tbDocument tb)
        mapM childToHTMLNode docNodes
      (htmlElem:_) -> do
        children <- readIORef (nodeChildren htmlElem)
        mapM tbNodeToHTMLNode (reverse children)
  where
    getNodeChildren node = do
      let ns = nodeNs node
      case tbFragmentContext tb of
        Just (ctxTag, Just ctxNs) | nodeName node == ctxTag && ns == Just ctxNs -> do
          children <- readIORef (nodeChildren node)
          pure (reverse children)
        _ -> pure [node]

tbNodeToHTMLNode :: TBNode -> IO HTMLNode
tbNodeToHTMLNode node
  | nodeName node == "#text" = do
      attrs <- nodeAttrs node
      let txt = case lookup "#data" attrs of
            Just t -> t
            Nothing -> ""
      pure (HTMLText txt)
  | nodeName node == "#comment" = do
      attrs <- nodeAttrs node
      let txt = case lookup "#data" attrs of
            Just t -> t
            Nothing -> ""
      pure (HTMLComment txt)
  | nodeIsTemplate node = do
      tmplChildren <- readIORef (nodeTemplateContents node)
      children <- readIORef (nodeChildren node)
      attrs <- nodeAttrs node
      let actualChildren = if null tmplChildren then children else tmplChildren
      childHtml <- mapM tbNodeToHTMLNode (reverse actualChildren)
      let displayName = nameWithNs (nodeName node) (nodeNs node)
      pure $ HTMLElement displayName
        (V.fromList [HTMLAttribute n v | (n,v) <- attrs])
        (V.fromList childHtml)
  | otherwise = do
      children <- readIORef (nodeChildren node)
      attrs <- nodeAttrs node
      childHtml <- mapM tbNodeToHTMLNode (reverse children)
      let displayName = nameWithNs (nodeName node) (nodeNs node)
      pure $ HTMLElement displayName
        (V.fromList [HTMLAttribute n v | (n,v) <- attrs])
        (V.fromList childHtml)

childToHTMLNode :: ChildNode -> IO HTMLNode
childToHTMLNode (CElement node) = tbNodeToHTMLNode node
childToHTMLNode (CText t) = pure (HTMLText t)
childToHTMLNode (CComment t) = pure (HTMLComment t)
childToHTMLNode (CDoctype n p s) = pure (HTMLDoctype n p s)

extractDoctype :: [HTMLNode] -> Maybe Doctype
extractDoctype [] = Nothing
extractDoctype (HTMLDoctype n p s : _) = Just (Doctype (Just n) p s)
extractDoctype (_:rest) = extractDoctype rest

findOrCreateRoot :: [HTMLNode] -> HTMLNode
findOrCreateRoot nodes =
  case [n | n@(HTMLElement "html" _ _) <- nodes] of
    (r:_) -> r
    [] ->
      let nonDt = [n | n <- nodes, not (isDt n)]
      in HTMLElement "html" V.empty (V.fromList nonDt)
  where
    isDt (HTMLDoctype _ _ _) = True
    isDt _ = False

------------------------------------------------------------------------
-- Process a single token
------------------------------------------------------------------------

processToken :: TreeBuilder -> Token -> IO ()
processToken !tb tok = do
  ignoreLF <- readIORef (tbIgnoreLF tb)
  if ignoreLF
  then do
    writeIORef (tbIgnoreLF tb) False
    case tok of
      TChar '\n' -> pure ()
      _ -> dispatchToken tb tok
  else dispatchToken tb tok

dispatchToken :: TreeBuilder -> Token -> IO ()
dispatchToken tb tok = do
  openElems <- readIORef (tbOpenElements tb)
  case openElems of
    [] -> processInMode tb tok
    (current:_) ->
      let ns = nodeNs current
      in if ns == Nothing || ns == Just "" || ns == Just "html"
         then processInMode tb tok
         else do
           useForeign <- shouldUseForeignContent tb tok current
           if useForeign
           then processForeignContent tb tok
           else processInMode tb tok

shouldUseForeignContent :: TreeBuilder -> Token -> TBNode -> IO Bool
shouldUseForeignContent tb tok current = do
  let ns = nodeNs current
      name = nodeName current
  case ns of
    Nothing -> pure False
    Just "html" -> pure False
    Just "" -> pure False
    Just nsVal -> case tok of
      TEOF -> pure False
      TChar _ -> do
        if isMathMLTIP name nsVal
        then pure False
        else do
          hip <- isHTMLIP current nsVal
          if hip then pure False else pure True
      TStartTag tname _ _ -> do
        if isMathMLTIP name nsVal && tname /= "mglyph" && tname /= "malignmark"
        then pure False
        else if name == "annotation-xml" && nsVal == "math" && tname == "svg"
        then pure False
        else do
          hip <- isHTMLIP current nsVal
          if hip then pure False else pure True
      TComment _ -> pure True
      TEndTag _ -> pure True
      _ -> pure True
  where
    isMathMLTIP n ns = ns == "math" && n `elem` ["mi","mo","mn","ms","mtext"]
    isHTMLIP node ns
      | ns == "svg" = pure $ nodeName node `elem` ["foreignObject","desc","title"]
      | ns == "math" && nodeName node == "annotation-xml" = do
          attrs <- nodeAttrs node
          pure $ case lookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html","application/xhtml+xml"]
            Nothing -> False
      | otherwise = pure False

processInMode :: TreeBuilder -> Token -> IO ()
processInMode tb tok = do
  mode <- readIORef (tbMode tb)
  case mode of
    MInitial          -> modeInitial tb tok
    MBeforeHtml       -> modeBeforeHtml tb tok
    MBeforeHead       -> modeBeforeHead tb tok
    MInHead           -> modeInHead tb tok
    MInHeadNoscript   -> modeInHeadNoscript tb tok
    MAfterHead        -> modeAfterHead tb tok
    MInBody           -> modeInBody tb tok
    MText             -> modeText tb tok
    MInTable          -> modeInTable tb tok
    MInTableText      -> modeInTableText tb tok
    MInCaption        -> modeInCaption tb tok
    MInColumnGroup    -> modeInColumnGroup tb tok
    MInTableBody      -> modeInTableBody tb tok
    MInRow            -> modeInRow tb tok
    MInCell           -> modeInCell tb tok
    MInSelect         -> modeInSelect tb tok
    MInSelectInTable  -> modeInSelectInTable tb tok
    MInTemplate       -> modeInTemplate tb tok
    MAfterBody        -> modeAfterBody tb tok
    MInFrameset       -> modeInFrameset tb tok
    MAfterFrameset    -> modeAfterFrameset tb tok
    MAfterAfterBody   -> modeAfterAfterBody tb tok
    MAfterAfterFrameset -> modeAfterAfterFrameset tb tok

------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------

specialElements :: S.Set Text
specialElements = S.fromList
  [ "address","applet","area","article","aside","base","basefont","bgsound"
  , "blockquote","body","br","button","caption","center","col","colgroup"
  , "dd","details","dialog","dir","div","dl","dt","embed","fieldset"
  , "figcaption","figure","footer","form","frame","frameset"
  , "h1","h2","h3","h4","h5","h6","head","header","hgroup","hr","html"
  , "iframe","img","input","keygen","li","link","listing","main","marquee"
  , "menu","menuitem","meta","nav","noembed","noframes","noscript","object"
  , "ol","p","param","plaintext","pre","script","search","section","select"
  , "source","style","summary","table","tbody","td","template","textarea"
  , "tfoot","th","thead","title","tr","track","ul","wbr"
  ]

formattingElements :: S.Set Text
formattingElements = S.fromList
  ["a","b","big","code","em","font","i","nobr","s","small","strike","strong","tt","u"]

headingElements :: S.Set Text
headingElements = S.fromList ["h1","h2","h3","h4","h5","h6"]

impliedEndTags :: S.Set Text
impliedEndTags = S.fromList
  ["dd","dt","li","option","optgroup","p","rb","rp","rt","rtc"]

defaultScopeTerminators :: S.Set Text
defaultScopeTerminators = S.fromList
  ["applet","caption","html","table","td","th","marquee","object","template"]

buttonScopeTerminators :: S.Set Text
buttonScopeTerminators = S.insert "button" defaultScopeTerminators

listItemScopeTerminators :: S.Set Text
listItemScopeTerminators = S.fromList ["ol","ul"] `S.union` defaultScopeTerminators

definitionScopeTerminators :: S.Set Text
definitionScopeTerminators = S.insert "dl" defaultScopeTerminators

tableScopeTerminators :: S.Set Text
tableScopeTerminators = S.fromList ["html","table","template"]

foreignBreakoutElements :: S.Set Text
foreignBreakoutElements = S.fromList
  ["b","big","blockquote","body","br","center","code","dd","div","dl","dt"
  ,"em","embed","h1","h2","h3","h4","h5","h6","head","hr","i","img","li"
  ,"listing","menu","meta","nobr","ol","p","pre","ruby","s","small","span"
  ,"strong","strike","sub","sup","table","tt","u","ul","var"]

------------------------------------------------------------------------
-- Scope checking
------------------------------------------------------------------------

hasElementInScope :: Text -> S.Set Text -> TreeBuilder -> IO Bool
hasElementInScope target terminators tb = do
  elems <- readIORef (tbOpenElements tb)
  go elems
  where
    go [] = pure False
    go (node:rest)
      | nodeName node == target && isHTMLNs (nodeNs node) = pure True
      | isHTMLNs (nodeNs node) && nodeName node `S.member` terminators = pure False
      | not (isHTMLNs (nodeNs node)) = do
          isIP <- isForeignScopeTerminator node
          if isIP then pure False else go rest
      | otherwise = go rest
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"
    isForeignScopeTerminator node = do
      let ns = nodeNs node
          name = nodeName node
      case ns of
        Just "math" | name `elem` ["mi","mo","mn","ms","mtext"] -> pure True
        Just "math" | name == "annotation-xml" -> do
          attrs <- nodeAttrs node
          pure $ case lookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html","application/xhtml+xml"]
            Nothing -> False
        Just "svg" | name `elem` ["foreignObject","desc","title"] -> pure True
        _ -> pure False

hasInScope :: Text -> TreeBuilder -> IO Bool
hasInScope t = hasElementInScope t defaultScopeTerminators

hasInButtonScope :: Text -> TreeBuilder -> IO Bool
hasInButtonScope t = hasElementInScope t buttonScopeTerminators

hasInListItemScope :: Text -> TreeBuilder -> IO Bool
hasInListItemScope t = hasElementInScope t listItemScopeTerminators

hasInDefinitionScope :: Text -> TreeBuilder -> IO Bool
hasInDefinitionScope t = hasElementInScope t definitionScopeTerminators

hasInTableScope :: Text -> TreeBuilder -> IO Bool
hasInTableScope t = hasElementInScope t tableScopeTerminators

hasAnyInScope :: S.Set Text -> TreeBuilder -> IO Bool
hasAnyInScope targets tb = do
  elems <- readIORef (tbOpenElements tb)
  pure (go elems)
  where
    go [] = False
    go (node:rest)
      | nodeName node `S.member` targets && isHTMLNs (nodeNs node) = True
      | isHTMLNs (nodeNs node) && nodeName node `S.member` defaultScopeTerminators = False
      | otherwise = go rest
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"

------------------------------------------------------------------------
-- Stack helpers
------------------------------------------------------------------------

currentNode :: TreeBuilder -> IO (Maybe TBNode)
currentNode tb = do
  elems <- readIORef (tbOpenElements tb)
  pure (case elems of { (x:_) -> Just x; [] -> Nothing })

currentNodeName :: TreeBuilder -> IO Text
currentNodeName tb = do
  mn <- currentNode tb
  pure (maybe "" nodeName mn)

popElement :: TreeBuilder -> IO ()
popElement tb = do
  elems <- readIORef (tbOpenElements tb)
  case elems of
    (_:rest) -> writeIORef (tbOpenElements tb) rest
    [] -> pure ()

popUntilInclusive :: Text -> TreeBuilder -> IO ()
popUntilInclusive target tb = do
  elems <- readIORef (tbOpenElements tb)
  writeIORef (tbOpenElements tb) (go elems)
  where
    go [] = []
    go (node:rest)
      | nodeName node == target = rest
      | otherwise = go rest

popUntilOneOf :: S.Set Text -> TreeBuilder -> IO ()
popUntilOneOf targets tb = do
  elems <- readIORef (tbOpenElements tb)
  writeIORef (tbOpenElements tb) (go elems)
  where
    go [] = []
    go xs@(node:rest)
      | nodeName node `S.member` targets = xs
      | otherwise = go rest

isOnStack :: Text -> TreeBuilder -> IO Bool
isOnStack name tb = do
  elems <- readIORef (tbOpenElements tb)
  pure (any (\n -> nodeName n == name) elems)

------------------------------------------------------------------------
-- Insert operations
------------------------------------------------------------------------

appendChild :: TBNode -> TBNode -> IO ()
appendChild parent child = do
  writeIORef (nodeParent child) (Just parent)
  if nodeIsTemplate parent
  then modifyIORef' (nodeTemplateContents parent) (child:)
  else modifyIORef' (nodeChildren parent) (child:)

removeChild :: TBNode -> TBNode -> IO ()
removeChild parent child = do
  if nodeIsTemplate parent
  then modifyIORef' (nodeTemplateContents parent) (filter (/= child))
  else modifyIORef' (nodeChildren parent) (filter (/= child))
  writeIORef (nodeParent child) Nothing


insertElement :: TreeBuilder -> Text -> [(Text,Text)] -> Maybe Text -> IO TBNode
insertElement tb name attrs ns = do
  let isTmpl = name == "template" && (ns == Nothing || ns == Just "" || ns == Just "html")
  node <- newTBNode tb name attrs ns isTmpl
  insertFromTable <- readIORef (tbInsertFromTable tb)
  elems <- readIORef (tbOpenElements tb)
  case elems of
    (current:_)
      | insertFromTable && nodeName current `elem` ["table","tbody","tfoot","thead","tr"] ->
          fosterParentNode tb node
      | otherwise -> appendChild current node
    [] -> modifyIORef' (tbDocument tb) (++ [CElement node])
  modifyIORef' (tbOpenElements tb) (node:)
  pure node

insertVoidElement :: TreeBuilder -> Text -> [(Text,Text)] -> Maybe Text -> IO TBNode
insertVoidElement tb name attrs ns = do
  node <- insertElement tb name attrs ns
  popElement tb
  pure node

insertComment :: TreeBuilder -> Text -> IO ()
insertComment tb txt = do
  let commentText = fixCDATAComment txt
  elems <- readIORef (tbOpenElements tb)
  case elems of
    (current:_) -> do
      commentNode <- newTBNode tb "#comment" [("#data", commentText)] Nothing False
      appendChild current commentNode
    [] -> modifyIORef' (tbDocument tb) (++ [CComment commentText])

cdataMarker :: Text
cdataMarker = T.pack ['\xFFFE', 'C', 'D']

isCDATA :: Text -> Bool
isCDATA t = cdataMarker `T.isPrefixOf` t

cdataContent :: Text -> Text
cdataContent t = T.drop (T.length cdataMarker) t

fixCDATAComment :: Text -> Text
fixCDATAComment t
  | isCDATA t = "[CDATA[" <> cdataContent t <> "]]"
  | otherwise = t

insertCommentToDocument :: TreeBuilder -> Text -> IO ()
insertCommentToDocument tb txt =
  modifyIORef' (tbDocument tb) (++ [CComment (fixCDATAComment txt)])

insertDoctype :: TreeBuilder -> Text -> Maybe Text -> Maybe Text -> IO ()
insertDoctype tb name pub sys =
  modifyIORef' (tbDocument tb) (++ [CDoctype name pub sys])

------------------------------------------------------------------------
-- Foster parenting
------------------------------------------------------------------------

fosterParentText :: TreeBuilder -> Text -> IO ()
fosterParentText tb txt = do
  elems <- readIORef (tbOpenElements tb)
  case findLastTable elems of
    Just (tableNode, _parentAboveTable) -> do
      mPar <- readIORef (nodeParent tableNode)
      case mPar of
        Just parent -> do
          let childRef = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
          children <- readIORef childRef
          case findTextAfter tableNode children of
            Just textNode -> do
              prevTxt <- do
                a <- nodeAttrs textNode
                pure $ case lookup "#data" a of { Just t -> t; Nothing -> "" }
              writeIORef (nodeAttrsRef textNode) [("#data", prevTxt <> txt)]
            Nothing -> do
              textNode <- newTBNode tb "#text" [("#data", txt)] Nothing False
              insertBefore parent tableNode textNode
        Nothing ->
          appendTextToCurrentNode tb txt
    Nothing -> appendTextToCurrentNode tb txt

findTextAfter :: TBNode -> [TBNode] -> Maybe TBNode
findTextAfter ref (x:next:rest)
  | x == ref && nodeName next == "#text" = Just next
  | otherwise = findTextAfter ref (next:rest)
findTextAfter _ _ = Nothing

fosterParentNode :: TreeBuilder -> TBNode -> IO ()
fosterParentNode tb node = do
  elems <- readIORef (tbOpenElements tb)
  case findLastTable elems of
    Just (tableNode, _) -> do
      mPar <- readIORef (nodeParent tableNode)
      case mPar of
        Just parent -> insertBefore parent tableNode node
        Nothing -> case drop 1 elems of
          (above:_) -> appendChild above node
          [] -> pure ()
    Nothing -> case elems of
      (current:_) -> appendChild current node
      [] -> pure ()

findLastTable :: [TBNode] -> Maybe (TBNode, Maybe TBNode)
findLastTable [] = Nothing
findLastTable (node:rest)
  | nodeName node == "table" = Just (node, case rest of { (x:_) -> Just x; [] -> Nothing })
  | otherwise = findLastTable rest

insertBefore :: TBNode -> TBNode -> TBNode -> IO ()
insertBefore parent refNode newNode = do
  writeIORef (nodeParent newNode) (Just parent)
  let childRef = if nodeIsTemplate parent then nodeTemplateContents parent else nodeChildren parent
  children <- readIORef childRef
  let updated = insertAfterInList refNode newNode children
  writeIORef childRef updated

insertAfterInList :: TBNode -> TBNode -> [TBNode] -> [TBNode]
insertAfterInList ref new [] = [new]
insertAfterInList ref new (x:xs)
  | x == ref = x : new : xs
  | otherwise = x : insertAfterInList ref new xs

------------------------------------------------------------------------
-- More helpers
------------------------------------------------------------------------

generateImpliedEndTags :: Maybe Text -> TreeBuilder -> IO ()
generateImpliedEndTags mexclude tb = do
  elems <- readIORef (tbOpenElements tb)
  case elems of
    (node:_)
      | nodeName node `S.member` impliedEndTags && Just (nodeName node) /= mexclude -> do
          popElement tb
          generateImpliedEndTags mexclude tb
    _ -> pure ()

closePElement :: TreeBuilder -> IO ()
closePElement tb = do
  inScope <- hasInButtonScope "p" tb
  if inScope
  then do
    generateImpliedEndTags (Just "p") tb
    popUntilInclusive "p" tb
  else pure ()

clearStackToTableContext :: TreeBuilder -> IO ()
clearStackToTableContext tb = popUntilOneOf (S.fromList ["table","template","html"]) tb

clearStackToTableBodyContext :: TreeBuilder -> IO ()
clearStackToTableBodyContext tb = popUntilOneOf (S.fromList ["tbody","tfoot","thead","template","html"]) tb

clearStackToTableRowContext :: TreeBuilder -> IO ()
clearStackToTableRowContext tb = popUntilOneOf (S.fromList ["tr","template","html"]) tb

------------------------------------------------------------------------
-- Active formatting
------------------------------------------------------------------------

reconstructActiveFormatting :: TreeBuilder -> IO ()
reconstructActiveFormatting tb = do
  af <- readIORef (tbActiveFormatting tb)
  case af of
    [] -> pure ()
    (AFMarker:_) -> pure ()
    (AFEntry _ _ node : _) -> do
      openElems <- readIORef (tbOpenElements tb)
      if node `elem` openElems
      then pure ()
      else doReconstruct tb
    _ -> pure ()

doReconstruct :: TreeBuilder -> IO ()
doReconstruct tb = do
  af <- readIORef (tbActiveFormatting tb)
  openElems <- readIORef (tbOpenElements tb)
  let (toReinsert, _) = collectEntries af openElems
  mapM_ (reinsertEntry tb) toReinsert
  where
    collectEntries entries openElems = go [] entries
      where
        go acc [] = (acc, [])
        go acc (AFMarker:_) = (acc, [])
        go acc (e@(AFEntry _ _ node):rest)
          | node `elem` openElems = (acc, [])
          | otherwise = go (e:acc) rest

    reinsertEntry tb' (AFEntry name attrs _) = do
      node <- insertElement tb' name attrs Nothing
      modifyIORef' (tbActiveFormatting tb') (updateEntry name node)
    reinsertEntry _ _ = pure ()

    updateEntry name newNode (AFEntry n a _:rest)
      | n == name = AFEntry n a newNode : rest
    updateEntry name newNode (x:rest) = x : updateEntry name newNode rest
    updateEntry _ _ [] = []

pushFormattingMarker :: TreeBuilder -> IO ()
pushFormattingMarker tb =
  modifyIORef' (tbActiveFormatting tb) (AFMarker:)

pushFormattingEntry :: Text -> [(Text,Text)] -> TBNode -> TreeBuilder -> IO ()
pushFormattingEntry name attrs node tb = do
  af <- readIORef (tbActiveFormatting tb)
  let cleaned = removeExcess name attrs af
  writeIORef (tbActiveFormatting tb) (AFEntry name attrs node : cleaned)

removeExcess :: Text -> [(Text,Text)] -> [AFEntry] -> [AFEntry]
removeExcess name attrs entries =
  let (beforeMarker, _) = break isMarker entries
      matching = [i | (i, AFEntry n a _) <- zip [0..] beforeMarker, n == name, sameAttrs a attrs]
  in if length matching >= 3
     then removeAt (last matching) entries
     else entries
  where
    isMarker AFMarker = True
    isMarker _ = False
    sameAttrs a b = sortBy (comparing fst) a == sortBy (comparing fst) b

removeAt :: Int -> [a] -> [a]
removeAt _ [] = []
removeAt 0 (_:xs) = xs
removeAt n (x:xs) = x : removeAt (n-1) xs

clearActiveFormattingToMarker :: TreeBuilder -> IO ()
clearActiveFormattingToMarker tb =
  modifyIORef' (tbActiveFormatting tb) go
  where
    go [] = []
    go (AFMarker:rest) = rest
    go (_:rest) = go rest

hasActiveFormattingEntry :: Text -> TreeBuilder -> IO Bool
hasActiveFormattingEntry name tb = do
  af <- readIORef (tbActiveFormatting tb)
  pure (go af)
  where
    go [] = False
    go (AFMarker:_) = False
    go (AFEntry n _ _:rest)
      | n == name = True
      | otherwise = go rest

------------------------------------------------------------------------
-- Adoption agency algorithm
------------------------------------------------------------------------

adoptionAgency :: Text -> TreeBuilder -> IO ()
adoptionAgency subject tb = do
  cn <- currentNodeName tb
  hasAF <- hasActiveFormattingEntry subject tb
  if cn == subject && not hasAF
  then popUntilInclusive subject tb
  else outerLoop 0
  where
    outerLoop :: Int -> IO ()
    outerLoop !iter
      | iter >= 8 = pure ()
      | otherwise = do
          af <- readIORef (tbActiveFormatting tb)
          case findFormattingElement subject af of
            Nothing -> anyOtherEndTag subject tb
            Just (fmtIdx, AFEntry _ fmtAttrs fmtNode) -> do
              openElems <- readIORef (tbOpenElements tb)
              if fmtNode `notElem` openElems
              then do
                modifyIORef' (tbActiveFormatting tb) (removeAtIdx fmtIdx)
              else do
                inScope <- hasInScope subject tb
                if not inScope then pure ()
                else do
                  let mFb = findFurthestBlock fmtNode openElems
                  case mFb of
                    Nothing -> do
                      elems2 <- readIORef (tbOpenElements tb)
                      writeIORef (tbOpenElements tb) (dropThrough' fmtNode elems2)
                      modifyIORef' (tbActiveFormatting tb) (removeAtIdx fmtIdx)
                    Just furthestBlock -> do
                      doAdoption fmtNode fmtIdx fmtAttrs furthestBlock
                      outerLoop (iter+1)
            _ -> pure ()

    doAdoption :: TBNode -> Int -> [(Text,Text)] -> TBNode -> IO ()
    doAdoption fmtNode fmtIdx fmtAttrs furthestBlock = do
      openElems <- readIORef (tbOpenElements tb)
      let fmtStackIdx = case elemIndex fmtNode openElems of { Just i -> i; Nothing -> 0 }
          commonAncestor = if fmtStackIdx + 1 < length openElems
                           then openElems !! (fmtStackIdx + 1)
                           else fmtNode
      let bookmark0 = fmtIdx

      let fbStackIdx = case elemIndex furthestBlock openElems of { Just i -> i; Nothing -> 0 }
      (lastNode, finalBookmark) <- innerLoop 0 furthestBlock fmtNode bookmark0 furthestBlock

      mp <- readIORef (nodeParent lastNode)
      case mp of
        Just parent -> removeChild parent lastNode
        Nothing -> pure ()

      if nodeIsTemplate commonAncestor
      then modifyIORef' (nodeTemplateContents commonAncestor) (lastNode:)
      else modifyIORef' (nodeChildren commonAncestor) (lastNode:)
      writeIORef (nodeParent lastNode) (Just commonAncestor)

      newFmtNode <- newTBNode tb (nodeName fmtNode) fmtAttrs (nodeNs fmtNode) False

      fbChildren <- readIORef (nodeChildren furthestBlock)
      writeIORef (nodeChildren furthestBlock) []
      mapM_ (\child -> writeIORef (nodeParent child) (Just newFmtNode)) fbChildren
      writeIORef (nodeChildren newFmtNode) fbChildren
      appendChild furthestBlock newFmtNode

      af2 <- readIORef (tbActiveFormatting tb)
      let af3 = removeAtIdx fmtIdx af2
          newEntry = AFEntry (nodeName fmtNode) fmtAttrs newFmtNode
          insertPos = min finalBookmark (length af3)
          af4 = insertAtIdx insertPos newEntry af3
      writeIORef (tbActiveFormatting tb) af4

      openElems2 <- readIORef (tbOpenElements tb)
      let elems3 = filter (/= fmtNode) openElems2
      case elemIndex furthestBlock elems3 of
        Just idx -> writeIORef (tbOpenElements tb)
          (take idx elems3 ++ [newFmtNode] ++ drop idx elems3)
        Nothing -> writeIORef (tbOpenElements tb) (newFmtNode : elems3)

    innerLoop :: Int -> TBNode -> TBNode -> Int -> TBNode -> IO (TBNode, Int)
    innerLoop !count nodeRef fmtNode' bookmark fb = do
      openElems <- readIORef (tbOpenElements tb)
      let nodeRefIdx = case elemIndex nodeRef openElems of { Just i -> i; Nothing -> 0 }
          nextIdx = nodeRefIdx + 1
      if nextIdx >= length openElems
      then pure (nodeRef, bookmark)
      else do
        let node = openElems !! nextIdx
            newCount = count + 1
        if node == fmtNode'
        then pure (nodeRef, bookmark)
        else do
          af <- readIORef (tbActiveFormatting tb)
          let mAfIdx = findAFIndex node af
          case mAfIdx of
            Just afIdx | newCount > 3 -> do
              modifyIORef' (tbActiveFormatting tb) (removeAtIdx afIdx)
              let newBookmark = if afIdx < bookmark then bookmark - 1 else bookmark
              removeNodeFromStack node tb
              innerLoop newCount nodeRef fmtNode' newBookmark fb
            Nothing -> do
              removeNodeFromStack node tb
              innerLoop newCount nodeRef fmtNode' bookmark fb
            Just afIdx -> do
              af2 <- readIORef (tbActiveFormatting tb)
              let AFEntry eName eAttrs _ = af2 !! afIdx
              newElem <- newTBNode tb eName eAttrs (nodeNs node) False
              modifyIORef' (tbActiveFormatting tb) (\afs ->
                take afIdx afs ++ [AFEntry eName eAttrs newElem] ++ drop (afIdx+1) afs)
              openElems2 <- readIORef (tbOpenElements tb)
              let idx2 = case elemIndex node openElems2 of { Just i -> i; Nothing -> nextIdx }
              writeIORef (tbOpenElements tb)
                (take idx2 openElems2 ++ [newElem] ++ drop (idx2+1) openElems2)
              let newBookmark = if nodeRef == fb then afIdx + 1 else bookmark
              mpLast <- readIORef (nodeParent nodeRef)
              case mpLast of
                Just p -> removeChild p nodeRef
                Nothing -> pure ()
              appendChild newElem nodeRef
              innerLoop newCount newElem fmtNode' newBookmark fb

    dropThrough' :: TBNode -> [TBNode] -> [TBNode]
    dropThrough' target (x:xs) | x == target = xs
    dropThrough' target (_:xs) = dropThrough' target xs
    dropThrough' _ [] = []

findFormattingElement :: Text -> [AFEntry] -> Maybe (Int, AFEntry)
findFormattingElement _ [] = Nothing
findFormattingElement subject (AFMarker:_) = Nothing
findFormattingElement subject entries = go 0 entries
  where
    go _ [] = Nothing
    go _ (AFMarker:_) = Nothing
    go i (e@(AFEntry n _ _):rest)
      | n == subject = Just (i, e)
      | otherwise = go (i+1) rest

findFurthestBlock :: TBNode -> [TBNode] -> Maybe TBNode
findFurthestBlock fmtNode openElems =
  let aboveFmt = takeWhile (/= fmtNode) openElems
      specialOnes = filter isSpecial aboveFmt
  in case specialOnes of
    [] -> Nothing
    _ -> Just (last specialOnes)
  where
    isSpecial n = isHTMLNs (nodeNs n) && nodeName n `S.member` specialElements
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"

removeAtIdx :: Int -> [a] -> [a]
removeAtIdx _ [] = []
removeAtIdx 0 (_:xs) = xs
removeAtIdx n (x:xs) = x : removeAtIdx (n-1) xs

runAdoptionInner :: TBNode -> Int -> [(Text,Text)] -> TBNode -> TreeBuilder -> IO ()
runAdoptionInner fmtNode fmtIdx fmtAttrs furthestBlock tb = do
  openElems <- readIORef (tbOpenElements tb)
  let fmtStackIdx = elemIndex fmtNode openElems
      fbStackIdx = elemIndex furthestBlock openElems
  case (fmtStackIdx, fbStackIdx) of
    (Just fi, Just fbi) -> do
      let commonAncestor = openElems !! (fi + 1)
      af <- readIORef (tbActiveFormatting tb)
      let bookmark = fmtIdx
      innerLoop 0 (fbi - 1) bookmark fi fbi commonAncestor
    _ -> pure ()
  where
    innerLoop :: Int -> Int -> Int -> Int -> Int -> TBNode -> IO ()
    innerLoop !innerCount !nodeIdx !bookmark !fmtSIdx !fbSIdx !commonAnc
      | innerCount >= 3 = finishAdoption bookmark fmtSIdx commonAnc
      | otherwise = do
          openElems <- readIORef (tbOpenElements tb)
          if nodeIdx <= fmtSIdx || nodeIdx >= length openElems
          then finishAdoption bookmark fmtSIdx commonAnc
          else do
            let node = openElems !! nodeIdx
            af <- readIORef (tbActiveFormatting tb)
            let mafIdx = findAFIndex node af
            case mafIdx of
              Nothing -> do
                removeNodeFromStack node tb
                innerLoop innerCount (nodeIdx - 1) bookmark fmtSIdx fbSIdx commonAnc
              Just afIdx -> do
                finishAdoption bookmark fmtSIdx commonAnc
    finishAdoption :: Int -> Int -> TBNode -> IO ()
    finishAdoption bookmark fmtSIdx commonAnc = do
      newFmtNode <- newTBNode tb (nodeName fmtNode) fmtAttrs (nodeNs fmtNode) False
      fbChildren <- readIORef (nodeChildren furthestBlock)
      writeIORef (nodeChildren furthestBlock) []
      mapM_ (\child -> do
        writeIORef (nodeParent child) (Just newFmtNode)
        modifyIORef' (nodeChildren newFmtNode) (child:)) (reverse fbChildren)
      appendChild furthestBlock newFmtNode
      modifyIORef' (tbActiveFormatting tb) (\af ->
        let af1 = removeAtIdx fmtIdx af
            newEntry = AFEntry (nodeName fmtNode) fmtAttrs newFmtNode
        in insertAtIdx (min bookmark (length af1)) newEntry af1)
      removeNodeFromStack fmtNode tb
      openElems2 <- readIORef (tbOpenElements tb)
      let fbIdx2 = elemIndex furthestBlock openElems2
      case fbIdx2 of
        Just idx -> writeIORef (tbOpenElements tb)
          (take idx openElems2 ++ [newFmtNode] ++ drop idx openElems2)
        Nothing -> pure ()
      -- Reparent fmtNode's later children into commonAncestor
      mp <- readIORef (nodeParent fmtNode)
      case mp of
        Just parent -> removeChild parent fmtNode
        Nothing -> pure ()
      appendChild commonAnc furthestBlock
      mp2 <- readIORef (nodeParent furthestBlock)
      case mp2 of
        Just oldParent | oldParent /= commonAnc -> removeChild oldParent furthestBlock
        _ -> pure ()

findAFIndex :: TBNode -> [AFEntry] -> Maybe Int
findAFIndex _ [] = Nothing
findAFIndex node entries = go 0 entries
  where
    go _ [] = Nothing
    go _ (AFMarker:_) = Nothing
    go i (AFEntry _ _ n : rest)
      | n == node = Just i
      | otherwise = go (i+1) rest

insertAtIdx :: Int -> a -> [a] -> [a]
insertAtIdx 0 x xs = x : xs
insertAtIdx n x (y:ys) = y : insertAtIdx (n-1) x ys
insertAtIdx _ x [] = [x]

removeNodeFromStack :: TBNode -> TreeBuilder -> IO ()
removeNodeFromStack node tb =
  modifyIORef' (tbOpenElements tb) (filter (/= node))

elemIndex :: TBNode -> [TBNode] -> Maybe Int
elemIndex _ [] = Nothing
elemIndex target xs = go 0 xs
  where
    go _ [] = Nothing
    go i (x:rest)
      | x == target = Just i
      | otherwise = go (i+1) rest

anyOtherEndTag :: Text -> TreeBuilder -> IO ()
anyOtherEndTag name tb = do
  elems <- readIORef (tbOpenElements tb)
  go elems
  where
    go [] = pure ()
    go (node:rest)
      | nodeName node == name = do
          generateImpliedEndTags (Just name) tb
          elems <- readIORef (tbOpenElements tb)
          writeIORef (tbOpenElements tb) (dropThrough node elems)
      | isHTMLNs (nodeNs node) && nodeName node `S.member` specialElements = pure ()
      | otherwise = go rest
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"
    dropThrough target (x:xs) | x == target = xs
    dropThrough target (_:xs) = dropThrough target xs
    dropThrough _ [] = []

------------------------------------------------------------------------
-- Reset insertion mode
------------------------------------------------------------------------

resetInsertionMode :: TreeBuilder -> IO ()
resetInsertionMode tb = do
  elems <- readIORef (tbOpenElements tb)
  go elems
  where
    go [] = writeIORef (tbMode tb) MInBody
    go [node] = void (check node True)
    go (node:rest) = do
      done <- check node False
      if done then pure () else go rest
    check node isLast = do
      let name = nodeName node
      case name of
        "select" -> do
          allElems <- readIORef (tbOpenElements tb)
          if any (\n -> nodeName n == "table" || nodeName n == "template") allElems
          then writeIORef (tbMode tb) MInSelectInTable
          else writeIORef (tbMode tb) MInSelect
          pure True
        "td" -> writeIORef (tbMode tb) (if isLast then MInBody else MInCell) >> pure True
        "th" -> writeIORef (tbMode tb) (if isLast then MInBody else MInCell) >> pure True
        "tr" -> writeIORef (tbMode tb) MInRow >> pure True
        "tbody" -> writeIORef (tbMode tb) MInTableBody >> pure True
        "thead" -> writeIORef (tbMode tb) MInTableBody >> pure True
        "tfoot" -> writeIORef (tbMode tb) MInTableBody >> pure True
        "caption" -> writeIORef (tbMode tb) MInCaption >> pure True
        "colgroup" -> writeIORef (tbMode tb) MInColumnGroup >> pure True
        "table" -> writeIORef (tbMode tb) MInTable >> pure True
        "template" -> do
          tms <- readIORef (tbTemplateModes tb)
          case tms of
            (m:_) -> writeIORef (tbMode tb) m
            [] -> writeIORef (tbMode tb) MInTemplate
          pure True
        "head" -> writeIORef (tbMode tb) (if isLast then MInBody else MInHead) >> pure True
        "body" -> writeIORef (tbMode tb) MInBody >> pure True
        "frameset" -> writeIORef (tbMode tb) MInFrameset >> pure True
        "html" -> do
          mHead <- readIORef (tbHeadElement tb)
          case mHead of
            Nothing -> writeIORef (tbMode tb) MBeforeHead
            Just _ -> writeIORef (tbMode tb) MAfterHead
          pure True
        _ -> if isLast
             then writeIORef (tbMode tb) MInBody >> pure True
             else pure False

------------------------------------------------------------------------
-- Insertion mode implementations
------------------------------------------------------------------------

isWS :: Char -> Bool
isWS c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'

addMissingAttrs :: TBNode -> [(Text,Text)] -> IO ()
addMissingAttrs node newAttrs = do
  existingAttrs <- nodeAttrs node
  let existingNames = S.fromList [n | (n,_) <- existingAttrs]
      toAdd = [(n,v) | (n,v) <- newAttrs, not (S.member n existingNames)]
  if null toAdd
  then pure ()
  else writeIORef (nodeAttrsRef node) (existingAttrs ++ toAdd)

modeInitial :: TreeBuilder -> Token -> IO ()
modeInitial tb tok = case tok of
  TChar c | isWS c -> pure ()
  TComment t -> insertCommentToDocument tb t
  TDoctype name pub sys fq -> do
    insertDoctype tb (T.toLower name) pub sys
    let qm = determineQuirksMode name pub sys fq
    writeIORef (tbQuirksMode tb) qm
    writeIORef (tbMode tb) MBeforeHtml
  _ -> do
    writeIORef (tbQuirksMode tb) "quirks"
    writeIORef (tbMode tb) MBeforeHtml
    modeBeforeHtml tb tok

modeBeforeHtml :: TreeBuilder -> Token -> IO ()
modeBeforeHtml tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> pure ()
  TChar c | isWS c -> pure ()
  TStartTag "html" attrs _ -> do
    node <- newTBNode tb "html" attrs Nothing False
    modifyIORef' (tbDocument tb) (++ [CElement node])
    modifyIORef' (tbOpenElements tb) (node:)
    writeIORef (tbMode tb) MBeforeHead
  TEndTag name | name `elem` ["head","body","html","br"] -> do
    node <- newTBNode tb "html" [] Nothing False
    modifyIORef' (tbDocument tb) (++ [CElement node])
    modifyIORef' (tbOpenElements tb) (node:)
    writeIORef (tbMode tb) MBeforeHead
    processInMode tb tok
  TEndTag _ -> pure ()
  _ -> do
    node <- newTBNode tb "html" [] Nothing False
    modifyIORef' (tbDocument tb) (++ [CElement node])
    modifyIORef' (tbOpenElements tb) (node:)
    writeIORef (tbMode tb) MBeforeHead
    processInMode tb tok

modeBeforeHead :: TreeBuilder -> Token -> IO ()
modeBeforeHead tb tok = case tok of
  TChar c | isWS c -> pure ()
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" attrs _ -> modeInBody tb tok
  TStartTag "head" attrs _ -> do
    node <- insertElement tb "head" attrs Nothing
    writeIORef (tbHeadElement tb) (Just node)
    writeIORef (tbMode tb) MInHead
  TEndTag name | name `elem` ["head","body","html","br"] -> do
    node <- insertElement tb "head" [] Nothing
    writeIORef (tbHeadElement tb) (Just node)
    writeIORef (tbMode tb) MInHead
    modeInHead tb tok
  TEndTag _ -> pure ()
  _ -> do
    node <- insertElement tb "head" [] Nothing
    writeIORef (tbHeadElement tb) (Just node)
    writeIORef (tbMode tb) MInHead
    modeInHead tb tok

modeInHead :: TreeBuilder -> Token -> IO ()
modeInHead tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag name attrs _ | name `elem` ["base","basefont","bgsound","link","meta"] ->
    void $ insertVoidElement tb name attrs Nothing
  TStartTag "title" attrs _ -> do
    void $ insertElement tb "title" attrs Nothing
    curMode <- readIORef (tbMode tb)
    writeIORef (tbOriginalMode tb) curMode
    writeIORef (tbMode tb) MText
  TStartTag "noscript" attrs _ -> do
    void $ insertElement tb "noscript" attrs Nothing
    curMode <- readIORef (tbMode tb)
    writeIORef (tbOriginalMode tb) curMode
    writeIORef (tbMode tb) MText
  TStartTag "noframes" attrs _ -> do
    void $ insertElement tb "noframes" attrs Nothing
    curMode <- readIORef (tbMode tb)
    writeIORef (tbOriginalMode tb) curMode
    writeIORef (tbMode tb) MText
  TStartTag "style" attrs _ -> do
    void $ insertElement tb "style" attrs Nothing
    curMode <- readIORef (tbMode tb)
    writeIORef (tbOriginalMode tb) curMode
    writeIORef (tbMode tb) MText
  TStartTag "script" attrs _ -> do
    void $ insertElement tb "script" attrs Nothing
    curMode <- readIORef (tbMode tb)
    writeIORef (tbOriginalMode tb) curMode
    writeIORef (tbMode tb) MText
  TStartTag "template" attrs _ -> do
    void $ insertElement tb "template" attrs Nothing
    pushFormattingMarker tb
    writeIORef (tbFramesetOk tb) False
    writeIORef (tbMode tb) MInTemplate
    modifyIORef' (tbTemplateModes tb) (MInTemplate:)
  TEndTag "template" -> do
    onStack <- isOnStack "template" tb
    if not onStack then pure ()
    else do
      generateImpliedEndTags Nothing tb
      popUntilInclusive "template" tb
      clearActiveFormattingToMarker tb
      modifyIORef' (tbTemplateModes tb) safeTail
      resetInsertionMode tb
  TStartTag "head" _ _ -> pure ()
  TEndTag name | name `elem` ["body","html","br"] -> do
    popElement tb
    writeIORef (tbMode tb) MAfterHead
    processInMode tb tok
  TEndTag "head" -> do
    popElement tb
    writeIORef (tbMode tb) MAfterHead
  TEndTag _ -> pure ()
  _ -> do
    popElement tb
    writeIORef (tbMode tb) MAfterHead
    processInMode tb tok

modeInHeadNoscript :: TreeBuilder -> Token -> IO ()
modeInHeadNoscript tb tok = case tok of
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TEndTag "noscript" -> do
    popElement tb
    writeIORef (tbMode tb) MInHead
  TChar c | isWS c -> modeInHead tb tok
  TComment _ -> modeInHead tb tok
  TStartTag name _ _ | name `elem` ["basefont","bgsound","link","meta","noframes","style"] ->
    modeInHead tb tok
  TEndTag "br" -> do
    popElement tb
    writeIORef (tbMode tb) MInHead
    processInMode tb tok
  TStartTag name _ _ | name == "head" || name == "noscript" -> pure ()
  TEndTag _ -> pure ()
  _ -> do
    popElement tb
    writeIORef (tbMode tb) MInHead
    processInMode tb tok

modeAfterHead :: TreeBuilder -> Token -> IO ()
modeAfterHead tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag "body" attrs _ -> do
    void $ insertElement tb "body" attrs Nothing
    writeIORef (tbFramesetOk tb) False
    writeIORef (tbMode tb) MInBody
  TStartTag "frameset" attrs _ -> do
    void $ insertElement tb "frameset" attrs Nothing
    writeIORef (tbMode tb) MInFrameset
  TStartTag name _ _ | name `elem` ["base","basefont","bgsound","link","meta","noframes","script","style","title"] -> do
    mHead <- readIORef (tbHeadElement tb)
    case mHead of
      Just headNode -> do
        modifyIORef' (tbOpenElements tb) (headNode:)
        modeInHead tb tok
        modifyIORef' (tbOpenElements tb) (filter (/= headNode))
      Nothing -> modeInHead tb tok
  TStartTag "template" _ _ -> do
    mHead <- readIORef (tbHeadElement tb)
    case mHead of
      Just headNode -> do
        modifyIORef' (tbOpenElements tb) (headNode:)
        writeIORef (tbMode tb) MInHead
        processInMode tb tok
      Nothing -> modeInHead tb tok
  TEndTag "template" -> modeInHead tb tok
  TEndTag name | name `elem` ["body","html","br"] -> do
    void $ insertElement tb "body" [] Nothing
    writeIORef (tbMode tb) MInBody
    processInMode tb tok
  TStartTag "head" _ _ -> pure ()
  TEndTag _ -> pure ()
  _ -> do
    void $ insertElement tb "body" [] Nothing
    writeIORef (tbMode tb) MInBody
    processInMode tb tok

modeInBody :: TreeBuilder -> Token -> IO ()
modeInBody tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c | isWS c -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb (T.singleton c)
  TChar c -> do
    reconstructActiveFormatting tb
    appendTextToCurrentNode tb (T.singleton c)
    writeIORef (tbFramesetOk tb) False
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TEOF -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms)
    then modeInTemplate tb tok
    else pure ()
  TStartTag "html" attrs _ -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms) then pure ()
    else do
      elems <- readIORef (tbOpenElements tb)
      case reverse elems of
        (htmlNode:_) -> addMissingAttrs htmlNode attrs
        _ -> pure ()
  TStartTag "body" attrs _ -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms) then pure ()
    else do
      elems <- readIORef (tbOpenElements tb)
      case reverse elems of
        (_:bodyNode:_) | nodeName bodyNode == "body" -> do
          addMissingAttrs bodyNode attrs
          writeIORef (tbFramesetOk tb) False
        _ -> pure ()
  TStartTag "frameset" attrs _ -> do
    fo <- readIORef (tbFramesetOk tb)
    if not fo then pure ()
    else do
      elems <- readIORef (tbOpenElements tb)
      case reverse elems of
        (htmlNode:bodyNode:_) | nodeName bodyNode == "body" -> do
          removeChild htmlNode bodyNode
          writeIORef (tbOpenElements tb) [htmlNode]
          void $ insertElement tb "frameset" attrs Nothing
          writeIORef (tbMode tb) MInFrameset
        _ -> do
          popUntilOneOf (S.singleton "html") tb
          void $ insertElement tb "frameset" attrs Nothing
          writeIORef (tbMode tb) MInFrameset
  TStartTag name attrs _sc
    | name `elem` ["address","article","aside","blockquote","center","details","dialog","dir","div","dl","fieldset","figcaption","figure","footer","header","hgroup","main","menu","nav","ol","p","search","section","summary","ul"] -> do
        closePElement tb
        void $ insertElement tb name attrs Nothing
    | name `S.member` headingElements -> do
        closePElement tb
        cn <- currentNodeName tb
        if cn `S.member` headingElements
        then popElement tb >> void (insertElement tb name attrs Nothing)
        else void $ insertElement tb name attrs Nothing
    | name == "pre" || name == "listing" -> do
        closePElement tb
        void $ insertElement tb name attrs Nothing
        writeIORef (tbFramesetOk tb) False
        writeIORef (tbIgnoreLF tb) True
    | name == "form" -> do
        mForm <- readIORef (tbFormElement tb)
        onStack <- isOnStack "template" tb
        if mForm /= Nothing && not onStack
        then pure ()
        else do
          closePElement tb
          node <- insertElement tb "form" attrs Nothing
          if not onStack then writeIORef (tbFormElement tb) (Just node) else pure ()
    | name == "li" -> do
        writeIORef (tbFramesetOk tb) False
        closeLiElements tb
        closePElement tb
        void $ insertElement tb "li" attrs Nothing
    | name == "dd" || name == "dt" -> do
        writeIORef (tbFramesetOk tb) False
        closeDdDtElements tb
        closePElement tb
        void $ insertElement tb name attrs Nothing
    | name == "plaintext" -> do
        closePElement tb
        void $ insertElement tb "plaintext" attrs Nothing
    | name == "button" -> do
        inScope <- hasInScope "button" tb
        if inScope
        then do
          generateImpliedEndTags Nothing tb
          popUntilInclusive "button" tb
          reconstructActiveFormatting tb
          node <- insertElement tb "button" attrs Nothing
          writeIORef (tbFramesetOk tb) False
        else do
          reconstructActiveFormatting tb
          void $ insertElement tb "button" attrs Nothing
          writeIORef (tbFramesetOk tb) False
    | name == "a" -> do
        hasA <- hasActiveFormattingEntry "a" tb
        if hasA
        then do
          adoptionAgency "a" tb
          removeActiveFormattingByName "a" tb
          removeNameFromStack "a" tb
        else pure ()
        reconstructActiveFormatting tb
        node <- insertElement tb "a" attrs Nothing
        pushFormattingEntry "a" attrs node tb
    | name `S.member` formattingElements -> do
        reconstructActiveFormatting tb
        node <- insertElement tb name attrs Nothing
        pushFormattingEntry name attrs node tb
    | name == "nobr" -> do
        reconstructActiveFormatting tb
        inScope <- hasInScope "nobr" tb
        if inScope
        then do
          adoptionAgency "nobr" tb
          reconstructActiveFormatting tb
        else pure ()
        node <- insertElement tb "nobr" attrs Nothing
        pushFormattingEntry "nobr" attrs node tb
    | name `elem` ["applet","marquee","object"] -> do
        reconstructActiveFormatting tb
        void $ insertElement tb name attrs Nothing
        pushFormattingMarker tb
        writeIORef (tbFramesetOk tb) False
    | name == "table" -> do
        qm <- readIORef (tbQuirksMode tb)
        if qm /= "quirks" then closePElement tb else pure ()
        void $ insertElement tb "table" attrs Nothing
        writeIORef (tbFramesetOk tb) False
        writeIORef (tbMode tb) MInTable
    | name `elem` ["area","br","embed","img","keygen","wbr"] -> do
        reconstructActiveFormatting tb
        void $ insertVoidElement tb name attrs Nothing
        writeIORef (tbFramesetOk tb) False
    | name == "input" -> do
        reconstructActiveFormatting tb
        void $ insertVoidElement tb "input" attrs Nothing
        let isHidden = case lookup "type" attrs of
              Just v -> T.toLower v == "hidden"
              Nothing -> False
        if not isHidden then writeIORef (tbFramesetOk tb) False else pure ()
    | name `elem` ["param","source","track"] ->
        void $ insertVoidElement tb name attrs Nothing
    | name == "hr" -> do
        closePElement tb
        void $ insertVoidElement tb "hr" attrs Nothing
        writeIORef (tbFramesetOk tb) False
    | name == "image" ->
        modeInBody tb (TStartTag "img" attrs _sc)
    | name == "textarea" -> do
        void $ insertElement tb "textarea" attrs Nothing
        writeIORef (tbFramesetOk tb) False
        writeIORef (tbIgnoreLF tb) True
    | name == "xmp" -> do
        closePElement tb
        reconstructActiveFormatting tb
        void $ insertElement tb "xmp" attrs Nothing
        writeIORef (tbFramesetOk tb) False
        writeIORef (tbOriginalMode tb) MInBody
        writeIORef (tbMode tb) MText
    | name == "iframe" -> do
        void $ insertElement tb "iframe" attrs Nothing
        writeIORef (tbFramesetOk tb) False
        writeIORef (tbOriginalMode tb) MInBody
        writeIORef (tbMode tb) MText
    | name == "noembed" -> do
        void $ insertElement tb "noembed" attrs Nothing
        writeIORef (tbOriginalMode tb) MInBody
        writeIORef (tbMode tb) MText
    | name == "select" -> do
        reconstructActiveFormatting tb
        void $ insertElement tb "select" attrs Nothing
        writeIORef (tbFramesetOk tb) False
        resetInsertionMode tb
    | name == "optgroup" || name == "option" -> do
        cn <- currentNodeName tb
        if cn == "option" then popElement tb else pure ()
        reconstructActiveFormatting tb
        void $ insertElement tb name attrs Nothing
    | name == "rb" || name == "rtc" -> do
        inScope <- hasInScope "ruby" tb
        if inScope then generateImpliedEndTags Nothing tb else pure ()
        void $ insertElement tb name attrs Nothing
    | name == "rp" || name == "rt" -> do
        inScope <- hasInScope "ruby" tb
        if inScope then generateImpliedEndTags (Just "rtc") tb else pure ()
        void $ insertElement tb name attrs Nothing
    | name == "math" -> do
        reconstructActiveFormatting tb
        let adjustedAttrs = adjustMathMLAttrs attrs
            fAttrs = adjustForeignAttrs adjustedAttrs
        if _sc
        then void $ insertVoidElement tb name fAttrs (Just "math")
        else void $ insertElement tb name fAttrs (Just "math")
    | name == "svg" -> do
        reconstructActiveFormatting tb
        let adjustedAttrs = adjustSVGAttrs attrs
            fAttrs = adjustForeignAttrs adjustedAttrs
        if _sc
        then void $ insertVoidElement tb "svg" fAttrs (Just "svg")
        else void $ insertElement tb "svg" fAttrs (Just "svg")
    | name `elem` ["caption","col","colgroup","frame","head","tbody","td","tfoot","th","thead","tr"] ->
        pure ()
    | name `elem` ["base","basefont","bgsound","link","meta","template","title","noframes","script","style"] ->
        modeInHead tb tok
    | name == "noscript" -> do
        reconstructActiveFormatting tb
        void $ insertElement tb name attrs Nothing
        writeIORef (tbFramesetOk tb) False
    | otherwise -> do
        reconstructActiveFormatting tb
        void $ insertElement tb name attrs Nothing
        writeIORef (tbFramesetOk tb) False

  TEndTag name
    | name == "body" -> do
        inScope <- hasInScope "body" tb
        if inScope then writeIORef (tbMode tb) MAfterBody else pure ()
    | name == "html" -> do
        inScope <- hasInScope "body" tb
        if inScope then do
          writeIORef (tbMode tb) MAfterBody
          processInMode tb tok
        else pure ()
    | name `elem` ["address","article","aside","blockquote","button","center","details","dialog","dir","div","dl","fieldset","figcaption","figure","footer","header","hgroup","listing","main","menu","nav","ol","pre","search","section","summary","ul"] -> do
        inScope <- hasInScope name tb
        if inScope then do
          generateImpliedEndTags Nothing tb
          popUntilInclusive name tb
        else pure ()
    | name == "form" -> do
        onStack <- isOnStack "template" tb
        mForm <- readIORef (tbFormElement tb)
        if not onStack && mForm /= Nothing
        then do
          writeIORef (tbFormElement tb) Nothing
          formOnStack <- isOnStack "form" tb
          if formOnStack then do
            generateImpliedEndTags Nothing tb
            removeNameFromStack "form" tb
          else pure ()
        else if onStack then do
          inScope <- hasInScope "form" tb
          if inScope then do
            generateImpliedEndTags Nothing tb
            popUntilInclusive "form" tb
          else pure ()
        else pure ()
    | name == "p" -> do
        inScope <- hasInButtonScope "p" tb
        if inScope then do
          generateImpliedEndTags (Just "p") tb
          popUntilInclusive "p" tb
        else do
          void $ insertElement tb "p" [] Nothing
          popUntilInclusive "p" tb
    | name == "li" -> do
        inScope <- hasInListItemScope "li" tb
        if inScope then do
          generateImpliedEndTags (Just "li") tb
          popUntilInclusive "li" tb
        else pure ()
    | name == "dd" || name == "dt" -> do
        inScope <- hasInDefinitionScope name tb
        if inScope then do
          generateImpliedEndTags (Just name) tb
          popUntilInclusive name tb
        else pure ()
    | name `S.member` headingElements -> do
        inScope <- hasAnyInScope headingElements tb
        if inScope then do
          generateImpliedEndTags Nothing tb
          popUntilHeading tb
        else pure ()
    | name `S.member` formattingElements || name == "a" ->
        adoptionAgency name tb
    | name `elem` ["applet","marquee","object"] -> do
        inScope <- hasInScope name tb
        if inScope then do
          generateImpliedEndTags Nothing tb
          popUntilInclusive name tb
          clearActiveFormattingToMarker tb
        else pure ()
    | name == "br" -> do
        reconstructActiveFormatting tb
        void $ insertVoidElement tb "br" [] Nothing
        writeIORef (tbFramesetOk tb) False
    | name == "template" -> modeInHead tb tok
    | otherwise -> anyOtherEndTag name tb
  _ -> pure ()

popUntilHeading :: TreeBuilder -> IO ()
popUntilHeading tb = do
  cn <- currentNodeName tb
  if cn `S.member` headingElements
  then popElement tb
  else do
    popElement tb
    cn2 <- currentNodeName tb
    if cn2 == "" then pure ()
    else popUntilHeading tb

closeLiElements :: TreeBuilder -> IO ()
closeLiElements tb = do
  inScope <- hasInListItemScope "li" tb
  if inScope then do
    generateImpliedEndTags (Just "li") tb
    popUntilInclusive "li" tb
  else pure ()

closeDdDtElements :: TreeBuilder -> IO ()
closeDdDtElements tb = do
  elems <- readIORef (tbOpenElements tb)
  go elems
  where
    go [] = pure ()
    go (node:rest)
      | nodeName node == "dd" || nodeName node == "dt" = do
          generateImpliedEndTags (Just (nodeName node)) tb
          popUntilInclusive (nodeName node) tb
      | isHTMLNs (nodeNs node) && nodeName node `S.member` specialElements
        && nodeName node `notElem` ["address","div","p"] = pure ()
      | otherwise = go rest
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"

removeActiveFormattingByName :: Text -> TreeBuilder -> IO ()
removeActiveFormattingByName name tb =
  modifyIORef' (tbActiveFormatting tb) go
  where
    go [] = []
    go (AFMarker:rest) = AFMarker : rest
    go (AFEntry n a nd : rest)
      | n == name = rest
      | otherwise = AFEntry n a nd : go rest

removeNameFromStack :: Text -> TreeBuilder -> IO ()
removeNameFromStack name tb =
  modifyIORef' (tbOpenElements tb) (filter (\n -> nodeName n /= name))

modeText :: TreeBuilder -> Token -> IO ()
modeText tb tok = case tok of
  TChar c -> appendTextToCurrentNode tb (T.singleton c)
  TEOF -> do
    popElement tb
    origMode <- readIORef (tbOriginalMode tb)
    writeIORef (tbMode tb) origMode
    processInMode tb tok
  TEndTag _ -> do
    popElement tb
    origMode <- readIORef (tbOriginalMode tb)
    writeIORef (tbMode tb) origMode
  _ -> pure ()

modeInTable :: TreeBuilder -> Token -> IO ()
modeInTable tb tok = case tok of
  TChar _ -> do
    cn <- currentNodeName tb
    if cn `elem` ["table","tbody","tfoot","thead","tr"]
    then do
      writeIORef (tbPendingTableText tb) []
      origMode <- readIORef (tbMode tb)
      writeIORef (tbOriginalMode tb) origMode
      writeIORef (tbMode tb) MInTableText
      modeInTableText tb tok
    else fosterParentToken tb tok
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "caption" attrs _ -> do
    clearStackToTableContext tb
    pushFormattingMarker tb
    void $ insertElement tb "caption" attrs Nothing
    writeIORef (tbMode tb) MInCaption
  TStartTag "colgroup" attrs _ -> do
    clearStackToTableContext tb
    void $ insertElement tb "colgroup" attrs Nothing
    writeIORef (tbMode tb) MInColumnGroup
  TStartTag "col" _ _ -> do
    clearStackToTableContext tb
    void $ insertElement tb "colgroup" [] Nothing
    writeIORef (tbMode tb) MInColumnGroup
    processInMode tb tok
  TStartTag name attrs _ | name `elem` ["tbody","tfoot","thead"] -> do
    clearStackToTableContext tb
    void $ insertElement tb name attrs Nothing
    writeIORef (tbMode tb) MInTableBody
  TStartTag name _ _ | name `elem` ["td","th","tr"] -> do
    clearStackToTableContext tb
    void $ insertElement tb "tbody" [] Nothing
    writeIORef (tbMode tb) MInTableBody
    processInMode tb tok
  TStartTag "table" _ _ -> do
    inScope <- hasInTableScope "table" tb
    if inScope then do
      popUntilInclusive "table" tb
      resetInsertionMode tb
      processInMode tb tok
    else pure ()
  TEndTag "table" -> do
    inScope <- hasInTableScope "table" tb
    if inScope then do
      popUntilInclusive "table" tb
      resetInsertionMode tb
    else pure ()
  TEndTag name | name `elem` ["body","caption","col","colgroup","html","tbody","td","tfoot","th","thead","tr"] ->
    pure ()
  TStartTag name _ _ | name `elem` ["style","script","template"] ->
    modeInHead tb tok
  TEndTag "template" -> modeInHead tb tok
  TStartTag "input" attrs _ ->
    case lookup "type" attrs of
      Just v | T.toLower v == "hidden" ->
        void $ insertVoidElement tb "input" attrs Nothing
      _ -> fosterParentToken tb tok
  TStartTag "form" attrs _ -> do
    mForm <- readIORef (tbFormElement tb)
    onStack <- isOnStack "template" tb
    if mForm == Nothing && not onStack
    then do
      node <- insertElement tb "form" attrs Nothing
      writeIORef (tbFormElement tb) (Just node)
      popElement tb
    else pure ()
  TEOF -> do
    tms <- readIORef (tbTemplateModes tb)
    if not (null tms)
    then modeInTemplate tb tok
    else pure ()
  _ -> fosterParentToken tb tok

fosterParentToken :: TreeBuilder -> Token -> IO ()
fosterParentToken tb tok = do
  writeIORef (tbInsertFromTable tb) True
  modeInBody tb tok
  writeIORef (tbInsertFromTable tb) False

modeInTableText :: TreeBuilder -> Token -> IO ()
modeInTableText tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c ->
    modifyIORef' (tbPendingTableText tb) (++ [c])
  _ -> do
    pending <- readIORef (tbPendingTableText tb)
    origMode <- readIORef (tbOriginalMode tb)
    writeIORef (tbMode tb) origMode
    writeIORef (tbPendingTableText tb) []
    if all isWS pending
    then do
      mapM_ (\c -> appendTextToCurrentNode tb (T.singleton c)) pending
      processInMode tb tok
    else do
      writeIORef (tbInsertFromTable tb) True
      mapM_ (\c -> do
        reconstructActiveFormatting tb
        appendTextToCurrentNode tb (T.singleton c)
        writeIORef (tbFramesetOk tb) False) pending
      writeIORef (tbInsertFromTable tb) False
      processInMode tb tok

modeInCaption :: TreeBuilder -> Token -> IO ()
modeInCaption tb tok = case tok of
  TEndTag "caption" -> do
    inScope <- hasInTableScope "caption" tb
    if inScope then do
      generateImpliedEndTags Nothing tb
      popUntilInclusive "caption" tb
      clearActiveFormattingToMarker tb
      writeIORef (tbMode tb) MInTable
    else pure ()
  TStartTag name _ _ | name `elem` ["caption","col","colgroup","tbody","td","tfoot","th","thead","tr"] -> do
    inScope <- hasInTableScope "caption" tb
    if inScope then do
      generateImpliedEndTags Nothing tb
      popUntilInclusive "caption" tb
      clearActiveFormattingToMarker tb
      writeIORef (tbMode tb) MInTable
      processInMode tb tok
    else pure ()
  TEndTag "table" -> do
    inScope <- hasInTableScope "caption" tb
    if inScope then do
      generateImpliedEndTags Nothing tb
      popUntilInclusive "caption" tb
      clearActiveFormattingToMarker tb
      writeIORef (tbMode tb) MInTable
      processInMode tb tok
    else pure ()
  TEndTag name | name `elem` ["body","col","colgroup","html","tbody","td","tfoot","th","thead","tr"] ->
    pure ()
  _ -> modeInBody tb tok

modeInColumnGroup :: TreeBuilder -> Token -> IO ()
modeInColumnGroup tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag "col" attrs _ -> void $ insertVoidElement tb "col" attrs Nothing
  TEndTag "colgroup" -> do
    cn <- currentNodeName tb
    if cn == "colgroup" then do
      popElement tb
      writeIORef (tbMode tb) MInTable
    else pure ()
  TEndTag "col" -> pure ()
  TStartTag "template" _ _ -> modeInHead tb tok
  TEndTag "template" -> modeInHead tb tok
  TEOF -> modeInBody tb tok
  _ -> do
    cn <- currentNodeName tb
    if cn == "colgroup" then do
      popElement tb
      writeIORef (tbMode tb) MInTable
      processInMode tb tok
    else pure ()

modeInTableBody :: TreeBuilder -> Token -> IO ()
modeInTableBody tb tok = case tok of
  TStartTag "tr" attrs _ -> do
    clearStackToTableBodyContext tb
    void $ insertElement tb "tr" attrs Nothing
    writeIORef (tbMode tb) MInRow
  TStartTag name attrs _ | name == "th" || name == "td" -> do
    clearStackToTableBodyContext tb
    void $ insertElement tb "tr" [] Nothing
    writeIORef (tbMode tb) MInRow
    processInMode tb tok
  TEndTag name | name `elem` ["tbody","tfoot","thead"] -> do
    inScope <- hasInTableScope name tb
    if inScope then do
      clearStackToTableBodyContext tb
      popElement tb
      writeIORef (tbMode tb) MInTable
    else pure ()
  TStartTag name _ _ | name `elem` ["caption","col","colgroup","tbody","tfoot","thead"] -> do
    tb1 <- hasInTableScope "tbody" tb
    tb2 <- hasInTableScope "thead" tb
    tb3 <- hasInTableScope "tfoot" tb
    if tb1 || tb2 || tb3 then do
      clearStackToTableBodyContext tb
      popElement tb
      writeIORef (tbMode tb) MInTable
      processInMode tb tok
    else pure ()
  TEndTag "table" -> do
    tb1 <- hasInTableScope "tbody" tb
    tb2 <- hasInTableScope "thead" tb
    tb3 <- hasInTableScope "tfoot" tb
    if tb1 || tb2 || tb3 then do
      clearStackToTableBodyContext tb
      popElement tb
      writeIORef (tbMode tb) MInTable
      processInMode tb tok
    else pure ()
  TEndTag name | name `elem` ["body","caption","col","colgroup","html","td","th","tr"] ->
    pure ()
  _ -> modeInTable tb tok

modeInRow :: TreeBuilder -> Token -> IO ()
modeInRow tb tok = case tok of
  TStartTag name attrs _ | name == "th" || name == "td" -> do
    clearStackToTableRowContext tb
    void $ insertElement tb name attrs Nothing
    pushFormattingMarker tb
    writeIORef (tbMode tb) MInCell
  TEndTag "tr" -> do
    inScope <- hasInTableScope "tr" tb
    if inScope then do
      clearStackToTableRowContext tb
      popElement tb
      writeIORef (tbMode tb) MInTableBody
    else pure ()
  TStartTag name _ _ | name `elem` ["caption","col","colgroup","tbody","tfoot","thead","tr"] -> do
    inScope <- hasInTableScope "tr" tb
    if inScope then do
      clearStackToTableRowContext tb
      popElement tb
      writeIORef (tbMode tb) MInTableBody
      processInMode tb tok
    else pure ()
  TEndTag "table" -> do
    inScope <- hasInTableScope "tr" tb
    if inScope then do
      clearStackToTableRowContext tb
      popElement tb
      writeIORef (tbMode tb) MInTableBody
      processInMode tb tok
    else pure ()
  TEndTag name | name `elem` ["tbody","tfoot","thead"] -> do
    inScope <- hasInTableScope name tb
    if inScope then do
      trScope <- hasInTableScope "tr" tb
      if trScope then do
        clearStackToTableRowContext tb
        popElement tb
        writeIORef (tbMode tb) MInTableBody
        processInMode tb tok
      else pure ()
    else pure ()
  TEndTag name | name `elem` ["body","caption","col","colgroup","html","td","th"] ->
    pure ()
  _ -> modeInTable tb tok

modeInCell :: TreeBuilder -> Token -> IO ()
modeInCell tb tok = case tok of
  TEndTag name | name == "td" || name == "th" -> do
    inScope <- hasInTableScope name tb
    if inScope then do
      generateImpliedEndTags Nothing tb
      popUntilInclusive name tb
      clearActiveFormattingToMarker tb
      writeIORef (tbMode tb) MInRow
    else pure ()
  TStartTag name _ _ | name `elem` ["caption","col","colgroup","tbody","td","tfoot","th","thead","tr"] -> do
    tdScope <- hasInTableScope "td" tb
    thScope <- hasInTableScope "th" tb
    if tdScope || thScope then do
      let cellName = if tdScope then "td" else "th"
      generateImpliedEndTags Nothing tb
      popUntilInclusive cellName tb
      clearActiveFormattingToMarker tb
      writeIORef (tbMode tb) MInRow
      processInMode tb tok
    else pure ()
  TEndTag name | name `elem` ["body","caption","col","colgroup","html"] -> pure ()
  TEndTag name | name `elem` ["table","tbody","tfoot","thead","tr"] -> do
    inScope <- hasInTableScope name tb
    if inScope then do
      tdScope <- hasInTableScope "td" tb
      thScope <- hasInTableScope "th" tb
      if tdScope || thScope then do
        let cellName = if tdScope then "td" else "th"
        generateImpliedEndTags Nothing tb
        popUntilInclusive cellName tb
        clearActiveFormattingToMarker tb
        writeIORef (tbMode tb) MInRow
        processInMode tb tok
      else pure ()
    else pure ()
  _ -> modeInBody tb tok

modeInSelect :: TreeBuilder -> Token -> IO ()
modeInSelect tb tok = case tok of
  TChar '\0' -> pure ()
  TChar c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag "option" attrs _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    void $ insertElement tb "option" attrs Nothing
  TStartTag "optgroup" attrs _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    cn2 <- currentNodeName tb
    if cn2 == "optgroup" then popElement tb else pure ()
    void $ insertElement tb "optgroup" attrs Nothing
  TStartTag "hr" attrs _ -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
    cn2 <- currentNodeName tb
    if cn2 == "optgroup" then popElement tb else pure ()
    void $ insertVoidElement tb "hr" attrs Nothing
  TEndTag "optgroup" -> do
    cn <- currentNodeName tb
    if cn == "option" then do
      elems <- readIORef (tbOpenElements tb)
      case elems of
        (_:node2:_) | nodeName node2 == "optgroup" -> do
          popElement tb
          popElement tb
        _ -> pure ()
    else if cn == "optgroup" then popElement tb
    else pure ()
  TEndTag "option" -> do
    cn <- currentNodeName tb
    if cn == "option" then popElement tb else pure ()
  TEndTag "select" -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
  TStartTag "select" _ _ -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
  TStartTag name _ _ | name `elem` ["input","textarea"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TStartTag "keygen" attrs _ ->
    void $ insertVoidElement tb "keygen" attrs Nothing
  TStartTag "script" _ _ -> modeInHead tb tok
  TStartTag "template" _ _ -> modeInHead tb tok
  TEndTag "template" -> modeInHead tb tok
  TEOF -> modeInBody tb tok
  _ -> pure ()

modeInSelectInTable :: TreeBuilder -> Token -> IO ()
modeInSelectInTable tb tok = case tok of
  TStartTag name _ _ | name `elem` ["caption","table","tbody","tfoot","thead","tr","td","th"] -> do
    popUntilInclusive "select" tb
    resetInsertionMode tb
    processInMode tb tok
  TEndTag name | name `elem` ["caption","table","tbody","tfoot","thead","tr","td","th"] -> do
    inScope <- hasInTableScope name tb
    if inScope then do
      popUntilInclusive "select" tb
      resetInsertionMode tb
      processInMode tb tok
    else pure ()
  _ -> modeInSelect tb tok

modeInTemplate :: TreeBuilder -> Token -> IO ()
modeInTemplate tb tok = case tok of
  TChar _ -> modeInBody tb tok
  TComment _ -> modeInBody tb tok
  TDoctype _ _ _ _ -> modeInBody tb tok
  TStartTag name _ _ | name `elem` ["base","basefont","bgsound","link","meta","noframes","script","style","template","title"] ->
    modeInHead tb tok
  TEndTag "template" -> modeInHead tb tok
  TStartTag name _ _ | name `elem` ["caption","colgroup","tbody","tfoot","thead"] -> do
    replaceTemplateMode MInTable tb
    writeIORef (tbMode tb) MInTable
    processInMode tb tok
  TStartTag "col" _ _ -> do
    replaceTemplateMode MInColumnGroup tb
    writeIORef (tbMode tb) MInColumnGroup
    processInMode tb tok
  TStartTag "tr" _ _ -> do
    replaceTemplateMode MInTableBody tb
    writeIORef (tbMode tb) MInTableBody
    processInMode tb tok
  TStartTag name _ _ | name == "td" || name == "th" -> do
    replaceTemplateMode MInRow tb
    writeIORef (tbMode tb) MInRow
    processInMode tb tok
  TEOF -> do
    onStack <- isOnStack "template" tb
    if not onStack then pure ()
    else do
      popUntilInclusive "template" tb
      clearActiveFormattingToMarker tb
      modifyIORef' (tbTemplateModes tb) safeTail
      resetInsertionMode tb
      processInMode tb tok
  TStartTag _ _ _ -> do
    replaceTemplateMode MInBody tb
    writeIORef (tbMode tb) MInBody
    processInMode tb tok
  _ -> pure ()

replaceTemplateMode :: InsertionMode -> TreeBuilder -> IO ()
replaceTemplateMode newMode tb =
  modifyIORef' (tbTemplateModes tb) (\case { (_:rest) -> newMode:rest; [] -> [newMode] })

modeAfterBody :: TreeBuilder -> Token -> IO ()
modeAfterBody tb tok = case tok of
  TChar c | isWS c -> modeInBody tb tok
  TComment t -> do
    elems <- readIORef (tbOpenElements tb)
    case reverse elems of
      (htmlNode:_) -> do
        commentNode <- newTBNode tb "#comment" [("#data", t)] Nothing False
        appendChild htmlNode commentNode
      [] -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TEndTag "html" -> writeIORef (tbMode tb) MAfterAfterBody
  TEOF -> pure ()
  _ -> do
    writeIORef (tbMode tb) MInBody
    processInMode tb tok

modeInFrameset :: TreeBuilder -> Token -> IO ()
modeInFrameset tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag "frameset" attrs _ -> void $ insertElement tb "frameset" attrs Nothing
  TEndTag "frameset" -> do
    cn <- currentNodeName tb
    if cn == "html" then pure ()
    else do
      popElement tb
      cn2 <- currentNodeName tb
      if cn2 /= "frameset"
      then writeIORef (tbMode tb) MAfterFrameset
      else pure ()
  TStartTag "frame" attrs _ -> void $ insertVoidElement tb "frame" attrs Nothing
  TStartTag "noframes" attrs _ -> do
    void $ insertElement tb "noframes" attrs Nothing
    writeIORef (tbOriginalMode tb) MInFrameset
    writeIORef (tbMode tb) MText
  TEOF -> pure ()
  _ -> pure ()

modeAfterFrameset :: TreeBuilder -> Token -> IO ()
modeAfterFrameset tb tok = case tok of
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TComment t -> insertComment tb t
  TDoctype _ _ _ _ -> pure ()
  TStartTag "html" _ _ -> modeInBody tb tok
  TEndTag "html" -> writeIORef (tbMode tb) MAfterAfterFrameset
  TStartTag "noframes" attrs _ -> do
    void $ insertElement tb "noframes" attrs Nothing
    writeIORef (tbOriginalMode tb) MAfterFrameset
    writeIORef (tbMode tb) MText
  TEOF -> pure ()
  _ -> pure ()

modeAfterAfterBody :: TreeBuilder -> Token -> IO ()
modeAfterAfterBody tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> modeInBody tb tok
  TChar c | isWS c -> modeInBody tb tok
  TStartTag "html" _ _ -> modeInBody tb tok
  TEOF -> pure ()
  _ -> do
    writeIORef (tbMode tb) MInBody
    processInMode tb tok

modeAfterAfterFrameset :: TreeBuilder -> Token -> IO ()
modeAfterAfterFrameset tb tok = case tok of
  TComment t -> insertCommentToDocument tb t
  TDoctype _ _ _ _ -> modeInBody tb tok
  TChar c | isWS c -> modeInBody tb tok
  TStartTag "html" _ _ -> modeInBody tb tok
  TStartTag "noframes" _ _ -> modeInHead tb tok
  TEOF -> pure ()
  _ -> pure ()

------------------------------------------------------------------------
-- Foreign content
------------------------------------------------------------------------

processForeignContent :: TreeBuilder -> Token -> IO ()
processForeignContent tb tok = case tok of
  TChar '\0' -> appendTextToCurrentNode tb "\xFFFD"
  TChar c | isWS c -> appendTextToCurrentNode tb (T.singleton c)
  TChar c -> do
    appendTextToCurrentNode tb (T.singleton c)
    writeIORef (tbFramesetOk tb) False
  TComment t
    | isCDATA t ->
        appendTextToCurrentNode tb (cdataContent t)
    | otherwise -> insertComment tb t
  TStartTag name attrs sc -> do
    let nameLower = T.toLower name
    if nameLower `S.member` foreignBreakoutElements
       || (nameLower == "font" && hasFontBreakoutAttr attrs)
    then do
      popUntilHTMLOrIntegrationPoint tb
      resetInsertionMode tb
      processInMode tb tok
    else do
      mn <- currentNode tb
      let ns = maybe Nothing nodeNs mn
          adjustedName = case ns of
            Just "svg" -> adjustSVGTagName name
            _ -> name
          adjustedAttrs = case ns of
            Just "svg" -> adjustForeignAttrs (adjustSVGAttrs attrs)
            Just "math" -> adjustForeignAttrs (adjustMathMLAttrs attrs)
            _ -> adjustForeignAttrs attrs
      if sc
      then void $ insertVoidElement tb adjustedName adjustedAttrs ns
      else void $ insertElement tb adjustedName adjustedAttrs ns
  TEndTag name -> do
    let nameLower = T.toLower name
    if nameLower == "br" || nameLower == "p"
    then do
      popUntilHTMLOrIntegrationPoint tb
      resetInsertionMode tb
      processInMode tb tok
    else foreignEndTag nameLower tb
  _ -> pure ()

foreignEndTag :: Text -> TreeBuilder -> IO ()
foreignEndTag name tb = do
  elems <- readIORef (tbOpenElements tb)
  go elems
  where
    go [] = pure ()
    go (node:rest)
      | T.toLower (nodeName node) == name = do
          elems <- readIORef (tbOpenElements tb)
          writeIORef (tbOpenElements tb) (dropThrough node elems)
      | isHTMLNs (nodeNs node) = processInMode tb (TEndTag name)
      | otherwise = go rest
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"
    dropThrough target (x:xs) | x == target = xs
    dropThrough target (_:xs) = dropThrough target xs
    dropThrough _ [] = []

hasFontBreakoutAttr :: [(Text,Text)] -> Bool
hasFontBreakoutAttr attrs =
  any (\(n,_) -> T.toLower n `elem` ["color","face","size"]) attrs

popUntilHTMLOrIntegrationPoint :: TreeBuilder -> IO ()
popUntilHTMLOrIntegrationPoint tb = do
  elems <- readIORef (tbOpenElements tb)
  case elems of
    [] -> pure ()
    (node:_)
      | isHTMLNs (nodeNs node) -> pure ()
      | otherwise -> do
          isHIP <- isHTMLIntegrationPoint node
          isMTIP <- isMathMLTIP node
          if isHIP || isMTIP
          then pure ()
          else do
            popElement tb
            popUntilHTMLOrIntegrationPoint tb
  where
    isHTMLNs ns = ns == Nothing || ns == Just "" || ns == Just "html"
    isMathMLTIP node = pure $ nodeNs node == Just "math" && nodeName node `elem` ["mi","mo","mn","ms","mtext"]
    isHTMLIntegrationPoint node
      | nodeNs node == Just "svg" = pure $ nodeName node `elem` ["foreignObject","desc","title"]
      | nodeNs node == Just "math" && nodeName node == "annotation-xml" = do
          attrs <- nodeAttrs node
          pure $ case lookup "encoding" attrs of
            Just enc -> T.toLower enc `elem` ["text/html","application/xhtml+xml"]
            Nothing -> False
      | otherwise = pure False

------------------------------------------------------------------------
-- Text node helpers
------------------------------------------------------------------------

appendTextToCurrentNode :: TreeBuilder -> Text -> IO ()
appendTextToCurrentNode tb txt = do
  insertFromTable <- readIORef (tbInsertFromTable tb)
  elems <- readIORef (tbOpenElements tb)
  case elems of
    [] -> modifyIORef' (tbDocument tb) (appendTextToDocChildren txt)
    (current:_)
      | insertFromTable && nodeName current `elem` ["table","tbody","tfoot","thead","tr"] ->
          fosterParentText tb txt
      | otherwise -> do
          let childRef = if nodeIsTemplate current then nodeTemplateContents current else nodeChildren current
          children <- readIORef childRef
          case children of
            (lastChild:_) | nodeName lastChild == "#text" -> do
              lastAttrs <- nodeAttrs lastChild
              let prevTxt = case lookup "#data" lastAttrs of { Just t -> t; Nothing -> "" }
              writeIORef (nodeAttrsRef lastChild) [("#data", prevTxt <> txt)]
            _ -> do
              textNode <- newTBNode tb "#text" [("#data", txt)] Nothing False
              writeIORef (nodeParent textNode) (Just current)
              modifyIORef' childRef (textNode:)

appendTextToDocChildren :: Text -> [ChildNode] -> [ChildNode]
appendTextToDocChildren txt [] = [CText txt]
appendTextToDocChildren txt children =
  case last children of
    CText prev -> init children ++ [CText (prev <> txt)]
    _ -> children ++ [CText txt]

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

nameWithNs :: Text -> Maybe Text -> Text
nameWithNs name Nothing = name
nameWithNs name (Just "") = name
nameWithNs name (Just "html") = name
nameWithNs name (Just ns) = ns <> " " <> name

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs

void :: IO a -> IO ()
void m = m >> pure ()

------------------------------------------------------------------------
-- Quirks mode
------------------------------------------------------------------------

determineQuirksMode :: Text -> Maybe Text -> Maybe Text -> Bool -> Text
determineQuirksMode name pub sys fq
  | fq = "quirks"
  | T.toLower name /= "html" = "quirks"
  | matchesQuirkyPublic (fmap T.toLower pub) = "quirks"
  | matchesQuirkySystem (fmap T.toLower sys) = "quirks"
  | matchesLimitedQuirky (fmap T.toLower pub) sys = "limited-quirks"
  | otherwise = "no-quirks"

matchesQuirkyPublic :: Maybe Text -> Bool
matchesQuirkyPublic Nothing = False
matchesQuirkyPublic (Just p) =
  p `elem` ["-//w3o//dtd w3 html strict 3.0//en//","-/w3c/dtd html 4.0 transitional/en","html"]
  || any (`T.isPrefixOf` p) quirkyPublicPrefixes

matchesQuirkySystem :: Maybe Text -> Bool
matchesQuirkySystem Nothing = False
matchesQuirkySystem (Just s) = s == "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd"

matchesLimitedQuirky :: Maybe Text -> Maybe Text -> Bool
matchesLimitedQuirky pub _sys = case pub of
  Nothing -> False
  Just p ->
    any (`T.isPrefixOf` p)
      ["-//w3c//dtd xhtml 1.0 frameset//","-//w3c//dtd xhtml 1.0 transitional//"]
    || (any (`T.isPrefixOf` p)
        ["-//w3c//dtd html 4.01 frameset//","-//w3c//dtd html 4.01 transitional//"]
       && _sys /= Nothing)

quirkyPublicPrefixes :: [Text]
quirkyPublicPrefixes =
  ["-//advasoft ltd//dtd html 3.0 aswedit + extensions//"
  ,"-//as//dtd html 3.0 aswedit + extensions//"
  ,"-//ietf//dtd html 2.0 level 1//","-//ietf//dtd html 2.0 level 2//"
  ,"-//ietf//dtd html 2.0 strict level 1//","-//ietf//dtd html 2.0 strict level 2//"
  ,"-//ietf//dtd html 2.0 strict//","-//ietf//dtd html 2.0//"
  ,"-//ietf//dtd html 2.1e//","-//ietf//dtd html 3.0//"
  ,"-//ietf//dtd html 3.2 final//","-//ietf//dtd html 3.2//"
  ,"-//ietf//dtd html 3//","-//ietf//dtd html level 0//"
  ,"-//ietf//dtd html level 1//","-//ietf//dtd html level 2//"
  ,"-//ietf//dtd html level 3//","-//ietf//dtd html strict level 0//"
  ,"-//ietf//dtd html strict level 1//","-//ietf//dtd html strict level 2//"
  ,"-//ietf//dtd html strict level 3//","-//ietf//dtd html strict//"
  ,"-//ietf//dtd html//","-//metrius//dtd metrius presentational//"
  ,"-//microsoft//dtd internet explorer 2.0 html strict//"
  ,"-//microsoft//dtd internet explorer 2.0 html//"
  ,"-//microsoft//dtd internet explorer 2.0 tables//"
  ,"-//microsoft//dtd internet explorer 3.0 html strict//"
  ,"-//microsoft//dtd internet explorer 3.0 html//"
  ,"-//microsoft//dtd internet explorer 3.0 tables//"
  ,"-//netscape comm. corp.//dtd html//"
  ,"-//netscape comm. corp.//dtd strict html//"
  ,"-//o'reilly and associates//dtd html 2.0//"
  ,"-//o'reilly and associates//dtd html extended 1.0//"
  ,"-//o'reilly and associates//dtd html extended relaxed 1.0//"
  ,"-//softquad software//dtd hotmetal pro 6.0::19990601::extensions to html 4.0//"
  ,"-//softquad//dtd hotmetal pro 4.0::19971010::extensions to html 4.0//"
  ,"-//spyglass//dtd html 2.0 extended//"
  ,"-//sq//dtd html 2.0 hotmetal + extensions//"
  ,"-//sun microsystems corp.//dtd hotjava html//"
  ,"-//sun microsystems corp.//dtd hotjava strict html//"
  ,"-//w3c//dtd html 3 1995-03-24//","-//w3c//dtd html 3.2 draft//"
  ,"-//w3c//dtd html 3.2 final//","-//w3c//dtd html 3.2//"
  ,"-//w3c//dtd html 3.2s draft//","-//w3c//dtd html 4.0 frameset//"
  ,"-//w3c//dtd html 4.0 transitional//"
  ,"-//w3c//dtd html experimental 19960712//"
  ,"-//w3c//dtd html experimental 970421//"
  ,"-//w3c//dtd w3 html//","-//w3o//dtd w3 html 3.0//"
  ,"-//webtechs//dtd mozilla html 2.0//","-//webtechs//dtd mozilla html//"
  ]

------------------------------------------------------------------------
-- SVG/MathML/Foreign attribute adjustments
------------------------------------------------------------------------

svgTagNameAdjustments :: [(Text,Text)]
svgTagNameAdjustments =
  [("altglyph","altGlyph"),("altglyphdef","altGlyphDef"),("altglyphitem","altGlyphItem")
  ,("animatecolor","animateColor"),("animatemotion","animateMotion")
  ,("animatetransform","animateTransform"),("clippath","clipPath")
  ,("feblend","feBlend"),("fecolormatrix","feColorMatrix")
  ,("fecomponenttransfer","feComponentTransfer"),("fecomposite","feComposite")
  ,("feconvolvematrix","feConvolveMatrix"),("fediffuselighting","feDiffuseLighting")
  ,("fedisplacementmap","feDisplacementMap"),("fedistantlight","feDistantLight")
  ,("feflood","feFlood"),("fefunca","feFuncA"),("fefuncb","feFuncB")
  ,("fefuncg","feFuncG"),("fefuncr","feFuncR"),("fegaussianblur","feGaussianBlur")
  ,("feimage","feImage"),("femerge","feMerge"),("femergenode","feMergeNode")
  ,("femorphology","feMorphology"),("feoffset","feOffset")
  ,("fepointlight","fePointLight"),("fespecularlighting","feSpecularLighting")
  ,("fespotlight","feSpotLight"),("fetile","feTile"),("feturbulence","feTurbulence")
  ,("foreignobject","foreignObject"),("glyphref","glyphRef")
  ,("lineargradient","linearGradient"),("radialgradient","radialGradient")
  ,("textpath","textPath")]

adjustSVGTagName :: Text -> Text
adjustSVGTagName name = case lookup (T.toLower name) svgTagNameAdjustments of
  Just adj -> adj
  Nothing -> name

adjustSVGAttrs :: [(Text,Text)] -> [(Text,Text)]
adjustSVGAttrs = map (\(n,v) -> (lookupDef n (T.toLower n) svgAttrAdjustments, v))

svgAttrAdjustments :: [(Text,Text)]
svgAttrAdjustments =
  [("attributename","attributeName"),("attributetype","attributeType")
  ,("basefrequency","baseFrequency"),("baseprofile","baseProfile")
  ,("calcmode","calcMode"),("clippathunits","clipPathUnits")
  ,("diffuseconstant","diffuseConstant"),("edgemode","edgeMode")
  ,("filterunits","filterUnits"),("glyphref","glyphRef")
  ,("gradienttransform","gradientTransform"),("gradientunits","gradientUnits")
  ,("kernelmatrix","kernelMatrix"),("kernelunitlength","kernelUnitLength")
  ,("keypoints","keyPoints"),("keysplines","keySplines"),("keytimes","keyTimes")
  ,("lengthadjust","lengthAdjust"),("limitingconeangle","limitingConeAngle")
  ,("markerheight","markerHeight"),("markerunits","markerUnits")
  ,("markerwidth","markerWidth"),("maskcontentunits","maskContentUnits")
  ,("maskunits","maskUnits"),("numoctaves","numOctaves"),("pathlength","pathLength")
  ,("patterncontentunits","patternContentUnits"),("patterntransform","patternTransform")
  ,("patternunits","patternUnits"),("pointsatx","pointsAtX"),("pointsaty","pointsAtY")
  ,("pointsatz","pointsAtZ"),("preservealpha","preserveAlpha")
  ,("preserveaspectratio","preserveAspectRatio"),("primitiveunits","primitiveUnits")
  ,("refx","refX"),("refy","refY"),("repeatcount","repeatCount"),("repeatdur","repeatDur")
  ,("requiredextensions","requiredExtensions"),("requiredfeatures","requiredFeatures")
  ,("specularconstant","specularConstant"),("specularexponent","specularExponent")
  ,("spreadmethod","spreadMethod"),("startoffset","startOffset")
  ,("stddeviation","stdDeviation"),("stitchtiles","stitchTiles")
  ,("surfacescale","surfaceScale"),("systemlanguage","systemLanguage")
  ,("tablevalues","tableValues"),("targetx","targetX"),("targety","targetY")
  ,("textlength","textLength"),("viewbox","viewBox"),("viewtarget","viewTarget")
  ,("xchannelselector","xChannelSelector"),("ychannelselector","yChannelSelector")
  ,("zoomandpan","zoomAndPan")]

adjustMathMLAttrs :: [(Text,Text)] -> [(Text,Text)]
adjustMathMLAttrs = map (\(n,v) -> (lookupDef n (T.toLower n) [("definitionurl","definitionURL")], v))

adjustForeignAttrs :: [(Text,Text)] -> [(Text,Text)]
adjustForeignAttrs = map (\(n,v) -> case lookup (T.toLower n) foreignAttrAdj of
  Just (prefix, local) -> if T.null prefix then (local, v) else (prefix <> ":" <> local, v)
  Nothing -> (n, v))

foreignAttrAdj :: [(Text,(Text,Text))]
foreignAttrAdj =
  [("xlink:actuate",("xlink","actuate")),("xlink:arcrole",("xlink","arcrole"))
  ,("xlink:href",("xlink","href")),("xlink:role",("xlink","role"))
  ,("xlink:show",("xlink","show")),("xlink:title",("xlink","title"))
  ,("xlink:type",("xlink","type")),("xml:lang",("xml","lang"))
  ,("xml:space",("xml","space")),("xmlns",("","xmlns"))
  ,("xmlns:xlink",("xmlns","xlink"))]

lookupDef :: Text -> Text -> [(Text,Text)] -> Text
lookupDef def key table = case lookup key table of { Just v -> v; Nothing -> def }

------------------------------------------------------------------------
-- tbNodeToHTMLNode (for building final output)
------------------------------------------------------------------------
-- (already defined above in the "Build final document" section)

------------------------------------------------------------------------
-- Tokenizer (same as before)
------------------------------------------------------------------------

tokenize :: Text -> [Token]
tokenize txt = tokenizeNormal (T.unpack txt)

tokenizeNormal :: String -> [Token]
tokenizeNormal [] = []
tokenizeNormal ('<':rest) = tokenizeAfterLT rest
tokenizeNormal ('&':rest) =
  let (entity, remaining) = parseEntityRef rest
  in map TChar entity ++ tokenizeNormal remaining
tokenizeNormal ('\r':'\n':rest) = TChar '\n' : tokenizeNormal rest
tokenizeNormal ('\r':rest) = TChar '\n' : tokenizeNormal rest
tokenizeNormal (c:rest) = TChar c : tokenizeNormal rest

tokenizeAfterLT :: String -> [Token]
tokenizeAfterLT [] = [TChar '<']
tokenizeAfterLT ('!':rest) = tokenizeMarkupDecl rest
tokenizeAfterLT ('/':rest) = tokenizeEndTag rest
tokenizeAfterLT ('?':rest) =
  let (comment, remaining) = readUntilStr ">" rest
  in TComment (T.pack ('?' : comment)) : tokenizeNormal remaining
tokenizeAfterLT (c:rest)
  | isAlpha c =
      let (name, rest1) = span isTagNameChar (c:rest)
          lcName = map toLower name
          (attrs, selfClose, rest2) = readTagAttrs rest1
          tok = TStartTag (T.pack lcName) attrs selfClose
      in case lcName of
        n | n `elem` ["script","style","xmp","iframe","noembed","noframes","noscript"] ->
          if selfClose then tok : tokenizeNormal rest2
          else tok : tokenizeRawText rest2 (T.pack lcName)
        "textarea" ->
          if selfClose then tok : tokenizeNormal rest2
          else tok : tokenizeRCData rest2 (T.pack lcName)
        "title" ->
          if selfClose then tok : tokenizeNormal rest2
          else tok : tokenizeRCData rest2 (T.pack lcName)
        "plaintext" -> tok : map TChar rest2
        _ -> tok : tokenizeNormal rest2
  | otherwise = TChar '<' : tokenizeNormal (c:rest)

tokenizeEndTag :: String -> [Token]
tokenizeEndTag [] = [TChar '<', TChar '/']
tokenizeEndTag (c:rest)
  | isAlpha c =
      let (name, rest1) = span isTagNameChar (c:rest)
          lcName = map toLower name
          rest2 = skipToGtStr rest1
      in TEndTag (T.pack lcName) : tokenizeNormal rest2
  | c == '>' = TComment "" : tokenizeNormal rest
  | otherwise =
      let (comment, remaining) = readUntilStr ">" (c:rest)
      in TComment (T.pack comment) : tokenizeNormal remaining

tokenizeMarkupDecl :: String -> [Token]
tokenizeMarkupDecl ('-':'-':rest) =
  let (comment, remaining) = readComment rest
  in TComment (T.pack comment) : tokenizeNormal remaining
tokenizeMarkupDecl rest
  | matchCaseI rest "doctype" =
      let rest1 = drop 7 rest
      in tokenizeDoctype rest1
  | matchCaseI rest "[cdata[" =
      let rest1 = drop 7 rest
          (content, remaining) = readUntilStr "]]>" rest1
      in TComment (cdataMarker <> T.pack content) : tokenizeNormal remaining
  | otherwise =
      let (comment, remaining) = readBogusComment rest
      in TComment (T.pack comment) : tokenizeNormal remaining

readBogusComment :: String -> (String, String)
readBogusComment [] = ("", [])
readBogusComment ('>':rest) = ("", rest)
readBogusComment (c:rest) =
  let (more, remaining) = readBogusComment rest
  in (c:more, remaining)

readComment :: String -> (String, String)
readComment ('>':rest) = ("", rest)
readComment ('-':'>':rest) = ("", rest)
readComment cs = go [] cs
  where
    go acc [] = (reverse acc, [])
    go acc ('-':'-':'>':rest) = (reverse acc, rest)
    go acc ('-':'-':'!':'>':rest) = (reverse acc, rest)
    go acc ('-':'-':[]) = (reverse acc, [])
    go acc (c:rest) = go (c:acc) rest

tokenizeDoctype :: String -> [Token]
tokenizeDoctype cs =
  let cs1 = dropWhile isSp cs
      (name, cs2) = readDoctypeName cs1
      cs3 = dropWhile isSp cs2
      (pub, sys, fq, cs4) = readDoctypeIds cs3
  in TDoctype (T.pack name) pub sys fq : tokenizeNormal cs4
  where isSp c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'

readDoctypeName :: String -> (String, String)
readDoctypeName = go []
  where
    go acc [] = (reverse acc, [])
    go acc ('>':rest) = (reverse acc, '>':rest)
    go acc (c:rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' = (reverse acc, c:rest)
      | otherwise = go (toLower c : acc) rest

readDoctypeIds :: String -> (Maybe Text, Maybe Text, Bool, String)
readDoctypeIds [] = (Nothing, Nothing, False, [])
readDoctypeIds ('>':rest) = (Nothing, Nothing, False, rest)
readDoctypeIds cs
  | matchCaseI cs "public" =
      let cs1 = dropWhile isWSChar (drop 6 cs)
      in case cs1 of
        (q:rest) | q == '"' || q == '\'' ->
          let (pub, rest1) = readQuotedDoc rest q
              rest2 = dropWhile isWSChar rest1
          in case rest2 of
            (q2:rest3) | q2 == '"' || q2 == '\'' ->
              let (sys, rest4) = readQuotedDoc rest3 q2
              in (Just (T.pack pub), Just (T.pack sys), False, skipToGtStr rest4)
            ('>':rest3) -> (Just (T.pack pub), Just (T.pack ""), False, rest3)
            _ -> (Just (T.pack pub), Just (T.pack ""), False, skipToGtStr rest2)
        _ -> (Nothing, Nothing, True, skipToGtStr cs1)
  | matchCaseI cs "system" =
      let cs1 = dropWhile isWSChar (drop 6 cs)
      in case cs1 of
        (q:rest) | q == '"' || q == '\'' ->
          let (sys, rest1) = readQuotedDoc rest q
          in (Just (T.pack ""), Just (T.pack sys), False, skipToGtStr rest1)
        _ -> (Nothing, Nothing, True, skipToGtStr cs1)
  | otherwise = (Nothing, Nothing, True, skipToGtStr cs)
  where isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'

readQuotedDoc :: String -> Char -> (String, String)
readQuotedDoc cs q = go [] cs
  where
    go acc [] = (reverse acc, [])
    go acc (c:rest)
      | c == q = (reverse acc, rest)
      | otherwise = go (c:acc) rest

readTagAttrs :: String -> ([(Text,Text)], Bool, String)
readTagAttrs = go []
  where
    go acc [] = (reverse acc, False, [])
    go acc ('>':rest) = (reverse acc, False, rest)
    go acc ('/':'>':rest) = (reverse acc, True, rest)
    go acc ('/':rest) = go acc rest
    go acc (c:rest)
      | isWSChar c = go acc rest
      | otherwise =
          let (name, rest1) = readAttrName (c:rest)
              rest2 = dropWhile isWSChar rest1
          in if null name then go acc rest2
             else case rest2 of
               ('=':rest3) ->
                 let rest4 = dropWhile isWSChar rest3
                     (val, rest5) = readAttrValue rest4
                     lcName = T.toLower (T.pack name)
                 in if any (\(n,_) -> n == lcName) acc
                    then go acc rest5
                    else go ((lcName, val) : acc) rest5
               _ ->
                 let lcName = T.toLower (T.pack name)
                 in if any (\(n,_) -> n == lcName) acc
                    then go acc rest2
                    else go ((lcName, T.empty) : acc) rest2
    isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C'

readAttrName :: String -> (String, String)
readAttrName = go []
  where
    go acc [] = (reverse acc, [])
    go acc (c:rest)
      | c == '=' || c == '>' || c == '/' || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' =
          (reverse acc, c:rest)
      | otherwise = go (c:acc) rest

readAttrValue :: String -> (Text, String)
readAttrValue [] = (T.empty, [])
readAttrValue ('"':rest) = readQuotedAttr rest '"'
readAttrValue ('\'':rest) = readQuotedAttr rest '\''
readAttrValue cs = readUnquotedAttrVal cs

readQuotedAttr :: String -> Char -> (Text, String)
readQuotedAttr cs q = go [] cs
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c:rest)
      | c == q = (T.pack (reverse acc), rest)
      | c == '&' = let (entity, remaining) = parseEntityRefInAttr rest
                   in go (reverse entity ++ acc) remaining
      | otherwise = go (c:acc) rest

readUnquotedAttr :: String -> (Text, String)
readUnquotedAttr = go []
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c:rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '>' =
          (T.pack (reverse acc), c:rest)
      | otherwise = go (c:acc) rest

readUnquotedAttrVal :: String -> (Text, String)
readUnquotedAttrVal = go []
  where
    go acc [] = (T.pack (reverse acc), [])
    go acc (c:rest)
      | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '>' =
          (T.pack (reverse acc), c:rest)
      | c == '&' = let (entity, remaining) = parseEntityRefInAttr rest
                   in go (reverse entity ++ acc) remaining
      | otherwise = go (c:acc) rest

isTagNameChar :: Char -> Bool
isTagNameChar c = isAlphaNum c || c == '-' || c == '_' || c == ':' || c == '.' || c == '<'

skipToGtStr :: String -> String
skipToGtStr [] = []
skipToGtStr ('>':rest) = rest
skipToGtStr (_:rest) = skipToGtStr rest

skipToGtWithAttrs :: String -> String
skipToGtWithAttrs [] = []
skipToGtWithAttrs ('>':rest) = rest
skipToGtWithAttrs ('"':rest) = skipToGtWithAttrs (dropWhile (/= '"') rest |> safeDrop1)
skipToGtWithAttrs ('\'':rest) = skipToGtWithAttrs (dropWhile (/= '\'') rest |> safeDrop1)
skipToGtWithAttrs (_:rest) = skipToGtWithAttrs rest

(|>) :: a -> (a -> a) -> a
x |> f = f x

safeDrop1 :: [a] -> [a]
safeDrop1 [] = []
safeDrop1 (_:xs) = xs

readUntilStr :: String -> String -> (String, String)
readUntilStr _ [] = ("", [])
readUntilStr needle cs@(c:rest)
  | matchPrefix needle cs = ("", drop (length needle) cs)
  | otherwise = let (more, remaining) = readUntilStr needle rest
                in (c : more, remaining)

matchPrefix :: String -> String -> Bool
matchPrefix [] _ = True
matchPrefix _ [] = False
matchPrefix (n:ns) (c:cs) = n == c && matchPrefix ns cs

matchCaseI :: String -> String -> Bool
matchCaseI _ [] = True
matchCaseI [] _ = False
matchCaseI (c:cs) (p:ps) = toLower c == p && matchCaseI cs ps

tokenizeRawText :: String -> Text -> [Token]
tokenizeRawText cs tag
  | tag == "script" = tokenizeScriptData cs
  | otherwise = goRaw [] cs
  where
    tagStr = T.unpack tag
    goRaw acc [] = map TChar (reverse acc)
    goRaw acc ('<':'/':rest)
      | matchCloseTag rest tagStr =
          let rest1 = drop (length tagStr) rest
              rest2 = skipToGtWithAttrs rest1
          in map TChar (reverse acc) ++ [TEndTag tag] ++ tokenizeNormal rest2
    goRaw acc (c:rest) = goRaw (c:acc) rest

tokenizeScriptData :: String -> [Token]
tokenizeScriptData cs = scriptNormal [] cs
  where
    scriptNormal acc [] = map TChar (reverse acc)
    scriptNormal acc ('<':'/':rest)
      | matchCloseTag rest "script" =
          let rest1 = drop 6 rest
              rest2 = skipToGtWithAttrs rest1
          in map TChar (reverse acc) ++ [TEndTag "script"] ++ tokenizeNormal rest2
    scriptNormal acc ('<':'!':'-':'-':rest) =
      scriptEscaped ('-':'-':'!':'<':acc) rest
    scriptNormal acc (c:rest) = scriptNormal (c:acc) rest

    scriptEscaped acc [] = map TChar (reverse acc)
    scriptEscaped acc ('-':'-':'>':rest) =
      scriptNormal ('>':'-':'-':acc) rest
    scriptEscaped acc ('<':'/':rest)
      | matchCloseTag rest "script" =
          let rest1 = drop 6 rest
              rest2 = skipToGtWithAttrs rest1
          in map TChar (reverse acc) ++ [TEndTag "script"] ++ tokenizeNormal rest2
    scriptEscaped acc ('<':rest) =
      let (tag, rest') = tryMatchScriptStart rest
      in case tag of
        Just suffix ->
          scriptDoubleEscaped (reverse suffix ++ '<':acc) rest'
        Nothing ->
          scriptEscaped ('<':acc) rest
    scriptEscaped acc (c:rest) = scriptEscaped (c:acc) rest

    scriptDoubleEscaped acc [] = map TChar (reverse acc)
    scriptDoubleEscaped acc ('-':'-':'>':rest) =
      scriptEscaped ('>':'-':'-':acc) rest
    scriptDoubleEscaped acc ('<':'/':rest) =
      let (isScript, consumed, rest') = tryMatchScriptEnd rest
      in if isScript
         then scriptEscaped (reverse consumed ++ '/':'<':acc) rest'
         else scriptDoubleEscaped (reverse consumed ++ '/':'<':acc) rest'
    scriptDoubleEscaped acc (c:rest) = scriptDoubleEscaped (c:acc) rest

    tryMatchScriptStart cs =
      case cs of
        (c1:c2:c3:c4:c5:c6:rest)
          | map toLower [c1,c2,c3,c4,c5,c6] == "script"
          , case rest of
              [] -> True
              (r:_) -> r `elem` (" \t\n\r\x0C/>"::[Char])
          -> (Just [c1,c2,c3,c4,c5,c6], rest)
        _ -> (Nothing, cs)

    tryMatchScriptEnd cs =
      case cs of
        (c1:c2:c3:c4:c5:c6:rest)
          | map toLower [c1,c2,c3,c4,c5,c6] == "script"
          , case rest of
              [] -> True
              (r:_) -> r `elem` (" \t\n\r\x0C/>"::[Char])
          -> (True, [c1,c2,c3,c4,c5,c6], rest)
        _ -> (False, [], cs)

matchCloseTagEOF :: String -> String -> Bool
matchCloseTagEOF cs tag =
  let (name, rest) = span (\c -> isAlpha c || isDigit c || c == '-') cs
  in map toLower name == map toLower tag && null rest

tokenizeRCData :: String -> Text -> [Token]
tokenizeRCData cs tag = go [] cs
  where
    tagStr = T.unpack tag
    go acc [] = map TChar (reverse acc)
    go acc ('<':'/':rest)
      | matchCloseTag rest tagStr =
          let rest1 = drop (length tagStr) rest
              rest2 = skipToGtWithAttrs rest1
          in map TChar (reverse acc) ++ [TEndTag tag] ++ tokenizeNormal rest2
    go acc ('&':rest) =
      let (entity, remaining) = parseEntityRef rest
      in go (reverse entity ++ acc) remaining
    go acc ('\r':'\n':rest) = go ('\n':acc) rest
    go acc ('\r':rest) = go ('\n':acc) rest
    go acc (c:rest) = go (c:acc) rest

matchCloseTag :: String -> String -> Bool
matchCloseTag cs tag =
  let (name, rest) = span (\c -> isAlpha c || isDigit c || c == '-') cs
  in map toLower name == map toLower tag
     && case rest of
       [] -> False
       (c:_) -> c == '>' || c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\x0C' || c == '/'

------------------------------------------------------------------------
-- Entity resolution
------------------------------------------------------------------------

resolveEntitiesT :: Text -> Text
resolveEntitiesT t
  | T.any (== '&') t = T.pack (resolveChars (T.unpack t))
  | otherwise = t

parseEntityRef :: String -> (String, String)
parseEntityRef ('#':'x':rest) = parseHexEntity "#x" rest
parseEntityRef ('#':'X':rest) = parseHexEntity "#X" rest
parseEntityRef ('#':rest) = parseDecEntity rest
parseEntityRef rest = case matchNamedEntity rest of
  Just (name, replacement, remaining) ->
    if hasSemicolon name rest
    then (replacement, remaining)
    else (replacement, remaining)
  Nothing -> ("&", rest)
  where
    hasSemicolon _ _ = True

parseEntityRefInAttr :: String -> (String, String)
parseEntityRefInAttr ('#':'x':rest) = parseHexEntity "#x" rest
parseEntityRefInAttr ('#':'X':rest) = parseHexEntity "#X" rest
parseEntityRefInAttr ('#':rest) = parseDecEntity rest
parseEntityRefInAttr rest = case matchNamedEntityAttr rest of
  Just (_, replacement, remaining) -> (replacement, remaining)
  Nothing -> ("&", rest)

parseHexEntity :: String -> String -> (String, String)
parseHexEntity prefix rest =
  let (hex, after) = span isHexDigit rest
  in if null hex then ("&" ++ prefix, rest)
     else let val = foldl' (\a d -> a * 16 + digitToInt d) 0 hex
              after1 = case after of { (';':r) -> r; _ -> after }
          in ([safeChar val], after1)

parseDecEntity :: String -> (String, String)
parseDecEntity rest =
  let (dec, after) = span isDigit rest
  in if null dec then ("&#", rest)
     else let val = foldl' (\a d -> a * 10 + digitToInt d) 0 dec
              after1 = case after of { (';':r) -> r; _ -> after }
          in ([safeChar val], after1)

safeChar :: Int -> Char
safeChar 0 = '\xFFFD'
safeChar n | n > 0x10FFFF = '\xFFFD'
           | n >= 0xD800 && n <= 0xDFFF = '\xFFFD'
           | n >= 0x80 && n <= 0x9F = case lookup n windows1252Table of
               Just c  -> c
               Nothing -> chr n
           | otherwise = chr n

windows1252Table :: [(Int, Char)]
windows1252Table =
  [(0x80, '\x20AC'),(0x82, '\x201A'),(0x83, '\x0192'),(0x84, '\x201E')
  ,(0x85, '\x2026'),(0x86, '\x2020'),(0x87, '\x2021'),(0x88, '\x02C6')
  ,(0x89, '\x2030'),(0x8A, '\x0160'),(0x8B, '\x2039'),(0x8C, '\x0152')
  ,(0x8E, '\x017D'),(0x91, '\x2018'),(0x92, '\x2019'),(0x93, '\x201C')
  ,(0x94, '\x201D'),(0x95, '\x2022'),(0x96, '\x2013'),(0x97, '\x2014')
  ,(0x98, '\x02DC'),(0x99, '\x2122'),(0x9A, '\x0161'),(0x9B, '\x203A')
  ,(0x9C, '\x0153'),(0x9E, '\x017E'),(0x9F, '\x0178')]

matchNamedEntity :: String -> Maybe (String, String, String)
matchNamedEntity cs =
  let (allAlpha, rest) = span isAlphaNum cs
  in if null allAlpha then Nothing
     else case rest of
       (';':after) -> case lookup allAlpha namedEntities of
         Just rep -> Just (allAlpha, rep, after)
         Nothing -> tryPrefixesWithSemi allAlpha (';':after)
       _ -> tryPrefixesNoSemi allAlpha rest

tryPrefixesWithSemi :: String -> String -> Maybe (String, String, String)
tryPrefixesWithSemi name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
        Just rep -> Just (prefix, rep, suffix)
        Nothing -> go (n-1)

tryPrefixesNoSemi :: String -> String -> Maybe (String, String, String)
tryPrefixesNoSemi name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
        Just rep ->
          if prefix `elem` legacyEntities
          then Just (prefix, rep, suffix)
          else go (n-1)
        Nothing -> go (n-1)

legacyEntities :: [String]
legacyEntities =
  ["amp","lt","gt","quot","apos","AMP","LT","GT","QUOT"
  ,"Aacute","aacute","Acirc","acirc","acute","AElig","aelig"
  ,"Agrave","agrave","Aring","aring","Atilde","atilde","Auml","auml"
  ,"brvbar","Ccedil","ccedil","cedil","cent","copy","COPY","curren"
  ,"deg","divide","Eacute","eacute","Ecirc","ecirc","Egrave","egrave"
  ,"ETH","eth","Euml","euml","frac12","frac14","frac34"
  ,"Iacute","iacute","Icirc","icirc","iexcl","Igrave","igrave"
  ,"iquest","Iuml","iuml","laquo","macr","micro","middot"
  ,"nbsp","not","Ntilde","ntilde","Oacute","oacute","Ocirc","ocirc"
  ,"Ograve","ograve","ordf","ordm","Oslash","oslash","Otilde","otilde"
  ,"Ouml","ouml","para","plusmn","pound","raquo","REG","reg"
  ,"sect","shy","sup1","sup2","sup3","szlig"
  ,"THORN","thorn","times","Uacute","uacute","Ucirc","ucirc"
  ,"Ugrave","ugrave","uml","Uuml","uuml","Yacute","yacute","yen","yuml"]

matchNamedEntityAttr :: String -> Maybe (String, String, String)
matchNamedEntityAttr cs =
  let (allAlpha, rest) = span isAlphaNum cs
  in if null allAlpha then Nothing
     else case rest of
       (';':after) -> case lookup allAlpha namedEntities of
         Just rep -> Just (allAlpha, rep, after)
         Nothing -> Nothing
       _ -> tryAttrPrefixesLegacy allAlpha rest

tryAttrPrefixesLegacy :: String -> String -> Maybe (String, String, String)
tryAttrPrefixesLegacy name rest = go (length name)
  where
    go 0 = Nothing
    go n =
      let prefix = take n name
          suffix = drop n name ++ rest
      in case lookup prefix namedEntities of
        Just rep ->
          if prefix `elem` legacyEntities
          then
            let nextChar = case suffix of { (c:_) -> Just c; [] -> Nothing }
                nextIsAlnumOrEq = nextChar == Just '=' || maybe False isAlphaNum nextChar
            in if nextIsAlnumOrEq
               then go (n-1)
               else Just (prefix, rep, suffix)
          else go (n-1)
        Nothing -> go (n-1)

resolveChars :: String -> String
resolveChars [] = []
resolveChars ('&':rest) =
  let (resolved, remaining) = parseEntityRef rest
  in resolved ++ resolveChars remaining
resolveChars (c:rest) = c : resolveChars rest

namedEntities :: [(String, String)]
namedEntities =
  [("amp","&"),("lt","<"),("gt",">"),("quot","\""),("apos","'")
  ,("nbsp","\x00A0"),("iexcl","\x00A1"),("cent","\x00A2"),("pound","\x00A3")
  ,("curren","\x00A4"),("yen","\x00A5"),("brvbar","\x00A6"),("sect","\x00A7")
  ,("uml","\x00A8"),("copy","\x00A9"),("ordf","\x00AA"),("laquo","\x00AB")
  ,("not","\x00AC"),("shy","\x00AD"),("reg","\x00AE"),("macr","\x00AF")
  ,("deg","\x00B0"),("plusmn","\x00B1"),("sup2","\x00B2"),("sup3","\x00B3")
  ,("acute","\x00B4"),("micro","\x00B5"),("para","\x00B6"),("middot","\x00B7")
  ,("cedil","\x00B8"),("sup1","\x00B9"),("ordm","\x00BA"),("raquo","\x00BB")
  ,("frac14","\x00BC"),("frac12","\x00BD"),("frac34","\x00BE"),("iquest","\x00BF")
  ,("Agrave","\x00C0"),("Aacute","\x00C1"),("Acirc","\x00C2"),("Atilde","\x00C3")
  ,("Auml","\x00C4"),("Aring","\x00C5"),("AElig","\x00C6"),("Ccedil","\x00C7")
  ,("Egrave","\x00C8"),("Eacute","\x00C9"),("Ecirc","\x00CA"),("Euml","\x00CB")
  ,("Igrave","\x00CC"),("Iacute","\x00CD"),("Icirc","\x00CE"),("Iuml","\x00CF")
  ,("ETH","\x00D0"),("Ntilde","\x00D1"),("Ograve","\x00D2"),("Oacute","\x00D3")
  ,("Ocirc","\x00D4"),("Otilde","\x00D5"),("Ouml","\x00D6"),("times","\x00D7")
  ,("Oslash","\x00D8"),("Ugrave","\x00D9"),("Uacute","\x00DA"),("Ucirc","\x00DB")
  ,("Uuml","\x00DC"),("Yacute","\x00DD"),("THORN","\x00DE"),("szlig","\x00DF")
  ,("agrave","\x00E0"),("aacute","\x00E1"),("acirc","\x00E2"),("atilde","\x00E3")
  ,("auml","\x00E4"),("aring","\x00E5"),("aelig","\x00E6"),("ccedil","\x00E7")
  ,("egrave","\x00E8"),("eacute","\x00E9"),("ecirc","\x00EA"),("euml","\x00EB")
  ,("igrave","\x00EC"),("iacute","\x00ED"),("icirc","\x00EE"),("iuml","\x00EF")
  ,("eth","\x00F0"),("ntilde","\x00F1"),("ograve","\x00F2"),("oacute","\x00F3")
  ,("ocirc","\x00F4"),("otilde","\x00F5"),("ouml","\x00F6"),("divide","\x00F7")
  ,("oslash","\x00F8"),("ugrave","\x00F9"),("uacute","\x00FA"),("ucirc","\x00FB")
  ,("uuml","\x00FC"),("yacute","\x00FD"),("thorn","\x00FE"),("yuml","\x00FF")
  ,("ndash","\x2013"),("mdash","\x2014"),("lsquo","\x2018"),("rsquo","\x2019")
  ,("sbquo","\x201A"),("ldquo","\x201C"),("rdquo","\x201D"),("bdquo","\x201E")
  ,("dagger","\x2020"),("Dagger","\x2021"),("bull","\x2022"),("hellip","\x2026")
  ,("prime","\x2032"),("Prime","\x2033"),("lsaquo","\x2039"),("rsaquo","\x203A")
  ,("oline","\x203E"),("frasl","\x2044"),("euro","\x20AC"),("image","\x2111")
  ,("weierp","\x2118"),("real","\x211C"),("trade","\x2122"),("alefsym","\x2135")
  ,("larr","\x2190"),("uarr","\x2191"),("rarr","\x2192"),("darr","\x2193")
  ,("harr","\x2194"),("crarr","\x21B5"),("lArr","\x21D0"),("uArr","\x21D1")
  ,("rArr","\x21D2"),("dArr","\x21D3"),("hArr","\x21D4"),("nabla","\x2207")
  ,("isin","\x2208"),("notin","\x2209"),("ni","\x220B"),("prod","\x220F")
  ,("sum","\x2211"),("minus","\x2212"),("lowast","\x2217"),("radic","\x221A")
  ,("prop","\x221D"),("infin","\x221E"),("ang","\x2220"),("and","\x2227")
  ,("or","\x2228"),("cap","\x2229"),("cup","\x222A"),("int","\x222B")
  ,("there4","\x2234"),("sim","\x223C"),("cong","\x2245"),("asymp","\x2248")
  ,("ne","\x2260"),("equiv","\x2261"),("le","\x2264"),("ge","\x2265")
  ,("sub","\x2282"),("sup","\x2283"),("nsub","\x2284"),("sube","\x2286")
  ,("supe","\x2287"),("oplus","\x2295"),("otimes","\x2297"),("perp","\x22A5")
  ,("sdot","\x22C5"),("lceil","\x2308"),("rceil","\x2309"),("lfloor","\x230A")
  ,("rfloor","\x230B"),("lang","\x2329"),("rang","\x232A"),("loz","\x25CA")
  ,("spades","\x2660"),("clubs","\x2663"),("hearts","\x2665"),("diams","\x2666")
  ,("OElig","\x0152"),("oelig","\x0153"),("Scaron","\x0160"),("scaron","\x0161")
  ,("Yuml","\x0178"),("fnof","\x0192"),("circ","\x02C6"),("tilde","\x02DC")
  ,("ensp","\x2002"),("emsp","\x2003"),("thinsp","\x2009"),("zwnj","\x200C")
  ,("zwj","\x200D"),("lrm","\x200E"),("rlm","\x200F"),("permil","\x2030")
  ,("lang","\x27E8"),("rang","\x27E9")
  ,("ImaginaryI","\x2148"),("Kopf","\x1D542"),("notinva","\x2209")
  ,("NotEqualTilde","\x2242\x0338"),("ThickSpace","\x205F\x200A")
  ,("NotSubset","\x2282\x20D2"),("Gopf","\x1D53E")
  ,("AMP","&"),("COPY","\xA9"),("GT",">"),("LT","<"),("QUOT","\""),("REG","\xAE")
  ,("Tab","\x0009"),("NewLine","\x000A")
  ,("Alpha","\x0391"),("Beta","\x0392"),("Gamma","\x0393"),("Delta","\x0394")
  ,("Epsilon","\x0395"),("Zeta","\x0396"),("Eta","\x0397"),("Theta","\x0398")
  ,("Iota","\x0399"),("Kappa","\x039A"),("Lambda","\x039B"),("Mu","\x039C")
  ,("Nu","\x039D"),("Xi","\x039E"),("Omicron","\x039F"),("Pi","\x03A0")
  ,("Rho","\x03A1"),("Sigma","\x03A3"),("Tau","\x03A4"),("Upsilon","\x03A5")
  ,("Phi","\x03A6"),("Chi","\x03A7"),("Psi","\x03A8"),("Omega","\x03A9")
  ,("alpha","\x03B1"),("beta","\x03B2"),("gamma","\x03B3"),("delta","\x03B4")
  ,("epsilon","\x03B5"),("zeta","\x03B6"),("eta","\x03B7"),("theta","\x03B8")
  ,("iota","\x03B9"),("kappa","\x03BA"),("lambda","\x03BB"),("mu","\x03BC")
  ,("nu","\x03BD"),("xi","\x03BE"),("omicron","\x03BF"),("pi","\x03C0")
  ,("rho","\x03C1"),("sigmaf","\x03C2"),("sigma","\x03C3"),("tau","\x03C4")
  ,("upsilon","\x03C5"),("phi","\x03C6"),("chi","\x03C7"),("psi","\x03C8")
  ,("omega","\x03C9"),("thetasym","\x03D1"),("upsih","\x03D2"),("piv","\x03D6")]
