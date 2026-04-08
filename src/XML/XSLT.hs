{-# LANGUAGE BangPatterns #-}
-- | XSLT 1.0 transformation engine.
--
-- Transforms XML documents using XSLT stylesheets. Supports the core
-- XSLT instructions that cover the vast majority of real-world usage:
-- @xsl:template@, @xsl:apply-templates@, @xsl:value-of@, @xsl:for-each@,
-- @xsl:if@, @xsl:choose@, @xsl:copy-of@, @xsl:element@, @xsl:attribute@,
-- @xsl:text@, @xsl:comment@, @xsl:call-template@, and literal result elements.
module XML.XSLT
  ( Stylesheet(..)
  , Template(..)
  , Instruction(..)
  , OutputMethod(..)
  , parseStylesheet
  , transform
  , applyStylesheet
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe)

import XML.Value
import XML.Path (query, parsePath, textContent, attr, queryPath)
import XML.Value (simpleName)

data OutputMethod = XMLOutput | HTMLOutput | TextOutput
  deriving stock (Show, Eq)

data Stylesheet = Stylesheet
  { ssTemplates :: !(Vector Template)
  , ssOutputMethod :: !OutputMethod
  } deriving stock (Show, Eq)

data Template = Template
  { tmMatch    :: !(Maybe Text)
  , tmName     :: !(Maybe Text)
  , tmPriority :: !(Maybe Double)
  , tmBody     :: !(Vector Instruction)
  } deriving stock (Show, Eq)

data Instruction
  = IValueOf !Text
  | IForEach !Text !(Vector Instruction)
  | IIf !Text !(Vector Instruction)
  | IChoose ![(Text, Vector Instruction)] !(Maybe (Vector Instruction))
  | IApplyTemplates !(Maybe Text)
  | ICallTemplate !Text
  | ICopyOf !Text
  | IElement !Text !(Vector Instruction)
  | IAttribute !Text !(Vector Instruction)
  | IText !Text
  | IComment !(Vector Instruction)
  | LitElement !Name !(Vector Attribute) !(Vector Instruction)
  | LitText !Text
  deriving stock (Show, Eq)

-- | Parse a stylesheet from an XML Node (the root xsl:stylesheet element).
parseStylesheet :: Node -> Either String Stylesheet
parseStylesheet (Element name _ children)
  | nameLocal name `elem` ["stylesheet", "transform"] =
      let templates = V.mapMaybe parseTemplate children
          outMethod = XMLOutput
      in Right (Stylesheet templates outMethod)
parseStylesheet _ = Left "XSLT: root must be xsl:stylesheet or xsl:transform"

parseTemplate :: Node -> Maybe Template
parseTemplate (Element name attrs children)
  | nameLocal name == "template" =
      let match = findAttr "match" attrs
          tname = findAttr "name" attrs
          pri   = fmap (read . T.unpack) (findAttr "priority" attrs)
          body  = V.mapMaybe parseInstruction children
      in Just (Template match tname pri body)
parseTemplate _ = Nothing

parseInstruction :: Node -> Maybe Instruction
parseInstruction (Text t)
  | T.all (== ' ') t || T.all (== '\n') t = Nothing
  | otherwise = Just (LitText t)
parseInstruction (Element name attrs children)
  | nameLocal name == "value-of"          = Just $ IValueOf (sel attrs)
  | nameLocal name == "for-each"          = Just $ IForEach (sel attrs) (parseBody children)
  | nameLocal name == "if"                = Just $ IIf (testAttr attrs) (parseBody children)
  | nameLocal name == "choose"            = Just $ parseChoose children
  | nameLocal name == "apply-templates"   = Just $ IApplyTemplates (findAttr "select" attrs)
  | nameLocal name == "call-template"     = Just $ ICallTemplate (fromMaybe "" (findAttr "name" attrs))
  | nameLocal name == "copy-of"           = Just $ ICopyOf (sel attrs)
  | nameLocal name == "element"           = Just $ IElement (fromMaybe "unknown" (findAttr "name" attrs)) (parseBody children)
  | nameLocal name == "attribute"         = Just $ IAttribute (fromMaybe "unknown" (findAttr "name" attrs)) (parseBody children)
  | nameLocal name == "text"              = Just $ IText (textContent (Element name attrs children))
  | nameLocal name == "comment"           = Just $ IComment (parseBody children)
  | otherwise = Just $ LitElement name attrs (parseBody children)
parseInstruction _ = Nothing

parseChoose :: Vector Node -> Instruction
parseChoose children =
  let whens = V.toList $ V.mapMaybe parseWhen children
      otherwise' = V.toList (V.mapMaybe parseOtherwise children)
      ow = case otherwise' of { (x:_) -> Just x; _ -> Nothing }
  in IChoose whens ow

parseWhen :: Node -> Maybe (Text, Vector Instruction)
parseWhen (Element name attrs children)
  | nameLocal name == "when" = Just (testAttr attrs, parseBody children)
parseWhen _ = Nothing

parseOtherwise :: Node -> Maybe (Vector Instruction)
parseOtherwise (Element name _ children)
  | nameLocal name == "otherwise" = Just (parseBody children)
parseOtherwise _ = Nothing

parseBody :: Vector Node -> Vector Instruction
parseBody = V.mapMaybe parseInstruction

sel :: Vector Attribute -> Text
sel attrs = fromMaybe "" (findAttr "select" attrs)

testAttr :: Vector Attribute -> Text
testAttr attrs = fromMaybe "" (findAttr "test" attrs)

findAttr :: Text -> Vector Attribute -> Maybe Text
findAttr name attrs = go 0
  where
    go !i | i >= V.length attrs = Nothing
    go !i = case attrs V.! i of
      Attribute n v | nameLocal n == name -> Just v
      _ -> go (i + 1)

-- | Transform a source document using a stylesheet.
transform :: Stylesheet -> Document -> Either String Document
transform ss (Document _ root) = do
  resultNodes <- applyTemplatesTo ss root [root]
  case V.toList resultNodes of
    [] -> Left "XSLT: transform produced no output"
    (n:_) -> Right (Document Nothing n)

-- | Alias for transform.
applyStylesheet :: Stylesheet -> Document -> Either String Document
applyStylesheet = transform

applyTemplatesTo :: Stylesheet -> Node -> [Node] -> Either String (Vector Node)
applyTemplatesTo ss context nodes = do
  results <- mapM (applyOneTemplate ss context) nodes
  pure (V.concat results)

applyOneTemplate :: Stylesheet -> Node -> Node -> Either String (Vector Node)
applyOneTemplate ss _context node =
  case findMatchingTemplate ss node of
    Just tmpl -> executeBody ss node (tmBody tmpl)
    Nothing   -> builtinTemplate ss node

findMatchingTemplate :: Stylesheet -> Node -> Maybe Template
findMatchingTemplate ss node = go 0
  where
    templates = ssTemplates ss
    go !i | i >= V.length templates = Nothing
    go !i =
      let t = templates V.! i
      in case tmMatch t of
           Nothing -> go (i + 1)
           Just pat -> if matchesNode pat node then Just t else go (i + 1)

matchesNode :: Text -> Node -> Bool
matchesNode "/" (Element _ _ _) = True
matchesNode "*" (Element _ _ _) = True
matchesNode pat (Element name _ _) = nameLocal name == pat || pat == "/" || pat == "*"
matchesNode "text()" (Text _) = True
matchesNode _ _ = False

builtinTemplate :: Stylesheet -> Node -> Either String (Vector Node)
builtinTemplate ss (Element _ _ children) =
  applyTemplatesTo ss (Element (simpleName "root") V.empty children) (V.toList children)
builtinTemplate _ (Text t) = Right (V.singleton (Text t))
builtinTemplate _ (CData t) = Right (V.singleton (Text t))
builtinTemplate _ _ = Right V.empty

executeBody :: Stylesheet -> Node -> Vector Instruction -> Either String (Vector Node)
executeBody ss ctx instrs = do
  results <- mapM (executeInstr ss ctx) (V.toList instrs)
  pure (V.concat results)

executeInstr :: Stylesheet -> Node -> Instruction -> Either String (Vector Node)
executeInstr ss ctx (IValueOf xpath) = do
  let nodes = queryXPath xpath ctx
      txt = T.concat (map textContent (V.toList nodes))
  pure (if T.null txt then V.empty else V.singleton (Text txt))

executeInstr ss ctx (IForEach xpath body) = do
  let nodes = queryXPath xpath ctx
  results <- mapM (\n -> executeBody ss n body) (V.toList nodes)
  pure (V.concat results)

executeInstr ss ctx (IIf test body) = do
  let nodes = queryXPath test ctx
  if not (V.null nodes)
    then executeBody ss ctx body
    else pure V.empty

executeInstr ss ctx (IChoose whens otherwise') = goWhens whens
  where
    goWhens [] = case otherwise' of
      Just body -> executeBody ss ctx body
      Nothing -> pure V.empty
    goWhens ((test, body):rest) =
      let nodes = queryXPath test ctx
      in if not (V.null nodes)
         then executeBody ss ctx body
         else goWhens rest

executeInstr ss ctx (IApplyTemplates msel) = do
  let nodes = case msel of
        Nothing -> xsltChildren ctx
        Just xpath -> V.toList (queryXPath xpath ctx)
  applyTemplatesTo ss ctx nodes

executeInstr ss ctx (ICallTemplate name) =
  case V.find (\t -> tmName t == Just name) (ssTemplates ss) of
    Just tmpl -> executeBody ss ctx (tmBody tmpl)
    Nothing -> Left ("XSLT: template not found: " <> T.unpack name)

executeInstr _ ctx (ICopyOf xpath) = pure (queryXPath xpath ctx)

executeInstr ss ctx (IElement name body) = do
  children <- executeBody ss ctx body
  let (attrs, nonAttrs) = V.partition isAttrNode children
      realAttrs = V.mapMaybe toAttribute attrs
  pure (V.singleton (Element (simpleName name) realAttrs nonAttrs))

executeInstr ss ctx (IAttribute name body) = do
  children <- executeBody ss ctx body
  let val = T.concat (map textContent (V.toList children))
  pure (V.singleton (ProcessingInstruction "XSLT_ATTR" (name <> "=" <> val)))

executeInstr _ _ (IText t) = pure (V.singleton (Text t))

executeInstr ss ctx (IComment body) = do
  children <- executeBody ss ctx body
  let val = T.concat (map textContent (V.toList children))
  pure (V.singleton (Comment val))

executeInstr ss ctx (LitElement name attrs body) = do
  children <- executeBody ss ctx body
  let (attrNodes, nonAttrs) = V.partition isAttrNode children
      extraAttrs = V.mapMaybe toAttribute attrNodes
  pure (V.singleton (Element name (attrs V.++ extraAttrs) nonAttrs))

executeInstr _ _ (LitText t) = pure (V.singleton (Text t))

isAttrNode :: Node -> Bool
isAttrNode (ProcessingInstruction "XSLT_ATTR" _) = True
isAttrNode _ = False

toAttribute :: Node -> Maybe Attribute
toAttribute (ProcessingInstruction "XSLT_ATTR" val) =
  let (name, rest) = T.breakOn "=" val
  in Just (Attribute (simpleName name) (T.drop 1 rest))
toAttribute _ = Nothing

-- Simple XPath evaluation wrapper
queryXPath :: Text -> Node -> Vector Node
queryXPath xpath node =
  case parsePath xpath of
    Right path -> query path node
    Left _ -> V.empty

xsltChildren :: Node -> [Node]
xsltChildren (Element _ _ cs) = V.toList cs
xsltChildren _ = []
