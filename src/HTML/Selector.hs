{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | CSS selector parser and matching engine.
--
-- Supports the selector subset needed for both DOM queries and the
-- streaming rewriter:
--
-- __Both modes__: type, universal, ID, class, attribute selectors
-- (existence, exact, prefix, suffix, substring, word, hyphen),
-- descendant and child combinators, comma groups.
--
-- __DOM only__: adjacent sibling, general sibling, @:first-child@,
-- @:last-child@, @:nth-child@, @:nth-of-type@, @:not@, @:has@, @:empty@.
module HTML.Selector
  ( -- * Types
    Selector(..)
  , ComplexSelector(..)
  , CompoundSelector(..)
  , Combinator(..)
  , TypeSel(..)
  , SubSel(..)
  , SelectorError(..)
    -- * Parsing
  , parseSelector
    -- * Matching (simple / rewriter mode)
  , matchCompound
    -- * Querying
  , hasPseudoClasses
  , isRewriterCompatible
  ) where

import Data.Char (isAlpha, isAlphaNum, isDigit, digitToInt)
import Data.Primitive.SmallArray (SmallArray, sizeofSmallArray, indexSmallArray)
import Data.Text (Text)
import qualified Data.Text as T

import HTML.Value (HTMLAttribute(..))

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Parsed CSS selector: comma-separated list of complex selectors.
newtype Selector = Selector [ComplexSelector]
  deriving (Show, Eq)

-- | Chain of compound selectors linked by combinators.
-- The first compound is the leftmost (ancestor-side in descendant selectors).
data ComplexSelector = ComplexSelector
  !CompoundSelector
  ![(Combinator, CompoundSelector)]
  deriving (Show, Eq)

data Combinator
  = Descendant
  | Child
  | AdjacentSibling
  | GeneralSibling
  deriving (Show, Eq, Ord, Enum, Bounded)

data CompoundSelector = CompoundSelector
  !(Maybe TypeSel)
  ![SubSel]
  deriving (Show, Eq)

data TypeSel
  = TypeTag !Text
  | TypeUniversal
  deriving (Show, Eq)

data SubSel
  = SelId !Text
  | SelClass !Text
  | SelAttrExists !Text
  | SelAttrExact !Text !Text
  | SelAttrPrefix !Text !Text
  | SelAttrSuffix !Text !Text
  | SelAttrContains !Text !Text
  | SelAttrWord !Text !Text
  | SelAttrHyphen !Text !Text
  | SelNot !Selector
  | SelHas !ComplexSelector
  | SelFirstChild
  | SelLastChild
  | SelNthChild !Int !Int
  | SelNthOfType !Int !Int
  | SelEmpty
  deriving (Show, Eq)

data SelectorError
  = SelectorSyntaxError !Int !Text
  | UnsupportedSelector !Text
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Querying
-- ---------------------------------------------------------------------------

-- | Whether any compound in the selector uses pseudo-classes
-- (DOM-only features not available in rewriter mode).
hasPseudoClasses :: Selector -> Bool
hasPseudoClasses (Selector cs) = any complexHasPseudo cs
  where
    complexHasPseudo (ComplexSelector hd tl) =
      compoundHasPseudo hd || any (compoundHasPseudo . snd) tl
    compoundHasPseudo (CompoundSelector _ subs) = any subIsPseudo subs
    subIsPseudo SelFirstChild     = True
    subIsPseudo SelLastChild      = True
    subIsPseudo (SelNthChild _ _) = True
    subIsPseudo (SelNthOfType _ _) = True
    subIsPseudo SelEmpty          = True
    subIsPseudo (SelNot _)        = True
    subIsPseudo (SelHas _)        = True
    subIsPseudo _                 = False

-- | Whether the selector can be used in rewriter (streaming) mode.
-- Rejects sibling combinators and pseudo-classes.
isRewriterCompatible :: Selector -> Bool
isRewriterCompatible (Selector cs) = all complexOk cs
  where
    complexOk (ComplexSelector hd tl) =
      compoundOk hd && all pairOk tl
    pairOk (AdjacentSibling, _) = False
    pairOk (GeneralSibling, _)  = False
    pairOk (_, c)               = compoundOk c
    compoundOk (CompoundSelector _ subs) = all (not . subIsPseudo) subs
    subIsPseudo SelFirstChild     = True
    subIsPseudo SelLastChild      = True
    subIsPseudo (SelNthChild _ _) = True
    subIsPseudo (SelNthOfType _ _) = True
    subIsPseudo SelEmpty          = True
    subIsPseudo (SelNot _)        = True
    subIsPseudo (SelHas _)        = True
    subIsPseudo _                 = False

-- ---------------------------------------------------------------------------
-- Matching
-- ---------------------------------------------------------------------------

-- | Match a compound selector against an element's tag and attributes.
-- Handles type, ID, class, and attribute selectors.
-- Pseudo-classes are /not/ checked (they need tree context);
-- use 'isRewriterCompatible' to reject selectors that need them.
matchCompound :: CompoundSelector -> Text -> SmallArray HTMLAttribute -> Bool
matchCompound (CompoundSelector mtype subs) tag attrs =
  matchType mtype tag && all (matchSub attrs) subs
{-# INLINE matchCompound #-}

matchType :: Maybe TypeSel -> Text -> Bool
matchType Nothing _              = True
matchType (Just TypeUniversal) _ = True
matchType (Just (TypeTag t)) tag = t == tag
{-# INLINE matchType #-}

matchSub :: SmallArray HTMLAttribute -> SubSel -> Bool
matchSub attrs = \case
  SelId i          -> findAttr "id" attrs == Just i
  SelClass c       -> case findAttr "class" attrs of
                        Nothing  -> False
                        Just val -> c `elem` T.words val
  SelAttrExists n  -> attrExists n attrs
  SelAttrExact n v -> findAttr n attrs == Just v
  SelAttrPrefix n v -> case findAttr n attrs of
                         Nothing -> False
                         Just av -> T.isPrefixOf v av
  SelAttrSuffix n v -> case findAttr n attrs of
                         Nothing -> False
                         Just av -> T.isSuffixOf v av
  SelAttrContains n v -> case findAttr n attrs of
                           Nothing -> False
                           Just av -> T.isInfixOf v av
  SelAttrWord n v -> case findAttr n attrs of
                       Nothing -> False
                       Just av -> v `elem` T.words av
  SelAttrHyphen n v -> case findAttr n attrs of
                         Nothing -> False
                         Just av -> av == v || T.isPrefixOf (v <> "-") av
  -- Pseudo-classes: can't evaluate without tree context.
  -- Return True so they don't block matching when used in
  -- a context that has already validated selector compatibility.
  SelFirstChild     -> True
  SelLastChild      -> True
  SelNthChild _ _   -> True
  SelNthOfType _ _  -> True
  SelEmpty          -> True
  SelNot _          -> True
  SelHas _          -> True
{-# INLINE matchSub #-}

findAttr :: Text -> SmallArray HTMLAttribute -> Maybe Text
findAttr name attrs = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = Nothing
      | HTMLAttribute k v <- indexSmallArray attrs i, k == name = Just v
      | otherwise = go (i + 1)
{-# INLINE findAttr #-}

attrExists :: Text -> SmallArray HTMLAttribute -> Bool
attrExists name attrs = go 0
  where
    !n = sizeofSmallArray attrs
    go !i
      | i >= n = False
      | HTMLAttribute k _ <- indexSmallArray attrs i, k == name = True
      | otherwise = go (i + 1)
{-# INLINE attrExists #-}

-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

data P = P !Text !Int

pOff :: P -> Int
pOff (P _ n) = n
{-# INLINE pOff #-}

peek :: P -> Maybe Char
peek (P t _) = case T.uncons t of
  Just (c, _) -> Just c
  Nothing     -> Nothing
{-# INLINE peek #-}

advance :: P -> P
advance (P t n) = P (T.drop 1 t) (n + 1)
{-# INLINE advance #-}

skipWS :: P -> P
skipWS p@(P t n) =
  let !ws = T.length (T.takeWhile isWSChar t)
  in if ws == 0 then p else P (T.drop ws t) (n + ws)
  where
    isWSChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f'

atEnd :: P -> Bool
atEnd (P t _) = T.null t
{-# INLINE atEnd #-}

err :: P -> Text -> Either SelectorError a
err (P _ n) msg = Left (SelectorSyntaxError n msg)

-- | Parse a CSS selector string.
parseSelector :: Text -> Either SelectorError Selector
parseSelector input = do
  let !p0 = skipWS (P input 0)
  if atEnd p0
    then err p0 "empty selector"
    else do
      (sels, p1) <- parseSelectorList p0
      let !p2 = skipWS p1
      if atEnd p2
        then Right (Selector sels)
        else err p2 ("unexpected: " <> T.take 20 (pRest p2))
  where
    pRest (P t _) = t

parseSelectorList :: P -> Either SelectorError ([ComplexSelector], P)
parseSelectorList p = do
  (first, p1) <- parseComplexSelector p
  loop [first] p1
  where
    loop !acc pp = do
      let !pp' = skipWS pp
      case peek pp' of
        Just ',' -> do
          let !pp'' = skipWS (advance pp')
          (next, pp3) <- parseComplexSelector pp''
          loop (next : acc) pp3
        _ -> Right (reverse acc, pp')

parseComplexSelector :: P -> Either SelectorError (ComplexSelector, P)
parseComplexSelector p = do
  (hd, p1) <- parseCompoundSelector p
  (tl, p2) <- parseComplexTail p1
  Right (ComplexSelector hd tl, p2)

parseComplexTail :: P -> Either SelectorError ([(Combinator, CompoundSelector)], P)
parseComplexTail p =
  case parseCombinator p of
    Just (comb, p2) -> do
      (compound, p3) <- parseCompoundSelector p2
      (more, p4) <- parseComplexTail p3
      Right ((comb, compound) : more, p4)
    Nothing -> Right ([], p)

parseCombinator :: P -> Maybe (Combinator, P)
parseCombinator p =
  let !p' = skipWS p
      !hadWS = pOff p' > pOff p
  in case peek p' of
    Just '>' -> Just (Child, skipWS (advance p'))
    Just '+' -> Just (AdjacentSibling, skipWS (advance p'))
    Just '~' -> Just (GeneralSibling, skipWS (advance p'))
    Just c | hadWS && isCompoundStart c -> Just (Descendant, p')
    _ -> Nothing

isCompoundStart :: Char -> Bool
isCompoundStart c =
  isIdentStart c || c == '*' || c == '.' || c == '#' || c == '[' || c == ':'

parseCompoundSelector :: P -> Either SelectorError (CompoundSelector, P)
parseCompoundSelector p = do
  let !p' = skipWS p
  case peek p' of
    Just '*' -> do
      (subs, p3) <- parseSubSelectors (advance p')
      Right (CompoundSelector (Just TypeUniversal) subs, p3)
    Just c | isIdentStart c -> do
      (ident, p'') <- readIdent p'
      (subs, p3) <- parseSubSelectors p''
      Right (CompoundSelector (Just (TypeTag (T.toLower ident))) subs, p3)
    Just c | c == '.' || c == '#' || c == '[' || c == ':' -> do
      (subs, p3) <- parseSubSelectors p'
      Right (CompoundSelector Nothing subs, p3)
    Just c -> err p' ("unexpected '" <> T.singleton c <> "' in selector")
    Nothing -> err p' "unexpected end of selector"

parseSubSelectors :: P -> Either SelectorError ([SubSel], P)
parseSubSelectors p =
  case peek p of
    Just '.' -> do
      (cls, p') <- readIdent (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (SelClass cls : more, p'')
    Just '#' -> do
      (ident, p') <- readIdent (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (SelId ident : more, p'')
    Just '[' -> do
      (attr, p') <- parseAttrSelector (skipWS (advance p))
      (more, p'') <- parseSubSelectors p'
      Right (attr : more, p'')
    Just ':' -> do
      (pseudo, p') <- parsePseudo (advance p)
      (more, p'') <- parseSubSelectors p'
      Right (pseudo : more, p'')
    _ -> Right ([], p)

-- ---------------------------------------------------------------------------
-- Attribute selector
-- ---------------------------------------------------------------------------

parseAttrSelector :: P -> Either SelectorError (SubSel, P)
parseAttrSelector p = do
  (name, p1) <- readIdent p
  let !lname = T.toLower name
      !p2 = skipWS p1
  case peek p2 of
    Just ']' -> Right (SelAttrExists lname, advance p2)
    Just '=' -> do
      (val, p3) <- readAttrValue (skipWS (advance p2))
      p4 <- expectChar ']' (skipWS p3)
      Right (SelAttrExact lname val, p4)
    Just '^' -> parseAttrOp SelAttrPrefix lname p2
    Just '$' -> parseAttrOp SelAttrSuffix lname p2
    Just '*' -> parseAttrOp SelAttrContains lname p2
    Just '~' -> parseAttrOp SelAttrWord lname p2
    Just '|' -> parseAttrOp SelAttrHyphen lname p2
    _ -> err p2 "expected ] or attribute operator"

parseAttrOp :: (Text -> Text -> SubSel) -> Text -> P -> Either SelectorError (SubSel, P)
parseAttrOp ctor name p = do
  let !p1 = advance p
  case peek p1 of
    Just '=' -> do
      (val, p2) <- readAttrValue (skipWS (advance p1))
      p3 <- expectChar ']' (skipWS p2)
      Right (ctor name val, p3)
    _ -> err p "expected = after operator"

readAttrValue :: P -> Either SelectorError (Text, P)
readAttrValue p = case peek p of
  Just '"'  -> readString '"' (advance p)
  Just '\'' -> readString '\'' (advance p)
  Just c | isIdentStart c || isDigit c -> readIdent p
  _ -> err p "expected attribute value"

-- ---------------------------------------------------------------------------
-- Pseudo-class
-- ---------------------------------------------------------------------------

parsePseudo :: P -> Either SelectorError (SubSel, P)
parsePseudo p = do
  (name, p1) <- readIdent p
  case T.toLower name of
    "first-child"  -> Right (SelFirstChild, p1)
    "last-child"   -> Right (SelLastChild, p1)
    "empty"        -> Right (SelEmpty, p1)
    "not" -> do
      p2 <- expectChar '(' p1
      (inner, p3) <- parseSelectorList (skipWS p2)
      p4 <- expectChar ')' (skipWS p3)
      Right (SelNot (Selector inner), p4)
    "has" -> do
      p2 <- expectChar '(' p1
      (inner, p3) <- parseComplexSelector (skipWS p2)
      p4 <- expectChar ')' (skipWS p3)
      Right (SelHas inner, p4)
    "nth-child" -> do
      p2 <- expectChar '(' p1
      (a, b, p3) <- parseNthExpr (skipWS p2)
      p4 <- expectChar ')' (skipWS p3)
      Right (SelNthChild a b, p4)
    "nth-of-type" -> do
      p2 <- expectChar '(' p1
      (a, b, p3) <- parseNthExpr (skipWS p2)
      p4 <- expectChar ')' (skipWS p3)
      Right (SelNthOfType a b, p4)
    other -> Left (UnsupportedSelector other)

-- ---------------------------------------------------------------------------
-- nth expression: an+b
-- ---------------------------------------------------------------------------

parseNthExpr :: P -> Either SelectorError (Int, Int, P)
parseNthExpr p =
  let !p' = skipWS p
  in case peek p' of
    Nothing -> err p' "expected nth expression"
    Just c
      | c == 'o' || c == 'O' -> do
          (w, p1) <- readIdent p'
          if T.toLower w == "odd"
            then Right (2, 1, p1)
            else err p' ("unexpected '" <> w <> "' in :nth-child")
      | c == 'e' || c == 'E' -> do
          (w, p1) <- readIdent p'
          if T.toLower w == "even"
            then Right (2, 0, p1)
            else err p' ("unexpected '" <> w <> "' in :nth-child")
      | c == 'n' || c == 'N' -> do
          let !p1 = advance p'
          parseNthB 1 p1
      | c == '-' -> do
          let !p1 = advance p'
          case peek p1 of
            Just cn | cn == 'n' || cn == 'N' -> parseNthB (-1) (advance p1)
            _ -> do
              (num, p2) <- readInt p1
              Right (0, negate num, p2)
      | c == '+' -> do
          let !p1 = advance p'
          case peek p1 of
            Just cn | cn == 'n' || cn == 'N' -> parseNthB 1 (advance p1)
            _ -> do
              (num, p2) <- readInt p1
              Right (0, num, p2)
      | isDigit c -> do
          (num, p1) <- readInt p'
          case peek p1 of
            Just cn | cn == 'n' || cn == 'N' -> parseNthB num (advance p1)
            _ -> Right (0, num, p1)
      | otherwise -> err p' "expected nth expression"

parseNthB :: Int -> P -> Either SelectorError (Int, Int, P)
parseNthB a p =
  let !p' = skipWS p
  in case peek p' of
    Just '+' -> do
      (b, p1) <- readInt (skipWS (advance p'))
      Right (a, b, p1)
    Just '-' -> do
      (b, p1) <- readInt (skipWS (advance p'))
      Right (a, negate b, p1)
    _ -> Right (a, 0, p')

readInt :: P -> Either SelectorError (Int, P)
readInt p@(P t n) =
  let digits = T.takeWhile isDigit t
      !dlen = T.length digits
  in if dlen == 0
     then err p "expected integer"
     else Right (textToInt digits, P (T.drop dlen t) (n + dlen))

textToInt :: Text -> Int
textToInt = T.foldl' (\acc c -> acc * 10 + digitToInt c) 0

-- ---------------------------------------------------------------------------
-- Lexer primitives
-- ---------------------------------------------------------------------------

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_' || c == '-' || c > '\x7F'
{-# INLINE isIdentStart #-}

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_' || c == '-' || c > '\x7F'
{-# INLINE isIdentChar #-}

readIdent :: P -> Either SelectorError (Text, P)
readIdent p@(P t n) =
  let ident = T.takeWhile isIdentChar t
      !len = T.length ident
  in if len == 0
     then err p "expected identifier"
     else Right (ident, P (T.drop len t) (n + len))

readString :: Char -> P -> Either SelectorError (Text, P)
readString quote (P t n) =
  let (content, after) = T.break (== quote) t
      !clen = T.length content
  in case T.uncons after of
    Just (_, rest) -> Right (content, P rest (n + clen + 1))
    Nothing        -> Left (SelectorSyntaxError n "unterminated string")

expectChar :: Char -> P -> Either SelectorError P
expectChar c p = case peek p of
  Just c' | c' == c -> Right (advance p)
  Just c' -> err p ("expected '" <> T.singleton c <> "' but got '" <> T.singleton c' <> "'")
  Nothing -> err p ("expected '" <> T.singleton c <> "' but got end of input")
