{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

-- | Efficient literal branching using Template Haskell.
--
-- Compiles string-literal case branches into a trie of primitive byte
-- comparisons with grouped bounds checks.
--
-- @
-- $(switch [| case _ of
--     \"if\"   -> pure TIf
--     \"else\" -> pure TElse
--     _      -> identifier |])
-- @
module Wireform.Parser.Switch
  ( switch
  , switchWithPost
    -- * Helpers used by generated code (not for direct use)
  , switchFailed
  , switchBranch
  , switchAnyWord8Unsafe
  ) where

import Control.Monad (forM)
import Data.Char (ord)
import Data.Foldable (toList, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Foreign.Ptr (Ptr (..))
import GHC.Exts
import GHC.Int (Int (..))
import GHC.Word (Word8 (..))
import Language.Haskell.TH

import Wireform.Parser.Internal

------------------------------------------------------------------------
-- Helpers called by generated code
------------------------------------------------------------------------

-- | Fail without consuming input.
switchFailed :: Parser e a
switchFailed = Parser \tag env eob s st -> (# st, Fail# #)
{-# INLINE switchFailed #-}

-- | Unsafe read — caller must have ensured at least 1 byte.
switchAnyWord8Unsafe :: Parser e Word8
switchAnyWord8Unsafe = Parser \tag env eob s st ->
  case indexWord8OffAddr# s 0# of
    w# -> (# st, OK# (W8# w#) (plusAddr# s 1#) #)
{-# INLINE switchAnyWord8Unsafe #-}

-- | Branch on an ensure check: if enough bytes, run @t@; else @f@.
switchBranch :: Int -> Parser e a -> Parser e a -> Parser e a
switchBranch (I# n#) (Parser t) (Parser f) = Parser \tag env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> t tag env eob s st
    _  -> f tag env eob s st
{-# INLINE switchBranch #-}

------------------------------------------------------------------------
-- Trie
------------------------------------------------------------------------

type Rule = Maybe Int
data Trie a = Branch !a !(Map Word8 (Trie a))

nilTrie :: Trie Rule
nilTrie = Branch Nothing mempty

insertTrie :: Int -> [Word8] -> Trie Rule -> Trie Rule
insertTrie rule = go where
  go [] (Branch r ts) = Branch (Just $ maybe rule (min rule) r) ts
  go (c:cs) (Branch r ts) =
    Branch r (M.alter (Just . maybe (go cs nilTrie) (go cs)) c ts)

listToTrie :: [(Int, String)] -> Trie Rule
listToTrie = foldl' (\t (!r, !s) -> insertTrie r (charToBytes s) t) nilTrie

charToBytes :: String -> [Word8]
charToBytes = concatMap go where
  go c
    | n < 0x80  = [fromIntegral n]
    | n < 0x800 = [fromIntegral (0xC0 + n `div` 64),
                   fromIntegral (0x80 + n `mod` 64)]
    | otherwise = [fromIntegral n]  -- simplified; ASCII-only for v1
    where n = ord c

mindepths :: Trie Rule -> Trie (Rule, Int)
mindepths (Branch rule ts)
  | M.null ts = Branch (rule, 0) mempty
  | otherwise =
      let ts' = M.map mindepths ts
          d   = minimum $ M.elems $ M.map (\(Branch (r, depth) _) ->
                  maybe (depth + 1) (const 1) r) ts'
      in Branch (rule, d) ts'

------------------------------------------------------------------------
-- TH code generation
------------------------------------------------------------------------

#if MIN_VERSION_base(4,15,0)
mkDoE :: [Stmt] -> Exp
mkDoE = DoE Nothing
#else
mkDoE :: [Stmt] -> Exp
mkDoE = DoE
#endif

switch :: Q Exp -> Q Exp
switch = switchWithPost Nothing

switchWithPost :: Maybe (Q Exp) -> Q Exp -> Q Exp
switchWithPost postAction qexp = do
  post <- sequence postAction
  (cases, deflt) <- parseSwitchExp qexp
  genSwitch post cases deflt

parseSwitchExp :: Q Exp -> Q ([(String, Exp)], Maybe Exp)
parseSwitchExp qexp = qexp >>= \case
  CaseE (UnboundVarE _) [] -> fail "switch: empty case list"
  CaseE (UnboundVarE _) matches -> do
    let (ini, lst) = (init matches, last matches)
    cases <- forM ini \case
      Match (LitP (StringL s)) (NormalB rhs) [] -> pure (s, rhs)
      _ -> fail "switch: expected string literal pattern"
    (cases', deflt) <- case lst of
      Match (LitP (StringL s)) (NormalB rhs) [] -> pure (cases <> [(s, rhs)], Nothing)
      Match WildP (NormalB rhs) []               -> pure (cases, Just rhs)
      _ -> fail "switch: expected string literal or wildcard pattern"
    pure (cases', deflt)
  _ -> fail "switch: expected 'case _ of' expression"

genSwitch :: Maybe Exp -> [(String, Exp)] -> Maybe Exp -> Q Exp
genSwitch post cases deflt = do
  let indexed = zip [0 :: Int ..] cases
      trie = mindepths (listToTrie [(i, s) | (i, (s, _)) <- indexed])

  let ruleMap = M.fromList $
        (Nothing, VarE 'switchFailed) :
        [(Just i, applyPost post rhs) | (i, (_, rhs)) <- indexed]
      defltExp = maybe (VarE 'switchFailed) id deflt

  bindings <- forM (M.toList (M.insert Nothing defltExp ruleMap)) \(k, e) -> do
    n <- newName ("r" <> maybe "D" show k)
    pure (k, n, e)

  let nameMap = M.fromList [(k, n) | (k, n, _) <- bindings]

  body <- genTrieCode nameMap trie
  letE
    [valD (varP n) (normalB (pure e)) [] | (_, n, e) <- bindings]
    (pure body)

genTrieCode :: Map (Maybe Int) Name -> Trie (Rule, Int) -> Q Exp
genTrieCode names = go 0 where
  ruleName r = case M.lookup r names of
    Just n  -> n
    Nothing -> error "switch: missing rule"

  go :: Int -> Trie (Rule, Int) -> Q Exp
  go ensured (Branch (rule, depth) ts)
    | M.null ts = pure (VarE (ruleName rule))
    | otherwise = do
        let need = ensured < 1
        branches <- forM (M.toList ts) \(w, sub) -> do
          e <- go (if need then depth - 1 else ensured - 1) sub
          pure (w, e)

        let defE = VarE (ruleName rule)
            body = mkDoE
              [ BindS (VarP (mkName "c")) (VarE 'switchAnyWord8Unsafe)
              , NoBindS (CaseE (VarE (mkName "c"))
                  (  [Match (LitP (IntegerL (fromIntegral w))) (NormalB e) []
                     | (w, e) <- branches]
                  <> [Match WildP (NormalB defE) []]))
              ]

        if need
          then [| switchBranch depth $(pure body) $(pure defE) |]
          else pure body

applyPost :: Maybe Exp -> Exp -> Exp
applyPost Nothing rhs  = rhs
applyPost (Just p) rhs = InfixE (Just p) (VarE '(>>)) (Just rhs)
