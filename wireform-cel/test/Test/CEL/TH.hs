{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests that CEL is compiled at compile time via "CEL.TH" (the spliced
-- 'Expr' is a baked-in constant; no runtime parsing).
module Test.CEL.TH (tests) where

import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import CEL
import CEL.TH (cel, celFn, compileCel)

-- Parsed at compile time into a top-level constant (interpreted at runtime).
arithExpr :: Expr
arithExpr = [cel| 1 + 2 * 3 |]

mapExpr :: Expr
mapExpr = [cel| [1, 2, 3].map(x, x * x) |]

-- Fully compiled to Haskell at compile time (no AST walk at runtime).
arithFn :: Env -> Either CelError Value
arithFn = [celFn| 1 + 2 * 3 |]

varFn :: Env -> Either CelError Value
varFn = [celFn| n * 2 |]

macroFn :: Env -> Either CelError Value
macroFn = [celFn| [1, 2, 3].exists(x, x > 2) && size('abc') == 3 |]

tests :: TestTree
tests =
  testGroup
    "CEL.TH (compile-time compilation)"
    [ testCase "cel quasiquoter: arithmetic" $
        evaluate emptyEnv arithExpr @?= Right (VInt 7)
    , testCase "cel quasiquoter: macro" $
        evaluate emptyEnv mapExpr @?= Right (VList (V.fromList [VInt 1, VInt 4, VInt 9]))
    , testCase "compileCel splice" $
        evaluate emptyEnv $(compileCel "'hello'.size()") @?= Right (VInt 5)
    , testCase "celFn: compiled arithmetic" $
        arithFn emptyEnv @?= Right (VInt 7)
    , testCase "celFn: compiled program reads the environment" $
        varFn (bind "n" (VInt 5) emptyEnv) @?= Right (VInt 10)
    , testCase "celFn: compiled macros + functions" $
        macroFn emptyEnv @?= Right (VBool True)
    ]
