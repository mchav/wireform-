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
import CEL.TH (cel, compileCel)

-- Parsed at compile time into a top-level constant.
arithExpr :: Expr
arithExpr = [cel| 1 + 2 * 3 |]

mapExpr :: Expr
mapExpr = [cel| [1, 2, 3].map(x, x * x) |]

tests :: TestTree
tests =
  testGroup
    "CEL.TH (compile-time compilation)"
    [ testCase "quasiquoter: arithmetic" $
        evaluate emptyEnv arithExpr @?= Right (VInt 7)
    , testCase "quasiquoter: macro" $
        evaluate emptyEnv mapExpr @?= Right (VList (V.fromList [VInt 1, VInt 4, VInt 9]))
    , testCase "splice form compiles at compile time" $
        evaluate emptyEnv $(compileCel "'hello'.size()") @?= Right (VInt 5)
    ]
