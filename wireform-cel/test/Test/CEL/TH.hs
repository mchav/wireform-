{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests that CEL is compiled at compile time via "CEL.TH" (the spliced
-- 'Expr' is a baked-in constant; no runtime parsing).
module Test.CEL.TH (tests) where

import qualified Data.Vector as V
import Test.Syd

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

tests :: Spec
tests =
  describe
    "CEL.TH (compile-time compilation)" $ sequence_
    [ it "cel quasiquoter: arithmetic" $
        evaluate emptyEnv arithExpr `shouldBe` Right (VInt 7)
    , it "cel quasiquoter: macro" $
        evaluate emptyEnv mapExpr `shouldBe` Right (VList (V.fromList [VInt 1, VInt 4, VInt 9]))
    , it "compileCel splice" $
        evaluate emptyEnv $(compileCel "'hello'.size()") `shouldBe` Right (VInt 5)
    , it "celFn: compiled arithmetic" $
        arithFn emptyEnv `shouldBe` Right (VInt 7)
    , it "celFn: compiled program reads the environment" $
        varFn (bind "n" (VInt 5) emptyEnv) `shouldBe` Right (VInt 10)
    , it "celFn: compiled macros + functions" $
        macroFn emptyEnv `shouldBe` Right (VBool True)
    ]
