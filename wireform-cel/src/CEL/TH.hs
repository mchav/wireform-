-- | Compile CEL source at /compile time/ into a baked-in 'Expr'.
--
-- For static CEL (string literals known at compile time), there is no reason
-- to re-parse and re-compile on every evaluation. This module parses the CEL
-- once, at compile time, and splices the resulting 'CEL.Syntax.Expr' into your
-- program as an ordinary Haskell constant (the AST derives 'Lift'). A syntax
-- error in the CEL becomes a compile error.
--
-- @
-- {-# LANGUAGE QuasiQuotes #-}
-- import CEL
-- import CEL.TH (cel)
--
-- program :: Expr
-- program = [cel| 1 + 2 * 3 |]   -- parsed at compile time
--
-- result :: Either CelError Value
-- result = evaluate emptyEnv program
-- @
--
-- Use the 'cel' quasiquoter or the 'compileCel' splice. The compiled 'Expr' is
-- evaluated with the normal 'CEL.evaluate', so it still respects the runtime
-- binding environment.
module CEL.TH
  ( cel
  , compileCel
  ) where

import qualified Data.Text as T
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (lift)

import CEL.Parser (parse)

-- | Splice that parses CEL source at compile time and yields the baked-in
-- 'CEL.Syntax.Expr'. Parse failures are reported as compile errors.
compileCel :: String -> Q Exp
compileCel src = case parse (T.pack src) of
  Left err -> fail ("CEL.TH: parse error: " <> err)
  Right expr -> lift expr

-- | An expression quasiquoter: @[cel| this.size() > 0 |]@ elaborates to a
-- compile-time-parsed 'CEL.Syntax.Expr'.
cel :: QuasiQuoter
cel =
  QuasiQuoter
    { quoteExp = compileCel
    , quotePat = \_ -> fail "CEL.TH.cel: cannot be used as a pattern"
    , quoteType = \_ -> fail "CEL.TH.cel: cannot be used as a type"
    , quoteDec = \_ -> fail "CEL.TH.cel: cannot be used as a declaration"
    }
