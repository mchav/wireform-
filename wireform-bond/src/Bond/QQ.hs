{- | QuasiQuoter for inline Bond IDL definitions.

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import Bond.QQ

[bond|
  struct Person { 0: string name; 1: int32 age; }
|]
@
-}
module Bond.QQ (
  bond,
) where

import Bond.CodeGen (deriveBond)
import Bond.Parser (parseBond)
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- | QuasiQuoter for Bond IDL.
bond :: QuasiQuoter
bond =
  QuasiQuoter
    { quoteExp = \_ -> fail "bond: not an expression quoter"
    , quotePat = \_ -> fail "bond: not a pattern quoter"
    , quoteType = \_ -> fail "bond: not a type quoter"
    , quoteDec = bondDec
    }


bondDec :: String -> Q [Dec]
bondDec src = do
  case parseBond (T.pack src) of
    Left err -> fail ("bond parse error: " ++ err)
    Right schema -> deriveBond schema
