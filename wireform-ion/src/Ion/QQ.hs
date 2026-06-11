{- | QuasiQuoter for inline Ion Schema Language (ISL) definitions.

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import Ion.QQ

[isl|
  type::{ name: Person, fields: { name: string, age: int } }
|]
@
-}
module Ion.QQ (
  isl,
) where

import Data.Text qualified as T
import Ion.ISLCodeGen (deriveISL)
import Ion.SchemaLang (parseISL)
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- | QuasiQuoter for Ion Schema Language.
isl :: QuasiQuoter
isl =
  QuasiQuoter
    { quoteExp = \_ -> fail "isl: not an expression quoter"
    , quotePat = \_ -> fail "isl: not a pattern quoter"
    , quoteType = \_ -> fail "isl: not a type quoter"
    , quoteDec = islDec
    }


islDec :: String -> Q [Dec]
islDec src = do
  case parseISL (T.pack src) of
    Left err -> fail ("isl parse error: " ++ err)
    Right schema -> deriveISL schema
