{- | QuasiQuoter for inline CDDL (RFC 8610) definitions.

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import CBOR.QQ

[cddl|
  person = { name: tstr, age: uint }
|]
@
-}
module CBOR.QQ (
  cddl,
) where

import CBOR.CDDL (parseCDDL)
import CBOR.CDDLCodeGen (deriveCDDL)
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- | QuasiQuoter for CDDL schemas.
cddl :: QuasiQuoter
cddl =
  QuasiQuoter
    { quoteExp = \_ -> fail "cddl: not an expression quoter"
    , quotePat = \_ -> fail "cddl: not a pattern quoter"
    , quoteType = \_ -> fail "cddl: not a type quoter"
    , quoteDec = cddlDec
    }


cddlDec :: String -> Q [Dec]
cddlDec src = do
  case parseCDDL (T.pack src) of
    Left err -> fail ("cddl parse error: " ++ err)
    Right schema -> deriveCDDL schema
