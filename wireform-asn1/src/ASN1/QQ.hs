{- | QuasiQuoter for inline ASN.1 module definitions.

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import ASN1.QQ

[asn1|
  MyModule DEFINITIONS AUTOMATIC TAGS ::= BEGIN
    Person ::= SEQUENCE { name UTF8String, age INTEGER }
  END
|]
@
-}
module ASN1.QQ (
  asn1,
) where

import ASN1.CodeGen (deriveASN1)
import ASN1.Parser (parseASN1Module)
import Data.Text qualified as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote


-- | QuasiQuoter for ASN.1 module definitions.
asn1 :: QuasiQuoter
asn1 =
  QuasiQuoter
    { quoteExp = \_ -> fail "asn1: not an expression quoter"
    , quotePat = \_ -> fail "asn1: not a pattern quoter"
    , quoteType = \_ -> fail "asn1: not a type quoter"
    , quoteDec = asn1Dec
    }


asn1Dec :: String -> Q [Dec]
asn1Dec src = do
  case parseASN1Module (T.pack src) of
    Left err -> fail ("asn1 parse error: " ++ err)
    Right modl -> deriveASN1 modl
