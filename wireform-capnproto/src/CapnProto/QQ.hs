-- | QuasiQuoter for inline Cap'n Proto schema definitions.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import CapnProto.QQ
--
-- [capnp|
--   struct Person { name \@0 :Text; age \@1 :UInt32; }
-- |]
-- @
module CapnProto.QQ
  ( capnp
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import CapnProto.Parser (parseCapnProto)
import CapnProto.CodeGen (deriveCapnProto)

-- | QuasiQuoter for Cap'n Proto schema.
capnp :: QuasiQuoter
capnp = QuasiQuoter
  { quoteExp  = \_ -> fail "capnp: not an expression quoter"
  , quotePat  = \_ -> fail "capnp: not a pattern quoter"
  , quoteType = \_ -> fail "capnp: not a type quoter"
  , quoteDec  = capnpDec
  }

capnpDec :: String -> Q [Dec]
capnpDec src = do
  case parseCapnProto (T.pack src) of
    Left err -> fail ("capnp parse error: " ++ err)
    Right schema -> deriveCapnProto schema
