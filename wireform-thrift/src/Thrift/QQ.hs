-- | QuasiQuoter for inline Thrift IDL definitions.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Thrift.QQ
--
-- [thrift|
--   struct Person { 1: string name; 2: i32 age; }
-- |]
-- @
module Thrift.QQ
  ( thrift
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Thrift.Parser (parseThrift)
import Thrift.CodeGen (deriveThrift)

-- | QuasiQuoter for Thrift IDL.
thrift :: QuasiQuoter
thrift = QuasiQuoter
  { quoteExp  = \_ -> fail "thrift: not an expression quoter"
  , quotePat  = \_ -> fail "thrift: not a pattern quoter"
  , quoteType = \_ -> fail "thrift: not a type quoter"
  , quoteDec  = thriftDec
  }

thriftDec :: String -> Q [Dec]
thriftDec src = do
  case parseThrift (T.pack src) of
    Left err -> fail ("thrift parse error: " ++ err)
    Right schema -> deriveThrift schema
