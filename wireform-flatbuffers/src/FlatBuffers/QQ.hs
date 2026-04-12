-- | QuasiQuoter for inline FlatBuffers schema definitions.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import FlatBuffers.QQ
--
-- [fbs|
--   table Person { name:string; age:int; }
-- |]
-- @
module FlatBuffers.QQ
  ( fbs
  ) where

import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import FlatBuffers.Parser (parseFlatBuffers)
import FlatBuffers.CodeGen (deriveFlatBuffers)

-- | QuasiQuoter for FlatBuffers schema.
fbs :: QuasiQuoter
fbs = QuasiQuoter
  { quoteExp  = \_ -> fail "fbs: not an expression quoter"
  , quotePat  = \_ -> fail "fbs: not a pattern quoter"
  , quoteType = \_ -> fail "fbs: not a type quoter"
  , quoteDec  = fbsDec
  }

fbsDec :: String -> Q [Dec]
fbsDec src = do
  case parseFlatBuffers (T.pack src) of
    Left err -> fail ("fbs parse error: " ++ err)
    Right schema -> deriveFlatBuffers schema
