-- | QuasiQuoters for inline Avro definitions.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Avro.QQ
--
-- [avdl|
--   protocol P { record Person { string name; int age; } }
-- |]
--
-- [avsc|
--   {"type":"record","name":"Event","fields":[{"name":"id","type":"long"}]}
-- |]
-- @
module Avro.QQ
  ( avdl
  , avsc
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Avro.CodeGen (deriveAvro)
import Avro.IDL (parseAvroIDL, AvroIDL(..))
import Avro.IDLConvert (idlToType)
import Avro.Schema.Parse (parseAvroSchema)
import qualified Data.Vector as V

-- | QuasiQuoter for Avro IDL (.avdl syntax).
avdl :: QuasiQuoter
avdl = QuasiQuoter
  { quoteExp  = \_ -> fail "avdl: not an expression quoter"
  , quotePat  = \_ -> fail "avdl: not a pattern quoter"
  , quoteType = \_ -> fail "avdl: not a type quoter"
  , quoteDec  = avdlDec
  }

avdlDec :: String -> Q [Dec]
avdlDec src = do
  case parseAvroIDL (T.pack src) of
    Left err -> fail ("avdl parse error: " ++ err)
    Right idl -> do
      let types = map idlToType (V.toList (aidlDeclarations idl))
      concat <$> mapM deriveAvro types

-- | QuasiQuoter for Avro JSON schema (.avsc syntax).
avsc :: QuasiQuoter
avsc = QuasiQuoter
  { quoteExp  = \_ -> fail "avsc: not an expression quoter"
  , quotePat  = \_ -> fail "avsc: not a pattern quoter"
  , quoteType = \_ -> fail "avsc: not a type quoter"
  , quoteDec  = avscDec
  }

avscDec :: String -> Q [Dec]
avscDec src = do
  case parseAvroSchema (TE.encodeUtf8 (T.pack src)) of
    Left err -> fail ("avsc parse error: " ++ err)
    Right schema -> deriveAvro schema
