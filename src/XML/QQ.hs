{-# LANGUAGE TemplateHaskell #-}
-- | QuasiQuoter for inline XML literals.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
-- import XML.QQ
--
-- myNode :: Node
-- myNode = [xml|\<person\>\<name\>John\<\/name\>\<age\>30\<\/age\>\<\/person\>|]
-- @
--
-- Parses the XML at compile time and produces a 'Node' literal.
module XML.QQ
  ( xml
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import qualified XML.Value as XV
import qualified XML.Decode as XD

-- | QuasiQuoter for XML literals.
xml :: QuasiQuoter
xml = QuasiQuoter
  { quoteExp  = xmlExp
  , quotePat  = \_ -> fail "xml: patterns not supported"
  , quoteType = \_ -> fail "xml: types not supported"
  , quoteDec  = \_ -> fail "xml: declarations not supported"
  }

xmlExp :: String -> Q Exp
xmlExp str =
  case XD.decode (TE.encodeUtf8 (T.pack str)) of
    Left err -> fail $ "xml quasi-quoter: parse error: " ++ err
    Right doc -> liftNode (XV.docRoot doc)

liftNode :: XV.Node -> Q Exp
liftNode (XV.Element name attrs children) = do
  n <- liftXName name
  as <- liftVector liftAttr attrs
  cs <- liftVector liftNode children
  [| XV.Element $(pure n) $(pure as) $(pure cs) |]
liftNode (XV.Text t) =
  [| XV.Text $(litE (stringL (T.unpack t))) |]
liftNode (XV.CData t) =
  [| XV.CData $(litE (stringL (T.unpack t))) |]
liftNode (XV.Comment t) =
  [| XV.Comment $(litE (stringL (T.unpack t))) |]
liftNode (XV.ProcessingInstruction target content) =
  [| XV.ProcessingInstruction $(litE (stringL (T.unpack target)))
                              $(litE (stringL (T.unpack content))) |]

liftXName :: XV.Name -> Q Exp
liftXName (XV.Name local mPrefix mNs) = do
  let localE = litE (stringL (T.unpack local))
  pfxE <- case mPrefix of
    Nothing -> [| Nothing |]
    Just p  -> [| Just $(litE (stringL (T.unpack p))) |]
  nsE <- case mNs of
    Nothing -> [| Nothing |]
    Just n  -> [| Just $(litE (stringL (T.unpack n))) |]
  [| XV.Name $(localE) $(pure pfxE) $(pure nsE) |]

liftAttr :: XV.Attribute -> Q Exp
liftAttr (XV.Attribute name val) = do
  n <- liftXName name
  [| XV.Attribute $(pure n) $(litE (stringL (T.unpack val))) |]

liftVector :: (a -> Q Exp) -> Vector a -> Q Exp
liftVector f vec = do
  elems <- mapM f (V.toList vec)
  [| V.fromList $(listE (map pure elems)) |]
