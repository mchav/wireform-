{-# LANGUAGE TemplateHaskell #-}

{- | QuasiQuoters for inline XML literals and XSD schema declarations.

@
{\-\# LANGUAGE QuasiQuotes \#-\}
import XML.QQ

myNode :: Node
myNode = [xml|\<person\>\<name\>John\<\/name\>\<age\>30\<\/age\>\<\/person\>|]
@

@
{\-\# LANGUAGE QuasiQuotes \#-\}
{\-\# LANGUAGE TemplateHaskell \#-\}
import XML.QQ

[xsd|
  \<xs:schema xmlns:xs=\"http:\/\/www.w3.org\/2001\/XMLSchema\"\>
    \<xs:complexType name=\"Person\"\>
      \<xs:sequence\>
        \<xs:element name=\"name\" type=\"xs:string\"\/\>
        \<xs:element name=\"age\" type=\"xs:integer\"\/\>
      \<\/xs:sequence\>
    \<\/xs:complexType\>
  \<\/xs:schema\>
|]
@

Parses the XML at compile time and produces a 'Node' literal (@xml@),
or generates Haskell data types from an XSD schema (@xsd@).
-}
module XML.QQ (
  xml,
  xsd,
) where

import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import XML.CodeGen (deriveXSD)
import XML.Decode qualified as XD
import XML.Schema (parseXSD)
import XML.Value qualified as XV


-- | QuasiQuoter for XML literals.
xml :: QuasiQuoter
xml =
  QuasiQuoter
    { quoteExp = xmlExp
    , quotePat = \_ -> fail "xml: patterns not supported"
    , quoteType = \_ -> fail "xml: types not supported"
    , quoteDec = \_ -> fail "xml: declarations not supported"
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
  [|XV.Element $(pure n) $(pure as) $(pure cs)|]
liftNode (XV.Text t) =
  [|XV.Text $(litE (stringL (T.unpack t)))|]
liftNode (XV.CData t) =
  [|XV.CData $(litE (stringL (T.unpack t)))|]
liftNode (XV.Comment t) =
  [|XV.Comment $(litE (stringL (T.unpack t)))|]
liftNode (XV.ProcessingInstruction target content) =
  [|
    XV.ProcessingInstruction
      $(litE (stringL (T.unpack target)))
      $(litE (stringL (T.unpack content)))
    |]


liftXName :: XV.Name -> Q Exp
liftXName (XV.Name local mPrefix mNs) = do
  let localE = litE (stringL (T.unpack local))
  pfxE <- case mPrefix of
    Nothing -> [|Nothing|]
    Just p -> [|Just $(litE (stringL (T.unpack p)))|]
  nsE <- case mNs of
    Nothing -> [|Nothing|]
    Just n -> [|Just $(litE (stringL (T.unpack n)))|]
  [|XV.Name $(localE) $(pure pfxE) $(pure nsE)|]


liftAttr :: XV.Attribute -> Q Exp
liftAttr (XV.Attribute name val) = do
  n <- liftXName name
  [|XV.Attribute $(pure n) $(litE (stringL (T.unpack val)))|]


liftVector :: (a -> Q Exp) -> Vector a -> Q Exp
liftVector f vec = do
  elems <- mapM f (V.toList vec)
  [|V.fromList $(listE (map pure elems))|]


{- | QuasiQuoter for XSD schema declarations.
Parses the XSD at compile time and generates Haskell data types
with @ToXML@\/@FromXML@ instances.
-}
xsd :: QuasiQuoter
xsd =
  QuasiQuoter
    { quoteExp = \_ -> fail "xsd: not an expression quoter"
    , quotePat = \_ -> fail "xsd: not a pattern quoter"
    , quoteType = \_ -> fail "xsd: not a type quoter"
    , quoteDec = xsdDec
    }


xsdDec :: String -> Q [Dec]
xsdDec src =
  case parseXSD (T.pack src) of
    Left err -> fail ("xsd parse error: " ++ err)
    Right schema -> deriveXSD schema
