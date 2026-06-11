{- | Direct-to-AST HTML encoding, mirroring aeson's @toEncoding@
approach.

HTML's structural escape rules and tag-context-sensitive output
(script/style raw bodies, void elements, etc.) make a streaming
'Builder' a poor fit at the public boundary. 'HTML.Encoding.Encoding'
therefore wraps an 'HTML.Value.HTMLNode' directly. The aeson-style
API matches the rest of the formats so 'HTML.Class.toEncoding' is
uniform with them.
-}
module HTML.Encoding (
  Encoding (..),
  encodingToNode,
  encodingToByteString,
  node,
) where

import Data.ByteString (ByteString)
import HTML.Encode qualified as HE
import HTML.Value (HTMLDocument (..), HTMLNode)


newtype Encoding = Encoding {runEncoding :: HTMLNode}


encodingToNode :: Encoding -> HTMLNode
encodingToNode = runEncoding


encodingToByteString :: Encoding -> ByteString
encodingToByteString e = HE.encodeHTML (HTMLDocument Nothing (runEncoding e))


node :: HTMLNode -> Encoding
node = Encoding
