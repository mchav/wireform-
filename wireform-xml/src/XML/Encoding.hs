{- | Direct-to-AST XML encoding, mirroring aeson's @toEncoding@
approach.

XML's wire format requires balanced tag emission and namespace
bookkeeping that depend on the surrounding context, so a streaming
'Builder' would lose those guarantees. 'XML.Encoding.Encoding'
therefore wraps an 'XML.Value.Node' directly. The aeson-style API
gives 'XML.Class.toEncoding' a uniform shape with the rest of the
formats.
-}
module XML.Encoding (
  Encoding (..),
  encodingToNode,
  encodingToByteString,
  node,
) where

import Data.ByteString (ByteString)
import XML.Encode qualified as XE
import XML.Value (Document (..), Node)


newtype Encoding = Encoding {runEncoding :: Node}


encodingToNode :: Encoding -> Node
encodingToNode = runEncoding


encodingToByteString :: Encoding -> ByteString
encodingToByteString e = XE.encode (Document Nothing (runEncoding e))


node :: Node -> Encoding
node = Encoding
