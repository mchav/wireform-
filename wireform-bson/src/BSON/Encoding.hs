-- | Direct-to-AST BSON encoding, mirroring aeson's @toEncoding@
-- approach.
--
-- Because BSON documents carry a leading length prefix (computed
-- after the body is fully built), a streaming 'Data.ByteString.Builder'
-- offers little benefit over the existing AST-driven 'BSON.Encode.encode'.
-- 'BSON.Encoding.Encoding' therefore wraps a 'BSON.Value.Value'
-- directly: it gives 'BSON.Class.toEncoding' a uniform aeson-style
-- API, and the AST-to-bytes pass already uses 'directEncode' for a
-- single allocation per document.
module BSON.Encoding
  ( Encoding (..)
  , encodingToValue
  , encodingToByteString
  , value
  ) where

import Data.ByteString (ByteString)

import qualified BSON.Encode as BE
import qualified BSON.Value as B

newtype Encoding = Encoding { runEncoding :: B.Value }

encodingToValue :: Encoding -> B.Value
encodingToValue = runEncoding

encodingToByteString :: Encoding -> ByteString
encodingToByteString = BE.encode . runEncoding

-- | Lift a fully-built 'B.Value' into an 'Encoding'.
value :: B.Value -> Encoding
value = Encoding
