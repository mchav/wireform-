{- | Direct-to-AST Amazon Ion encoding, mirroring aeson's
@toEncoding@ approach.

Ion's binary wire format relies on length-prefixed and TLV-encoded
containers; a streaming 'Builder' offers little benefit over the
existing AST-driven 'Ion.Encode.encode'. 'Ion.Encoding.Encoding'
therefore wraps a 'Ion.Value.Value' directly. The API matches the
aeson-style 'toEncoding' shape so 'Ion.Class' is uniform with the
other formats and a future direct-write path can be slotted in
without an API break.
-}
module Ion.Encoding (
  Encoding (..),
  encodingToValue,
  encodingToByteString,
  value,
) where

import Data.ByteString (ByteString)
import Ion.Encode qualified as IE
import Ion.Value qualified as IV


newtype Encoding = Encoding {runEncoding :: IV.Value}


encodingToValue :: Encoding -> IV.Value
encodingToValue = runEncoding


encodingToByteString :: Encoding -> ByteString
encodingToByteString = IE.encode . runEncoding


value :: IV.Value -> Encoding
value = Encoding
