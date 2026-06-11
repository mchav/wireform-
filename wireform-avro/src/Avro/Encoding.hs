{- | Direct-to-AST Avro encoding, mirroring aeson's @toEncoding@
approach.

Avro requires a schema to write bytes (the wire format is
self-describing only via the accompanying schema), so unlike the
self-describing formats we cannot ship a 'ByteString' producer
here. Instead we expose 'Encoding' as a thin wrapper around an
'Avro.Value.Value' so 'Avro.Class.toEncoding' has the same
aeson-style shape as the other formats.
-}
module Avro.Encoding (
  Encoding (..),
  encodingToValue,
  value,
) where

import Avro.Value qualified as AV


newtype Encoding = Encoding {runEncoding :: AV.Value}


encodingToValue :: Encoding -> AV.Value
encodingToValue = runEncoding


value :: AV.Value -> Encoding
value = Encoding
