-- | Direct-to-AST Thrift encoding, mirroring aeson's @toEncoding@
-- approach.
--
-- Thrift's wire formats (binary and compact) require both a
-- container's field id and the inferred 'ThriftType' of every field
-- before any bytes can be emitted; building an 'Encoding' that
-- streams bytes directly would force the caller to pre-commit to a
-- protocol. We therefore expose 'Encoding' as a thin wrapper around
-- 'Thrift.Value.Value' so 'Thrift.Class.toEncoding' is uniform with
-- the other formats and a future direct-write path can be slotted
-- in without an API break.
module Thrift.Encoding
  ( Encoding (..)
  , encodingToValue
  , encodingToBinaryByteString
  , encodingToCompactByteString
  , value
  ) where

import Data.ByteString (ByteString)

import qualified Thrift.Encode as TE
import qualified Thrift.Value as TV

newtype Encoding = Encoding { runEncoding :: TV.Value }

encodingToValue :: Encoding -> TV.Value
encodingToValue = runEncoding

encodingToBinaryByteString :: Encoding -> ByteString
encodingToBinaryByteString = TE.encodeBinary . runEncoding

encodingToCompactByteString :: Encoding -> ByteString
encodingToCompactByteString = TE.encodeCompact . runEncoding

value :: TV.Value -> Encoding
value = Encoding
