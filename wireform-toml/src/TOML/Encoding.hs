-- | Direct-to-AST TOML encoding, mirroring aeson's @toEncoding@
-- approach.
--
-- TOML's section-aware syntax (top-level inline-vs-table decisions
-- depend on the document position) makes a streaming 'Builder' a
-- poor fit; 'TOML.Encoding.Encoding' therefore wraps a fully-built
-- 'TOML.Value.Value' and routes through 'TOML.Encode.encode'. The
-- aeson-style API matches the rest of the formats so it can be
-- swapped in transparently if a streaming pretty-printer arrives.
module TOML.Encoding
  ( Encoding (..)
  , encodingToValue
  , encodingToText
  , value
  ) where

import Data.Text (Text)

import qualified TOML.Encode as TE
import qualified TOML.Value as TV

newtype Encoding = Encoding { runEncoding :: TV.Value }

encodingToValue :: Encoding -> TV.Value
encodingToValue = runEncoding

encodingToText :: Encoding -> Text
encodingToText = TE.encode . runEncoding

value :: TV.Value -> Encoding
value = Encoding
