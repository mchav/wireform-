{- | Direct-to-AST YAML encoding, mirroring aeson's @toEncoding@
approach.

YAML's block-vs-flow / indentation decisions depend on the
surrounding document position, so a streaming 'Builder' is a
poor fit; 'YAML.Encoding.Encoding' wraps a fully-built
'YAML.Value.Value' and routes through 'YAML.Encode.encode'. The
aeson-style API matches the rest of the formats so it can be
swapped in transparently if a streaming pretty-printer arrives.
-}
module YAML.Encoding (
  Encoding (..),
  encodingToValue,
  encodingToText,
  value,
) where

import Data.Text (Text)
import YAML.Encode qualified as YE
import YAML.Value qualified as YV


newtype Encoding = Encoding {runEncoding :: YV.Value}


encodingToValue :: Encoding -> YV.Value
encodingToValue = runEncoding


encodingToText :: Encoding -> Text
encodingToText = YE.encode . runEncoding


value :: YV.Value -> Encoding
value = Encoding
