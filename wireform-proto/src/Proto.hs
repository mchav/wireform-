{- | Convenience re-export module for the common wireform-proto surface.

Import this module to get encoding, decoding, schema metadata,
the type registry, and extension support in one go.

@
import Proto

let bs = encodeMessage myMsg
case decodeMessage bs of
  Left err  -> handleError err
  Right msg -> use msg
@
-}
module Proto (
  -- * Encoding
  module Proto.Encode,

  -- * Decoding
  module Proto.Decode,

  -- * Schema metadata
  module Proto.Schema,

  -- * Type registry
  module Proto.Registry,

  -- * Extensions (proto2)
  module Proto.Extension,
) where

import Proto.Decode
import Proto.Encode
import Proto.Extension
import Proto.Registry
import Proto.Schema

