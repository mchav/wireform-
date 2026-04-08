-- | Convenience re-exports for NDJSON (Newline-Delimited JSON) serialization.
--
-- @
-- import qualified Wireform.NDJSON as NDJSON
-- @
module Wireform.NDJSON
  ( module NDJSON.Decode
  , module NDJSON.Encode
  ) where

import NDJSON.Decode
import NDJSON.Encode
