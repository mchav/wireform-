-- | Convenience re-exports for Avro serialization.
--
-- @
-- import qualified Wireform.Avro as Avro
-- @
module Wireform.Avro
  ( module Avro.Value
  , module Avro.Schema
  , module Avro.Encode
  , module Avro.Decode
  , module Avro.Class
  ) where

import Avro.Value
import Avro.Schema
import Avro.Encode
import Avro.Decode
import Avro.Class
