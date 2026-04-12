-- | Convenience re-exports for BSON (Binary JSON) serialization.
--
-- @
-- import qualified Wireform.BSON as BSON
-- @
module Wireform.BSON
  ( module BSON.Value
  , module BSON.Encode
  , module BSON.Decode
  , module BSON.Class
  ) where

import BSON.Value
import BSON.Encode
import BSON.Decode
import BSON.Class
