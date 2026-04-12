-- | Convenience re-exports for Cap'n Proto serialization.
--
-- @
-- import qualified Wireform.CapnProto as CapnProto
-- @
module Wireform.CapnProto
  ( module CapnProto.Value
  , module CapnProto.Encode
  , module CapnProto.Decode
  ) where

import CapnProto.Value
import CapnProto.Encode
import CapnProto.Decode
