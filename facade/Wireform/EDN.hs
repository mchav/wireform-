-- | Convenience re-exports for EDN (Extensible Data Notation) serialization.
--
-- @
-- import qualified Wireform.EDN as EDN
-- @
module Wireform.EDN
  ( module EDN.Value
  , module EDN.Encode
  , module EDN.Decode
  , module EDN.Class
  ) where

import EDN.Value
import EDN.Encode
import EDN.Decode
import EDN.Class
