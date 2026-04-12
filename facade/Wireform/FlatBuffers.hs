-- | Convenience re-exports for FlatBuffers serialization.
--
-- @
-- import qualified Wireform.FlatBuffers as FlatBuffers
-- @
module Wireform.FlatBuffers
  ( module FlatBuffers.Value
  , module FlatBuffers.Encode
  , module FlatBuffers.Decode
  ) where

import FlatBuffers.Value
import FlatBuffers.Encode
import FlatBuffers.Decode
