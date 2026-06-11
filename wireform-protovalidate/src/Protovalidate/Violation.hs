-- | A validation violation: the equivalent of a @buf.validate.Violation@.
module Protovalidate.Violation (
  Violation (..),
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import GHC.Generics (Generic)


-- | A single failed constraint.
data Violation = Violation
  { violationFieldPath :: !Text
  {- ^ Dotted/indexed path to the offending field, e.g. @"items[2].id"@.
  Empty for message-level constraints.
  -}
  , violationConstraintId :: !Text
  -- ^ The constraint identifier, e.g. @"string.email"@ or a custom id.
  , violationMessage :: !Text
  -- ^ Human-readable description of why validation failed.
  }
  deriving stock (Eq, Show, Generic)


instance NFData Violation
