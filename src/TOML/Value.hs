-- | TOML value representation.
--
-- Supports all TOML v1.0 types: strings, integers, floats, booleans,
-- date/time types, arrays, and tables (ordered key-value pairs).
module TOML.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Value
  = TString !Text
  | TInteger !Integer
  | TFloat !Double
  | TBool !Bool
  | TDateTime !Text
  | TDate !Text
  | TTime !Text
  | TArray !(Vector Value)
  | TTable !(Vector (Text, Value))
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
