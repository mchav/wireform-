-- | EDN (Extensible Data Notation) value representation.
module EDN.Value
  ( Value(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data Value
  = Nil
  | Bool       !Bool
  | Integer    !Integer
  | Float      {-# UNPACK #-} !Double
  | String     !Text
  | Char       !Char
  | Keyword    !(Maybe Text) !Text
  | Symbol     !(Maybe Text) !Text
  | List       !(Vector Value)
  | Vector     !(Vector Value)
  | Map        !(Vector (Value, Value))
  | Set        !(Vector Value)
  | Tagged     !Text !Text !Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
