{- | EDN (Extensible Data Notation) value representation.

EDN is a subset of Clojure's data syntax, commonly used for configuration
files and data exchange in the Clojure ecosystem. This module defines a
dynamically-typed value that can represent any EDN datum: nil, booleans,
integers, floats, strings, characters, keywords, symbols, lists, vectors,
maps, sets, and tagged literals.

@
import qualified EDN.Value as E
import qualified EDN.Encode as EE
import qualified EDN.Decode as ED
import qualified Data.Vector as V

let val = E.Map (V.fromList [(E.Keyword Nothing \"name\", E.String \"Alice\")])
let text = EE.encode val
let Right decoded = ED.decode text
@
-}
module EDN.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)


data Value
  = Nil
  | Bool !Bool
  | Integer !Integer
  | Float {-# UNPACK #-} !Double
  | String !Text
  | Char !Char
  | Keyword !(Maybe Text) !Text
  | Symbol !(Maybe Text) !Text
  | List !(Vector Value)
  | Vector !(Vector Value)
  | Map !(Vector (Value, Value))
  | Set !(Vector Value)
  | Tagged !Text !Text !Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
