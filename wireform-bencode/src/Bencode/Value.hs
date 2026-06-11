{- | Bencode value representation (BitTorrent encoding).

Bencode is a simple binary encoding used by BitTorrent. It supports
byte strings, integers, lists, and dictionaries (sorted by key).
-}
module Bencode.Value (
  Value (..),
) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import GHC.Generics (Generic)


data Value
  = BString !ByteString
  | BInteger !Integer
  | BList !(Vector Value)
  | BDict !(Vector (ByteString, Value))
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)
