{-# LANGUAGE DeriveGeneric #-}
module Kafka.Streams.Types
  ( Timestamp(..)
  , Header
  , Headers
  , Record(..)
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Timestamp = Timestamp { unTimestamp :: Int64 }
  deriving (Eq, Ord, Show, Generic)

type Header = (Text, ByteString)
type Headers = Map Text ByteString

data Record k v = Record
  { recordKey       :: !(Maybe k)
  , recordValue     :: !v
  , recordTimestamp :: !Timestamp
  , recordHeaders   :: !Headers
  } deriving (Eq, Show, Generic)


