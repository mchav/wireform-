module Kafka.Streams.Serde
  ( Serde(..)
  , imap
  ) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)

data Serde a = Serde
  { serialize   :: a -> ByteString
  , deserialize :: ByteString -> Either String a
  }

imap :: (b -> a) -> (a -> b) -> Serde a -> Serde b
imap toA fromA s = Serde
  { serialize = serialize s . toA
  , deserialize = fmap fromA . deserialize s
  }


