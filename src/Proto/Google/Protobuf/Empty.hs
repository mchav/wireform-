{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Empty
  ( Empty (..)
  , defaultEmpty
  ) where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))

data Empty = Empty
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NFData

defaultEmpty :: Empty
defaultEmpty = Empty

instance MessageEncode Empty where
  buildMessage Empty = mempty
  {-# INLINE buildMessage #-}

instance MessageSize Empty where
  messageSize Empty = 0
  {-# INLINE messageSize #-}

instance MessageDecode Empty where
  messageDecoder = loop
    where
      loop = do
        mt <- getTagOr
        case mt of
          Nothing -> pure Empty
          Just (Tag _ wt) -> skipField wt >> loop
  {-# INLINE messageDecoder #-}
