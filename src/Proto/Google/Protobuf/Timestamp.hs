{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Timestamp
  ( Timestamp (..)
  , defaultTimestamp
  ) where

import Data.Int (Int32, Int64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize)

data Timestamp = Timestamp
  { seconds :: {-# UNPACK #-} !Int64
  , nanos   :: {-# UNPACK #-} !Int32
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

defaultTimestamp :: Timestamp
defaultTimestamp = Timestamp 0 0

instance MessageEncode Timestamp where
  buildMessage (Timestamp s n) =
    (if s == 0 then mempty else encodeFieldVarint 1 (fromIntegral s)) <>
    (if n == 0 then mempty else encodeFieldVarint 2 (fromIntegral n))
  {-# INLINE buildMessage #-}

instance MessageSize Timestamp where
  messageSize (Timestamp s n) =
    (if s == 0 then 0 else fieldVarintSize 1 (fromIntegral s)) +
    (if n == 0 then 0 else fieldVarintSize 2 (fromIntegral n))
  {-# INLINE messageSize #-}

instance MessageDecode Timestamp where
  messageDecoder = loop 0 0
    where
      loop !s !n = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Timestamp s n)
          Just (Tag fn wt) -> case fn of
            1 -> do v <- getVarint; loop (fromIntegral v) n
            2 -> do v <- getVarint; loop s (fromIntegral v)
            _ -> skipField wt >> loop s n
  {-# INLINE messageDecoder #-}
