{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Duration
  ( Duration (..)
  , defaultDuration
  ) where

import Data.Int (Int32, Int64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldVarintSize)

data Duration = Duration
  { seconds :: {-# UNPACK #-} !Int64
  , nanos   :: {-# UNPACK #-} !Int32
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass NFData

defaultDuration :: Duration
defaultDuration = Duration 0 0

instance MessageEncode Duration where
  buildMessage (Duration s n) =
    (if s == 0 then mempty else encodeFieldVarint 1 (fromIntegral s)) <>
    (if n == 0 then mempty else encodeFieldVarint 2 (fromIntegral n))
  {-# INLINE buildMessage #-}

instance MessageSize Duration where
  messageSize (Duration s n) =
    (if s == 0 then 0 else fieldVarintSize 1 (fromIntegral s)) +
    (if n == 0 then 0 else fieldVarintSize 2 (fromIntegral n))
  {-# INLINE messageSize #-}

instance MessageDecode Duration where
  messageDecoder = loop 0 0
    where
      loop !s !n = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Duration s n)
          Just (Tag fn wt) -> case fn of
            1 -> do v <- getVarint; loop (fromIntegral v) n
            2 -> do v <- getVarint; loop s (fromIntegral v)
            _ -> skipField wt >> loop s n
  {-# INLINE messageDecoder #-}
