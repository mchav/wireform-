{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.Any
  ( Any (..)
  , defaultAny
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldTextSize, fieldBytesSize)

data Any = Any
  { typeUrl :: !Text
  , value   :: !ByteString
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultAny :: Any
defaultAny = Any "" ""

instance MessageEncode Any where
  buildMessage (Any tu v) =
    (if tu == "" then mempty else encodeFieldString 1 tu) <>
    (if BS.null v then mempty else encodeFieldBytes 2 v)
  {-# INLINE buildMessage #-}

instance MessageSize Any where
  messageSize (Any tu v) =
    (if tu == "" then 0 else fieldTextSize 1 tu) +
    (if BS.null v then 0 else fieldBytesSize 2 v)
  {-# INLINE messageSize #-}

instance MessageDecode Any where
  messageDecoder = loop "" ""
    where
      loop !tu !v = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Any tu v)
          Just (Tag fn wt) -> case fn of
            1 -> do t <- decodeFieldString; loop t v
            2 -> do b <- decodeFieldBytes; loop tu b
            _ -> skipField wt >> loop tu v
  {-# INLINE messageDecoder #-}
