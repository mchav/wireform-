{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.SourceContext
  ( SourceContext (..)
  , defaultSourceContext
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldTextSize)

data SourceContext = SourceContext
  { fileName :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultSourceContext :: SourceContext
defaultSourceContext = SourceContext ""

instance MessageEncode SourceContext where
  buildMessage (SourceContext fn) =
    if fn == "" then mempty else encodeFieldString 1 fn
  {-# INLINE buildMessage #-}

instance MessageSize SourceContext where
  messageSize (SourceContext fn) =
    if fn == "" then 0 else fieldTextSize 1 fn
  {-# INLINE messageSize #-}

instance MessageDecode SourceContext where
  messageDecoder = loop ""
    where
      loop !fn = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (SourceContext fn)
          Just (Tag 1 _) -> decodeFieldString >>= \x -> loop x
          Just (Tag _ wt) -> skipField wt >> loop fn
  {-# INLINE messageDecoder #-}
