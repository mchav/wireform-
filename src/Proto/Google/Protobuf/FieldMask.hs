{-# LANGUAGE BangPatterns #-}
module Proto.Google.Protobuf.FieldMask
  ( FieldMask (..)
  , defaultFieldMask
  ) where

import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.JSON
import Proto.Decode
import Proto.Wire (Tag (..))
import Proto.Wire.Encode (fieldTextSize)

data FieldMask = FieldMask
  { paths :: !(V.Vector Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

defaultFieldMask :: FieldMask
defaultFieldMask = FieldMask V.empty

instance MessageEncode FieldMask where
  buildMessage (FieldMask ps) =
    V.foldl' (\acc p -> acc <> encodeFieldString 1 p) mempty ps
  {-# INLINE buildMessage #-}

instance MessageDecode FieldMask where
  messageDecoder = loop V.empty
    where
      loop !ps = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (FieldMask ps)
          Just (Tag 1 _) -> do
            p <- decodeFieldString
            loop (V.snoc ps p)
          Just (Tag _ wt) -> skipField wt >> loop ps
  {-# INLINE messageDecoder #-}

instance MessageSize FieldMask where
  messageSize (FieldMask ps) = V.foldl' (\acc p -> acc + fieldTextSize 1 p) 0 ps

instance ProtoToJSON FieldMask where
  protoToJSON _ = JsonNull

instance ProtoFromJSON FieldMask where
  protoFromJSON _ = Right defaultFieldMask
