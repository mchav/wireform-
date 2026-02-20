{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
-- | hs-proto types matching bench.proto for benchmark comparison.
module HsProtoTypes where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Wire (Tag(..), WireType(..))
import Proto.Wire.Encode (fieldVarintSize, fieldTextSize, fieldBytesSize,
  fieldBoolSize, fieldDoubleSize, fieldFloatSize, fieldMessageSize,
  putTag, putVarint, putLengthDelimited)
import Proto.Encode (encodePackedVarint)
import Proto.Wire.Decode (runDecoder', DecodeResult(..))

-- Small

data HSmall = HSmall
  { hsId     :: {-# UNPACK #-} !Int64
  , hsName   :: !Text
  , hsActive :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

instance MessageEncode HSmall where
  buildMessage (HSmall i n a) =
    (if i == 0 then mempty else encodeFieldVarint 1 (fromIntegral i)) <>
    (if n == "" then mempty else encodeFieldString 2 n) <>
    (if not a then mempty else encodeFieldBool 3 a)
  {-# INLINE buildMessage #-}

instance MessageSize HSmall where
  messageSize (HSmall i n a) =
    (if i == 0 then 0 else fieldVarintSize 1 (fromIntegral i)) +
    (if n == "" then 0 else fieldTextSize 2 n) +
    (if not a then 0 else fieldBoolSize 3)
  {-# INLINE messageSize #-}

instance MessageDecode HSmall where
  messageDecoder = loop 0 "" False
    where
      loop !i !n !a = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (HSmall i n a)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop (fromIntegral v) n a
            2 -> getText >>= \v -> loop i v a
            3 -> getVarint >>= \v -> loop i n (v /= 0)
            _ -> skipField wt >> loop i n a
  {-# INLINE messageDecoder #-}

-- Medium

data HMedium = HMedium
  { hmTitle       :: !Text
  , hmCount       :: {-# UNPACK #-} !Int32
  , hmScore       :: {-# UNPACK #-} !Double
  , hmPayload     :: !ByteString
  , hmEnabled     :: !Bool
  , hmTimestamp   :: {-# UNPACK #-} !Int64
  , hmDescription :: !Text
  , hmRatio       :: {-# UNPACK #-} !Float
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

instance MessageEncode HMedium where
  buildMessage m =
    (if hmTitle m == "" then mempty else encodeFieldString 1 (hmTitle m)) <>
    (if hmCount m == 0 then mempty else encodeFieldVarint 2 (fromIntegral (hmCount m))) <>
    (if hmScore m == 0 then mempty else encodeFieldDouble 3 (hmScore m)) <>
    (if BS.null (hmPayload m) then mempty else encodeFieldBytes 4 (hmPayload m)) <>
    (if not (hmEnabled m) then mempty else encodeFieldBool 5 (hmEnabled m)) <>
    (if hmTimestamp m == 0 then mempty else encodeFieldVarint 6 (fromIntegral (hmTimestamp m))) <>
    (if hmDescription m == "" then mempty else encodeFieldString 7 (hmDescription m)) <>
    (if hmRatio m == 0 then mempty else encodeFieldFloat 8 (hmRatio m))
  {-# INLINE buildMessage #-}

instance MessageDecode HMedium where
  messageDecoder = loop "" 0 0 "" False 0 "" 0
    where
      loop !t !c !sc !p !e !ts !d !r = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (HMedium t c sc p e ts d r)
          Just (Tag fn wt) -> case fn of
            1 -> decodeFieldString >>= \v -> loop v c sc p e ts d r
            2 -> getVarint >>= \v -> loop t (fromIntegral v) sc p e ts d r
            3 -> getDouble >>= \v -> loop t c v p e ts d r
            4 -> decodeFieldBytes >>= \v -> loop t c sc v e ts d r
            5 -> getVarint >>= \v -> loop t c sc p (v /= 0) ts d r
            6 -> getVarint >>= \v -> loop t c sc p e (fromIntegral v) d r
            7 -> decodeFieldString >>= \v -> loop t c sc p e ts v r
            8 -> getFloat >>= \v -> loop t c sc p e ts d v
            _ -> skipField wt >> loop t c sc p e ts d r
  {-# INLINE messageDecoder #-}

-- WithNested

data HWithNested = HWithNested
  { hwnId    :: {-# UNPACK #-} !Int64
  , hwnInner :: !(Maybe HSmall)
  , hwnLabel :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

instance MessageEncode HWithNested where
  buildMessage m =
    (if hwnId m == 0 then mempty else encodeFieldVarint 1 (fromIntegral (hwnId m))) <>
    maybe mempty (encodeFieldMessage 2) (hwnInner m) <>
    (if hwnLabel m == "" then mempty else encodeFieldString 3 (hwnLabel m))
  {-# INLINE buildMessage #-}

instance MessageDecode HWithNested where
  messageDecoder = loop 0 Nothing ""
    where
      loop !i !inner !lbl = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (HWithNested i inner lbl)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop (fromIntegral v) inner lbl
            2 -> decodeFieldMessage >>= \v -> loop i (Just v) lbl
            3 -> decodeFieldString >>= \v -> loop i inner v
            _ -> skipField wt >> loop i inner lbl
  {-# INLINE messageDecoder #-}

-- WithRepeated

data HWithRepeated = HWithRepeated
  { hwrValues :: !(V.Vector Int32)
  , hwrTags   :: !(V.Vector Text)
  , hwrItems  :: !(V.Vector HSmall)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

instance MessageEncode HWithRepeated where
  buildMessage m =
    (let vs = hwrValues m in if V.null vs then mempty
       else encodePackedVarint 1 (VU.convert (V.map fromIntegral vs))) <>
    V.foldl' (\acc s -> acc <> encodeFieldString 2 s) mempty (hwrTags m) <>
    V.foldl' (\acc item -> acc <> encodeFieldMessage 3 item) mempty (hwrItems m)
  {-# INLINE buildMessage #-}

instance MessageDecode HWithRepeated where
  messageDecoder = loop [] [] []
    where
      loop !vals !tags !items = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (HWithRepeated (V.fromList (reverse vals)) (V.fromList (reverse tags)) (V.fromList (reverse items)))
          Just (Tag fn wt) -> case fn of
            1 -> case wt of
              WireLengthDelimited -> do
                bs <- getLengthDelimited
                let !parsed = decodePacked bs
                loop (reversePrepend parsed vals) tags items
              _ -> getVarint >>= \v -> loop (fromIntegral v : vals) tags items
            2 -> decodeFieldString >>= \v -> loop vals (v : tags) items
            3 -> decodeFieldMessage >>= \v -> loop vals tags (v : items)
            _ -> skipField wt >> loop vals tags items
  {-# INLINE messageDecoder #-}

reversePrepend :: [a] -> [a] -> [a]
reversePrepend [] ys = ys
reversePrepend (x:xs) ys = reversePrepend xs (x : ys)

decodePacked :: ByteString -> [Int32]
decodePacked bs = go [] 0
  where
    len = BS.length bs
    go !acc !off
      | off >= len = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (fromIntegral v : acc) off'
          DecodeFail _    -> acc

