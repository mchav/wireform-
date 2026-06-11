{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedTuples #-}

-- | wireform types matching bench.proto for benchmark comparison.
module WireformTypes where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64, Word8)
import GHC.Exts (Int (I#), Int#)
import GHC.Generics (Generic)
import Proto.Decode
import Proto.Encode
import Proto.Internal.Encode.Archetype
import Proto.Internal.GrowList
import Proto.Internal.SizedBuilder qualified as SB
import Proto.Internal.Wire.Decode (Decoder (..), runDecoder')
import Proto.Internal.Wire.Encode (putText, putVarint, varintSize)
import Wireform.Builder qualified as FB


-- Small

data HSmall = HSmall
  { hsId :: {-# UNPACK #-} !Int64
  , hsName :: !Text
  , hsActive :: !Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


buildSizedSmall :: HSmall -> SB.SizedBuilder
buildSizedSmall (HSmall i n a) =
  (if i == 0 then mempty else sbArchVarint 0x08 (fromIntegral i))
    <> (if n == "" then mempty else sbArchString 0x12 n)
    <> (if not a then mempty else sbArchBool 0x18 True)
{-# INLINE buildSizedSmall #-}


instance MessageEncode HSmall where
  buildMessage = buildSmallDirect
  {-# INLINE buildMessage #-}


instance MessageSize HSmall where
  messageSize = sizeSmall
  {-# INLINE messageSize #-}


-- Direct builder path (skip SizedBuilder overhead)
buildSmallDirect :: HSmall -> FB.Builder
buildSmallDirect (HSmall i n a) =
  (if i == 0 then mempty else FB.word8 0x08 <> putVarint (fromIntegral i))
    <> (if n == "" then mempty else FB.word8 0x12 <> putText n)
    <> (if not a then mempty else FB.word8 0x18 <> FB.word8 1)
{-# INLINE buildSmallDirect #-}


encodeSmallDirect :: HSmall -> ByteString
encodeSmallDirect m =
  let !sz = sizeSmall m
  in FB.toStrictByteStringExact sz (buildSmallDirect m)
{-# NOINLINE encodeSmallDirect #-}


buildMediumDirect :: HMedium -> FB.Builder
buildMediumDirect m =
  (if hmTitle m == "" then mempty else FB.word8 0x0a <> putText (hmTitle m))
    <> (if hmCount m == 0 then mempty else FB.word8 0x10 <> putVarint (fromIntegral (hmCount m)))
    <> (if hmScore m == 0 then mempty else FB.word8 0x19 <> FB.doubleLE (hmScore m))
    <> (if BS.null (hmPayload m) then mempty else FB.word8 0x22 <> putVarint (fromIntegral (BS.length (hmPayload m))) <> FB.byteStringCopy (hmPayload m))
    <> (if not (hmEnabled m) then mempty else FB.word8 0x28 <> FB.word8 1)
    <> (if hmTimestamp m == 0 then mempty else FB.word8 0x30 <> putVarint (fromIntegral (hmTimestamp m)))
    <> (if hmDescription m == "" then mempty else FB.word8 0x3a <> putText (hmDescription m))
    <> (if hmRatio m == 0 then mempty else FB.word8 0x45 <> FB.floatLE (hmRatio m))
{-# INLINE buildMediumDirect #-}


encodeMediumDirect :: HMedium -> ByteString
encodeMediumDirect m =
  let !sz = sizeMedium m
  in FB.toStrictByteStringExact sz (buildMediumDirect m)
{-# NOINLINE encodeMediumDirect #-}


instance MessageDecode HSmall where
  messageDecoder = Decoder (loop 0 "" False)
    where
      loop :: Int64 -> Text -> Bool -> ByteString -> Int# -> (# (# HSmall, Int# #) | DecodeError #)
      loop !i !n !a !bs !off =
        withTag
          bs
          off
          (\off' -> (# (# HSmall i n a, off' #) | #))
          ( \fn _wt off' -> case I# fn of
              1 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop (fromIntegral v) n a bs off''
                (# | e #) -> (# | e #)
              2 -> case runDecoder# getText bs off' of
                (# (# v, off'' #) | #) -> loop i v a bs off''
                (# | e #) -> (# | e #)
              3 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop i n (v /= 0) bs off''
                (# | e #) -> (# | e #)
              _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
                (# (# _, off'' #) | #) -> loop i n a bs off''
                (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}


-- Medium

data HMedium = HMedium
  { hmTitle :: !Text
  , hmCount :: {-# UNPACK #-} !Int32
  , hmScore :: {-# UNPACK #-} !Double
  , hmPayload :: !ByteString
  , hmEnabled :: !Bool
  , hmTimestamp :: {-# UNPACK #-} !Int64
  , hmDescription :: !Text
  , hmRatio :: {-# UNPACK #-} !Float
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


buildSizedMedium :: HMedium -> SB.SizedBuilder
buildSizedMedium m =
  (if hmTitle m == "" then mempty else sbArchString 0x0a (hmTitle m))
    <> (if hmCount m == 0 then mempty else sbArchVarint 0x10 (fromIntegral (hmCount m)))
    <> (if hmScore m == 0 then mempty else sbArchDouble 0x19 (hmScore m))
    <> (if BS.null (hmPayload m) then mempty else sbArchBytes 0x22 (hmPayload m))
    <> (if not (hmEnabled m) then mempty else sbArchBool 0x28 True)
    <> (if hmTimestamp m == 0 then mempty else sbArchVarint 0x30 (fromIntegral (hmTimestamp m)))
    <> (if hmDescription m == "" then mempty else sbArchString 0x3a (hmDescription m))
    <> (if hmRatio m == 0 then mempty else sbArchFloat 0x45 (hmRatio m))
{-# INLINE buildSizedMedium #-}


instance MessageEncode HMedium where
  buildMessage m = SB.toBuilder (buildSizedMedium m)
  {-# INLINE buildMessage #-}


instance MessageSize HMedium where
  messageSize m = SB.size (buildSizedMedium m)
  {-# INLINE messageSize #-}


instance MessageDecode HMedium where
  messageDecoder = Decoder (loop "" 0 0 "" False 0 "" 0)
    where
      loop :: Text -> Int32 -> Double -> ByteString -> Bool -> Int64 -> Text -> Float -> ByteString -> Int# -> (# (# HMedium, Int# #) | DecodeError #)
      loop !t !c !sc !p !e !ts !d !r !bs !off =
        withTag
          bs
          off
          (\off' -> (# (# HMedium t c sc p e ts d r, off' #) | #))
          ( \fn _wt off' -> case I# fn of
              1 -> case runDecoder# decodeFieldString bs off' of
                (# (# v, off'' #) | #) -> loop v c sc p e ts d r bs off''
                (# | err #) -> (# | err #)
              2 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop t (fromIntegral v) sc p e ts d r bs off''
                (# | err #) -> (# | err #)
              3 -> case runDecoder# getDouble bs off' of
                (# (# v, off'' #) | #) -> loop t c v p e ts d r bs off''
                (# | err #) -> (# | err #)
              4 -> case runDecoder# decodeFieldBytes bs off' of
                (# (# v, off'' #) | #) -> loop t c sc v e ts d r bs off''
                (# | err #) -> (# | err #)
              5 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop t c sc p (v /= 0) ts d r bs off''
                (# | err #) -> (# | err #)
              6 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop t c sc p e (fromIntegral v) d r bs off''
                (# | err #) -> (# | err #)
              7 -> case runDecoder# decodeFieldString bs off' of
                (# (# v, off'' #) | #) -> loop t c sc p e ts v r bs off''
                (# | err #) -> (# | err #)
              8 -> case runDecoder# getFloat bs off' of
                (# (# v, off'' #) | #) -> loop t c sc p e ts d v bs off''
                (# | err #) -> (# | err #)
              _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
                (# (# _, off'' #) | #) -> loop t c sc p e ts d r bs off''
                (# | err #) -> (# | err #)
          )
          (\err -> (# | err #))
  {-# INLINE messageDecoder #-}


-- WithNested

data HWithNested = HWithNested
  { hwnId :: {-# UNPACK #-} !Int64
  , hwnInner :: !(Maybe HSmall)
  , hwnLabel :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


buildSizedNested :: HWithNested -> SB.SizedBuilder
buildSizedNested m =
  (if hwnId m == 0 then mempty else sbArchVarint 0x08 (fromIntegral (hwnId m)))
    <> maybe mempty (sbArchSubmessage 0x12 . buildSizedSmall) (hwnInner m)
    <> (if hwnLabel m == "" then mempty else sbArchString 0x1a (hwnLabel m))
{-# INLINE buildSizedNested #-}


instance MessageEncode HWithNested where
  buildMessage m = SB.toBuilder (buildSizedNested m)
  {-# INLINE buildMessage #-}


instance MessageSize HWithNested where
  messageSize m = SB.size (buildSizedNested m)
  {-# INLINE messageSize #-}


instance MessageDecode HWithNested where
  messageDecoder = Decoder (loop 0 Nothing "")
    where
      loop :: Int64 -> Maybe HSmall -> Text -> ByteString -> Int# -> (# (# HWithNested, Int# #) | DecodeError #)
      loop !i !inner !lbl !bs !off =
        withTag
          bs
          off
          (\off' -> (# (# HWithNested i inner lbl, off' #) | #))
          ( \fn _wt off' -> case I# fn of
              1 -> case runDecoder# getVarint bs off' of
                (# (# v, off'' #) | #) -> loop (fromIntegral v) inner lbl bs off''
                (# | e #) -> (# | e #)
              2 -> case runDecoder# decodeFieldMessage bs off' of
                (# (# v, off'' #) | #) -> loop i (Just v) lbl bs off''
                (# | e #) -> (# | e #)
              3 -> case runDecoder# decodeFieldString bs off' of
                (# (# v, off'' #) | #) -> loop i inner v bs off''
                (# | e #) -> (# | e #)
              _ -> case runDecoder# (skipField (toEnum (I# _wt))) bs off' of
                (# (# _, off'' #) | #) -> loop i inner lbl bs off''
                (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}


-- WithRepeated

data HWithRepeated = HWithRepeated
  { hwrValues :: !(VU.Vector Int32)
  , hwrTags :: !(V.Vector Text)
  , hwrItems :: !(V.Vector HSmall)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


buildSizedRepeated :: HWithRepeated -> SB.SizedBuilder
buildSizedRepeated m =
  sbArchPackedVarints 0x0a (hwrValues m)
    <> V.foldl' (\acc s -> acc <> sbArchString 0x12 s) mempty (hwrTags m)
    <> V.foldl' (\acc item -> acc <> sbArchSubmessage 0x1a (buildSizedSmall item)) mempty (hwrItems m)
{-# INLINE buildSizedRepeated #-}


instance MessageEncode HWithRepeated where
  buildMessage m = SB.toBuilder (buildSizedRepeated m)
  {-# INLINE buildMessage #-}


instance MessageSize HWithRepeated where
  messageSize m = SB.size (buildSizedRepeated m)
  {-# INLINE messageSize #-}


instance MessageDecode HWithRepeated where
  messageDecoder = Decoder (loop emptyGrowList emptyGrowList emptyGrowList)
    where
      loop :: GrowList Int32 -> GrowList Text -> GrowList HSmall -> ByteString -> Int# -> (# (# HWithRepeated, Int# #) | DecodeError #)
      loop !vals !tags !items !bs !off =
        withTag
          bs
          off
          (\off' -> (# (# HWithRepeated (growListToVectorU vals) (growListToVector tags) (growListToVector items), off' #) | #))
          ( \fn wt off' -> case I# fn of
              1 -> case I# wt of
                2 -> case runDecoder# getLengthDelimited bs off' of
                  (# (# chunk, off'' #) | #) ->
                    let !vals' = decodePackedInto vals chunk
                    in loop vals' tags items bs off''
                  (# | e #) -> (# | e #)
                _ -> case runDecoder# getVarint bs off' of
                  (# (# v, off'' #) | #) -> loop (snocGrowList vals (fromIntegral v)) tags items bs off''
                  (# | e #) -> (# | e #)
              2 -> case runDecoder# decodeFieldString bs off' of
                (# (# v, off'' #) | #) -> loop vals (snocGrowList tags v) items bs off''
                (# | e #) -> (# | e #)
              3 -> case runDecoder# decodeFieldMessage bs off' of
                (# (# v, off'' #) | #) -> loop vals tags (snocGrowList items v) bs off''
                (# | e #) -> (# | e #)
              _ -> case runDecoder# (skipField (toEnum (I# wt))) bs off' of
                (# (# _, off'' #) | #) -> loop vals tags items bs off''
                (# | e #) -> (# | e #)
          )
          (\e -> (# | e #))
  {-# INLINE messageDecoder #-}


decodePackedInto :: GrowList Int32 -> ByteString -> GrowList Int32
decodePackedInto !gl bs = go gl 0
  where
    bsLen = BS.length bs
    go !acc !off
      | off >= bsLen = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (snocGrowList acc (fromIntegral v)) off'
          DecodeFail _ -> acc


sizeSmall :: HSmall -> Int
sizeSmall (HSmall i n a) =
  (if i == 0 then 0 else archVarintSize (fromIntegral i))
    + (if n == "" then 0 else archStringSize n)
    + (if not a then 0 else archBoolSize)
{-# INLINE sizeSmall #-}


sizeMedium :: HMedium -> Int
sizeMedium m =
  (if hmTitle m == "" then 0 else archStringSize (hmTitle m))
    + (if hmCount m == 0 then 0 else archVarintSize (fromIntegral (hmCount m)))
    + (if hmScore m == 0 then 0 else archFixed64Size)
    + (if BS.null (hmPayload m) then 0 else archBytesSize (hmPayload m))
    + (if not (hmEnabled m) then 0 else archBoolSize)
    + (if hmTimestamp m == 0 then 0 else archVarintSize (fromIntegral (hmTimestamp m)))
    + (if hmDescription m == "" then 0 else archStringSize (hmDescription m))
    + (if hmRatio m == 0 then 0 else archFixed32Size)
{-# INLINE sizeMedium #-}


sizeNested :: HWithNested -> Int
sizeNested m =
  (if hwnId m == 0 then 0 else archVarintSize (fromIntegral (hwnId m)))
    + maybe 0 (archSubmessageSize . sizeSmall) (hwnInner m)
    + (if hwnLabel m == "" then 0 else archStringSize (hwnLabel m))
{-# INLINE sizeNested #-}


sizeRepeated :: HWithRepeated -> Int
sizeRepeated m =
  ( let vs = hwrValues m
    in if VU.null vs
         then 0
         else
           let !packedSz = VU.foldl' (\acc v -> acc + varintSize (fromIntegral v :: Word64)) 0 vs
           in 1 + varintSize (fromIntegral packedSz) + packedSz
  )
    + V.foldl' (\acc s -> acc + archStringSize s) 0 (hwrTags m)
    + V.foldl' (\acc item -> acc + archSubmessageSize (sizeSmall item)) 0 (hwrItems m)
{-# INLINE sizeRepeated #-}
