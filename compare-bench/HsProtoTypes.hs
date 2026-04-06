{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
-- | hs-proto types matching bench.proto for benchmark comparison.
module HsProtoTypes where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Generics (Generic)
import GHC.Exts (Int#, Int(I#))
import Control.DeepSeq (NFData)

import Proto.Encode
import Proto.Decode
import Proto.Encode.Archetype
import Proto.Encode.Direct
import qualified Proto.SizedBuilder as SB
import Proto.Wire (Tag(..), WireType(..))
import Proto.Wire.Encode (fieldVarintSize, fieldTextSize, fieldBytesSize,
  fieldBoolSize, fieldDoubleSize, fieldFloatSize, fieldMessageSize,
  fieldFixed32Size, fieldFixed64Size,
  putTag, putVarint, putLengthDelimited, putText, putByteString,
  precomputeTag, putPrecomputedTag, varintSize)
import Proto.Wire.Decode (runDecoder', DecodeResult(..), Decoder(..), withTag)
import Proto.VectorBuilder

-- Small

data HSmall = HSmall
  { hsId     :: {-# UNPACK #-} !Int64
  , hsName   :: !Text
  , hsActive :: !Bool
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedSmall :: HSmall -> SB.SizedBuilder
buildSizedSmall (HSmall i n a) =
  (if i == 0 then mempty else sbArchVarint 0x08 (fromIntegral i)) <>
  (if n == "" then mempty else sbArchString 0x12 n) <>
  (if not a then mempty else sbArchBool 0x18 True)
{-# INLINE buildSizedSmall #-}

instance MessageEncode HSmall where
  buildMessage m = SB.toBuilder (buildSizedSmall m)
  {-# INLINE buildMessage #-}

instance MessageSize HSmall where
  messageSize m = SB.size (buildSizedSmall m)
  {-# INLINE messageSize #-}

instance MessageDecode HSmall where
  messageDecoder = Decoder (\bs off -> loop 0 "" False bs off)
    where
      loop :: Int64 -> Text -> Bool -> ByteString -> Int# -> (# (# HSmall, Int# #) | DecodeError #)
      loop !i !n !a !bs !off =
        withTag bs off
          (\off' -> (# (# HSmall i n a, off' #) | #))
          (\fn _wt off' -> case I# fn of
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

buildSizedMedium :: HMedium -> SB.SizedBuilder
buildSizedMedium m =
  (if hmTitle m == "" then mempty else sbArchString 0x0a (hmTitle m)) <>
  (if hmCount m == 0 then mempty else sbArchVarint 0x10 (fromIntegral (hmCount m))) <>
  (if hmScore m == 0 then mempty else sbArchDouble 0x19 (hmScore m)) <>
  (if BS.null (hmPayload m) then mempty else sbArchBytes 0x22 (hmPayload m)) <>
  (if not (hmEnabled m) then mempty else sbArchBool 0x28 True) <>
  (if hmTimestamp m == 0 then mempty else sbArchVarint 0x30 (fromIntegral (hmTimestamp m))) <>
  (if hmDescription m == "" then mempty else sbArchString 0x3a (hmDescription m)) <>
  (if hmRatio m == 0 then mempty else sbArchFloat 0x45 (hmRatio m))
{-# INLINE buildSizedMedium #-}

instance MessageEncode HMedium where
  buildMessage m = SB.toBuilder (buildSizedMedium m)
  {-# INLINE buildMessage #-}

instance MessageSize HMedium where
  messageSize m = SB.size (buildSizedMedium m)
  {-# INLINE messageSize #-}

instance MessageDecode HMedium where
  messageDecoder = Decoder (\bs off -> loop "" 0 0 "" False 0 "" 0 bs off)
    where
      loop :: Text -> Int32 -> Double -> ByteString -> Bool -> Int64 -> Text -> Float -> ByteString -> Int# -> (# (# HMedium, Int# #) | DecodeError #)
      loop !t !c !sc !p !e !ts !d !r !bs !off =
        withTag bs off
          (\off' -> (# (# HMedium t c sc p e ts d r, off' #) | #))
          (\fn _wt off' -> case I# fn of
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
  { hwnId    :: {-# UNPACK #-} !Int64
  , hwnInner :: !(Maybe HSmall)
  , hwnLabel :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedNested :: HWithNested -> SB.SizedBuilder
buildSizedNested m =
  (if hwnId m == 0 then mempty else sbArchVarint 0x08 (fromIntegral (hwnId m))) <>
  maybe mempty (\inner -> sbArchSubmessage 0x12 (buildSizedSmall inner)) (hwnInner m) <>
  (if hwnLabel m == "" then mempty else sbArchString 0x1a (hwnLabel m))
{-# INLINE buildSizedNested #-}

instance MessageEncode HWithNested where
  buildMessage m = SB.toBuilder (buildSizedNested m)
  {-# INLINE buildMessage #-}

instance MessageSize HWithNested where
  messageSize m = SB.size (buildSizedNested m)
  {-# INLINE messageSize #-}

instance MessageDecode HWithNested where
  messageDecoder = Decoder (\bs off -> loop 0 Nothing "" bs off)
    where
      loop :: Int64 -> Maybe HSmall -> Text -> ByteString -> Int# -> (# (# HWithNested, Int# #) | DecodeError #)
      loop !i !inner !lbl !bs !off =
        withTag bs off
          (\off' -> (# (# HWithNested i inner lbl, off' #) | #))
          (\fn _wt off' -> case I# fn of
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
  , hwrTags   :: !(V.Vector Text)
  , hwrItems  :: !(V.Vector HSmall)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass NFData

buildSizedRepeated :: HWithRepeated -> SB.SizedBuilder
buildSizedRepeated m =
  sbArchPackedVarints 0x0a (hwrValues m) <>
  V.foldl' (\acc s -> acc <> sbArchString 0x12 s) mempty (hwrTags m) <>
  V.foldl' (\acc item -> acc <> sbArchSubmessage 0x1a (buildSizedSmall item)) mempty (hwrItems m)
{-# INLINE buildSizedRepeated #-}

instance MessageEncode HWithRepeated where
  buildMessage m = SB.toBuilder (buildSizedRepeated m)
  {-# INLINE buildMessage #-}

instance MessageSize HWithRepeated where
  messageSize m = SB.size (buildSizedRepeated m)
  {-# INLINE messageSize #-}

instance MessageDecode HWithRepeated where
  messageDecoder = Decoder (\bs off -> loop emptyGrowList emptyGrowList emptyGrowList bs off)
    where
      loop :: GrowList Int32 -> GrowList Text -> GrowList HSmall -> ByteString -> Int# -> (# (# HWithRepeated, Int# #) | DecodeError #)
      loop !vals !tags !items !bs !off =
        withTag bs off
          (\off' -> (# (# HWithRepeated (growListToVectorU vals) (growListToVector tags) (growListToVector items), off' #) | #))
          (\fn wt off' -> case I# fn of
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
    len = BS.length bs
    go !acc !off
      | off >= len = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (snocGrowList acc (fromIntegral v)) off'
          DecodeFail _    -> acc

-- | Direct-write encode for HSmall. Zero Builder overhead.
directEncodeSmall :: HSmall -> ByteString
directEncodeSmall msg =
  let !sz = SB.size (buildSizedSmall msg)
  in directEncode sz (writeSmall msg)
{-# NOINLINE directEncodeSmall #-}

writeSmall :: HSmall -> WriteCtx -> IO WriteCtx
writeSmall (HSmall i n a) ctx = do
  ctx1 <- if i == 0 then pure ctx
          else dWord8 0x08 ctx >>= dVarint (fromIntegral i)
  ctx2 <- if n == "" then pure ctx1
          else dWord8 0x12 ctx1 >>= dText n
  if not a then pure ctx2
  else dWord8 0x18 ctx2 >>= dWord8 1
{-# INLINE writeSmall #-}

-- | Direct-write encode for HMedium.
directEncodeMedium :: HMedium -> ByteString
directEncodeMedium msg =
  let !sz = SB.size (buildSizedMedium msg)
  in directEncode sz (writeMedium msg)
{-# NOINLINE directEncodeMedium #-}

writeMedium :: HMedium -> WriteCtx -> IO WriteCtx
writeMedium m ctx = do
  ctx1 <- if hmTitle m == "" then pure ctx
          else dWord8 0x0a ctx >>= dText (hmTitle m)
  ctx2 <- if hmCount m == 0 then pure ctx1
          else dWord8 0x10 ctx1 >>= dVarint (fromIntegral (hmCount m))
  ctx3 <- if hmScore m == 0 then pure ctx2
          else dWord8 0x19 ctx2 >>= dDoubleLE (hmScore m)
  ctx4 <- if BS.null (hmPayload m) then pure ctx3
          else dWord8 0x22 ctx3 >>= dByteString (hmPayload m)
  ctx5 <- if not (hmEnabled m) then pure ctx4
          else dWord8 0x28 ctx4 >>= dWord8 1
  ctx6 <- if hmTimestamp m == 0 then pure ctx5
          else dWord8 0x30 ctx5 >>= dVarint (fromIntegral (hmTimestamp m))
  ctx7 <- if hmDescription m == "" then pure ctx6
          else dWord8 0x3a ctx6 >>= dText (hmDescription m)
  if hmRatio m == 0 then pure ctx7
  else dWord8 0x45 ctx7 >>= dFloatLE (hmRatio m)
{-# INLINE writeMedium #-}

-- | Direct-write encode for HWithNested.
directEncodeNested :: HWithNested -> ByteString
directEncodeNested msg =
  let !sz = SB.size (buildSizedNested msg)
  in directEncode sz (writeNested msg)
{-# NOINLINE directEncodeNested #-}

writeNested :: HWithNested -> WriteCtx -> IO WriteCtx
writeNested m ctx = do
  ctx1 <- if hwnId m == 0 then pure ctx
          else dWord8 0x08 ctx >>= dVarint (fromIntegral (hwnId m))
  ctx2 <- case hwnInner m of
    Nothing -> pure ctx1
    Just inner -> do
      let !innerSz = SB.size (buildSizedSmall inner)
      ctx1a <- dWord8 0x12 ctx1
      ctx1b <- dVarint (fromIntegral innerSz) ctx1a
      writeSmall inner ctx1b
  if hwnLabel m == "" then pure ctx2
  else dWord8 0x1a ctx2 >>= dText (hwnLabel m)
{-# INLINE writeNested #-}

-- | Direct-write encode for HWithRepeated.
directEncodeRepeated :: HWithRepeated -> ByteString
directEncodeRepeated msg =
  let !sz = SB.size (buildSizedRepeated msg)
  in directEncode sz (writeRepeated msg)
{-# NOINLINE directEncodeRepeated #-}

writeRepeated :: HWithRepeated -> WriteCtx -> IO WriteCtx
writeRepeated m ctx = do
  ctx1 <- if VU.null (hwrValues m) then pure ctx
          else do
            let (!packedSz, _) = VU.foldl' (\(!sz, !_) v ->
                  let !w = fromIntegral v :: Word64
                  in (sz + varintSize w, ())) (0, ()) (hwrValues m)
            ctx1a <- dWord8 0x0a ctx
            ctx1b <- dVarint (fromIntegral packedSz) ctx1a
            VU.foldM' (\c v -> dVarint (fromIntegral v) c) ctx1b (hwrValues m)
  ctx2 <- V.foldM' (\c s -> dWord8 0x12 c >>= dText s) ctx1 (hwrTags m)
  V.foldM' (\c item -> do
    let !innerSz = SB.size (buildSizedSmall item)
    c1 <- dWord8 0x1a c
    c2 <- dVarint (fromIntegral innerSz) c1
    writeSmall item c2) ctx2 (hwrItems m)
{-# INLINE writeRepeated #-}

