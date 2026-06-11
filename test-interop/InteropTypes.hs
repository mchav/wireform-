{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Haskell types matching interop.proto, with hand-rolled encode/decode.
module InteropTypes where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Proto.Decode
import Proto.Encode
import Proto.Internal.Wire (Tag (..), WireType (..))
import Proto.Internal.Wire.Decode (runDecoder')
import Proto.Internal.Wire.Encode (putLengthDelimited, putTag, putVarint)


data Color = ColorUnspecified | ColorRed | ColorGreen | ColorBlue
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (NFData)


colorToInt :: Color -> Int
colorToInt ColorUnspecified = 0
colorToInt ColorRed = 1
colorToInt ColorGreen = 2
colorToInt ColorBlue = 3


colorFromInt :: Word64 -> Color
colorFromInt 0 = ColorUnspecified
colorFromInt 1 = ColorRed
colorFromInt 2 = ColorGreen
colorFromInt 3 = ColorBlue
colorFromInt _ = ColorUnspecified


data Scalars = Scalars
  { sfDouble :: !Double
  , sfFloat :: !Float
  , sfInt32 :: !Int32
  , sfInt64 :: !Int64
  , sfUint32 :: !Word32
  , sfUint64 :: !Word64
  , sfSint32 :: !Int32
  , sfSint64 :: !Int64
  , sfFixed32 :: !Word32
  , sfFixed64 :: !Word64
  , sfSfixed32 :: !Int32
  , sfSfixed64 :: !Int64
  , sfBool :: !Bool
  , sfString :: !Text
  , sfBytes :: !ByteString
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


defaultScalars :: Scalars
defaultScalars = Scalars 0 0 0 0 0 0 0 0 0 0 0 0 False "" ""


instance MessageEncode Scalars where
  buildMessage s =
    (if sfDouble s == 0 then mempty else encodeFieldDouble 1 (sfDouble s))
      <> (if sfFloat s == 0 then mempty else encodeFieldFloat 2 (sfFloat s))
      <> (if sfInt32 s == 0 then mempty else encodeFieldVarint 3 (fromIntegral (sfInt32 s)))
      <> (if sfInt64 s == 0 then mempty else encodeFieldVarint 4 (fromIntegral (sfInt64 s)))
      <> (if sfUint32 s == 0 then mempty else encodeFieldVarint 5 (fromIntegral (sfUint32 s)))
      <> (if sfUint64 s == 0 then mempty else encodeFieldVarint 6 (sfUint64 s))
      <> (if sfSint32 s == 0 then mempty else encodeFieldSVarint32 7 (sfSint32 s))
      <> (if sfSint64 s == 0 then mempty else encodeFieldSVarint64 8 (sfSint64 s))
      <> (if sfFixed32 s == 0 then mempty else encodeFieldFixed32 9 (sfFixed32 s))
      <> (if sfFixed64 s == 0 then mempty else encodeFieldFixed64 10 (sfFixed64 s))
      <> (if sfSfixed32 s == 0 then mempty else encodeFieldFixed32 11 (fromIntegral (sfSfixed32 s)))
      <> (if sfSfixed64 s == 0 then mempty else encodeFieldFixed64 12 (fromIntegral (sfSfixed64 s)))
      <> (if not (sfBool s) then mempty else encodeFieldBool 13 True)
      <> (if sfString s == "" then mempty else encodeFieldString 14 (sfString s))
      <> (if BS.null (sfBytes s) then mempty else encodeFieldBytes 15 (sfBytes s))


instance MessageDecode Scalars where
  messageDecoder = go defaultScalars
    where
      go !s = do
        mt <- getTagOr
        case mt of
          Nothing -> pure s
          Just (Tag fn wt) -> case fn of
            1 -> getDouble >>= \v -> go s {sfDouble = v}
            2 -> getFloat >>= \v -> go s {sfFloat = v}
            3 -> getVarint >>= \v -> go s {sfInt32 = fromIntegral v}
            4 -> getVarint >>= \v -> go s {sfInt64 = fromIntegral v}
            5 -> getVarint >>= \v -> go s {sfUint32 = fromIntegral v}
            6 -> getVarint >>= \v -> go s {sfUint64 = v}
            7 -> getSVarint32 >>= \v -> go s {sfSint32 = v}
            8 -> getSVarint64 >>= \v -> go s {sfSint64 = v}
            9 -> getFixed32 >>= \v -> go s {sfFixed32 = v}
            10 -> getFixed64 >>= \v -> go s {sfFixed64 = v}
            11 -> getFixed32 >>= \v -> go s {sfSfixed32 = fromIntegral v}
            12 -> getFixed64 >>= \v -> go s {sfSfixed64 = fromIntegral v}
            13 -> getVarint >>= \v -> go s {sfBool = v /= 0}
            14 -> decodeFieldString >>= \v -> go s {sfString = v}
            15 -> decodeFieldBytes >>= \v -> go s {sfBytes = v}
            _ -> skipField wt >> go s


data Nested = Nested
  { nLabel :: !Text
  , nPayload :: !(Maybe Scalars)
  , nColor :: !Color
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance MessageEncode Nested where
  buildMessage n =
    (if nLabel n == "" then mempty else encodeFieldString 1 (nLabel n))
      <> maybe mempty (encodeFieldMessage 2) (nPayload n)
      <> (if nColor n == ColorUnspecified then mempty else encodeFieldVarint 3 (fromIntegral (colorToInt (nColor n))))


instance MessageDecode Nested where
  messageDecoder = go "" Nothing ColorUnspecified
    where
      go !lbl !payload !color = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Nested lbl payload color)
          Just (Tag fn wt) -> case fn of
            1 -> decodeFieldString >>= \v -> go v payload color
            2 -> decodeFieldMessage >>= \v -> go lbl (Just v) color
            3 -> getVarint >>= \v -> go lbl payload (colorFromInt v)
            _ -> skipField wt >> go lbl payload color


data Repeated = Repeated
  { rInts :: !(V.Vector Int32)
  , rStrings :: !(V.Vector Text)
  , rItems :: !(V.Vector Scalars)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (NFData)


instance MessageEncode Repeated where
  buildMessage r =
    let ints = rInts r
        packedInts =
          if V.null ints
            then mempty
            else
              let payload = messageToByteString (V.foldl' (\acc v -> acc <> putVarint (fromIntegral v)) mempty ints)
              in putTag 1 WireLengthDelimited <> putLengthDelimited payload
    in packedInts
         <> V.foldl' (\acc s -> acc <> encodeFieldString 2 s) mempty (rStrings r)
         <> V.foldl' (\acc item -> acc <> encodeFieldMessage 3 item) mempty (rItems r)


instance MessageDecode Repeated where
  messageDecoder = go V.empty V.empty V.empty
    where
      go !ints !strs !items = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (Repeated ints strs items)
          Just (Tag fn wt) -> case fn of
            1 -> case wt of
              WireLengthDelimited -> do
                bs <- getLengthDelimited
                case decodePackedInts bs of
                  Left _ -> go ints strs items
                  Right vs -> go (ints V.++ vs) strs items
              _ -> getVarint >>= \v -> go (V.snoc ints (fromIntegral v)) strs items
            2 -> decodeFieldString >>= \v -> go ints (V.snoc strs v) items
            3 -> decodeFieldMessage >>= \v -> go ints strs (V.snoc items v)
            _ -> skipField wt >> go ints strs items


decodePackedInts :: ByteString -> Either DecodeError (V.Vector Int32)
decodePackedInts bs = Right (go V.empty 0)
  where
    len = BS.length bs
    go !acc !off
      | off >= len = acc
      | otherwise = case runDecoder' getVarint bs off of
          DecodeOK v off' -> go (V.snoc acc (fromIntegral v)) off'
          DecodeFail _ -> acc
