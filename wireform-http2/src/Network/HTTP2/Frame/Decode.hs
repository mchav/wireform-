module Network.HTTP2.Frame.Decode
  ( decodeFrameHeader
  , decodeFramePayload
  , DecodeResult (..)
  , FrameDecodeError (..)
  ) where

import Control.DeepSeq (NFData)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Network.HTTP2.Frame.Types
import Network.HTTP2.Internal.BitOps
import Network.HTTP2.Types

data FrameDecodeError
  = FrameTooShort
  | PayloadTooShort
  | InvalidPadding
  | InvalidSettingsLength
  | InvalidWindowUpdateIncrement
  | InvalidStreamId
  deriving stock (Eq, Show, Generic)

instance NFData FrameDecodeError

data DecodeResult
  = DecodeSuccess !Frame
  | DecodeError !FrameDecodeError !ErrorCode
  deriving stock (Eq, Show)

decodeFrameHeader :: ByteString -> Either FrameDecodeError FrameHeader
decodeFrameHeader bs
  | BS.length bs < frameHeaderLength = Left FrameTooShort
  | otherwise = unsafePerformIO $
      BSU.unsafeUseAsCStringLen bs $ \(cstr, _) -> do
        let p = castPtr cstr :: Ptr Word8
        len <- readWord24BE p
        typ <- peekByteOff p 3 :: IO Word8
        flags <- peekByteOff p 4 :: IO Word8
        sid <- readWord32BE (p `plusPtr` 5)
        pure $ Right FrameHeader
          { fhLength = len
          , fhType = word8ToFrameType typ
          , fhFlags = flags
          , fhStreamId = sid .&. 0x7FFFFFFF
          }

word8ToFrameType :: Word8 -> FrameType
word8ToFrameType = \case
  0x0 -> FrameData
  0x1 -> FrameHeaders
  0x2 -> FramePriority
  0x3 -> FrameRSTStream
  0x4 -> FrameSettings
  0x5 -> FramePushPromise
  0x6 -> FramePing
  0x7 -> FrameGoAway
  0x8 -> FrameWindowUpdate
  0x9 -> FrameContinuation
  w -> FrameUnknown w

-- | Decode frame payload. With the newtype FramePayload design,
-- this just validates and wraps the raw bytes. The actual decoding
-- happens lazily when pattern synonyms are matched.
decodeFramePayload :: FrameHeader -> ByteString -> Either FrameDecodeError FramePayload
decodeFramePayload hdr payload
  | BS.length payload < fromIntegral (fhLength hdr) = Left PayloadTooShort
  | otherwise =
      let body = BS.take (fromIntegral (fhLength hdr)) payload
      in case fhType hdr of
        -- Validate frame-specific constraints that would be errors
        FrameSettings
          | testFlag (fhFlags hdr) flagAck && not (BS.null body) -> Left InvalidSettingsLength
          | BS.length body `mod` 6 /= 0 -> Left InvalidSettingsLength
          | otherwise -> Right (FramePayloadRaw body)
        FrameWindowUpdate
          | BS.length body /= 4 -> Left PayloadTooShort
          | decodeWord32Raw body .&. 0x7FFFFFFF == 0 -> Left InvalidWindowUpdateIncrement
          | otherwise -> Right (FramePayloadRaw body)
        FramePing
          | BS.length body /= 8 -> Left PayloadTooShort
          | otherwise -> Right (FramePayloadRaw body)
        FrameRSTStream
          | BS.length body /= 4 -> Left PayloadTooShort
          | otherwise -> Right (FramePayloadRaw body)
        FramePriority
          | BS.length body /= 5 -> Left PayloadTooShort
          | otherwise -> Right (FramePayloadRaw body)
        FrameGoAway
          | BS.length body < 8 -> Left PayloadTooShort
          | otherwise -> Right (FramePayloadRaw body)
        _ -> Right (FramePayloadRaw body)

{-# INLINE decodeWord32Raw #-}
decodeWord32Raw :: ByteString -> Word32
decodeWord32Raw bs =
  (fromIntegral (BS.index bs 0) `unsafeShiftL` 24)
  .|. (fromIntegral (BS.index bs 1) `unsafeShiftL` 16)
  .|. (fromIntegral (BS.index bs 2) `unsafeShiftL` 8)
  .|. fromIntegral (BS.index bs 3)
