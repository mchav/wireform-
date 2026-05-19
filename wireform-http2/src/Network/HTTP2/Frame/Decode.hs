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
import Foreign.ForeignPtr
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

decodeFramePayload :: FrameHeader -> ByteString -> Either FrameDecodeError FramePayload
decodeFramePayload hdr payload
  | BS.length payload < fromIntegral (fhLength hdr) = Left PayloadTooShort
  | otherwise =
      let body = BS.take (fromIntegral (fhLength hdr)) payload
          flags = fhFlags hdr
      in case fhType hdr of
           FrameData -> decodeDataFrame flags body
           FrameHeaders -> decodeHeadersFrame flags body
           FramePriority -> decodePriorityFrame body
           FrameRSTStream -> decodeRSTStreamFrame body
           FrameSettings -> decodeSettingsFrame flags body
           FramePushPromise -> decodePushPromiseFrame flags body
           FramePing -> decodePingFrame body
           FrameGoAway -> decodeGoAwayFrame body
           FrameWindowUpdate -> decodeWindowUpdateFrame body
           FrameContinuation -> Right (ContinuationFrame body)
           FrameUnknown w -> Right (UnknownFrame w body)

decodeDataFrame :: FrameFlags -> ByteString -> Either FrameDecodeError FramePayload
decodeDataFrame flags body
  | testFlag flags flagPadded =
      if BS.null body
        then Left PayloadTooShort
        else let padLen = fromIntegral (BS.index body 0)
                 dataLen = BS.length body - 1 - padLen
             in if dataLen < 0
                  then Left InvalidPadding
                  else Right (DataFrame (BS.take dataLen (BS.drop 1 body)))
  | otherwise = Right (DataFrame body)

decodeHeadersFrame :: FrameFlags -> ByteString -> Either FrameDecodeError FramePayload
decodeHeadersFrame flags body = do
  let (body', padLen) = if testFlag flags flagPadded
        then if BS.null body
               then (body, -1)
               else (BS.drop 1 body, fromIntegral (BS.index body 0))
        else (body, 0)
  if padLen < 0
    then Left PayloadTooShort
    else do
      let (mpri, headerBlock) = if testFlag flags flagPriority
            then if BS.length body' < 5
                   then (Nothing, BS.empty)
                   else unsafePerformIO $ BSU.unsafeUseAsCStringLen body' $ \(cstr, _) -> do
                     let p = castPtr cstr :: Ptr Word8
                     depRaw <- readWord32BE p
                     weight <- peekByteOff p 4 :: IO Word8
                     let excl = testBit depRaw 31
                         dep = depRaw .&. 0x7FFFFFFF
                         pri = Priority excl dep weight
                         rest = BS.drop 5 body'
                     pure (Just pri, rest)
            else (Nothing, body')
      if testFlag flags flagPriority && BS.length body' < 5
        then Left PayloadTooShort
        else let hbLen = BS.length headerBlock - padLen
             in if hbLen < 0
                  then Left InvalidPadding
                  else Right (HeadersFrame mpri (BS.take hbLen headerBlock))

decodePriorityFrame :: ByteString -> Either FrameDecodeError FramePayload
decodePriorityFrame body
  | BS.length body /= 5 = Left PayloadTooShort
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen body $ \(cstr, _) -> do
      let p = castPtr cstr :: Ptr Word8
      depRaw <- readWord32BE p
      weight <- peekByteOff p 4 :: IO Word8
      let excl = testBit depRaw 31
          dep = depRaw .&. 0x7FFFFFFF
      pure $ Right (PriorityFrame (Priority excl dep weight))

decodeRSTStreamFrame :: ByteString -> Either FrameDecodeError FramePayload
decodeRSTStreamFrame body
  | BS.length body /= 4 = Left PayloadTooShort
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen body $ \(cstr, _) -> do
      code <- readWord32BE (castPtr cstr)
      pure $ Right (RSTStreamFrame (word32ToErrorCode code))

decodeSettingsFrame :: FrameFlags -> ByteString -> Either FrameDecodeError FramePayload
decodeSettingsFrame flags body
  | testFlag flags flagAck && not (BS.null body) = Left InvalidSettingsLength
  | BS.length body `mod` 6 /= 0 = Left InvalidSettingsLength
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen body $ \(cstr, _) -> do
      let p = castPtr cstr :: Ptr Word8
          n = BS.length body `div` 6
          go i acc
            | i >= n = pure $ Right (SettingsFrame (reverse acc))
            | otherwise = do
                ident <- readWord16BE (p `plusPtr` (i * 6))
                val <- readWord32BE (p `plusPtr` (i * 6 + 2))
                go (i + 1) ((ident, val) : acc)
      go 0 []

decodePushPromiseFrame :: FrameFlags -> ByteString -> Either FrameDecodeError FramePayload
decodePushPromiseFrame flags body = do
  let (body', padLen) = if testFlag flags flagPadded
        then if BS.null body
               then (body, -1)
               else (BS.drop 1 body, fromIntegral (BS.index body 0))
        else (body, 0)
  if padLen < 0
    then Left PayloadTooShort
    else if BS.length body' < 4
      then Left PayloadTooShort
      else unsafePerformIO $ BSU.unsafeUseAsCStringLen body' $ \(cstr, _) -> do
        promisedId <- readWord32BE (castPtr cstr)
        let headerBlock = BS.drop 4 body'
            hbLen = BS.length headerBlock - padLen
        if hbLen < 0
          then pure $ Left InvalidPadding
          else pure $ Right (PushPromiseFrame (promisedId .&. 0x7FFFFFFF)
                                             (BS.take hbLen headerBlock))

decodePingFrame :: ByteString -> Either FrameDecodeError FramePayload
decodePingFrame body
  | BS.length body /= 8 = Left PayloadTooShort
  | otherwise = Right (PingFrame body)

decodeGoAwayFrame :: ByteString -> Either FrameDecodeError FramePayload
decodeGoAwayFrame body
  | BS.length body < 8 = Left PayloadTooShort
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen body $ \(cstr, _) -> do
      let p = castPtr cstr :: Ptr Word8
      lastId <- readWord32BE p
      code <- readWord32BE (p `plusPtr` 4)
      let debug = BS.drop 8 body
      pure $ Right (GoAwayFrame (lastId .&. 0x7FFFFFFF) (word32ToErrorCode code) debug)

decodeWindowUpdateFrame :: ByteString -> Either FrameDecodeError FramePayload
decodeWindowUpdateFrame body
  | BS.length body /= 4 = Left PayloadTooShort
  | otherwise = unsafePerformIO $ BSU.unsafeUseAsCStringLen body $ \(cstr, _) -> do
      increment <- readWord32BE (castPtr cstr)
      let inc = increment .&. 0x7FFFFFFF
      if inc == 0
        then pure $ Left InvalidWindowUpdateIncrement
        else pure $ Right (WindowUpdateFrame inc)

word32ToErrorCode :: Word32 -> ErrorCode
word32ToErrorCode = \case
  0x0 -> NoError
  0x1 -> ProtocolError
  0x2 -> InternalError
  0x3 -> FlowControlError
  0x4 -> SettingsTimeout
  0x5 -> StreamClosed
  0x6 -> FrameSizeError
  0x7 -> RefusedStream
  0x8 -> Cancel
  0x9 -> CompressionError
  0xa -> ConnectError
  0xb -> EnhanceYourCalm
  0xc -> InadequateSecurity
  0xd -> HTTP11Required
  w -> UnknownError w
