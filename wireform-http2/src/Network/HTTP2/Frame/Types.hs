{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module Network.HTTP2.Frame.Types
  ( FrameHeader (..)
  , FramePayload
      ( FramePayloadRaw
      , DataFrame
      , HeadersFrame
      , PriorityFrame
      , RSTStreamFrame
      , SettingsFrame
      , PushPromiseFrame
      , PingFrame
      , GoAwayFrame
      , WindowUpdateFrame
      , ContinuationFrame
      , UnknownFrame
      )
  , Frame (..)
  , rawPayload
  , FrameFlags
  , flagEndStream
  , flagEndHeaders
  , flagPadded
  , flagPriority
  , flagAck
  , testFlag
  , setFlag
  , frameHeaderLength
  , connectionPreface
    -- * Payload decode helpers (used by the server engine)
  , decodeGoAway
  , decodeSettings
  ) where

import Control.DeepSeq (NFData(..))
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import GHC.Generics (Generic)

import Network.HTTP2.Types

type FrameFlags = Word8

flagEndStream :: FrameFlags
flagEndStream = 0x1

flagAck :: FrameFlags
flagAck = 0x1

flagEndHeaders :: FrameFlags
flagEndHeaders = 0x4

flagPadded :: FrameFlags
flagPadded = 0x8

flagPriority :: FrameFlags
flagPriority = 0x20

{-# INLINE testFlag #-}
testFlag :: FrameFlags -> FrameFlags -> Bool
testFlag flags flag = flags .&. flag /= 0

{-# INLINE setFlag #-}
setFlag :: FrameFlags -> FrameFlags -> FrameFlags
setFlag flags flag = flags .|. flag

frameHeaderLength :: Int
frameHeaderLength = 9

connectionPreface :: ByteString
connectionPreface = "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"

data FrameHeader = FrameHeader
  { fhLength :: {-# UNPACK #-} !Word32
  , fhType :: !FrameType
  , fhFlags :: {-# UNPACK #-} !FrameFlags
  , fhStreamId :: {-# UNPACK #-} !StreamId
  }
  deriving stock (Eq, Show, Generic)

instance NFData FrameHeader

-- | Frame payload: a newtype over raw bytes.
-- Pattern synonyms provide typed access without allocating an ADT.
-- On the receive path, this is just a pointer into the recv ring buffer.
-- On the send path, constructing via a pattern synonym builds the bytes.
newtype FramePayload = FramePayloadRaw { rawPayload :: ByteString }
  deriving stock (Eq, Show)

instance NFData FramePayload where
  rnf (FramePayloadRaw bs) = rnf bs

-- | Frame = header + payload. No intermediate ADT allocation.
data Frame = Frame
  { frameHeader :: !FrameHeader
  , framePayload :: !FramePayload
  }
  deriving stock (Eq, Show)

-- Bidirectional pattern synonyms for FramePayload.
-- Matching: extracts data from raw bytes (zero-cost for simple cases).
-- Construction: builds raw bytes from typed arguments.

pattern DataFrame :: ByteString -> FramePayload
pattern DataFrame body = FramePayloadRaw body

pattern HeadersFrame :: Maybe Priority -> ByteString -> FramePayload
pattern HeadersFrame pri block <- ((\(FramePayloadRaw bs) -> (Nothing, bs)) -> (pri, block))
  where HeadersFrame Nothing block = FramePayloadRaw block
        HeadersFrame (Just (Priority excl dep weight)) block =
          FramePayloadRaw (encodePriorityPrefix excl dep weight <> block)

pattern PriorityFrame :: Priority -> FramePayload
pattern PriorityFrame pri <- (decodePriority . rawPayload -> Just pri)
  where PriorityFrame (Priority excl dep weight) =
          FramePayloadRaw (encodePriorityPrefix excl dep weight)

pattern RSTStreamFrame :: ErrorCode -> FramePayload
pattern RSTStreamFrame code <- (decodeErrorCode . rawPayload -> Just code)
  where RSTStreamFrame code = FramePayloadRaw (encodeWord32 (errorCodeToWord32 code))

pattern SettingsFrame :: [(Word16, Word32)] -> FramePayload
pattern SettingsFrame params <- (decodeSettings . rawPayload -> Just params)
  where SettingsFrame params = FramePayloadRaw (encodeSettingsRaw params)

pattern PushPromiseFrame :: StreamId -> ByteString -> FramePayload
pattern PushPromiseFrame sid block <- (decodePushPromise . rawPayload -> Just (sid, block))
  where PushPromiseFrame sid block =
          FramePayloadRaw (encodeWord32 (sid .&. 0x7FFFFFFF) <> block)

pattern PingFrame :: ByteString -> FramePayload
pattern PingFrame body = FramePayloadRaw body

pattern GoAwayFrame :: StreamId -> ErrorCode -> ByteString -> FramePayload
pattern GoAwayFrame sid code debug <- (decodeGoAway . rawPayload -> Just (sid, code, debug))
  where GoAwayFrame sid code debug =
          FramePayloadRaw (encodeWord32 (sid .&. 0x7FFFFFFF)
                        <> encodeWord32 (errorCodeToWord32 code)
                        <> debug)

pattern WindowUpdateFrame :: Word32 -> FramePayload
pattern WindowUpdateFrame inc <- (decodeWindowUpdate . rawPayload -> Just inc)
  where WindowUpdateFrame inc = FramePayloadRaw (encodeWord32 (inc .&. 0x7FFFFFFF))

pattern ContinuationFrame :: ByteString -> FramePayload
pattern ContinuationFrame block = FramePayloadRaw block

pattern UnknownFrame :: Word8 -> ByteString -> FramePayload
pattern UnknownFrame typ body <- ((\(FramePayloadRaw bs) -> (0 :: Word8, bs)) -> (typ, body))
  where UnknownFrame _typ body = FramePayloadRaw body

-- Decode helpers (only called when pattern is matched)

decodePriority :: ByteString -> Maybe Priority
decodePriority bs
  | BS.length bs >= 5 =
      let b0 = BS.index bs 0; b1 = BS.index bs 1; b2 = BS.index bs 2; b3 = BS.index bs 3
          depRaw = (fromIntegral b0 `unsafeShiftL` 24) .|. (fromIntegral b1 `unsafeShiftL` 16)
               .|. (fromIntegral b2 `unsafeShiftL` 8) .|. fromIntegral b3 :: Word32
      in Just (Priority (testBit depRaw 31) (depRaw .&. 0x7FFFFFFF) (BS.index bs 4))
  | otherwise = Nothing

decodeErrorCode :: ByteString -> Maybe ErrorCode
decodeErrorCode bs
  | BS.length bs >= 4 = Just (word32ToErrorCode (decodeWord32 bs))
  | otherwise = Nothing

decodeSettings :: ByteString -> Maybe [(Word16, Word32)]
decodeSettings bs
  | BS.length bs `mod` 6 /= 0 = Nothing
  | otherwise = Just (go 0)
  where
    n = BS.length bs `div` 6
    go i | i >= n = []
         | otherwise =
           let off = i * 6
               ident = (fromIntegral (BS.index bs off) `unsafeShiftL` 8)
                   .|. fromIntegral (BS.index bs (off + 1))
               val = (fromIntegral (BS.index bs (off+2)) `unsafeShiftL` 24)
                 .|. (fromIntegral (BS.index bs (off+3)) `unsafeShiftL` 16)
                 .|. (fromIntegral (BS.index bs (off+4)) `unsafeShiftL` 8)
                 .|. fromIntegral (BS.index bs (off+5))
           in (ident, val) : go (i + 1)

decodePushPromise :: ByteString -> Maybe (StreamId, ByteString)
decodePushPromise bs
  | BS.length bs >= 4 = Just (decodeWord32 bs .&. 0x7FFFFFFF, BS.drop 4 bs)
  | otherwise = Nothing

decodeGoAway :: ByteString -> Maybe (StreamId, ErrorCode, ByteString)
decodeGoAway bs
  | BS.length bs >= 8 =
      let sid = decodeWord32 bs .&. 0x7FFFFFFF
          code = word32ToErrorCode (decodeWord32 (BS.drop 4 bs))
          debug = BS.drop 8 bs
      in Just (sid, code, debug)
  | otherwise = Nothing

decodeWindowUpdate :: ByteString -> Maybe Word32
decodeWindowUpdate bs
  | BS.length bs >= 4 = Just (decodeWord32 bs .&. 0x7FFFFFFF)
  | otherwise = Nothing

-- Encode helpers

encodePriorityPrefix :: Bool -> StreamId -> Word8 -> ByteString
encodePriorityPrefix excl dep weight =
  let depW = dep .|. (if excl then 0x80000000 else 0)
  in encodeWord32 depW <> BS.singleton weight

encodeWord32 :: Word32 -> ByteString
encodeWord32 w = BS.pack
  [ fromIntegral (w `unsafeShiftR` 24)
  , fromIntegral (w `unsafeShiftR` 16)
  , fromIntegral (w `unsafeShiftR` 8)
  , fromIntegral w
  ]

decodeWord32 :: ByteString -> Word32
decodeWord32 bs =
  (fromIntegral (BS.index bs 0) `unsafeShiftL` 24)
  .|. (fromIntegral (BS.index bs 1) `unsafeShiftL` 16)
  .|. (fromIntegral (BS.index bs 2) `unsafeShiftL` 8)
  .|. fromIntegral (BS.index bs 3)

encodeSettingsRaw :: [(Word16, Word32)] -> ByteString
encodeSettingsRaw [] = BS.empty
encodeSettingsRaw params = BS.concat (map encParam params)
  where
    encParam (ident, val) = BS.pack
      [ fromIntegral (ident `unsafeShiftR` 8)
      , fromIntegral ident
      , fromIntegral (val `unsafeShiftR` 24)
      , fromIntegral (val `unsafeShiftR` 16)
      , fromIntegral (val `unsafeShiftR` 8)
      , fromIntegral val
      ]

errorCodeToWord32 :: ErrorCode -> Word32
errorCodeToWord32 = \case
  NoError -> 0x0
  ProtocolError -> 0x1
  InternalError -> 0x2
  FlowControlError -> 0x3
  SettingsTimeout -> 0x4
  StreamClosed -> 0x5
  FrameSizeError -> 0x6
  RefusedStream -> 0x7
  Cancel -> 0x8
  CompressionError -> 0x9
  ConnectError -> 0xa
  EnhanceYourCalm -> 0xb
  InadequateSecurity -> 0xc
  HTTP11Required -> 0xd
  UnknownError w -> w

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
