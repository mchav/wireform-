module Network.HTTP2.Frame.Encode
  ( encodeFrame
  , encodeFrameHeader
  , encodeFramePayload
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Network.HTTP2.Frame.Types
import Network.HTTP2.Internal.BitOps
import Network.HTTP2.Types

encodeFrame :: Frame -> ByteString
encodeFrame (Frame hdr payload) =
  let body = encodeFramePayload (fhFlags hdr) payload
      len = fromIntegral (BS.length body)
      header = encodeFrameHeader hdr { fhLength = len }
  in header <> body

encodeFrameHeader :: FrameHeader -> ByteString
encodeFrameHeader (FrameHeader len typ flags sid) =
  BSI.unsafeCreate frameHeaderLength $ \p -> do
    writeWord24BE p len
    pokeByteOff p 3 (frameTypeToWord8 typ)
    pokeByteOff p 4 flags
    writeWord32BE (p `plusPtr` 5) (sid .&. 0x7FFFFFFF)

frameTypeToWord8 :: FrameType -> Word8
frameTypeToWord8 = \case
  FrameData -> 0x0
  FrameHeaders -> 0x1
  FramePriority -> 0x2
  FrameRSTStream -> 0x3
  FrameSettings -> 0x4
  FramePushPromise -> 0x5
  FramePing -> 0x6
  FrameGoAway -> 0x7
  FrameWindowUpdate -> 0x8
  FrameContinuation -> 0x9
  FrameUnknown w -> w

encodeFramePayload :: FrameFlags -> FramePayload -> ByteString
encodeFramePayload _flags (DataFrame bs) = bs
encodeFramePayload flags (HeadersFrame mpri headerBlock) =
  case mpri of
    Nothing -> headerBlock
    Just (Priority excl dep weight) ->
      let depW = dep .|. (if excl then 0x80000000 else 0)
      in BSI.unsafeCreate 5 (\p -> do
           writeWord32BE p depW
           pokeByteOff p 4 weight
         ) <> headerBlock
encodeFramePayload _ (PriorityFrame (Priority excl dep weight)) =
  BSI.unsafeCreate 5 $ \p -> do
    let depW = dep .|. (if excl then 0x80000000 else 0)
    writeWord32BE p depW
    pokeByteOff p 4 weight
encodeFramePayload _ (RSTStreamFrame code) =
  BSI.unsafeCreate 4 $ \p -> writeWord32BE p (errorCodeToWord32 code)
encodeFramePayload _ (SettingsFrame params) =
  BSI.unsafeCreate (length params * 6) $ \p -> do
    let go _ [] = pure ()
        go off ((ident, val):rest) = do
          writeWord16BE (p `plusPtr` off) ident
          writeWord32BE (p `plusPtr` (off + 2)) val
          go (off + 6) rest
    go 0 params
encodeFramePayload _ (PushPromiseFrame promisedId headerBlock) =
  BSI.unsafeCreate 4 (\p -> writeWord32BE p (promisedId .&. 0x7FFFFFFF))
  <> headerBlock
encodeFramePayload _ (PingFrame bs) =
  if BS.length bs == 8 then bs else BS.take 8 (bs <> BS.replicate 8 0)
encodeFramePayload _ (GoAwayFrame lastId code debug) =
  BSI.unsafeCreate 8 (\p -> do
    writeWord32BE p (lastId .&. 0x7FFFFFFF)
    writeWord32BE (p `plusPtr` 4) (errorCodeToWord32 code)
  ) <> debug
encodeFramePayload _ (WindowUpdateFrame increment) =
  BSI.unsafeCreate 4 $ \p -> writeWord32BE p (increment .&. 0x7FFFFFFF)
encodeFramePayload _ (ContinuationFrame bs) = bs
encodeFramePayload _ (UnknownFrame _ bs) = bs

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
