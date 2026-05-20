module Network.HTTP2.Frame.Encode
  ( encodeFrame
  , encodeFrameHeader
  , encodeFramePayload
  , encodeFrameInto
  ) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Network.HTTP2.Frame.Types
import Network.HTTP2.Internal.BitOps
import Network.HTTP2.Types

-- | Encode a frame into a single contiguous ByteString.
-- With the newtype FramePayload, the payload bytes are already built
-- by the pattern synonym constructors. We just prepend the 9-byte header.
encodeFrame :: Frame -> ByteString
encodeFrame (Frame hdr (FramePayloadRaw body)) =
  let bodyLen = BS.length body
  in BSI.unsafeCreate (frameHeaderLength + bodyLen) $ \p -> do
    writeWord24BE p (fromIntegral bodyLen)
    pokeByteOff p 3 (frameTypeToWord8 (fhType hdr))
    pokeByteOff p 4 (fhFlags hdr)
    writeWord32BE (p `plusPtr` 5) (fhStreamId hdr .&. 0x7FFFFFFF)
    BSU.unsafeUseAsCStringLen body $ \(src, len) ->
      BSI.memcpy (p `plusPtr` frameHeaderLength) (castPtr src) len

-- | Encode frame into a pre-allocated buffer. Returns bytes written.
encodeFrameInto :: Frame -> Ptr Word8 -> IO Int
encodeFrameInto (Frame hdr (FramePayloadRaw body)) p = do
  let bodyLen = BS.length body
  writeWord24BE p (fromIntegral bodyLen)
  pokeByteOff p 3 (frameTypeToWord8 (fhType hdr))
  pokeByteOff p 4 (fhFlags hdr)
  writeWord32BE (p `plusPtr` 5) (fhStreamId hdr .&. 0x7FFFFFFF)
  BSU.unsafeUseAsCStringLen body $ \(src, len) ->
    BSI.memcpy (p `plusPtr` frameHeaderLength) (castPtr src) len
  pure (frameHeaderLength + bodyLen)

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

-- | Encode frame payload to wire bytes.
-- With the newtype design, the payload already IS the raw bytes
-- (constructed via pattern synonyms). This is now identity.
{-# INLINE encodeFramePayload #-}
encodeFramePayload :: FrameFlags -> FramePayload -> ByteString
encodeFramePayload _flags (FramePayloadRaw bs) = bs
