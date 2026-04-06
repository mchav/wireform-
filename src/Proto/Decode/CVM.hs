{-# LANGUAGE BangPatterns #-}
-- | C decode VM: full protobuf decode loop runs in C.
--
-- For string/bytes fields, the C decoder returns (offset, length) pairs
-- into the original input buffer. Haskell then slices the ByteString
-- and decodes Text from those ranges — zero-copy for bytes, one
-- Text construction for strings.
module Proto.Decode.CVM
  ( -- * Specialized decoders
    decodeSmallC
  , SmallResult (..)
  , decodeMediumC
  , MediumResult (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word8, Word64)
import Foreign.C.Types (CInt(..), CChar)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, peekByteOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Control.DeepSeq (NFData(..))
import Proto.Wire.Decode (DecodeError(..))

-- FFI declarations

foreign import ccall unsafe "hs_proto_decode_small"
  c_decode_small
    :: Ptr Word8 -> CInt
    -> Ptr Int64 -> Ptr CInt -> Ptr CInt -> Ptr Word8
    -> IO CInt

foreign import ccall unsafe "hs_proto_decode_medium"
  c_decode_medium
    :: Ptr Word8 -> CInt
    -> Ptr CInt -> Ptr CInt          -- title off/len
    -> Ptr Int32                     -- count
    -> Ptr Double                    -- score
    -> Ptr CInt -> Ptr CInt          -- payload off/len
    -> Ptr Word8                     -- enabled
    -> Ptr Int64                     -- timestamp
    -> Ptr CInt -> Ptr CInt          -- description off/len
    -> Ptr Float                     -- ratio
    -> IO CInt

data SmallResult = SmallResult
  { srId     :: {-# UNPACK #-} !Int64
  , srName   :: !Text
  , srActive :: !Bool
  } deriving stock (Show)

instance NFData SmallResult where
  rnf (SmallResult a b c) = rnf a `seq` rnf b `seq` rnf c

data MediumResult = MediumResult
  { mrTitle       :: !Text
  , mrCount       :: {-# UNPACK #-} !Int32
  , mrScore       :: {-# UNPACK #-} !Double
  , mrPayload     :: !ByteString
  , mrEnabled     :: !Bool
  , mrTimestamp   :: {-# UNPACK #-} !Int64
  , mrDescription :: !Text
  , mrRatio       :: {-# UNPACK #-} !Float
  } deriving stock (Show)

instance NFData MediumResult where
  rnf (MediumResult a b c d e f g h) = rnf a `seq` rnf b `seq` rnf c `seq` rnf d `seq` rnf e `seq` rnf f `seq` rnf g `seq` rnf h

-- | Decode an HSmall message entirely in C.
decodeSmallC :: ByteString -> Either DecodeError SmallResult
decodeSmallC (BSI.BS fp len) = unsafeDupablePerformIO $
  withForeignPtr fp $ \buf ->
  alloca $ \pId ->
  alloca $ \pNameOff ->
  alloca $ \pNameLen ->
  alloca $ \pActive -> do
    rc <- c_decode_small buf (fromIntegral len) pId pNameOff pNameLen pActive
    if rc /= 0
      then pure (Left (CustomError "C decode failed"))
      else do
        theId <- peek pId
        nameOff <- fromIntegral <$> peek pNameOff
        nameLen <- fromIntegral <$> peek pNameLen
        active <- peek pActive
        let !nameBs = BSU.unsafeTake nameLen (BSU.unsafeDrop nameOff (BSI.BS fp len))
            !nameText = TE.decodeUtf8Lenient nameBs
        pure $! Right $! SmallResult theId nameText (active /= 0)

-- | Decode an HMedium message entirely in C.
decodeMediumC :: ByteString -> Either DecodeError MediumResult
decodeMediumC (BSI.BS fp len) = unsafeDupablePerformIO $
  withForeignPtr fp $ \buf ->
  alloca $ \pTitleOff -> alloca $ \pTitleLen ->
  alloca $ \pCount ->
  alloca $ \pScore ->
  alloca $ \pPayloadOff -> alloca $ \pPayloadLen ->
  alloca $ \pEnabled ->
  alloca $ \pTimestamp ->
  alloca $ \pDescOff -> alloca $ \pDescLen ->
  alloca $ \pRatio -> do
    rc <- c_decode_medium buf (fromIntegral len)
      pTitleOff pTitleLen pCount pScore
      pPayloadOff pPayloadLen pEnabled pTimestamp
      pDescOff pDescLen pRatio
    if rc /= 0
      then pure (Left (CustomError "C decode failed"))
      else do
        titleOff <- fromIntegral <$> peek pTitleOff
        titleLen <- fromIntegral <$> peek pTitleLen
        count <- peek pCount
        score <- peek pScore
        payloadOff <- fromIntegral <$> peek pPayloadOff
        payloadLen <- fromIntegral <$> peek pPayloadLen
        enabled <- peek pEnabled
        timestamp <- peek pTimestamp
        descOff <- fromIntegral <$> peek pDescOff
        descLen <- fromIntegral <$> peek pDescLen
        ratio <- peek pRatio
        let !bsOrig = BSI.BS fp len
            !titleBs = BSU.unsafeTake titleLen (BSU.unsafeDrop titleOff bsOrig)
            !titleText = TE.decodeUtf8Lenient titleBs
            !payloadBs = BSU.unsafeTake payloadLen (BSU.unsafeDrop payloadOff bsOrig)
            !descBs = BSU.unsafeTake descLen (BSU.unsafeDrop descOff bsOrig)
            !descText = TE.decodeUtf8Lenient descBs
        pure $! Right $! MediumResult titleText count score payloadBs (enabled /= 0) timestamp descText ratio
