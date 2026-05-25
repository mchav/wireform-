{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | RFC 6455 \u00a75 frame format.

This module exposes the on-the-wire frame ADT plus a streaming
'Wireform.Parser.Parser' decoder and a 'Wireform.Builder.Builder'
encoder.  No I\/O lives here \u2014 callers wire the parser into a
'Wireform.Transport.Receive.ReceiveTransport' (via
'Wireform.Parser.Driver.runParser') and the builder into a
'Wireform.Transport.Send.SendTransport' (via
'Wireform.Transport.Send.sendBuilderDirect').

The decoder is parameterised over a strict 'PayloadLimit'; payloads
that exceed the limit are rejected with 'FramePayloadTooLarge'
rather than silently allocated.  See RFC 6455 \u00a75.2 for the
length encoding rules the parser implements.
-}
module Network.WebSocket.Frame
  (     -- * Frame ADT
    Frame (..)
  , Opcode (..)
  , Mask (..)
  , mkMask
  , randomMask
  , opcodeIsControl
  , isContinuation
  , isData
    -- * Errors
  , FrameError (..)
  , PayloadLimit (..)
  , defaultPayloadLimit
    -- * Decoding
  , parseFrame
    -- * Encoding
  , buildFrame
  , buildFrameMasked
  , frameHeaderBytes
  , frameHeaderLength
  , writeFrameHeader
    -- * Masking
  , maskPayload
  ) where

import Control.Exception (Exception)
import Data.Bits (shiftL, shiftR, testBit, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.Word (Word32, Word64, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Random.Stateful as Rnd

import qualified Wireform.Builder as B
import qualified Wireform.FFI as FFI
import Wireform.Parser
  ( Parser
  , anyWord16be
  , anyWord64be
  , anyWord8
  , err
  , takeBs
  )
import Wireform.Parser.Internal (ParserMode)

------------------------------------------------------------------------
-- Opcode
------------------------------------------------------------------------

-- | A WebSocket opcode, RFC 6455 \u00a75.2 / \u00a711.8.
data Opcode
  = OpContinuation   -- ^ 0x0
  | OpText           -- ^ 0x1
  | OpBinary         -- ^ 0x2
  | OpClose          -- ^ 0x8
  | OpPing           -- ^ 0x9
  | OpPong           -- ^ 0xA
  | OpReservedNonControl !Word8
    -- ^ 0x3-0x7 \u2014 reserved \"non-control\" opcodes.  Treated as
    -- protocol errors by the high-level layers but exposed so a
    -- transparent proxy can pass them through.
  | OpReservedControl    !Word8
    -- ^ 0xB-0xF \u2014 reserved control opcodes.  Same treatment.
  deriving stock (Eq, Show)

-- | True if the opcode is a control frame (RFC 6455 \u00a75.5):
-- @Close@, @Ping@, @Pong@, or any reserved 0xB-0xF.
opcodeIsControl :: Opcode -> Bool
opcodeIsControl = \case
  OpClose                -> True
  OpPing                 -> True
  OpPong                 -> True
  OpReservedControl _    -> True
  _                      -> False

-- | True for the continuation opcode.
isContinuation :: Opcode -> Bool
isContinuation OpContinuation = True
isContinuation _              = False

-- | True for the data-frame opcodes (Text, Binary, Continuation).
isData :: Opcode -> Bool
isData OpContinuation = True
isData OpText         = True
isData OpBinary       = True
isData _              = False

opcodeFromWord :: Word8 -> Opcode
opcodeFromWord = \case
  0x0 -> OpContinuation
  0x1 -> OpText
  0x2 -> OpBinary
  0x8 -> OpClose
  0x9 -> OpPing
  0xA -> OpPong
  w | w >= 0x3 && w <= 0x7 -> OpReservedNonControl w
    | otherwise            -> OpReservedControl w
{-# INLINE opcodeFromWord #-}

opcodeToWord :: Opcode -> Word8
opcodeToWord = \case
  OpContinuation         -> 0x0
  OpText                 -> 0x1
  OpBinary               -> 0x2
  OpClose                -> 0x8
  OpPing                 -> 0x9
  OpPong                 -> 0xA
  OpReservedNonControl w -> w
  OpReservedControl    w -> w
{-# INLINE opcodeToWord #-}

------------------------------------------------------------------------
-- Frame
------------------------------------------------------------------------

-- | One on-the-wire WebSocket frame.
--
-- The RSV bits are exposed verbatim; extensions (RFC 7692
-- @permessage-deflate@, etc.) use them.  No extension is supported
-- in the base 'Frame' decoder \u2014 the high-level
-- 'Network.WebSocket.Connection' rejects non-zero RSV bits unless
-- the caller installs an extension hook.
data Frame = Frame
  { frameFin     :: !Bool
  , frameRsv1    :: !Bool
  , frameRsv2    :: !Bool
  , frameRsv3    :: !Bool
  , frameOpcode  :: !Opcode
  , frameMask    :: !(Maybe Mask)
    -- ^ Server-to-client frames MUST set this to 'Nothing'
    -- (RFC 6455 \u00a75.1).  Client-to-server frames MUST set it
    -- to 'Just'.  The parser does not enforce this; the
    -- per-direction connection layer does.
  , framePayload :: !ByteString
    -- ^ Application data, /unmasked/.  The parser applies the mask
    -- in-place after copying out of the ring so callers never see
    -- the masked bytes.  The builder applies the mask in-place
    -- when encoding a 'Just'-masked frame.
  } deriving stock (Eq, Show)

-- | A 4-byte masking key (RFC 6455 \u00a75.3).
newtype Mask = Mask Word32
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Decoder error.  Surfaced as a streaming 'Wireform.Parser.err'.
data FrameError
  = FramePayloadTooLarge !Word64 !Word64
    -- ^ Frame announced @len@, limit was @limit@.
  | FrameInvalid7BitLength !Word8
    -- ^ Reserved length value (only happens if the wire bytes are
    -- crafted so the 7-bit length is 0x7E or 0x7F but the extended
    -- length is missing \u2014 the streaming parser ensures the
    -- extended length is present, so in practice this is unused).
  deriving stock (Eq, Show)

instance Exception FrameError

-- | Cap on a single frame's payload length.
newtype PayloadLimit = PayloadLimit { unPayloadLimit :: Word64 }
  deriving stock (Eq, Show)

-- | 16 MiB.  Matches the @permessage-deflate@ \"reasonable cap\" rule
-- of thumb \u2014 callers are expected to override for streaming
-- file uploads.
defaultPayloadLimit :: PayloadLimit
defaultPayloadLimit = PayloadLimit (16 * 1024 * 1024)

------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------

-- | Streaming decoder for one frame.  Reads from the receive
-- transport until a complete frame is available; suspends mid-frame
-- if the transport runs out of data.  Fails with 'FrameError' on
-- protocol violations the parser can detect locally.
--
-- Polymorphic in the parser mode so callers can drive it with
-- either 'Wireform.Parser.Driver.runParser' (streaming) or
-- 'Wireform.Parser.Driver.parseByteString' (whole input \u2014
-- handy for unit tests against a captured wire trace).
parseFrame :: ParserMode m => PayloadLimit -> Parser m FrameError Frame
parseFrame (PayloadLimit limit) = do
  b1 <- anyWord8
  b2 <- anyWord8
  let !fin    = testBit b1 7
      !rsv1   = testBit b1 6
      !rsv2   = testBit b1 5
      !rsv3   = testBit b1 4
      !opcode = opcodeFromWord (b1 .&. 0x0F)
      !masked = testBit b2 7
      !len7   = b2 .&. 0x7F
  len64 <- case len7 of
    126 -> fromIntegral <$> anyWord16be
    127 -> anyWord64be
    n   -> pure (fromIntegral n)
  if len64 > limit
    then err (FramePayloadTooLarge len64 limit)
    else do
      mMask <- if masked
                 then do
                   m0 <- anyWord8
                   m1 <- anyWord8
                   m2 <- anyWord8
                   m3 <- anyWord8
                   pure (Just (mkMask m0 m1 m2 m3))
                 else pure Nothing
      let !len = fromIntegral len64 :: Int
      payload <- case mMask of
        Just m  -> do
          -- Zero-copy slice into the recv ring, then XOR the
          -- masking key over those bytes in place.  Mutating ring
          -- memory is safe because the slice the caller receives
          -- references the same bytes \u2014 nothing else is
          -- reading them, and the driver does not advance the
          -- consumer tail past the frame until the caller has
          -- finished with the slice.  Saves a 'BS.copy' of the
          -- payload over the previous takeBsCopy-then-XOR path.
          raw <- takeBs len
          pure (maskInPlace m raw)
        Nothing -> takeBs len
      pure Frame
        { frameFin     = fin
        , frameRsv1    = rsv1
        , frameRsv2    = rsv2
        , frameRsv3    = rsv3
        , frameOpcode  = opcode
        , frameMask    = mMask
        , framePayload = payload
        }

-- | Construct a 'Mask' from its four bytes (RFC 6455 \u00a75.3 stores
-- the masking key in network byte order; @m0@ is the high-order
-- byte, @m3@ the low-order one).
mkMask :: Word8 -> Word8 -> Word8 -> Word8 -> Mask
mkMask m0 m1 m2 m3 = Mask $
      (fromIntegral m0 `shiftL` 24)
  .|. (fromIntegral m1 `shiftL` 16)
  .|. (fromIntegral m2 `shiftL` 8)
  .|.  fromIntegral m3

-- | Draw a fresh per-frame masking key from the global splitmix
-- generator.  Clients should call this once per frame; the
-- 'Mask' need not be cryptographically random per RFC 6455
-- \u00a75.3, but should be unpredictable to a network observer
-- on the same connection \u2014 splitmix is sufficient.
randomMask :: IO Mask
randomMask = Mask <$> Rnd.uniformM Rnd.globalStdGen

------------------------------------------------------------------------
-- Builder
------------------------------------------------------------------------

-- | Encode a frame as written.  The 'frameMask' field is honoured:
-- if 'Just', the mask key is emitted and the payload bytes are
-- XORed before going on the wire (RFC 6455 \u00a75.3).
--
-- Callers building from the /server/ side should ensure
-- 'frameMask' is 'Nothing'; callers building from the /client/
-- side should ensure it is 'Just'.
buildFrame :: Frame -> B.Builder
buildFrame f =
  let !b1 = bit7 (frameFin f)
        .|. bit6 (frameRsv1 f)
        .|. bit5 (frameRsv2 f)
        .|. bit4 (frameRsv3 f)
        .|. (opcodeToWord (frameOpcode f) .&. 0x0F)
      !plen = fromIntegral (BS.length (framePayload f)) :: Word64
      !maskBit = case frameMask f of
                   Just _  -> 0x80
                   Nothing -> 0x00
      lenEncoding
        | plen <= 125     = B.word8 (maskBit .|. fromIntegral plen)
        | plen <= 0xFFFF  =
            B.word8 (maskBit .|. 126)
            <> B.word16BE (fromIntegral plen)
        | otherwise       =
            B.word8 (maskBit .|. 127)
            <> B.word64BE plen
  in B.word8 b1
  <> lenEncoding
  <> maskAndPayload (frameMask f) (framePayload f)

-- | Build a frame with an explicit mask, replacing any mask already
-- on the frame.  Convenience for the client side: build once with
-- 'frameMask = Nothing' and stamp on the freshly-rolled per-frame
-- mask key here.
buildFrameMasked :: Mask -> Frame -> B.Builder
buildFrameMasked m f = buildFrame f { frameMask = Just m }

-- | Length in bytes of @frame@'s wire header (FIN \/ RSV \/ opcode
-- byte + length encoding + optional 4-byte mask key).  No
-- allocation; straight-line case dispatch on the payload size.
frameHeaderLength :: Frame -> Int
frameHeaderLength f =
  let !plen = BS.length (framePayload f)
      !lenField
        | plen <= 125     = 1
        | plen <= 0xFFFF  = 3
        | otherwise       = 9
      !maskLen = case frameMask f of { Just _ -> 4 ; Nothing -> 0 }
  in 1 + lenField + maskLen
{-# INLINE frameHeaderLength #-}

-- | Write @frame@'s wire header into @dst@.  The caller must have
-- already reserved at least 'frameHeaderLength' bytes at @dst@
-- (e.g. via 'Wireform.Transport.Send.reserveSend').  Returns the
-- number of bytes written, i.e. 'frameHeaderLength'.
writeFrameHeader :: Frame -> Ptr Word8 -> IO Int
writeFrameHeader f p = do
  let !b1 = bit7 (frameFin f)
        .|. bit6 (frameRsv1 f)
        .|. bit5 (frameRsv2 f)
        .|. bit4 (frameRsv3 f)
        .|. (opcodeToWord (frameOpcode f) .&. 0x0F)
      !plen = fromIntegral (BS.length (framePayload f)) :: Word64
      !mMask    = frameMask f
      !maskBit  = case mMask of { Just _ -> 0x80 ; Nothing -> 0x00 } :: Word8
      !lenField
        | plen <= 125     = 1
        | plen <= 0xFFFF  = 3
        | otherwise       = 9
  pokeByteOff p 0 b1
  case lenField of
    1 -> pokeByteOff p 1 (maskBit .|. fromIntegral plen :: Word8)
    3 -> do
      pokeByteOff p 1 (maskBit .|. 126 :: Word8)
      pokeByteOff p 2 (fromIntegral (plen `shiftR` 8) :: Word8)
      pokeByteOff p 3 (fromIntegral  plen             :: Word8)
    _ -> do
      pokeByteOff p 1 (maskBit .|. 127 :: Word8)
      pokeByteOff p 2 (fromIntegral (plen `shiftR` 56) :: Word8)
      pokeByteOff p 3 (fromIntegral (plen `shiftR` 48) :: Word8)
      pokeByteOff p 4 (fromIntegral (plen `shiftR` 40) :: Word8)
      pokeByteOff p 5 (fromIntegral (plen `shiftR` 32) :: Word8)
      pokeByteOff p 6 (fromIntegral (plen `shiftR` 24) :: Word8)
      pokeByteOff p 7 (fromIntegral (plen `shiftR` 16) :: Word8)
      pokeByteOff p 8 (fromIntegral (plen `shiftR`  8) :: Word8)
      pokeByteOff p 9 (fromIntegral  plen              :: Word8)
  case mMask of
    Nothing -> pure ()
    Just (Mask w) -> do
      let !off = 1 + lenField
      pokeByteOff p  off          (fromIntegral (w `shiftR` 24) :: Word8)
      pokeByteOff p (off + 1)     (fromIntegral (w `shiftR` 16) :: Word8)
      pokeByteOff p (off + 2)     (fromIntegral (w `shiftR`  8) :: Word8)
      pokeByteOff p (off + 3)     (fromIntegral  w              :: Word8)
  pure $! (1 + lenField + case mMask of { Just _ -> 4 ; Nothing -> 0 })
{-# INLINE writeFrameHeader #-}

-- | Render just the on-wire frame header (FIN \/ RSV \/ opcode byte,
-- the length encoding, and the optional masking key) into a fresh
-- pinned 'ByteString'.  The payload is /not/ included.
--
-- Wrapper around 'writeFrameHeader' for callers that need a
-- standalone 'ByteString'.  The hot send path in
-- 'Network.WebSocket.Connection.sendFrame' uses
-- 'writeFrameHeader' directly against the send ring instead.
frameHeaderBytes :: Frame -> ByteString
frameHeaderBytes f =
  BSI.unsafeCreate (frameHeaderLength f) $ \p -> do
    _ <- writeFrameHeader f p
    pure ()
{-# INLINE frameHeaderBytes #-}

maskAndPayload :: Maybe Mask -> ByteString -> B.Builder
maskAndPayload Nothing  payload = B.byteString payload
maskAndPayload (Just m) payload =
     maskBuilder m
  <> B.byteString (maskInPlace m (BS.copy payload))

maskBuilder :: Mask -> B.Builder
maskBuilder (Mask w) =
     B.word8 (fromIntegral (w `shiftR` 24))
  <> B.word8 (fromIntegral (w `shiftR` 16))
  <> B.word8 (fromIntegral (w `shiftR` 8))
  <> B.word8 (fromIntegral w)

bit7, bit6, bit5, bit4 :: Bool -> Word8
bit7 True = 0x80 ; bit7 False = 0
bit6 True = 0x40 ; bit6 False = 0
bit5 True = 0x20 ; bit5 False = 0
bit4 True = 0x10 ; bit4 False = 0

------------------------------------------------------------------------
-- Masking
------------------------------------------------------------------------

-- | Apply the WebSocket mask to a payload, returning a fresh
-- 'ByteString'.  Identity transform when the mask byte sequence
-- repeats over the payload (per RFC 6455 \u00a75.3, both directions
-- use the same XOR algorithm).
maskPayload :: Mask -> ByteString -> ByteString
maskPayload m bs = maskInPlace m (BS.copy bs)

-- | XOR the mask bytes over @bs@ in place via the SIMD masker in
-- "Wireform.FFI" ('Wireform.FFI.xorRepeatingKeyBS').
--
-- /Caller-owned/ memory: @bs@ must not be shared with any other
-- reader \u2014 the mutation is visible to anyone holding the
-- same backing.  The parser calls this on a freshly-sliced
-- payload immediately before handing it to the user; the user is
-- expected to consume the bytes before the next 'receiveFrame',
-- after which the recv ring may reuse the storage.
--
-- Returns the input 'ByteString' for chaining.
maskInPlace :: Mask -> ByteString -> ByteString
maskInPlace (Mask w) bs = unsafePerformIO $ do
  FFI.xorRepeatingKeyBS bs w
  pure bs
{-# NOINLINE maskInPlace #-}
