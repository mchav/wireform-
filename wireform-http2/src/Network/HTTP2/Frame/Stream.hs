{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

{- | Streaming HTTP\/2 frame reader on top of the wireform
@Stream@ parser surface ('Wireform.Parser') and a magic-ring
'Wireform.Transport'.

HTTP\/2 framing is regular: every frame is a 9-byte header followed
by a payload whose length is the first 24 bits of the header.  This
module turns that into a 'Wireform.Parser.Stream' parser that
suspends on the IO manager when bytes aren't yet on the wire,
returns the typed 'FrameHeader' + the zero-copy payload slice, and
re-uses 'decodeFramePayload' for the existing per-type validation
(SETTINGS multiple-of-6, PING \/ RST_STREAM \/ WINDOW_UPDATE
length-equality, etc.).

The streaming reader replaces the manual @tRecvBuf@ + 'RecvBuffer'
dance the connection layer currently runs.  No per-frame allocation
beyond the typed records the existing decoder builds; the payload
remains a slice into the ring's backing memory and stays valid
until the parser returns to the driver.
-}
module Network.HTTP2.Frame.Stream
  ( -- * Errors
    FrameStreamError (..)

    -- * Parsers
  , frameHeaderParser
  , frameParser
  , runFrameLoop
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Word (Word8, Word32)

import Wireform.Parser
  ( Parser
  , anyWord8
  , anyWord16be
  , anyWord32be
  , err
  , takeBs
  )
import Wireform.Parser.Driver
  ( LoopControl
  , runParserLoop
  )
import Wireform.Parser.Error (ParseError)
import Wireform.Parser.Internal (Stream)
import Wireform.Transport (Transport)

import Network.HTTP2.Frame.Decode (FrameDecodeError, decodeFramePayload)
import Network.HTTP2.Frame.Types
  ( Frame (..)
  , FrameHeader (..)
  )
import Network.HTTP2.Types (FrameType (..))

-- | Wireform-parser-level errors raised by the streaming reader.
--
-- 'FrameStreamDecode' wraps the existing per-type validation result
-- from 'decodeFramePayload' so the streaming reader produces the
-- same error vocabulary the classic decoder does.
newtype FrameStreamError
  = FrameStreamDecode FrameDecodeError
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------

-- | Parse the 9-byte HTTP\/2 frame header.
--
-- Wire layout (RFC 9113 § 4.1):
--
-- @
--  +-----------------------------------------------+
--  |                 Length (24)                   |
--  +---------------+---------------+---------------+
--  |   Type (8)    |   Flags (8)   |
--  +-+-------------+---------------+-------------------------------+
--  |R|                 Stream Identifier (31)                       |
--  +=+=============================================================+
-- @
frameHeaderParser :: Parser Stream FrameStreamError FrameHeader
frameHeaderParser = do
  hi    <- anyWord16be
  lo8   <- anyWord8
  let !len = ((fromIntegral hi :: Word32) `shiftL` 8)
         .|. fromIntegral lo8
  typ   <- anyWord8
  flags <- anyWord8
  sid   <- anyWord32be
  pure FrameHeader
    { fhLength   = len
    , fhType     = word8ToFrameType typ
    , fhFlags    = flags
    , fhStreamId = sid .&. 0x7FFFFFFF
    }

------------------------------------------------------------------------
-- Frame (header + payload)
------------------------------------------------------------------------

-- | Parse one full HTTP\/2 frame off the wire.
--
-- The returned payload 'ByteString' inside the 'Frame' is a
-- zero-copy slice of the ring's backing memory.  The driver
-- advances the tail past the frame only when this parser returns,
-- so the slice stays valid through the caller's handler.  If the
-- handler needs to retain the bytes beyond that scope it MUST
-- 'BS.copy' before stashing.
frameParser :: Parser Stream FrameStreamError Frame
frameParser = do
  !hdr  <- frameHeaderParser
  !body <- takeBs (fromIntegral (fhLength hdr))
  case decodeFramePayload hdr body of
    Right pl -> pure (Frame hdr pl)
    Left e   -> err (FrameStreamDecode e)

-- | Streaming driver: parse a frame, hand it to the supplied
-- handler, and loop.  Handler returns 'Continue' to keep reading
-- or 'Stop' to drop out of the loop voluntarily.
runFrameLoop
  :: Transport
  -> (Frame -> IO LoopControl)
  -> IO (Either (ParseError FrameStreamError) ())
runFrameLoop t = runParserLoop t frameParser

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

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
  w   -> FrameUnknown w
