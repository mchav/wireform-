{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

{- |
Module      : Kafka.Network.FrameParser
Description : Wireform-parser-based Kafka response frame reader
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

Kafka's response framing on the wire is

@
  [Int32 BE length] [Int32 BE correlationId] [body of (length - 4) bytes]
@

This module provides a streaming wireform 'Wireform.Parser.Stream'
parser that walks the magic-ring transport and yields one
@(correlationId, body)@ per frame.  The body 'BS.ByteString' is a
zero-copy slice of the ring's backing memory and stays valid only
within the parser thread's invocation of the response handler (the
caller usually @copy@s into a 'TMVar' or hands it to the application
under the same scope).

Used by 'Kafka.Client.Pipeline' to replace the legacy
@connectionGetExact@ / @runGet@ loop with a single allocation-free
streaming parse.
-}
module Kafka.Network.FrameParser
  ( FrameError (..)
  , kafkaFrameParser
  , runKafkaFrameLoop
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Int (Int32)

import Wireform.Parser
  ( Parser
  , anyInt32be
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

-- | Kafka-specific recoverable parse errors raised by 'cut' /
-- 'err' inside the frame parser.  The driver wraps these in a
-- 'Wireform.Parser.Error.ParseErr'.
data FrameError
  = FrameTooShort     -- ^ length prefix < 4 (no room for correlation id)
  | FrameNegativeLen  -- ^ length prefix is negative
  | FrameOversized !Int -- ^ length prefix is greater than the allowed maximum
  deriving stock (Eq, Show)

-- | Parse exactly one Kafka response frame off the wire.
--
--   * 4-byte big-endian frame length (the count of bytes that
--     follow, /including/ the correlation id).
--   * 4-byte big-endian correlation id.
--   * @length - 4@ body bytes, returned as a zero-copy slice into
--     the ring's backing memory.
--
-- The caller MUST consume / copy the body 'ByteString' before
-- returning from the response handler — once the driver advances
-- the ring tail past the frame the slice's bytes may be overwritten
-- by a subsequent recv.
--
-- A negative length, or a length less than 4, raises 'FrameTooShort'
-- / 'FrameNegativeLen' (recoverable via 'cut' in calling code).  Use
-- 'kafkaFrameParser' as-is when the upstream Kafka broker is trusted;
-- set a per-message cap by passing through 'oversizedAt'.
kafkaFrameParser :: Parser Stream FrameError (Int32, ByteString)
kafkaFrameParser = do
  !len <- anyInt32be
  case len of
    _ | len < 4   -> err (if len < 0 then FrameNegativeLen else FrameTooShort)
      | otherwise -> do
          !cid  <- anyInt32be
          !body <- takeBs (fromIntegral len - 4)
          pure (cid, body)
{-# INLINE kafkaFrameParser #-}

-- | Streaming driver: run 'kafkaFrameParser' against the transport
-- repeatedly, dispatching every parsed frame to the supplied
-- handler.  Returns 'Left' on parse / transport error, 'Right ()'
-- on clean EOF.
--
-- The handler may return 'Stop' to terminate the loop voluntarily
-- (matches the contract of 'runParserLoop' from wireform-core).
runKafkaFrameLoop
  :: Transport
  -> ((Int32, ByteString) -> IO LoopControl)
  -> IO (Either (ParseError FrameError) ())
runKafkaFrameLoop t = runParserLoop t kafkaFrameParser
