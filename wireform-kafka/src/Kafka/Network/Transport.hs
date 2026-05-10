{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{-|
Module      : Kafka.Network.Transport
Description : Pluggable network transport for Kafka client
Copyright   : (c) 2026
License     : BSD-3-Clause
Maintainer  : kafka-native

The default 'Kafka.Network.Connection' module is hard-coded to
@crypton-connection@. That works for production but is awkward for:

  * unix-socket / Vsock / abstract-socket transports (custom
    fabrics, micro-VM wireup);
  * in-process testing transports that pipe bytes through a 'TVar'
    rather than a real socket;
  * shared-process integration tests where a single Haskell process
    drives both a mock broker and a real client end without
    listening on a real TCP port.

The 'Transport' record is the minimal interface a custom transport
has to satisfy: open / read-up-to-N / write-bytes / close. The
existing 'Network.Connection' path implements this via
'mkTcpTransport' (and the test suite carries an in-memory
'mkPipeTransport' implementation).

This module is intentionally free of any TLS / SASL specifics —
those are layered on /top/ by 'Kafka.Network.Connection' and
'Kafka.Network.Auth.SASL'. A custom transport that wants to carry
a TLS session inside a unix socket just composes the two.
-}
module Kafka.Network.Transport
  ( -- * Transport interface
    Transport (..)
    -- * Built-in transports
  , mkTcpTransport
  , mkPipeTransport
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Network.Connection as NC

-- | Pluggable byte-stream transport.
--
-- Every operation is total in the type sense; failures are
-- surfaced via 'Left'. Implementations are expected to be
-- /thread-safe for sequential use/ — callers serialise reads and
-- writes via an external lock or by routing all I/O through a
-- single thread, which is what 'Kafka.Client.Pipeline' does.
data Transport = Transport
  { transportRead  :: Int -> IO (Either String ByteString)
    -- ^ Read up to @n@ bytes. Empty 'ByteString' means peer
    --   closed (EOF).
  , transportWrite :: ByteString -> IO (Either String ())
    -- ^ Write the given bytes, blocking until they're handed to
    --   the kernel / next layer.
  , transportClose :: IO ()
    -- ^ Close the transport. Idempotent: a second call is a
    --   no-op.
  , transportName  :: String
    -- ^ Human-readable label for logs / errors. Examples:
    --   @"tcp:broker1:9092"@, @"unix:/var/run/kafka.sock"@,
    --   @"pipe:test-456"@.
  }

-- | Wrap an existing @crypton-connection@ 'NC.Connection' as a
-- 'Transport'. This is what 'Kafka.Network.Connection' implements
-- under the hood; exposed so callers that want the default TCP /
-- TLS plumbing without committing to 'ConnectionManager' can opt
-- in piecemeal.
mkTcpTransport :: NC.Connection -> String -> Transport
mkTcpTransport conn label = Transport
  { transportRead  = \n -> do
      bs <- NC.connectionGet conn n
      pure (Right bs)
  , transportWrite = \bs -> do
      NC.connectionPut conn bs
      pure (Right ())
  , transportClose = NC.connectionClose conn
  , transportName  = label
  }

-- | Build a pair of 'Transport's connected by an in-memory
-- queue. Useful for tests that want to drive both ends of a
-- broker conversation in the same process.
--
-- @
-- (clientSide, brokerSide) <- mkPipeTransport
-- _ <- transportWrite clientSide \"hello\"
-- Right msg <- transportRead brokerSide 5
-- msg == \"hello\"
-- @
mkPipeTransport :: IO (Transport, Transport)
mkPipeTransport = do
  -- Two unbounded queues, one per direction. We lift to lists of
  -- chunks so we can preserve write boundaries — readers can ask
  -- for fewer bytes than a chunk and we re-queue the leftover.
  c2b <- newTVarIO (mempty :: ByteString)
  b2c <- newTVarIO (mempty :: ByteString)
  closed <- newTVarIO False

  let drain ref n = atomically $ do
        bs <- readTVar ref
        if BS.null bs
          then do
            isClosed <- readTVar closed
            if isClosed
              then pure BS.empty   -- EOF
              else retry
          else do
            let !taken = BS.take n bs
                !rest  = BS.drop n bs
            writeTVar ref rest
            pure taken

      append ref bs = atomically $ do
        cur <- readTVar ref
        writeTVar ref (cur <> bs)

      mkSide rRef wRef lbl = Transport
        { transportRead  = \n -> do
            isClosed <- readTVarIO closed
            cur <- readTVarIO rRef
            if isClosed && BS.null cur
              then pure (Right BS.empty)
              else fmap Right (drain rRef n)
        , transportWrite = \bs -> do
            isClosed <- readTVarIO closed
            if isClosed
              then pure (Left (lbl <> ": transport closed"))
              else do
                append wRef bs
                pure (Right ())
        , transportClose = atomically (writeTVar closed True)
        , transportName  = lbl
        }

      clientSide = mkSide b2c c2b "pipe:client"
      brokerSide = mkSide c2b b2c "pipe:broker"
  pure (clientSide, brokerSide)
