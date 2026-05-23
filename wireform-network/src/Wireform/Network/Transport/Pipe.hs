{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | In-memory paired 'DuplexTransport's for tests.
--
-- 'newDuplexPipe' returns @(clientSide, brokerSide)@: bytes
-- written to @clientSide@'s send appear at @brokerSide@'s receive,
-- and vice versa.  Used by every test that wants to drive both
-- ends of a protocol conversation in the same process without
-- opening a real socket.
module Wireform.Network.Transport.Pipe
  ( newDuplexPipe
  ) where

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Data.Word (Word8)

import Wireform.Transport.Config
import Wireform.Network.Transport.Duplex

-- | Build a pair of 'DuplexTransport's connected by in-memory
-- queues.  Closing one side's send half delivers EOF to the
-- other side's receive half.
--
-- @
-- (client, broker) <- newDuplexPipe defaultTransportConfig
-- ...
-- 'duplexClose' client
-- @
newDuplexPipe :: TransportConfig -> IO (DuplexTransport, DuplexTransport)
newDuplexPipe cfg = do
  c2b <- newTVarIO (mempty :: BS.ByteString)   -- client -> broker
  b2c <- newTVarIO (mempty :: BS.ByteString)   -- broker -> client
  c2bClosed <- newTVarIO False                  -- client shut WR
  b2cClosed <- newTVarIO False                  -- broker shut WR

  clientSide <-
    newDuplexBufTransport cfg
      (queueRecvFn b2c b2cClosed)               -- client recv = drain b->c
      (queueSendFn c2b c2bClosed)               -- client send = push c->b
      (atomically (writeTVar c2bClosed True))

  brokerSide <-
    newDuplexBufTransport cfg
      (queueRecvFn c2b c2bClosed)               -- broker recv = drain c->b
      (queueSendFn b2c b2cClosed)               -- broker send = push b->c
      (atomically (writeTVar b2cClosed True))

  pure (clientSide, brokerSide)

-- | Read up to @want@ bytes from a queue; block until either some
-- bytes are available or the queue's write side is closed (in
-- which case return 0 = EOF).
queueRecvFn
  :: TVar BS.ByteString
  -> TVar Bool
  -> Ptr Word8
  -> Int
  -> IO Int
queueRecvFn ref closedRef dst want = do
  bs <- atomically $ do
    cur <- readTVar ref
    if BS.null cur
      then do
        closed <- readTVar closedRef
        if closed then pure BS.empty else retry
      else do
        let !take_ = min want (BS.length cur)
            !taken = BS.take take_ cur
            !rest  = BS.drop take_ cur
        writeTVar ref rest
        pure taken
  if BS.null bs
    then pure 0
    else do
      copyBSInto dst bs
      pure (BS.length bs)

queueSendFn
  :: TVar BS.ByteString
  -> TVar Bool
  -> Ptr Word8
  -> Int
  -> IO Int
queueSendFn ref closedRef src n = do
  closed <- readTVarIO closedRef
  if closed
    then ioError (userError "pipe send: already shut WR")
    else do
      bs <- BSI.create n $ \dst -> copyBytes dst src n
      atomically $ modifyTVar' ref (<> bs)
      pure n

copyBSInto :: Ptr Word8 -> BS.ByteString -> IO ()
copyBSInto dst bs =
  let (fp, off, len) = BSI.toForeignPtr bs
  in withForeignPtr fp $ \src ->
       copyBytes dst (src `plusPtr` off) len
