{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
Module      : Kafka.Network.Transport
Description : Pluggable byte-stream transport for the Kafka client.

The 'Transport' record is the minimal interface a custom transport
has to satisfy: open / read-up-to-N / write-bytes / close.  The
default 'Kafka.Network.Connection' module is the canonical
implementation, backed by 'Wireform.Network.DuplexTransport' (one
magic-ring receive transport + one magic-ring send transport) and,
when TLS is in use, the OpenSSL 'Wireform.Network.TLS.OpenSSL.SslConn'.

This module is intentionally free of any TLS \/ SASL specifics —
those are layered on /top/ by 'Kafka.Network.Connection' and
'Kafka.Network.Auth.SASL'.
-}
module Kafka.Network.Transport (
  -- * Transport interface
  Transport (..),

  -- * Built-in transports
  mkConnectionTransport,
  mkPipeTransport,
) where

import Data.ByteString (ByteString)
import qualified Wireform.Builder as WB

import qualified Kafka.Network.Connection as Conn
import qualified Wireform.Network as WN
import qualified Wireform.Transport.Receive as WR
import qualified Wireform.Transport.Send as WS
import Data.Bits ((.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import Data.IORef
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (plusPtr)
import Control.Exception (SomeException, try, throwIO)
import Data.Word (Word64)


{- | Pluggable byte-stream transport.

Every operation is total in the type sense; failures are
surfaced via 'Left'.  Implementations are expected to be
/thread-safe for sequential use/ — callers serialise reads and
writes via an external lock or by routing all I/O through a
single thread, which is what 'Kafka.Client.Pipeline' does.
-}
data Transport = Transport
  { transportRead :: Int -> IO (Either String ByteString)
  , transportWrite :: ByteString -> IO (Either String ())
  , transportWriteBuilder :: WB.Builder -> IO (Either String ())
  , transportClose :: IO ()
  , transportName :: String
  }

{- | Wrap a 'Kafka.Network.Connection.Connection' as a 'Transport'.
    This is what the default code path does under the hood. -}
mkConnectionTransport :: Conn.Connection -> String -> Transport
mkConnectionTransport conn label =
  Transport
    { transportRead = \n -> do
        r <- try (Conn.connectionGet conn n) :: IO (Either SomeException ByteString)
        case r of
          Right bs -> pure (Right bs)
          Left  e  -> pure (Left (show e))
    , transportWrite = \bs -> do
        r <- try (Conn.connectionPut conn bs) :: IO (Either SomeException ())
        case r of
          Right () -> pure (Right ())
          Left  e  -> pure (Left (show e))
    , transportWriteBuilder = \b -> do
        r <- try (Conn.connectionPutBuilder conn b) :: IO (Either SomeException ())
        case r of
          Right () -> pure (Right ())
          Left  e  -> pure (Left (show e))
    , transportClose = Conn.connectionClose conn
    , transportName  = label
    }


{- | Build a pair of 'Transport's connected by in-memory queues.
Useful for tests that want to drive both ends of a broker
conversation in the same process.

Both sides go through magic-ring 'Wireform.Network.DuplexTransport's,
so the bytes flow through the same code path as a real network
connection (just without the kernel).
-}
mkPipeTransport :: IO (Transport, Transport)
mkPipeTransport = do
  (a, b) <- WN.newDuplexPipe WN.defaultTransportConfig
  cursorA <- newIORef (0 :: Word64)
  cursorB <- newIORef (0 :: Word64)
  pure (asTransport "pipe:client" a cursorA, asTransport "pipe:broker" b cursorB)
  where
    asTransport label duplex cursor =
      let rx = WN.duplexReceive duplex
          tx = WN.duplexSend duplex
      in Transport
           { transportRead = \n -> do
               r <- try (readUpTo rx cursor n) :: IO (Either SomeException ByteString)
               case r of
                 Right bs -> pure (Right bs)
                 Left  e  -> pure (Left (show e))
           , transportWrite = \bs -> do
               r <- try (WS.sendByteString tx bs) :: IO (Either SomeException ())
               case r of
                 Right () -> pure (Right ())
                 Left  e  -> pure (Left (show e))
           , transportWriteBuilder = \b -> do
               r <- try (WS.sendByteString tx (WB.toStrictByteString b))
                       :: IO (Either SomeException ())
               case r of
                 Right () -> pure (Right ())
                 Left  e  -> pure (Left (show e))
           , transportClose = WN.closeDuplexTransport duplex
           , transportName  = label
           }

    readUpTo rx cursor n
      | n <= 0 = pure BS.empty
      | otherwise = do
          pos <- readIORef cursor
          h0  <- WR.receiveLoadHead rx
          h <- if h0 > pos then pure h0
                 else do
                   r <- WR.receiveWaitData rx pos
                   case r of
                     WR.ReceiveMoreData h' -> pure h'
                     WR.ReceiveEndOfInput  -> pure pos
                     WR.ReceiveFailed e    -> throwIO e
          if h <= pos
            then pure BS.empty
            else do
              let !want = min n (fromIntegral (h - pos))
                  !off  = fromIntegral pos .&. WR.receiveRingMask rx
                  !ptr  = WR.receiveRingBase rx `plusPtr` off
              bs <- BSI.create want $ \dst -> copyBytes dst ptr want
              let !newPos = pos + fromIntegral want
              writeIORef cursor newPos
              WR.receiveAdvanceTail rx newPos
              pure bs
