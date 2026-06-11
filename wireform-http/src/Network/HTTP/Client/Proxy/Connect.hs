{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | HTTPS-via-proxy CONNECT tunnel helper (RFC 9110 \u00a79.3.6).

Given a 'Proxy' and a target @host:port@, 'connectThroughProxy' opens a
TCP connection to the proxy, sends a @CONNECT host:port HTTP\/1.1@
request line plus a 'Host' header, and waits for a @2xx@ response.
On success the returned 'NS.Socket' is the tunnel; the caller layers
TLS on top of it.

Per RFC 9110 \u00a79.3.6, the request body and any headers other than
@Host@ \/ @Proxy-Authorization@ are not sent; the proxy isn't expected
to read them. The response payload is also not read \u2014 the tunnel
becomes the connection right after the CRLF that ends the response
header block.

Note: this module deliberately uses raw socket I\/O rather than the
"Network.HTTP1.Client" stack because the latter consumes the socket
into a connection handle, and we want to hand the post-CONNECT
socket back to the caller untouched.
-}
module Network.HTTP.Client.Proxy.Connect (
  connectThroughProxy,
  ConnectError (..),
) where

import Control.Exception (Exception, throwIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Network.HTTP.Client.Proxy (Proxy (..))
import Network.Socket qualified as NS
import Network.Socket.ByteString qualified as NSB


-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

data ConnectError
  = ConnectProxyDialFailed !String
  | -- | The first line did not parse as an HTTP\/1.1 status line.
    ConnectProxyBadResponse !ByteString
  | {- | The proxy rejected the tunnel; carries the status code and
    the rest of the start line for diagnostics.
    -}
    ConnectProxyRefused !Int !ByteString
  deriving stock (Show)


instance Exception ConnectError


-- ---------------------------------------------------------------------------
-- The handshake
-- ---------------------------------------------------------------------------

{- | Open a TCP connection to @proxy@, issue @CONNECT host:port@,
read until the end of the response header block, and return the
live socket. The caller is responsible for layering TLS and
closing the socket on teardown.
-}
connectThroughProxy
  :: Proxy
  -> ByteString
  {- ^ Target host (the @host@ in @CONNECT host:port@). For an
  IPv6 literal pass it bracketed.
  -}
  -> Int
  -- ^ Target port.
  -> Maybe ByteString
  -- ^ Optional @Proxy-Authorization@ header value.
  -> IO NS.Socket
connectThroughProxy prx host port mAuth = do
  sock <- dial (BS8.unpack (proxyHost prx)) (show (proxyPort prx))
  let target = host <> ":" <> BS8.pack (show port)
      authHdr = case mAuth of
        Just v -> "Proxy-Authorization: " <> v <> "\r\n"
        Nothing -> ""
      reqBytes =
        BS.concat
          [ "CONNECT "
          , target
          , " HTTP/1.1\r\n"
          , "Host: "
          , target
          , "\r\n"
          , authHdr
          , "\r\n"
          ]
  NSB.sendAll sock reqBytes
  status <- readUntilDoubleCRLF sock BS.empty
  case parseStatusLine status of
    Just code
      | code >= 200 && code < 300 -> pure sock
      | otherwise ->
          throwIO (ConnectProxyRefused code status)
    Nothing -> throwIO (ConnectProxyBadResponse status)


dial :: String -> String -> IO NS.Socket
dial host port = do
  let hints = NS.defaultHints {NS.addrSocketType = NS.Stream}
  addrs <- NS.getAddrInfo (Just hints) (Just host) (Just port)
  case addrs of
    [] -> throwIO (ConnectProxyDialFailed (host <> ":" <> port))
    (a : _) -> do
      sock <- NS.socket (NS.addrFamily a) (NS.addrSocketType a) (NS.addrProtocol a)
      NS.connect sock (NS.addrAddress a)
      NS.setSocketOption sock NS.NoDelay 1
      pure sock


{- | Read from the socket until @\\r\\n\\r\\n@ appears, accumulating
everything up to and including the terminator.
-}
readUntilDoubleCRLF :: NS.Socket -> ByteString -> IO ByteString
readUntilDoubleCRLF sock acc
  | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
  | BS.length acc > 64 * 1024 = pure acc -- give up; downstream parser will error
  | otherwise = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then pure acc
        else readUntilDoubleCRLF sock (acc <> chunk)


parseStatusLine :: ByteString -> Maybe Int
parseStatusLine bs = do
  let firstLine = BS.takeWhile (/= 0x0D) bs
  case BS.split 0x20 firstLine of
    (_ver : code : _) -> case BS8.readInt code of
      Just (n, leftover) | BS.null leftover -> Just n
      _ -> Nothing
    _ -> Nothing
