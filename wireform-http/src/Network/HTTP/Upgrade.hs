{- | 101 Switching Protocols escape hatch (RFC 9110 \u00a715.2.2).

The HTTP-1.1 @Upgrade@ \/ 101 dance is how WebSocket and any other
post-HTTP byte protocol gets onto an HTTP connection. The wireform
unified API doesn't run those protocols itself \u2014 they're upstream
\u2014 but it ships the handshake substrate here so a WebSocket
client \/ server can ride on a wireform connection.

Two pieces:

* 'sendUpgrade' \u2014 client side. Send an HTTP\/1.1 request with the
  caller-supplied @Upgrade@ header and other handshake bits, and on
  101 hand back the live raw bytes of the post-handshake stream.
* 'acceptUpgrade' \u2014 server side. Parse the request, hand the
  caller a chance to validate the @Upgrade@ header and emit a 101
  response, and yield the post-handshake byte stream.

Both functions are 'NS.Socket'-shaped because the upgraded protocol
is, by definition, no longer HTTP. They live alongside but
orthogonal to the high-level @Transport@ \/ @Handler@ machinery,
which only knows about HTTP request \/ response shapes.

This module is deliberately minimal: it doesn't speak WebSocket,
SPDY, or anything else. It exposes the byte stream and lets a
downstream library do the rest.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Upgrade
  ( -- * Client side
    UpgradeRequest (..)
  , sendUpgrade
  , UpgradeError (..)
    -- * Server side
  , UpgradeAccepted (..)
  , acceptUpgrade
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Network.HTTP.Types.Header as H

-- ---------------------------------------------------------------------------
-- Client side
-- ---------------------------------------------------------------------------

data UpgradeRequest = UpgradeRequest
  { upgradeMethod  :: !ByteString
    -- ^ Usually @\"GET\"@.
  , upgradeTarget  :: !ByteString
    -- ^ Request target (path \/ query).
  , upgradeHost    :: !ByteString
    -- ^ Value for the @Host@ header.
  , upgradeHeaders :: !H.Headers
    -- ^ Headers to send. @Connection: upgrade@ and the protocol
    --   token in @Upgrade@ are mandatory \u2014 the helper does not
    --   add them, callers are expected to know what protocol
    --   they're upgrading to.
  }

data UpgradeError
  = UpgradeMalformedStatus !ByteString
  | UpgradeRejected !Int !ByteString
    -- ^ Server returned something other than 101.
  deriving stock (Show)

instance Exception UpgradeError

-- | Send an HTTP\/1.1 request with @Upgrade:@ over the supplied
-- socket. Reads the response header block; on @101@ returns the
-- socket plus any bytes already buffered after the header
-- terminator (callers should treat those as the first frame).
-- Throws 'UpgradeRejected' on any other status.
sendUpgrade :: NS.Socket -> UpgradeRequest -> IO (NS.Socket, ByteString)
sendUpgrade sock req = do
  let reqBytes = BS.concat
        [ upgradeMethod req, " "
        , upgradeTarget req, " HTTP/1.1\r\n"
        , "Host: ", upgradeHost req, "\r\n"
        , renderHeaders (upgradeHeaders req)
        , "\r\n"
        ]
  NSB.sendAll sock reqBytes
  block <- readHeaderBlock sock BS.empty
  let (head_, rest) = splitHeaderBlock block
  case parseStatusLine head_ of
    Just 101 -> pure (sock, rest)
    Just c   -> throwIO (UpgradeRejected c head_)
    Nothing  -> throwIO (UpgradeMalformedStatus head_)

-- ---------------------------------------------------------------------------
-- Server side
-- ---------------------------------------------------------------------------

-- | Server-side counterpart. Reads the upcoming request's header
-- block from @sock@, parses the start line and headers, and hands
-- them to the caller as raw bytes. The caller decides whether to
-- accept the upgrade and, if so, writes the @101 Switching
-- Protocols@ response and any handshake bytes into the supplied
-- 'NS.Socket' itself; this helper does not.
acceptUpgrade :: NS.Socket -> IO UpgradeAccepted
acceptUpgrade sock = do
  block <- readHeaderBlock sock BS.empty
  let (head_, leftover) = splitHeaderBlock block
  pure UpgradeAccepted
    { uaRequestHead    = head_
    , uaPrebuffered    = leftover
    , uaSocket         = sock
    }

data UpgradeAccepted = UpgradeAccepted
  { uaRequestHead :: !ByteString
    -- ^ Raw request-head block, without the trailing @\\r\\n@.
  , uaPrebuffered :: !ByteString
    -- ^ Any bytes already received past the header terminator.
    --   Treat as the first frame.
  , uaSocket      :: !NS.Socket
  }

-- ---------------------------------------------------------------------------
-- Wire helpers
-- ---------------------------------------------------------------------------

renderHeaders :: H.Headers -> ByteString
renderHeaders = BS.concat . map line
  where
    line (n, v) = CI.original n <> ": " <> v <> "\r\n"

readHeaderBlock :: NS.Socket -> ByteString -> IO ByteString
readHeaderBlock sock acc
  | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
  | BS.length acc > 64 * 1024     = pure acc
  | otherwise = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then pure acc
        else readHeaderBlock sock (acc <> chunk)

-- | Split @head + \\r\\n\\r\\n + rest@.
splitHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t) | BS.null t -> (h, BS.empty)
         | otherwise -> (h, BS.drop 4 t)

parseStatusLine :: ByteString -> Maybe Int
parseStatusLine bs =
  let firstLine = BS.takeWhile (/= 0x0D) bs
  in case BS.split 0x20 firstLine of
       (_ver : code : _) -> case BS8.readInt code of
         Just (n, leftover) | BS.null leftover -> Just n
         _ -> Nothing
       _ -> Nothing
