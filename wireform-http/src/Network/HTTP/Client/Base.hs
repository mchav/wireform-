{- | The base transport: a 'Transport' built on top of the
low-level "Network.HTTP.Connection" single-connection API.

This is the production wire shim. It does enough to be useful:

* Parses the request URI to extract scheme \/ host \/ port.
* Opens a fresh connection per request (no pool yet; pooling is
  intended to follow).
* Honours HTTPS via the existing TLS module.
* Bridges the low-level @'Network.HTTP.Types.Body.Body' =
  'IO (Maybe ByteString)'@ shape to the high-level
  @'Popper' = 'IO ByteString'@.

It /eagerly drains the response body/ before returning the
'RawResponse'. That's a deliberate limitation — the low-level
body popper's lifetime is tied to the connection scope, which
means streaming through 'Transport's @sendRaw :: IO RawResponse@
shape would require connection-lifetime threading we don't have
yet.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Base
  ( baseTransport
  , BaseTransportError (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)

import qualified Network.HTTP.Connection    as Conn
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as LB
import qualified Network.HTTP.Types.Version as LV
import qualified Network.HTTP.VersionRange  as VR

import           Network.HTTP.Client.BodyStream
import qualified Network.HTTP.Client.Request   as WReq
import           Network.HTTP.Client.Protocol
import           Network.HTTP.Client.Response
import           Network.HTTP.Client.Transport
import qualified Network.HTTP.Client.URI       as WURI

data BaseTransportError
  = BaseTransportInvalidURI !String
    -- ^ Could not parse the resolved URI.
  | BaseTransportLowLevel   !Conn.ConnectionError
  deriving stock (Show)

instance Exception BaseTransportError

-- | The default base transport. Each call opens a connection, ships
-- the request, drains the response, and closes the connection. Use
-- TLS if the URI scheme is @https@.
baseTransport :: VR.VersionRange -> Transport IO
baseTransport versionRange = Transport $ \req -> do
  uri_ <- case WURI.renderRequestURI (WReq.requestURI req) of
    Right u  -> pure u
    Left err -> throwIO (BaseTransportInvalidURI err)
  let scheme = WURI.uriScheme uri_
      lowScheme = case scheme of
        WURI.SchemeHttps -> Msg.SchemeHttps
        WURI.SchemeHttp  -> Msg.SchemeHttp
      host   = BS8.unpack (WURI.uriHost uri_)
      port   = show (WURI.uriPort uri_)
      tls    = case scheme of
        WURI.SchemeHttps -> Just (Conn.defaultTlsConnectionConfig host)
        WURI.SchemeHttp  -> Nothing
      connCfg = Conn.ConnectionConfig
        { Conn.connectionHost         = host
        , Conn.connectionPort         = port
        , Conn.connectionVersionRange = versionRange
        , Conn.connectionTls          = tls
        }
      target = WURI.uriPathAndQuery uri_
      authority = Just (WURI.uriHost uri_ <> case BS8.readInt (BS8.pack port) of
                          Just (p, _)
                            | scheme == WURI.SchemeHttp  && p == 80  -> ""
                            | scheme == WURI.SchemeHttps && p == 443 -> ""
                          _ -> ":" <> BS8.pack port)

  lowBody <- toLowLevelBody (WReq.body req)
  let lowReq = Msg.Request
        { Msg.requestMethod    = WReq.method req
        , Msg.requestTarget    = target
        , Msg.requestAuthority = authority
        , Msg.requestScheme    = lowScheme
        , Msg.requestHeaders   = WReq.headers req
        , Msg.requestBody      = lowBody
        , Msg.requestVersion   = VR.preferredVersion versionRange
        , Msg.requestTrailers  = pure []
        }

  Conn.withConnection connCfg $ \conn -> do
    resp <- Conn.sendOn conn lowReq
    materialised <- lowLevelBodyBytes (Msg.responseBody resp)
    newPopper <- popperFromStrict materialised
    pure RawResponse
      { statusCode   = Msg.responseStatus resp
      , headers      = Msg.responseHeaders resp
      , bodyPopper   = newPopper
      , protocolInfo = lowToProtocol (Msg.responseVersion resp)
        -- The low-level path doesn't surface HTTP/2 stream ids yet,
        -- so 'lowToProtocol' emits a placeholder Http2Info for
        -- HTTP/2 responses.
      }

lowToProtocol :: LV.Version -> ProtocolInfo WReq.Request RawResponse
lowToProtocol LV.HTTP2 = HTTP2 Http2Info { h2StreamId = 0, h2PushPromises = pure [], h2CancelStream = pure () }
lowToProtocol _        = HTTP1_1

-- | Bridge from the high-level 'BodyStream' (which signals EOF with
-- an empty 'ByteString') to the low-level 'LB.Body' (which signals
-- EOF with 'Nothing'). Bodies of unknown size become 'LB.BodyStream';
-- known-size ones get folded down to 'LB.BodyBytes' so the low-level
-- encoder can set @Content-Length@ without sniffing.
toLowLevelBody :: BodyStream -> IO LB.Body
toLowLevelBody bs = case knownSize bs of
  Just 0  -> pure LB.BodyEmpty
  Just _  -> LB.BodyBytes <$> bodyStreamBytes bs
  Nothing -> pure $ LB.BodyStream $ do
    chunk <- pull bs
    if BS.null chunk then pure Nothing else pure (Just chunk)

-- | Materialise a low-level 'LB.Body' into a strict 'ByteString'.
--
-- Streaming bodies are adapted to the wireform 'Popper' shape
-- (empty 'ByteString' signalling EOF instead of 'Nothing') and then
-- folded with 'popperBytes', which uses a builder for O(1)
-- per-chunk appends.
lowLevelBodyBytes :: LB.Body -> IO ByteString
lowLevelBodyBytes = \case
  LB.BodyEmpty    -> pure BS.empty
  LB.BodyBytes bs -> pure bs
  LB.BodyStream p -> popperBytes (fromMaybe BS.empty <$> p)
