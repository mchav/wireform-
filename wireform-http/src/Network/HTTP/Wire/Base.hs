{- | The base transport: a 'Transport' built on top of the existing
low-level "Network.HTTP.Client" connection API.

This is the production wire shim. It does enough to be useful:

* Parses the request URI to extract scheme \/ host \/ port.
* Opens a fresh connection per request (no pool; the connection
  pool is intended to follow, see @ClientConfig@'s @poolConfig@).
* Honours HTTPS via the existing TLS module.
* Bridges the low-level @'Network.HTTP.Types.Body.Body' =
  'BodyStream' (IO (Maybe ByteString))@ representation to the
  high-level @'Popper' = 'IO ByteString'@.

It /eagerly drains the response body/ before returning the
'RawResponse'. That's a deliberate limitation — the existing
low-level body popper's lifetime is tied to the connection scope,
which means streaming through 'Transport's @sendRaw :: IO
RawResponse@ shape would require connection-lifetime threading we
don't have yet. Once 'pooledTransport' lands, streaming will live
there.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Wire.Base
  ( baseTransport
  , BaseTransportError (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)

import qualified Network.HTTP.Client       as LowLevel
import           Network.HTTP.Message       (Scheme (..))
import qualified Network.HTTP.Message       as Msg
import qualified Network.HTTP.Types.Body    as LB
import qualified Network.HTTP.Types.Header  as LH
import qualified Network.HTTP.VersionRange  as VR

import           Network.HTTP.Wire.BodyStream
import qualified Network.HTTP.Wire.Request   as WReq
import           Network.HTTP.Wire.Protocol
import           Network.HTTP.Wire.Response
import           Network.HTTP.Wire.Transport
import qualified Network.HTTP.Wire.URI       as WURI

data BaseTransportError
  = BaseTransportInvalidURI !String
    -- ^ Could not parse the resolved URI.
  | BaseTransportLowLevel   !LowLevel.ClientError
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
      host   = BS8.unpack (WURI.uriHost uri_)
      port   = show (WURI.uriPort uri_)
      tls    = case scheme of
        SchemeHttps -> Just (LowLevel.defaultTlsClientConfig host)
        SchemeHttp  -> Nothing
      lowCfg = LowLevel.ClientConfig
        { LowLevel.clientHost         = host
        , LowLevel.clientPort         = port
        , LowLevel.clientVersionRange = versionRange
        , LowLevel.clientTls          = tls
        }
      target = WURI.uriPathAndQuery uri_
      authority = Just (WURI.uriHost uri_ <> case BS8.readInt (BS8.pack port) of
                          Just (p, _)
                            | (scheme == SchemeHttp  && p == 80)  -> ""
                            | (scheme == SchemeHttps && p == 443) -> ""
                          _ -> ":" <> BS8.pack port)

  lowBody <- toLowLevelBody (WReq.body req)
  let lowReq = Msg.Request
        { Msg.requestMethod    = WReq.method req
        , Msg.requestTarget    = target
        , Msg.requestAuthority = authority
        , Msg.requestScheme    = scheme
        , Msg.requestHeaders   = WReq.headers req
        , Msg.requestBody      = lowBody
        , Msg.requestVersion   = VR.preferredVersion versionRange
        , Msg.requestTrailers  = pure []
        }

  LowLevel.withClient lowCfg $ \client -> do
    resp <- LowLevel.sendRequest client lowReq
    drained <- drainLowLevelBody (Msg.responseBody resp)
    newPopper <- popperFromStrict drained
    pure RawResponse
      { statusCode   = Msg.responseStatus resp
      , headers      = Msg.responseHeaders resp
      , bodyPopper   = newPopper
      , protocolInfo = case Msg.responseVersion resp of
          -- HTTP/2 metadata: the low-level path doesn't surface the
          -- stream id yet, so we emit a placeholder Http2Info.
          v | v == LowLevel.clientNegotiatedVersion client ->
                lowToProtocol v
          v -> lowToProtocol v
      }

lowToProtocol :: VR.Version -> ProtocolInfo
lowToProtocol VR.HTTP2 = HTTP2 Http2Info { h2StreamId = 0, h2PushPromises = pure [] }
lowToProtocol _        = HTTP1_1

-- | Bridge from the high-level 'BodyStream' (which signals EOF with
-- an empty 'ByteString') to the low-level 'LB.Body' (which signals
-- EOF with 'Nothing'). Bodies of unknown size become 'LB.BodyStream';
-- known-size ones get folded down to 'LB.BodyBytes' so the low-level
-- encoder can set @Content-Length@ without sniffing.
toLowLevelBody :: BodyStream -> IO LB.Body
toLowLevelBody bs = case knownSize bs of
  Just 0  -> pure LB.BodyEmpty
  Just _  -> do
    fully <- drainPopper (pull bs)
    pure (LB.BodyBytes fully)
  Nothing -> pure $ LB.BodyStream $ do
    chunk <- pull bs
    if BS.null chunk then pure Nothing else pure (Just chunk)

drainLowLevelBody :: LB.Body -> IO ByteString
drainLowLevelBody = \case
  LB.BodyEmpty    -> pure BS.empty
  LB.BodyBytes bs -> pure bs
  LB.BodyStream p -> drainStream p
  where
    drainStream p = go []
      where
        go acc = p >>= \case
          Nothing  -> pure $! BS.concat (reverse acc)
          Just bs
            | BS.null bs -> go acc
            | otherwise  -> go (bs : acc)

-- Mute unused-import warnings on LH if no headers are referenced.
_unused :: LH.HeaderName
_unused = LH.hContentType
