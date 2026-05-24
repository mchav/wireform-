{- | Bidirectional adapter between WAI and wireform-http.

=== Testing existing WAI applications

@
import qualified Network.Wai as Wai
import Network.HTTP.WAI (waiToHandler, waiTransport)

myWaiApp :: Wai.'Wai.Application'
myWaiApp = ...

-- Option 1: use as a wireform-http Handler (server-level types)
handler :: 'Network.HTTP.Server.Handler'
handler = 'waiToHandler' myWaiApp

-- Option 2: use as a wireform-http Transport (client-level types)
-- for in-process testing with the full middleware stack
transport :: 'Network.HTTP.Client.Transport.Transport' IO
transport = 'waiTransport' myWaiApp
@

=== Serving wireform-http handlers through Warp

@
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.WAI (handlerToWai)

myHandler :: 'Network.HTTP.Server.Handler'
myHandler req = ...

main :: IO ()
main = Warp.run 8080 ('handlerToWai' myHandler)
@
-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.WAI
  ( -- * Server-level adapters
    waiToHandler
  , handlerToWai
    -- * Client-level adapter (in-process Transport)
  , waiTransport

    -- * Type conversions: wireform → WAI
  , toWaiRequest
  , toWaiResponse

    -- * Type conversions: WAI → wireform
  , fromWaiRequest
  , fromWaiResponse
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (byteString, toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word16)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vault.Lazy as Vault
import qualified "http-types" Network.HTTP.Types as WAIHttp
import qualified Network.Socket as NS
import qualified Network.Wai as Wai
import Network.Wai.Internal (ResponseReceived(ResponseReceived))
import qualified Network.Wai.Internal as WaiI

import qualified Network.HTTP.Message as U
import qualified "wireform-http" Network.HTTP.Types.Body as U
import qualified "wireform-http" Network.HTTP.Types.Header as U
import qualified "wireform-http" Network.HTTP.Types.Method as U
import qualified "wireform-http" Network.HTTP.Types.Status as U
import qualified "wireform-http" Network.HTTP.Types.Version as U

import qualified Network.HTTP.Client.BodyStream as CBS
import qualified Network.HTTP.Client.Protocol as P
import qualified Network.HTTP.Client.Request as CR
import qualified Network.HTTP.Client.Response as CResp
import qualified Network.HTTP.Client.Transport as CT
import qualified Network.HTTP.Client.URI as URI

------------------------------------------------------------------------
-- Primitive type conversions
------------------------------------------------------------------------

toWaiMethod :: U.Method -> WAIHttp.Method
toWaiMethod = U.fromMethod

fromWaiMethod :: WAIHttp.Method -> U.Method
fromWaiMethod = U.methodFromBytes

toWaiStatus :: U.Status -> WAIHttp.Status
toWaiStatus s = WAIHttp.mkStatus (fromIntegral (U.statusCode s)) (U.statusReason s)

fromWaiStatus :: WAIHttp.Status -> U.Status
fromWaiStatus s = U.Status (fromIntegral (WAIHttp.statusCode s) :: Word16)

toWaiVersion :: U.Version -> WAIHttp.HttpVersion
toWaiVersion v = WAIHttp.HttpVersion
  (fromIntegral (U.versionMajor v))
  (fromIntegral (U.versionMinor v))

fromWaiVersion :: WAIHttp.HttpVersion -> U.Version
fromWaiVersion v = U.mkVersion
  (fromIntegral (WAIHttp.httpMajor v))
  (fromIntegral (WAIHttp.httpMinor v))

-- Headers are structurally identical: both [(CI ByteString, ByteString)].
toWaiHeaders :: U.Headers -> WAIHttp.ResponseHeaders
toWaiHeaders = id

fromWaiHeaders :: [WAIHttp.Header] -> U.Headers
fromWaiHeaders = id

------------------------------------------------------------------------
-- Body conversions
------------------------------------------------------------------------

mkWaiBodyReader :: U.Body -> IO (IO ByteString)
mkWaiBodyReader = \case
  U.BodyEmpty -> pure (pure BS.empty)
  U.BodyBytes bs -> do
    ref <- newIORef (Just bs)
    pure $ do
      mb <- readIORef ref
      case mb of
        Nothing -> pure BS.empty
        Just b  -> do
          writeIORef ref Nothing
          pure b
  U.BodyStream producer -> pure $ do
    mb <- producer
    case mb of
      Nothing -> pure BS.empty
      Just b  -> pure b

waiBodyToBody :: IO ByteString -> U.Body
waiBodyToBody reader = U.BodyStream $ do
  chunk <- reader
  if BS.null chunk
    then pure Nothing
    else pure (Just chunk)

------------------------------------------------------------------------
-- Request conversions (server-level, Network.HTTP.Message)
------------------------------------------------------------------------

splitTarget :: ByteString -> (ByteString, ByteString)
splitTarget bs =
  let (path, rest) = BS.break (== 0x3F) bs
  in (path, rest)

decodePath :: ByteString -> [T.Text]
decodePath raw =
  let stripped = BS.dropWhile (== 0x2F) raw
  in if BS.null stripped
       then []
       else map TE.decodeUtf8 (BS.split 0x2F stripped)

-- | Convert a wireform unified 'U.Request' into a WAI 'Wai.Request'.
--
-- Fields WAI needs but wireform doesn't carry (vault, remoteHost) are
-- filled with sensible defaults.
toWaiRequest :: U.Request -> IO Wai.Request
toWaiRequest r = do
  bodyReader <- mkWaiBodyReader (U.requestBody r)
  let (rawPath, rawQS) = splitTarget (U.requestTarget r)
      secure = case U.requestScheme r of
        U.SchemeHttps -> True
        U.SchemeHttp  -> False
      bodyLen = case U.requestBody r of
        U.BodyEmpty    -> Wai.KnownLength 0
        U.BodyBytes bs -> Wai.KnownLength (fromIntegral (BS.length bs))
        U.BodyStream _ -> Wai.ChunkedBody
      waiHdrs = toWaiHeaders (U.requestHeaders r)
      hostHdr = U.lookupHeader U.hHost (U.requestHeaders r)
      authority = U.requestAuthority r
  pure $ Wai.setRequestBodyChunks bodyReader $ Wai.mapRequestHeaders (const waiHdrs)
    Wai.defaultRequest
      { WaiI.requestMethod = toWaiMethod (U.requestMethod r)
      , WaiI.httpVersion = toWaiVersion (U.requestVersion r)
      , WaiI.rawPathInfo = rawPath
      , WaiI.rawQueryString = rawQS
      , WaiI.isSecure = secure
      , WaiI.remoteHost = NS.SockAddrInet 0 0
      , WaiI.pathInfo = decodePath rawPath
      , WaiI.queryString = WAIHttp.parseQuery rawQS
      , WaiI.vault = Vault.empty
      , WaiI.requestBodyLength = bodyLen
      , WaiI.requestHeaderHost = firstJust hostHdr authority
      , WaiI.requestHeaderRange = U.lookupHeader U.hRange (U.requestHeaders r)
      , WaiI.requestHeaderReferer = lookupCI "Referer" (U.requestHeaders r)
      , WaiI.requestHeaderUserAgent = U.lookupHeader U.hUserAgent (U.requestHeaders r)
      }

-- | Convert a WAI 'Wai.Request' into a wireform unified 'U.Request'.
fromWaiRequest :: Wai.Request -> U.Request
fromWaiRequest r = U.Request
  { U.requestMethod    = fromWaiMethod (Wai.requestMethod r)
  , U.requestTarget    = Wai.rawPathInfo r <> Wai.rawQueryString r
  , U.requestAuthority = Wai.requestHeaderHost r
  , U.requestScheme    = if Wai.isSecure r then U.SchemeHttps else U.SchemeHttp
  , U.requestHeaders   = fromWaiHeaders (Wai.requestHeaders r)
  , U.requestBody      = waiBodyToBody (Wai.getRequestBodyChunk r)
  , U.requestVersion   = fromWaiVersion (Wai.httpVersion r)
  , U.requestTrailers  = pure []
  }

------------------------------------------------------------------------
-- Response conversions (server-level, Network.HTTP.Message)
------------------------------------------------------------------------

-- | Convert a wireform unified 'U.Response' into a WAI 'Wai.Response'.
toWaiResponse :: U.Response -> Wai.Response
toWaiResponse r =
  let status = toWaiStatus (U.responseStatus r)
      hdrs   = toWaiHeaders (U.responseHeaders r)
  in case U.responseBody r of
    U.BodyEmpty     -> Wai.responseLBS status hdrs LBS.empty
    U.BodyBytes bs  -> Wai.responseLBS status hdrs (LBS.fromStrict bs)
    U.BodyStream p  -> Wai.responseStream status hdrs $ \write flush -> do
      let loop = do
            mb <- p
            case mb of
              Nothing -> flush
              Just chunk -> do
                write (byteString chunk)
                loop
      loop

-- | Convert a WAI 'Wai.Response' into a wireform unified 'U.Response'.
--
-- Uses 'Wai.responseToStream' to normalise the WAI response. The
-- entire body is materialised into memory as a strict 'ByteString'.
fromWaiResponse :: Wai.Response -> IO U.Response
fromWaiResponse resp = do
  let (status, hdrs, withBody) = Wai.responseToStream resp
  body <- collectStreamingBody withBody
  pure U.Response
    { U.responseStatus     = fromWaiStatus status
    , U.responseVersion    = U.HTTP1_1
    , U.responseHeaders    = fromWaiHeaders hdrs
    , U.responseBody       = if BS.null body then U.BodyEmpty else U.BodyBytes body
    , U.responseTrailers   = pure []
    , U.responseH2StreamId = 0
    , U.responseCancel     = pure ()
    }

collectStreamingBody :: ((WaiI.StreamingBody -> IO ByteString) -> IO ByteString) -> IO ByteString
collectStreamingBody withBody = withBody $ \streamBody -> do
  chunksRef <- newIORef []
  streamBody
    (\builder -> do
      let bs = LBS.toStrict (toLazyByteString builder)
      chunks <- readIORef chunksRef
      writeIORef chunksRef (chunks <> [bs]))
    (pure ())
  chunks <- readIORef chunksRef
  pure (BS.concat chunks)

------------------------------------------------------------------------
-- Server-level adapters
------------------------------------------------------------------------

-- | Convert a WAI 'Wai.Application' into a wireform-http 'Handler'.
--
-- The resulting handler can be plugged into 'Network.HTTP.Server.ServerConfig'
-- or called directly in tests.
waiToHandler :: Wai.Application -> (U.Request -> IO U.Response)
waiToHandler app req = do
  waiReq <- toWaiRequest req
  fromWaiResponseCPS (app waiReq)

-- | Convert a wireform-http 'Handler' into a WAI 'Wai.Application'.
--
-- Useful for serving wireform-http handlers through Warp or any other
-- WAI-compatible server.
handlerToWai :: (U.Request -> IO U.Response) -> Wai.Application
handlerToWai handler waiReq respond = do
  let req = fromWaiRequest waiReq
  resp <- handler req
  respond (toWaiResponse resp)

------------------------------------------------------------------------
-- Client-level adapter: in-process Transport
------------------------------------------------------------------------

-- | Build a wireform-http 'CT.Transport' that dispatches requests to a
-- WAI 'Wai.Application' in-process, without any TCP connection.
--
-- This is the primary entry point for testing existing WAI apps with
-- wireform-http's client infrastructure (middleware stack, matchers,
-- assertions, VCR, etc.).
waiTransport :: Wai.Application -> CT.Transport IO
waiTransport app = CT.Transport $ \clientReq -> do
  waiReq <- clientRequestToWai clientReq
  fromWaiResponseToRaw (app waiReq)

------------------------------------------------------------------------
-- Client request → WAI request
------------------------------------------------------------------------

clientRequestToWai :: CR.Request CBS.BodyStream -> IO Wai.Request
clientRequestToWai req = do
  let renderedURI = URI.requestURIToText (CR.requestURI req)
      (rawPath, rawQS) = splitTarget (TE.encodeUtf8 renderedURI)
      waiHdrs = toWaiHeaders (CR.headers req)
      hostHdr = U.lookupHeader U.hHost (CR.headers req)
  bodyReader <- bodyStreamToWaiReader (CR.body req)
  let bodyLen = case CBS.knownSize (CR.body req) of
        Just n  -> Wai.KnownLength (fromIntegral n)
        Nothing -> Wai.ChunkedBody
  pure $ Wai.setRequestBodyChunks bodyReader $ Wai.mapRequestHeaders (const waiHdrs)
    Wai.defaultRequest
      { WaiI.requestMethod = U.fromMethod (CR.method req)
      , WaiI.httpVersion = WAIHttp.http11
      , WaiI.rawPathInfo = rawPath
      , WaiI.rawQueryString = rawQS
      , WaiI.isSecure = False
      , WaiI.remoteHost = NS.SockAddrInet 0 0
      , WaiI.pathInfo = decodePath rawPath
      , WaiI.queryString = WAIHttp.parseQuery rawQS
      , WaiI.vault = Vault.empty
      , WaiI.requestBodyLength = bodyLen
      , WaiI.requestHeaderHost = hostHdr
      , WaiI.requestHeaderRange = U.lookupHeader U.hRange (CR.headers req)
      , WaiI.requestHeaderReferer = lookupCI "Referer" (CR.headers req)
      , WaiI.requestHeaderUserAgent = U.lookupHeader U.hUserAgent (CR.headers req)
      }

bodyStreamToWaiReader :: CBS.BodyStream -> IO (IO ByteString)
bodyStreamToWaiReader stream = pure (CBS.pull stream)

------------------------------------------------------------------------
-- WAI response → wireform RawResponse (for Transport)
------------------------------------------------------------------------

fromWaiResponseToRaw :: ((Wai.Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO CResp.RawResponse
fromWaiResponseToRaw withRespond = do
  resultRef <- newIORef (error "WAI application did not call respond")
  _ <- withRespond $ \waiResp -> do
    let (status, hdrs, withBody) = Wai.responseToStream waiResp
    body <- collectStreamingBody withBody
    popper <- CBS.popperFromStrict body
    let raw = CResp.RawResponse
          { CResp.statusCode   = fromWaiStatus status
          , CResp.headers      = fromWaiHeaders hdrs
          , CResp.bodyPopper   = popper
          , CResp.protocolInfo = P.HTTP1_1
          }
    writeIORef resultRef raw
    pure ResponseReceived
  readIORef resultRef

------------------------------------------------------------------------
-- WAI response CPS helper (for waiToHandler)
------------------------------------------------------------------------

fromWaiResponseCPS :: ((Wai.Response -> IO ResponseReceived) -> IO ResponseReceived) -> IO U.Response
fromWaiResponseCPS withRespond = do
  resultRef <- newIORef (error "WAI application did not call respond")
  _ <- withRespond $ \waiResp -> do
    r <- fromWaiResponse waiResp
    writeIORef resultRef r
    pure ResponseReceived
  readIORef resultRef

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing  y = y

lookupCI :: ByteString -> U.Headers -> Maybe ByteString
lookupCI name = U.lookupHeader (CI.mk name)
