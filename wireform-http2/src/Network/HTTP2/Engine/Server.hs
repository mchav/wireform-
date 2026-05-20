{-# LANGUAGE OverloadedStrings #-}
-- | gRPC-friendly HTTP\/2 server engine.
--
-- This module exposes an @http-semantics@-shaped API in the
-- wireform-http2 namespace so wireform-grpc can use it without
-- depending on the upstream @http-semantics@, @http2@, or
-- @http2-tls@ packages.
--
-- The runtime ('run') lives in "Network.HTTP2.Engine.Run.Server"; this
-- module owns the public types and accessors.
module Network.HTTP2.Engine.Server
  ( -- * The server callback
    Server
    -- * Request
  , Request (..)
  , requestMethod
  , requestPath
  , requestAuthority
  , requestScheme
  , requestHeaders
  , requestBodySize
  , getRequestBodyChunk
  , getRequestBodyChunk'
  , getRequestTrailers
    -- * Response
  , Response (..)
  , responseNoBody
  , responseFile
  , responseBuilder
  , responseStreaming
  , responseStreamingIface
  , responseBodySize
  , setResponseTrailersMaker
    -- * Trailers (re-exported from Types)
  , TrailersMaker
  , NextTrailersMaker (..)
  , defaultTrailersMaker
    -- * Auxiliary information
  , Aux (..)
    -- * Push promises (placeholder; never used by gRPC)
  , PushPromise (..)
  , pushPromise
    -- * Streaming body interface
  , OutBodyIface (..)
    -- * Settings / config
  , module Network.HTTP2.Engine.Settings
  , Config (..)
    -- * Type aliases
  , Path
  , Authority
  , Scheme
  , FileSpec (..)
  , FileOffset
  , ByteCount
  , BufferSize
  , PositionReadMaker
  , PositionRead
  , Sentinel (..)
  , defaultPositionReadMaker
    -- * Engine entry point
  , run
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.UTF8 as UTF8
import Data.IORef (readIORef)
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import qualified Network.HTTP.Types as HTTP
import Network.Socket (SockAddr)
import qualified System.TimeManager as TM

import Network.HTTP2.Engine.Settings
import Network.HTTP2.Engine.Types

-- | The handler signature: receive 'Request' + 'Aux', call the
-- continuation with a 'Response' + any 'PushPromise's (always @[]@
-- for gRPC). The continuation must be called exactly once.
type Server = Request -> Aux -> (Response -> [PushPromise] -> IO ()) -> IO ()

-- | Per-request auxiliary information attached at handler invocation.
data Aux = Aux
  { auxTimeHandle :: !TM.Handle
  , auxMySockAddr :: !SockAddr
  , auxPeerSockAddr :: !SockAddr
  }

newtype Request = Request InpObj
  deriving stock (Show)

newtype Response = Response OutObj
  deriving stock (Show)

requestMethod :: Request -> Maybe HTTP.Method
requestMethod (Request req) = lookupToken ":method" (inpObjHeaders req)

requestPath :: Request -> Maybe Path
requestPath (Request req) = lookupToken ":path" (inpObjHeaders req)

requestAuthority :: Request -> Maybe Authority
requestAuthority (Request req) =
  UTF8.toString <$> lookupToken ":authority" (inpObjHeaders req)

requestScheme :: Request -> Maybe Scheme
requestScheme (Request req) = lookupToken ":scheme" (inpObjHeaders req)

requestHeaders :: Request -> TokenHeaderTable
requestHeaders (Request req) = inpObjHeaders req

requestBodySize :: Request -> Maybe Int
requestBodySize (Request req) = inpObjBodySize req

getRequestBodyChunk :: Request -> IO ByteString
getRequestBodyChunk = fmap fst . getRequestBodyChunk'

getRequestBodyChunk' :: Request -> IO (ByteString, Bool)
getRequestBodyChunk' (Request req) = inpObjBody req

getRequestTrailers :: Request -> IO (Maybe TokenHeaderTable)
getRequestTrailers (Request req) = readIORef (inpObjTrailers req)

responseNoBody :: HTTP.Status -> HTTP.ResponseHeaders -> Response
responseNoBody st hdr =
  Response (OutObj (setStatus st hdr) OutBodyNone defaultTrailersMaker)

responseFile :: HTTP.Status -> HTTP.ResponseHeaders -> FileSpec -> Response
responseFile st hdr fs =
  Response (OutObj (setStatus st hdr) (OutBodyFile fs) defaultTrailersMaker)

responseBuilder :: HTTP.Status -> HTTP.ResponseHeaders -> BSB.Builder -> Response
responseBuilder st hdr b =
  Response (OutObj (setStatus st hdr) (OutBodyBuilder b) defaultTrailersMaker)

responseStreaming
  :: HTTP.Status
  -> HTTP.ResponseHeaders
  -> ((BSB.Builder -> IO ()) -> IO () -> IO ())
  -> Response
responseStreaming st hdr body =
  Response (OutObj (setStatus st hdr) (OutBodyStreaming body) defaultTrailersMaker)

responseStreamingIface
  :: HTTP.Status
  -> HTTP.ResponseHeaders
  -> (OutBodyIface -> IO ())
  -> Response
responseStreamingIface st hdr body =
  Response (OutObj (setStatus st hdr) (OutBodyStreamingIface body) defaultTrailersMaker)

responseBodySize :: Response -> Maybe Int
responseBodySize (Response (OutObj _ (OutBodyFile (FileSpec _ _ len)) _)) =
  Just (fromIntegral len)
responseBodySize _ = Nothing

setResponseTrailersMaker :: Response -> TrailersMaker -> Response
setResponseTrailersMaker (Response o) tm = Response o { outObjTrailers = tm }

setStatus :: HTTP.Status -> HTTP.ResponseHeaders -> HTTP.ResponseHeaders
setStatus st hs = (":status", statusBS) : hs
  where statusBS = UTF8.fromString (show (HTTP.statusCode st))

data PushPromise = PushPromise
  { promiseRequestPath :: !ByteString
  , promiseResponse :: !Response
  }

pushPromise :: ByteString -> Response -> Int -> PushPromise
pushPromise path r _ = PushPromise path r

-- | HTTP\/2 server-side per-connection plumbing.
data Config = Config
  { confWriteBuffer :: !(Ptr Word8)
  , confBufferSize :: !BufferSize
  , confSendAll :: !(ByteString -> IO ())
  , confReadN :: !(Int -> IO ByteString)
  , confPositionReadMaker :: !PositionReadMaker
  , confTimeoutManager :: !TM.Manager
  , confMySockAddr :: !SockAddr
  , confPeerSockAddr :: !SockAddr
  }

-- | Run an HTTP\/2 server over the supplied I\/O plumbing.
--
-- TODO This is a placeholder; the full runtime (frame loop, stream
-- state machine, per-stream worker dispatch, half-close, trailers,
-- WINDOW_UPDATE, RST_STREAM cancellation, PING / SETTINGS bookkeeping)
-- is not implemented in this commit. The skeleton is here so
-- wireform-grpc can link against it and the dependency on the
-- @http2@ package can be dropped; a follow-up commit replaces this
-- stub with the working runtime.
run :: ServerConfig -> Config -> Server -> IO ()
run _sc _cfg _server =
  error "Network.HTTP2.Engine.Server.run: runtime not yet implemented"
