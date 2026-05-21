{- | The user-facing 'send' entry point.

'send' wires together the moving pieces: turn the request body into
a 'BodyStream', inject the @Accept@ header from the decoder, hand
off to the transport, drain and decode the response. It produces a
fully decoded 'Response'.

'withResponse' is the streaming counterpart: it gives you the raw
response inside a scoped callback so that the response popper can
be safely drained against the connection's lifetime.

The polymorphism over @m@ uses 'MonadUnliftIO' (from
@unliftio-core@) so that callers inside transformer stacks
(@ReaderT Env IO@, Servant handlers, etc.) can call 'send' directly.
The transport itself remains in 'IO'.
-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Client.Send
  ( send
  , sendIO
  , withResponse
  , sendRawIO
  , prepareRequest
  , RequestException (..)
  ) where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO, withRunInIO)
import qualified Data.ByteString.Char8 as BS8

import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.Body
import Network.HTTP.Client.Decoder
import Network.HTTP.Client.Media
import Network.HTTP.Client.Request
import qualified Network.HTTP.Client.Response as Resp
import Network.HTTP.Client.Response
  (Response (..), RawResponse (statusCode, bodyPopper, protocolInfo))
import Network.HTTP.Client.Transport

-- | Errors thrown by 'send' itself (separate from anything the
-- transport or middleware might throw).
data RequestException
  = DecodeFailure !DecodeError
  deriving stock (Show)

instance Exception RequestException

-- | Send a request, drain the response body, and decode it according
-- to the supplied 'ResponseDecoder'. Throws 'RequestException' on
-- decode failure; transport-level exceptions surface directly.
send
  :: forall m body a.
     ( MonadUnliftIO m, Body body )
  => Transport IO
  -> Request body
  -> ResponseDecoder a
  -> m (Response a)
send transport req decoder = liftIO (sendIO transport req decoder)

-- | 'send' specialised to 'IO'.
sendIO
  :: forall body a.
     Body body
  => Transport IO
  -> Request body
  -> ResponseDecoder a
  -> IO (Response a)
sendIO transport req decoder = do
  raw <- sendRawIO transport req (acceptable decoder)
  bodyBytes <- popperBytes (bodyPopper raw)
  let ct = contentTypeOf (Resp.headers raw)
  case decodeBody decoder (statusCode raw) ct bodyBytes of
    Right a  -> pure Response
      { responseStatus       = statusCode raw
      , responseHeaders      = Resp.headers raw
      , responseBody         = a
      , responseProtocolInfo = protocolInfo raw
      }
    Left err -> throwIO (DecodeFailure err)

-- | Stream the response. The callback is scoped: when it returns
-- (normally or via exception) the response popper is no longer
-- guaranteed valid. The @Accept@ header is taken from the optional
-- decoder argument; pass @[]@ to skip the header entirely.
withResponse
  :: forall m body a.
     ( MonadUnliftIO m, Body body )
  => Transport IO
  -> Request body
  -> [(MediaType, Quality)]
  -> (RawResponse -> m a)
  -> m a
withResponse transport req accept k = withRunInIO $ \run -> do
  raw <- sendRawIO transport req accept
  run (k raw)

-- | Lower-level: prepare the wire request (turning the body into a
-- 'BodyStream', filling in @Content-Length@ and @Accept@) and ship
-- it through the transport. Returns the raw response with the body
-- popper undrained.
sendRawIO
  :: forall body.
     Body body
  => Transport IO
  -> Request body
  -> [(MediaType, Quality)]
  -> IO RawResponse
sendRawIO transport req accept = do
  prepared <- prepareRequest accept req
  sendRaw transport prepared

-- | Convert a @Request body@ into the wire-level @Request BodyStream@
-- that the transport expects, including @Content-Length@ and
-- @Accept@ header bookkeeping. Exposed so user-defined middleware
-- that wants to operate on already-prepared requests can call it
-- directly.
prepareRequest
  :: Body body
  => [(MediaType, Quality)]
  -> Request body
  -> IO (Request BodyStream)
prepareRequest accept req = do
  bs <- toBodyStream (body req)
  let Request { headers = hdrs0 } = req
      hdrs1 = case knownSize bs of
                Just n | not (H.hasHeader H.hContentLength hdrs0) ->
                  H.insertHeader H.hContentLength (BS8.pack (show n)) hdrs0
                _ -> hdrs0
      hdrs2
        | null accept                = hdrs1
        | H.hasHeader H.hAccept hdrs1 = hdrs1
        | otherwise =
            H.insertHeader H.hAccept (acceptHeaderValue accept) hdrs1
  pure req
    { body    = bs
    , headers = hdrs2
    }
