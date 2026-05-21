{- | Response types.

'RawResponse' is the wire-level shape that 'Transport' produces; it
carries the status, headers, popper, and the negotiated protocol
metadata. 'Response' is the value the caller receives after the
chosen 'ResponseBody' has been folded and the chosen
'ResponseDecoder' has decoded the bytes.
-}
module Network.HTTP.Wire.Response
  ( -- * Raw response
    RawResponse (..)
  , rawResponseBytes
    -- * Decoded response
  , Response (..)
  , mapResponse
  ) where

import Data.ByteString (ByteString)

import qualified Network.HTTP.Types.Header as H
import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Wire.BodyStream (Popper, popperBytes)
import Network.HTTP.Wire.Protocol (ProtocolInfo)

-- | A raw wire response. The body is a 'Popper'; the recipient is
-- responsible for draining it before its scoped lifetime ends.
data RawResponse = RawResponse
  { statusCode    :: !S.Status
  , headers       :: !H.Headers
  , bodyPopper    :: !Popper
  , protocolInfo  :: !ProtocolInfo
  }

instance Show RawResponse where
  show r = "RawResponse "
        <> show (statusCode r)
        <> " " <> show (headers r)
        <> " <body>"
        <> " " <> show (protocolInfo r)

-- | Materialise the raw response body to a strict 'ByteString'.
-- Convenience for tests and assertions. After this returns, the
-- popper is exhausted.
rawResponseBytes :: RawResponse -> IO ByteString
rawResponseBytes = popperBytes . bodyPopper

-- | The value 'send' returns. Functor in the body type.
data Response a = Response
  { responseStatus  :: !S.Status
  , responseHeaders :: !H.Headers
  , responseBody    :: !a
  , responseProtocolInfo :: !ProtocolInfo
  }
  deriving stock (Functor)

instance Show a => Show (Response a) where
  show r = "Response "
        <> show (responseStatus r)
        <> " " <> show (responseHeaders r)
        <> " " <> show (responseBody r)
        <> " " <> show (responseProtocolInfo r)

mapResponse :: (a -> b) -> Response a -> Response b
mapResponse = fmap
