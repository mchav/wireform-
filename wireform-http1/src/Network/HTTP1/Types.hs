{- | HTTP message types.

This module re-exports the per-aspect modules ('Method', 'Status',
'Version', 'Headers') and adds the high-level 'Request' \/ 'Response'
records used by the client and server APIs.
-}
module Network.HTTP1.Types
  ( -- * Re-exports
    module Network.HTTP1.Method
  , module Network.HTTP1.Version
  , module Network.HTTP1.Status
  , module Network.HTTP1.Headers

    -- * Message bodies
  , Body (..)
  , noBody
  , byteStringBody
  , streamBody

    -- * Request \/ Response
  , Request (..)
  , Response (..)
  , RawTarget
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)

import Network.HTTP1.Headers
import Network.HTTP1.Method
import Network.HTTP1.Status
import Network.HTTP1.Version

-- | The raw HTTP\/1.x request-target. We do not parse this here:
-- depending on the deployment ('OriginForm' for direct origin servers,
-- 'AbsoluteForm' for forward proxies, 'AuthorityForm' for CONNECT,
-- 'AsteriskForm' for OPTIONS *) the right shape differs. Applications
-- that need a parsed URI should layer a URI library on top.
type RawTarget = ByteString

------------------------------------------------------------------------
-- Body
------------------------------------------------------------------------

-- | A request \/ response body.
--
-- Three variants:
--
--   ['BodyEmpty']   No body. Encoded as @Content-Length: 0@ when an
--                   explicit length is required (POST, PUT, etc.); no
--                   framing header otherwise.
--   ['BodyBytes']   A single contiguous strict 'ByteString'. The
--                   encoder knows its length and emits
--                   @Content-Length: n@.
--   ['BodyStream']  A producer @IO (Maybe ByteString)@ that yields
--                   chunks until it returns 'Nothing'. The encoder
--                   emits @Transfer-Encoding: chunked@ on HTTP\/1.1 and
--                   closes the connection on HTTP\/1.0 (the only legal
--                   way to delimit an unknown-length body there).
data Body
  = BodyEmpty
  | BodyBytes !ByteString
  | BodyStream !(IO (Maybe ByteString))

instance Show Body where
  show BodyEmpty = "BodyEmpty"
  show (BodyBytes bs) = "BodyBytes " <> show bs
  show (BodyStream _) = "BodyStream <IO>"

instance NFData Body where
  rnf BodyEmpty = ()
  rnf (BodyBytes bs) = rnf bs
  rnf (BodyStream _) = ()

noBody :: Body
noBody = BodyEmpty

byteStringBody :: ByteString -> Body
byteStringBody = BodyBytes

streamBody :: IO (Maybe ByteString) -> Body
streamBody = BodyStream

------------------------------------------------------------------------
-- Request / Response
------------------------------------------------------------------------

-- | A parsed (or about-to-be-sent) HTTP request.
--
-- The 'requestBody' is a producer that the request handler can call
-- multiple times. After the first call yields 'Nothing' (end-of-stream),
-- subsequent calls must continue to yield 'Nothing' so that handlers
-- can defensively read more than they need.
data Request = Request
  { requestMethod   :: !Method
  , requestTarget   :: !RawTarget
  , requestVersion  :: !Version
  , requestHeaders  :: !Headers
  , requestBody     :: !Body
    -- ^ For an incoming request this is a 'BodyStream' producer that
    -- reads from the connection's recv buffer. For an outgoing client
    -- request it is whatever the caller supplied.
  }

instance Show Request where
  show r =
    "Request " <> show (requestMethod r) <> " "
              <> show (requestTarget r) <> " "
              <> show (requestVersion r) <> " "
              <> show (requestHeaders r)

instance NFData Request where
  rnf Request{requestMethod=m,requestTarget=t,requestVersion=v,requestHeaders=h,requestBody=b} =
    rnf m `seq` rnf t `seq` rnf v `seq` rnf h `seq` rnf b

data Response = Response
  { responseStatus  :: !Status
  , responseVersion :: !Version
  , responseHeaders :: !Headers
  , responseBody    :: !Body
  }

instance Show Response where
  show r =
    "Response " <> show (responseStatus r) <> " "
                <> show (responseVersion r) <> " "
                <> show (responseHeaders r)

instance NFData Response where
  rnf Response{responseStatus=s,responseVersion=v,responseHeaders=h,responseBody=b} =
    rnf s `seq` rnf v `seq` rnf h `seq` rnf b
