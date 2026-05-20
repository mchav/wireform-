{- | Unified request and response records for the wireform HTTP API.

These types are protocol-agnostic: they carry the negotiated
'Version' but the rest of the record looks the same regardless of
whether the underlying wire is HTTP\/1.x or HTTP\/2.

Conversion to and from the version-specific records lives in
"Network.HTTP.Client" / "Network.HTTP.Server"; callers should
generally only need these unified types.
-}
module Network.HTTP.Message
  ( Request (..)
  , Response (..)
  , Scheme (..)
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

import Network.HTTP.Types.Body
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version

-- | URL scheme.  HTTP\/2 carries this as the @:scheme@ pseudo-header;
-- HTTP\/1.x doesn't, but the unified API still tracks it so callers
-- can write protocol-agnostic logic.
data Scheme = SchemeHttp | SchemeHttps
  deriving stock (Eq, Show, Generic)

instance NFData Scheme

-- | A unified HTTP request.
--
-- The @requestTarget@ is the same thing as HTTP\/1.x's request-target
-- (typically @\"/path?query\"@) and HTTP\/2's @:path@. The optional
-- @requestAuthority@ supplies the @Host@ header (HTTP\/1) or
-- @:authority@ pseudo-header (HTTP\/2).
data Request = Request
  { requestMethod    :: !Method
  , requestTarget    :: !ByteString
  , requestAuthority :: !(Maybe ByteString)
  , requestScheme    :: !Scheme
  , requestHeaders   :: !Headers
  , requestBody      :: !Body
  , requestVersion   :: !Version
    -- ^ For an outgoing client request this is the /preferred/ version
    -- (the actual on-wire version comes out of negotiation). For an
    -- incoming server request it is the version that the peer
    -- actually spoke.
  }

instance Show Request where
  show r =
    "Request "
      <> show (requestMethod r)
      <> " "
      <> show (requestTarget r)
      <> " "
      <> show (requestVersion r)
      <> " "
      <> show (requestHeaders r)

instance NFData Request where
  rnf (Request m t a s h b v) =
    rnf m `seq` rnf t `seq` rnf a `seq` rnf s `seq` rnf h `seq` rnf b `seq` rnf v

data Response = Response
  { responseStatus  :: !Status
  , responseVersion :: !Version
  , responseHeaders :: !Headers
  , responseBody    :: !Body
  , responseTrailers :: !(IO Headers)
    -- ^ Trailing headers carried after the body.
    --
    -- HTTP\/1.x trailers arrive as part of the chunked body's
    -- terminator block; HTTP\/2 trailers arrive as a final HEADERS
    -- frame with @END_STREAM@.  The unified API exposes them as an
    -- 'IO' action because, in either case, the trailers aren't
    -- materialised until the body has finished streaming.  Returns
    -- the empty list when the response had no trailers.
    --
    -- Outbound (sending a response): not yet wired through the
    -- unified API; build the version-specific response directly if
    -- you need to emit trailers from a server handler.
  }

instance Show Response where
  show r =
    "Response "
      <> show (responseStatus r)
      <> " "
      <> show (responseVersion r)
      <> " "
      <> show (responseHeaders r)

instance NFData Response where
  rnf (Response s v h b _) = rnf s `seq` rnf v `seq` rnf h `seq` rnf b
