{- | The 'Transport' newtype and 'Middleware' composition.

A 'Transport' is the I\/O boundary: it takes a request whose body
has already been turned into a 'BodyStream' and returns a
'RawResponse'. Everything above the transport — content
negotiation, retry, auth, tracing — is built up from middleware,
which is just @Transport m -> Transport m@.

The middleware layer is parameterised over @m@ but in practice the
base transport requires 'IO'. Middleware that needs to lift through
a transformer stack uses 'MonadUnliftIO' from @unliftio-core@; see
"Network.HTTP.Client.Send" for a sketch.
-}
module Network.HTTP.Client.Transport (
  -- * Transport
  Transport (..),
  unsafeMkTransport,

  -- * Middleware
  Middleware,
  noMiddleware,

  -- * Re-exports
  module Network.HTTP.Client.Response,
) where

import Network.HTTP.Client.BodyStream (BodyStream)
import Network.HTTP.Client.Request (Request)
import Network.HTTP.Client.Response


{- | The wire-level transport. Speaks 'BodyStream' in, 'RawResponse'
out. Always operates on @Request BodyStream@ — middleware that
converts user-facing bodies happens above.
-}
newtype Transport m = Transport
  {sendRaw :: Request BodyStream -> m RawResponse}


{- | The intended way to build a transport: pass any function that
maps a streaming request to a raw response. The constructor of
'Transport' is exported but this name reads better at call sites,
particularly for mocks.

> stub :: Transport IO
> stub = unsafeMkTransport (\\_ -> pure (ok200 "ok"))
-}
unsafeMkTransport :: (Request BodyStream -> m RawResponse) -> Transport m
unsafeMkTransport = Transport


{- | Function composition is the composition law for middleware. The
outermost middleware runs first.
-}
type Middleware m = Transport m -> Transport m


{- | The identity middleware. Useful when you want to build a chain
from a 'Foldable'.
-}
noMiddleware :: Middleware m
noMiddleware = id
