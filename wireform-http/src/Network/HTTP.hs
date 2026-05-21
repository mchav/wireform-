{- | Wireform HTTP — shared message primitives.

This umbrella re-exports the version-agnostic HTTP types
('Network.HTTP.Types.*'), the unified 'Request' \/ 'Response'
shapes, and the version-range vocabulary used to drive negotiation.

For the user-facing client and server APIs, import the dedicated
modules:

* @"Network.HTTP.Client"@ — the high-level, middleware-composable
  client (request-as-value, content-type tags, retry, tracing,
  VCR, mock transports). This is what application code wants.
* @"Network.HTTP.Server"@ — the server side of the unified API.
* @"Network.HTTP.Connection"@ — the low-level single-connection
  bracket that the high-level client sits on top of. Reach for
  this when you need to manage one connection's lifetime by hand
  (e.g. long-lived HTTP\/2 multiplexing where you want explicit
  control of stream concurrency).

The Network.HTTP.Types.* primitives were vendored from the @hermes@
library and rebranded into wireform.
-}
module Network.HTTP
  ( -- * Versions
    module Network.HTTP.Types.Version
  , module Network.HTTP.VersionRange
    -- * Messages
  , module Network.HTTP.Types.Method
  , module Network.HTTP.Types.Status
  , module Network.HTTP.Types.Header
  , module Network.HTTP.Types.Body
  , module Network.HTTP.Message
  ) where

import Network.HTTP.Message
import Network.HTTP.Types.Body
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.HTTP.VersionRange
