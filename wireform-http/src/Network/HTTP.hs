{- | Unified HTTP client and server for wireform.

This is the umbrella module: it re-exports the unified message
types, the version range constraints used for negotiation, and the
top-level client / server entry points.  Drop into the more
specific modules ("Network.HTTP.Client", "Network.HTTP.Server",
"Network.HTTP.VersionRange") if you only need part of the surface.

The vendored type primitives live under @Network.HTTP.Types.*@; they
originated in the @hermes@ library and were rebranded into wireform.
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
    -- * Client \/ Server
  , module Network.HTTP.Client
  , module Network.HTTP.Server
  ) where

import Network.HTTP.Client
import Network.HTTP.Message
import Network.HTTP.Server
import Network.HTTP.Types.Body
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Network.HTTP.VersionRange
