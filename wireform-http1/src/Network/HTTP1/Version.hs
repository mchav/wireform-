{- | HTTP protocol version.

We only ship the two HTTP\/1.x dialects (RFC 9112). Anything older
(0.9) is not supported; anything newer (HTTP\/2, HTTP\/3) goes through
@wireform-http2@ \/ a future @wireform-http3@.

Servers MUST accept either 1.0 or 1.1 and SHOULD respond with
'HTTP_1_1' regardless of the request's version (RFC 9112 § 2.5). The
parser preserves what came in so that the application layer can make
that choice.
-}
module Network.HTTP1.Version
  ( Version (..)
  , versionToBytes
  , versionFromBytes
  ) where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)

data Version
  = HTTP_1_0
  | HTTP_1_1
  deriving stock (Eq, Ord, Show, Generic)

instance NFData Version

{-# INLINE versionToBytes #-}
versionToBytes :: Version -> ByteString
versionToBytes HTTP_1_0 = "HTTP/1.0"
versionToBytes HTTP_1_1 = "HTTP/1.1"

-- | Strict parse: requires exactly @HTTP\/1.0@ or @HTTP\/1.1@. Anything
-- else is 'Nothing'.
{-# INLINE versionFromBytes #-}
versionFromBytes :: ByteString -> Maybe Version
versionFromBytes bs
  | bs == "HTTP/1.1" = Just HTTP_1_1
  | bs == "HTTP/1.0" = Just HTTP_1_0
  | otherwise        = Nothing
