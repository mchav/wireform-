{- | The high-level wireform HTTP client.

This is an umbrella module: it re-exports the user-facing pieces of
the @Network.HTTP.Wire.*@ tree so most callers only need one
import. The split modules are still available for callers who want
to be precise about their import surface (for example, test code
that needs the mock helpers).

A request is a 'Request' value with a phantom body type. The body
type is converted via 'Body' at the wire boundary. Responses come
back through 'Transport' as 'RawResponse's and are decoded via
'ResponseDecoder's. Middleware is just @'Transport' m -> 'Transport' m@.

@
import Network.HTTP.Wire

main :: IO ()
main = withClient defaultClientConfig \\transport -\> do
  user <- send transport
    (get [uri|https:\/\/api.example.com\/users\/{id}|]
       \`bindVar` ("id" :: Text) `42`)
    (as @JSON @User)
  print (responseBody user)
@
-}
{-# LANGUAGE DuplicateRecordFields #-}
module Network.HTTP.Wire
  ( -- * Body and streaming
    module Network.HTTP.Wire.BodyStream
  , module Network.HTTP.Wire.Body
    -- * Requests and responses
  , module Network.HTTP.Wire.Request
  , module Network.HTTP.Wire.Response
    -- * URIs and base URLs
  , module Network.HTTP.Wire.URI
    -- * Content type system
  , module Network.HTTP.Wire.Media
  , module Network.HTTP.Wire.Media.JSON
  , module Network.HTTP.Wire.Media.OctetStream
  , module Network.HTTP.Wire.Media.PlainText
  , module Network.HTTP.Wire.Media.FormUrlEncoded
  , module Network.HTTP.Wire.Decoder
    -- * Transports and middleware
  , module Network.HTTP.Wire.Transport
  , module Network.HTTP.Wire.Middleware
    -- * Send
  , module Network.HTTP.Wire.Send
    -- * Cookies
  , module Network.HTTP.Wire.Cookies
    -- * Client wiring
  , module Network.HTTP.Wire.Client
  , module Network.HTTP.Wire.Base
    -- * Protocol metadata
  , module Network.HTTP.Wire.Protocol
    -- * Test helpers (mock transports, stubs, request log, assertions)
  , module Network.HTTP.Wire.Test
    -- * VCR (record/replay)
  , module Network.HTTP.Wire.VCR
  ) where

import Network.HTTP.Wire.Base
import Network.HTTP.Wire.Body
import Network.HTTP.Wire.BodyStream
import Network.HTTP.Wire.Client
import Network.HTTP.Wire.Cookies
import Network.HTTP.Wire.Decoder
import Network.HTTP.Wire.Media
import Network.HTTP.Wire.Media.FormUrlEncoded
import Network.HTTP.Wire.Media.JSON
import Network.HTTP.Wire.Media.OctetStream
import Network.HTTP.Wire.Media.PlainText
import Network.HTTP.Wire.Middleware
import Network.HTTP.Wire.Protocol
import Network.HTTP.Wire.Request
import Network.HTTP.Wire.Response
import Network.HTTP.Wire.Send
import Network.HTTP.Wire.Test
import Network.HTTP.Wire.Transport
import Network.HTTP.Wire.URI
import Network.HTTP.Wire.VCR hiding
  ( RecordedRequest (..)
  , RecordedResponse (..)
  )
