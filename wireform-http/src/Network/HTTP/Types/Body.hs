{- | Unified message bodies for the wireform HTTP API.

This is the same shape as 'Network.HTTP1.Types.Body' minus the
HTTP\/1-specific 'BodyPreEncoded' (precomputed @head + body@ bytes)
and 'BodyFile' (sendfile path) variants — those are not directly
representable on HTTP\/2 because the framing layer needs to break the
payload into DATA frames anyway. Callers that want the HTTP\/1
fast-paths can drop down to "Network.HTTP1.Types" directly.
-}
{-# LANGUAGE LambdaCase #-}
module Network.HTTP.Types.Body
  ( Body (..)
  , noBody
  , byteStringBody
  , streamBody
  ) where

import Control.DeepSeq (NFData (..))
import Data.ByteString (ByteString)

data Body
  = BodyEmpty
    -- ^ No body. Encoded as @Content-Length: 0@ for methods that need
    -- an explicit length, no framing header otherwise.
  | BodyBytes !ByteString
    -- ^ A single contiguous payload. The encoder knows its length and
    -- emits @Content-Length: n@.
  | BodyStream !(IO (Maybe ByteString))
    -- ^ A chunked producer; yields chunks until it returns 'Nothing'.
    -- HTTP\/1.1 encodes as @Transfer-Encoding: chunked@; HTTP\/2 splits
    -- across DATA frames.

instance Show Body where
  show BodyEmpty       = "BodyEmpty"
  show (BodyBytes bs)  = "BodyBytes " <> show bs
  show (BodyStream _)  = "BodyStream <IO>"

instance NFData Body where
  rnf = \case
    BodyEmpty     -> ()
    BodyBytes bs  -> rnf bs
    BodyStream _  -> ()

noBody :: Body
noBody = BodyEmpty

byteStringBody :: ByteString -> Body
byteStringBody = BodyBytes

streamBody :: IO (Maybe ByteString) -> Body
streamBody = BodyStream
