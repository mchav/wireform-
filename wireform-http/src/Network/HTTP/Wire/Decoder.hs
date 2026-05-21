{- | Response decoders.

A 'ResponseDecoder' pairs an @Accept@ header (the set of media types
the caller is willing to handle, with @q=@ weights) with a function
that decodes the response body once the server has picked one of
them.

'as' is the most common constructor: given a content-type tag,
build a decoder that asks for exactly that media type and decodes
via the tag's 'Decode' instance. Decoders compose via '<!>' from
@semigroupoids@; for callers that don't want to depend on
@semigroupoids@ we also expose a plain @orDecoder@.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.Wire.Decoder
  ( ResponseDecoder (..)
  , as
  , orDecoder
  , statusAware
  , mapDecoder
  , bytesDecoder
  ) where

import Data.ByteString (ByteString)

import qualified Network.HTTP.Types.Status as S

import Network.HTTP.Wire.Media

-- | Content negotiation + body decoding bundled together.
data ResponseDecoder a = ResponseDecoder
  { acceptable :: ![(MediaType, Quality)]
    -- ^ Media types this decoder will accept, in preference order.
    --   Used to fill in the @Accept@ header.
  , decodeBody :: !(S.Status -> MediaType -> ByteString -> Either DecodeError a)
    -- ^ Decode the body. The status code is available for
    --   status-aware decoders (e.g. dual error\/success bodies); the
    --   media type is the parsed @Content-Type@ of the response.
  }

instance Functor ResponseDecoder where
  fmap f d = d { decodeBody = \s m b -> f <$> decodeBody d s m b }

-- | The standard smart constructor: accept the tag's media type with
-- quality 1.0 and decode the body via the tag's 'Decode' instance.
as :: forall tag a. (HasMediaType tag, Decode tag a) => ResponseDecoder a
as = ResponseDecoder
  { acceptable = [(mediaType @tag, maxQuality)]
  , decodeBody = \_status _ct bs -> decode @tag bs
  }

-- | Decoder combinator. The first decoder's accept list is preferred;
-- at response time, the decoder whose accept list matches the
-- @Content-Type@ wins. Falls back to the second if neither matches
-- (which generally indicates the server ignored our @Accept@).
orDecoder :: ResponseDecoder a -> ResponseDecoder a -> ResponseDecoder a
orDecoder a b = ResponseDecoder
  { acceptable = acceptable a <> acceptable b
  , decodeBody = \status ct bs ->
      if matchesAny ct (map fst (acceptable a))
        then decodeBody a status ct bs
        else decodeBody b status ct bs
  }

-- | Replace the body-decoder while keeping the same accept list. The
-- new decoder sees the parsed status.
statusAware
  :: ResponseDecoder a
  -> (S.Status -> MediaType -> ByteString -> Either DecodeError b)
  -> ResponseDecoder b
statusAware d k = d { decodeBody = k }

mapDecoder :: (a -> b) -> ResponseDecoder a -> ResponseDecoder b
mapDecoder = fmap

-- | Decoder that accepts anything and returns the raw bytes.
bytesDecoder :: ResponseDecoder ByteString
bytesDecoder = ResponseDecoder
  { acceptable = [("*/*", maxQuality)]
  , decodeBody = \_ _ -> Right
  }
