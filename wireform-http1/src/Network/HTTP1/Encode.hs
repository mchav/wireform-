{- | HTTP\/1.x message encoder.

All encoders return a 'Wireform.Builder.Builder' so the caller (client
or server) can chain them with chunked-body fragments and flush them in
one pinned write.

Framing rules (RFC 9112 § 6):

  * The body framing is decided up front and matched to the headers we
    emit.
  * If 'responseBody' is 'BodyEmpty' we emit @Content-Length: 0@ unless
    the status code forbids a body (1xx, 204, 304), in which case we
    omit it.
  * If the body is a 'BodyBytes', we emit @Content-Length: n@.
  * If the body is a 'BodyStream' and we're on HTTP\/1.1, we emit
    @Transfer-Encoding: chunked@. On HTTP\/1.0 (where chunked is not
    available) we omit framing headers; the caller must close the
    connection after streaming.

We avoid double-emitting framing headers: if the application supplied
@Content-Length@ or @Transfer-Encoding@ manually we drop our generated
one and trust theirs (so e.g. a proxy can pass-through an upstream's
framing decision verbatim).
-}
module Network.HTTP1.Encode
  ( -- * Builders
    requestBuilder
  , responseBuilder
  , headersBuilder

    -- * Convenience top-level encoders
  , encodeRequestHead
  , encodeResponseHead

    -- * Misc
  , spaceB
  , crlfB
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified Wireform.Builder as B

import Network.HTTP1.Headers
import Network.HTTP1.Method
import Network.HTTP1.Status
import Network.HTTP1.Types

------------------------------------------------------------------------
-- Public encoders
------------------------------------------------------------------------

-- | Build the wire form of an outgoing request *head* (request line +
-- headers + terminating CRLFCRLF). The body, if any, is emitted by the
-- caller — either as a single 'B.byteString' append for 'BodyBytes' or
-- via 'Network.HTTP1.Chunked.encodeChunk' for streaming.
requestBuilder :: Request -> B.Builder
requestBuilder Request{requestMethod=m, requestTarget=t, requestVersion=v, requestHeaders=h, requestBody=b} =
       B.byteString (methodToBytes m)
    <> spaceB
    <> B.byteString t
    <> spaceB
    <> B.byteString (versionToBytes v)
    <> crlfB
    <> headersBuilder (augmentRequestHeaders m v h b)
    <> crlfB

-- | Build the wire form of an outgoing response head (status line +
-- headers + terminating CRLFCRLF).
responseBuilder :: Response -> B.Builder
responseBuilder Response{responseStatus=s, responseVersion=v, responseHeaders=h, responseBody=b} =
       B.byteString (versionToBytes v)
    <> spaceB
    <> statusCodeBytes s
    <> spaceB
    <> B.byteString (statusReason s)
    <> crlfB
    <> headersBuilder (augmentResponseHeaders s v h b)
    <> crlfB

-- | Just the request head as a strict 'ByteString'.
encodeRequestHead :: Request -> BS.ByteString
encodeRequestHead = B.toStrictByteString . requestBuilder

encodeResponseHead :: Response -> BS.ByteString
encodeResponseHead = B.toStrictByteString . responseBuilder

------------------------------------------------------------------------
-- Header rendering
------------------------------------------------------------------------

-- | Render a header list as @name: value\\r\\n@ lines. We canonicalise
-- the separator to @": "@ (colon-space) but preserve case in field
-- names so user-controlled casing survives the round trip.
headersBuilder :: Headers -> B.Builder
headersBuilder = foldMap emit
  where
    emit (k, v) = B.byteString k <> colonSp <> B.byteString v <> crlfB
    colonSp = B.byteString ": "

------------------------------------------------------------------------
-- Framing-header injection
------------------------------------------------------------------------

-- | If the user did not supply explicit framing headers, append the
-- ones implied by the body shape. Also injects @Host@ for HTTP\/1.1
-- requests if missing (RFC 9112 § 3.2 makes it mandatory; we cannot
-- synthesise it without an authority, so we leave it absent and let
-- the application's request-builder fill it in).
augmentRequestHeaders :: Method -> Version -> Headers -> Body -> Headers
augmentRequestHeaders meth ver hdrs body =
  let
    hasCL = hHas "content-length" hdrs
    hasTE = hHas "transfer-encoding" hdrs
  in
    if hasCL || hasTE
      then hdrs
      else case body of
        BodyEmpty
          | bodyAllowedInRequest meth && shouldAddZeroCL meth ->
              hdrs <> [("Content-Length", "0")]
          | otherwise -> hdrs
        BodyBytes bs ->
          hdrs <> [("Content-Length", decimalBS (toInteger (BS.length bs)))]
        BodyStream _
          | ver == HTTP_1_1 -> hdrs <> [("Transfer-Encoding", "chunked")]
          | otherwise -> hdrs <> [("Connection", "close")]
  where
    shouldAddZeroCL POST = True
    shouldAddZeroCL PUT  = True
    shouldAddZeroCL PATCH = True
    shouldAddZeroCL _ = False

-- | Same idea for responses, with the status code factored in.
augmentResponseHeaders :: Status -> Version -> Headers -> Body -> Headers
augmentResponseHeaders st ver hdrs body
  | bodyForbidden = hdrs
  | hasCL || hasTE = hdrs
  | otherwise = case body of
      BodyEmpty -> hdrs <> [("Content-Length", "0")]
      BodyBytes bs ->
        hdrs <> [("Content-Length", decimalBS (toInteger (BS.length bs)))]
      BodyStream _
        | ver == HTTP_1_1 -> hdrs <> [("Transfer-Encoding", "chunked")]
        | otherwise -> hdrs <> [("Connection", "close")]
  where
    hasCL = hHas "content-length" hdrs
    hasTE = hHas "transfer-encoding" hdrs
    sc = statusCode st
    bodyForbidden =
         (sc >= 100 && sc < 200)
      || sc == 204
      || sc == 304

------------------------------------------------------------------------
-- Small builders
------------------------------------------------------------------------

{-# INLINE spaceB #-}
spaceB :: B.Builder
spaceB = B.word8 0x20

{-# INLINE crlfB #-}
crlfB :: B.Builder
crlfB = B.byteString "\r\n"

-- | Render a status code as 3 ASCII digits. Status codes are bounded
-- by RFC 9110 to 100..599 so this is always exactly 3 bytes; if the
-- caller stuffed a degenerate value in we fall back to general decimal.
statusCodeBytes :: Status -> B.Builder
statusCodeBytes (Status w)
  | w >= 100 && w < 1000 =
         B.word8 (0x30 + fromIntegral (w `div` 100))
      <> B.word8 (0x30 + fromIntegral ((w `div` 10) `mod` 10))
      <> B.word8 (0x30 + fromIntegral (w `mod` 10))
  | otherwise = B.byteString (decimalBS (toInteger w))

-- | Decimal-to-ASCII without going through 'show' (which would round-
-- trip via 'String').
decimalBS :: Integer -> BS.ByteString
decimalBS = BSC.pack . show
-- ^ NB: 'show' on 'Integer' is itself implemented in terms of an
-- internal divmod loop in @base@, not via @reads@, so this is fine for
-- the cold path (Content-Length is small) but we should swap it for a
-- direct decimal-into-builder if it ever shows up in a profile.
