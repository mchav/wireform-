{- | Content-encoding handling, with an open typeclass-based
registry that mirrors the content-type system in
"Network.HTTP.Client.Media".

A content-encoding tag is a phantom type that has a
'HasContentEncoding' instance projecting it onto a header token
(@gzip@, @br@, @deflate@, ...). Adding a new encoding is just
declaring a tag, a 'HasContentEncoding' instance, and a
'Compress' / 'Decompress' instance:

@
data Zstd

instance HasContentEncoding Zstd where
  contentEncodingToken = \"zstd\"

instance Decompress Zstd where
  decompressBytes = Zstd.decompress     -- whatever your binding is

instance Compress Zstd where
  compressBytes = Zstd.compress
@

The middleware in this module doesn't dispatch on a closed sum.
Instead it takes a /list of erased handlers/ ('EncodingHandler')
built via 'asDecompressor'. Adding a new encoding to the wire is
@asDecompressor \@MyTag : defaultDecompressors@; replacing the
shipped set entirely is a fresh list.

Three handlers ship in 'defaultDecompressors':

* __Brotli__ (@br@), via the @brotli@ Haskell binding to
  libbrotlidec.
* __Gzip__ (@gzip@), via @zlib@'s @Codec.Compression.GZip@.
* __Deflate__ (@deflate@), via @zlib@'s @Codec.Compression.Zlib@.
  Servers in the wild send either RFC 1950 zlib-framed deflate or
  RFC 1951 raw deflate under @Content-Encoding: deflate@; we try
  zlib first and transparently fall back to raw when that throws.

'withDecompression' is enabled by default in
'Network.HTTP.Client.Config.withClient', sitting close to the base
transport so retries and tracing see the original status codes but
operate on already-decompressed bodies.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Client.Compression
  ( -- * Tags and classes
    HasContentEncoding (..)
  , Decompress (..)
  , Compress (..)
    -- ** Shipped tags
  , Identity
  , Brotli
  , Gzip
  , Deflate
    -- * Type-erased handlers
  , EncodingHandler (..)
  , asDecompressor
  , asCompressor
  , defaultDecompressors
    -- * Middleware
  , withDecompression
  , withDecompressionPolicy
    -- * Header helpers
  , parseContentEncoding
  , renderAcceptEncoding
  ) where

import Control.Exception (SomeException, try, evaluate)
import qualified Codec.Compression.Brotli as Brotli
import qualified Codec.Compression.GZip   as GZip
import qualified Codec.Compression.Zlib   as Zlib
import qualified Codec.Compression.Zlib.Raw as ZlibRaw
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.CaseInsensitive as CI
import qualified Data.List as List

import FlatParse.Basic (Result (..), runParser)
import qualified Mason.Builder as M

import qualified Network.HTTP.ContentCoding as Hermes
import qualified Network.HTTP.Headers.HeaderFieldName as Hermes

import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.BodyStream
import qualified Network.HTTP.Client.Request as WReq
import qualified Network.HTTP.Client.Response as Resp
import Network.HTTP.Client.Transport

-- ---------------------------------------------------------------------------
-- Open typeclass machinery
-- ---------------------------------------------------------------------------

-- | Associate a content-encoding tag with a hermes 'ContentCoding'
-- value (the IANA-registered token, parsed and dispatched on as a
-- closed sum with a 'Custom' escape hatch for non-registered names).
class HasContentEncoding tag where
  contentEncoding :: Hermes.ContentCoding

-- | The on-the-wire token for an encoding tag — convenience
-- projection through 'contentEncoding' and 'Mason.Builder'.
contentEncodingToken :: forall tag. HasContentEncoding tag => ByteString
contentEncodingToken = renderCoding (contentEncoding @tag)

-- | Decompression for an encoding tag. Implementations are expected
-- to be deterministic; exceptions propagate to the caller as
-- transport errors.
class HasContentEncoding tag => Decompress tag where
  decompressBytes :: ByteString -> IO ByteString

-- | Compression for an encoding tag. Used for callers that want to
-- send compressed request bodies; not wired into the default
-- middleware stack.
class HasContentEncoding tag => Compress tag where
  compressBytes :: ByteString -> IO ByteString

-- ---------------------------------------------------------------------------
-- Shipped tags
-- ---------------------------------------------------------------------------

-- | The no-op encoding. @Content-Encoding: identity@.
data Identity

instance HasContentEncoding Identity where
  contentEncoding = Hermes.Identity

instance Decompress Identity where decompressBytes = pure
instance Compress   Identity where compressBytes   = pure

-- | Brotli (@br@).
data Brotli

instance HasContentEncoding Brotli where
  contentEncoding = Hermes.Brotli

instance Decompress Brotli where
  decompressBytes bs =
    evaluate (BSL.toStrict (Brotli.decompress (BSL.fromStrict bs)))

instance Compress Brotli where
  compressBytes bs =
    evaluate (BSL.toStrict (Brotli.compress   (BSL.fromStrict bs)))

-- | Gzip (@gzip@), via @zlib@.
data Gzip

instance HasContentEncoding Gzip where
  contentEncoding = Hermes.GZip

instance Decompress Gzip where
  decompressBytes bs =
    evaluate (BSL.toStrict (GZip.decompress (BSL.fromStrict bs)))

instance Compress Gzip where
  compressBytes bs =
    evaluate (BSL.toStrict (GZip.compress   (BSL.fromStrict bs)))

-- | Deflate (@deflate@) — historically ambiguous between RFC 1950
-- zlib-wrapped and RFC 1951 raw. 'decompressBytes' tries the zlib
-- framing first and falls back to raw deflate when that fails.
-- 'compressBytes' always emits the zlib framing.
data Deflate

instance HasContentEncoding Deflate where
  contentEncoding = Hermes.Deflate

instance Decompress Deflate where
  decompressBytes bs = do
    result <- try (evaluate (BSL.toStrict (Zlib.decompress (BSL.fromStrict bs))))
    case (result :: Either SomeException ByteString) of
      Right out -> pure out
      Left _    -> evaluate (BSL.toStrict (ZlibRaw.decompress (BSL.fromStrict bs)))

instance Compress Deflate where
  compressBytes bs =
    evaluate (BSL.toStrict (Zlib.compress (BSL.fromStrict bs)))

-- ---------------------------------------------------------------------------
-- Type-erased handlers
-- ---------------------------------------------------------------------------

-- | A type-erased decompressor: the encoding identity plus an action
-- that turns compressed bytes into plain bytes.
-- 'EncodingHandler's are what the middleware actually dispatches
-- on. Build them from a tag via 'asDecompressor' \/ 'asCompressor'.
data EncodingHandler = EncodingHandler
  { ehCoding :: !Hermes.ContentCoding
  , ehRun    :: !(ByteString -> IO ByteString)
  }

-- | The wire token for a handler's encoding, rendered via hermes.
ehToken :: EncodingHandler -> ByteString
ehToken h = renderCoding (ehCoding h)

-- | Render a 'ContentCoding' to its wire bytes through mason. Mason's
-- 'Builder' is rank-polymorphic over backends, so a small monomorphic
-- wrapper avoids the point-free type-inference traps that crop up
-- when piping it into 'toStrictByteString'.
renderCoding :: Hermes.ContentCoding -> ByteString
renderCoding c = M.toStrictByteString (Hermes.renderContentCoding c)

-- | Project a 'Decompress' tag into an 'EncodingHandler' suitable
-- for the decompression middleware.
asDecompressor :: forall tag. Decompress tag => EncodingHandler
asDecompressor = EncodingHandler
  { ehCoding = contentEncoding @tag
  , ehRun    = decompressBytes @tag
  }

-- | Project a 'Compress' tag into an 'EncodingHandler' that
-- compresses bytes. Symmetric with 'asDecompressor'; consumers (a
-- request-body compression middleware, say) decide which side they
-- want.
asCompressor :: forall tag. Compress tag => EncodingHandler
asCompressor = EncodingHandler
  { ehCoding = contentEncoding @tag
  , ehRun    = compressBytes @tag
  }

-- | The default decompressor set, in preference order. Brotli first
-- (smallest payloads on the modern web), then gzip (most compatible),
-- then deflate (legacy).
defaultDecompressors :: [EncodingHandler]
defaultDecompressors =
  [ asDecompressor @Brotli
  , asDecompressor @Gzip
  , asDecompressor @Deflate
  ]

-- ---------------------------------------------------------------------------
-- Middleware
-- ---------------------------------------------------------------------------

-- | Decompression middleware using 'defaultDecompressors'. Sets
-- @Accept-Encoding@ on outgoing requests (if the caller hasn't
-- already), dispatches on @Content-Encoding@ on responses, strips
-- @Content-Encoding@ \/ @Content-Length@ after a successful
-- decode, and falls through for unknown or absent encodings.
withDecompression :: Middleware IO
withDecompression = withDecompressionPolicy defaultDecompressors

-- | Same as 'withDecompression' but with a caller-supplied handler
-- set. Pass @[]@ to disable advertising or dispatching entirely;
-- pass @asDecompressor \@MyTag : defaultDecompressors@ to add a new
-- encoding without losing the shipped set.
withDecompressionPolicy :: [EncodingHandler] -> Middleware IO
withDecompressionPolicy handlers inner = Transport $ \req -> do
  let hdrs0 = WReq.headers req
      req'
        | null handlers                       = req
        | H.hasHeader H.hAcceptEncoding hdrs0 = req
        | otherwise = req
            { WReq.headers =
                H.insertHeader H.hAcceptEncoding
                  (renderAcceptEncoding handlers) hdrs0
            }
  raw <- sendRaw inner req'
  case H.lookupHeader hContentEncoding (Resp.headers raw) of
    Nothing  -> pure raw
    Just enc -> case parseContentEncoding enc of
      Nothing     -> pure raw                  -- unknown / multi-step
      Just coding -> case findHandler coding handlers of
        Nothing      -> pure raw
        Just handler -> do
          compressed <- popperBytes (Resp.bodyPopper raw)
          plain      <- ehRun handler compressed
          newPopper  <- popperFromStrict plain
          pure (stripEncoding raw) { Resp.bodyPopper = newPopper }

-- ---------------------------------------------------------------------------
-- Header helpers
-- ---------------------------------------------------------------------------

-- | Parse a @Content-Encoding@ header value to a hermes
-- 'ContentCoding'. Multi-step encodings (e.g. @gzip, br@) return
-- 'Nothing' because correctly applying a sequence means reversing
-- the order, which the middleware would rather punt on than get
-- wrong; the caller-facing result is "leave the body alone".
parseContentEncoding :: ByteString -> Maybe Hermes.ContentCoding
parseContentEncoding raw =
  let trimmed = BS.dropWhile (== 0x20) (BS.dropWhileEnd (== 0x20) raw)
  in case runParser Hermes.contentCodingParser trimmed of
       OK coding leftover | BS.null leftover -> Just coding
       _                                     -> Nothing

-- | Render a preference-ordered handler list as an
-- @Accept-Encoding@ value. Each handler's hermes 'ContentCoding'
-- is rendered through the mason builder and the tokens joined
-- with @\", \"@.
renderAcceptEncoding :: [EncodingHandler] -> ByteString
renderAcceptEncoding = BS.intercalate ", " . map ehToken

findHandler :: Hermes.ContentCoding -> [EncodingHandler] -> Maybe EncodingHandler
findHandler coding = List.find ((== coding) . ehCoding)

-- | 'Content-Encoding' lifted from hermes's interned name into our
-- 'CI ByteString' representation. Header comparison is
-- case-insensitive so the values match @content-encoding@ on the
-- wire after HPACK case folding.
hContentEncoding :: H.HeaderName
hContentEncoding = Hermes.toCIByteString Hermes.hContentEncoding

stripEncoding :: Resp.RawResponse -> Resp.RawResponse
stripEncoding raw = raw
  { Resp.headers =
        H.deleteHeader hContentEncoding
      $ H.deleteHeader H.hContentLength
      $ Resp.headers raw
  }
