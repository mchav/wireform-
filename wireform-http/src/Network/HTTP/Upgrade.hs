{- | 101 Switching Protocols escape hatch (RFC 9110 §15.2.2,
RFC 9110 §7.8).

The HTTP-1.1 @Upgrade@ \/ 101 dance is how WebSocket and any
other post-HTTP byte protocol gets onto an HTTP connection. The
wireform unified API doesn't run those protocols itself — they're
upstream — but it ships the handshake substrate here so a
WebSocket client \/ server can ride on a wireform connection.

Two pieces:

* 'sendUpgrade' — client side. Send an HTTP\/1.1 request with the
  caller-supplied @Upgrade@ header and other handshake bits, and
  on @101@ hand back the live raw bytes of the post-handshake
  stream.
* 'acceptUpgrade' — server side. Parse the request, hand the
  caller the parsed request-line + headers (plus the raw block
  for hashing purposes — WebSocket needs the verbatim
  @Sec-WebSocket-Key@), and yield the post-handshake byte
  stream.

Both functions are 'NS.Socket'-shaped because the upgraded
protocol is, by definition, no longer HTTP. They live alongside
but orthogonal to the high-level @Transport@ \/ @Handler@
machinery, which only knows about HTTP request \/ response
shapes.

This module is deliberately minimal: it doesn't speak WebSocket,
SPDY, or anything else. It exposes the byte stream and lets a
downstream library do the rest.

== What this module enforces

Per RFC 9110 §7.8, an @Upgrade@ request MUST also send
@Connection: upgrade@ and an @Upgrade: \<token\>@. A @101@
response MUST echo @Upgrade: \<token\>@ and SHOULD include
@Connection: upgrade@. 'sendUpgrade' rejects requests that omit
either header and rejects 101 responses whose @Upgrade@ doesn't
include the protocol the client asked for. 'readHeaderBlock'
throws 'UpgradeHeaderBlockTooLarge' instead of silently
returning a partial block.
-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Upgrade
  ( -- * Client side
    UpgradeRequest (..)
  , sendUpgrade
  , UpgradeError (..)
    -- * Server side
  , UpgradeAccepted (..)
  , acceptUpgrade
    -- * Parsing helpers (exposed for downstream tests)
  , parseRequestHead
  , parseResponseHead
  , RequestHead (..)
  , ResponseHead (..)
  ) where

import Control.Exception (Exception, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

import qualified Network.HTTP.Types.Header as H

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Cap on the header block size, both directions. RFC 9110 has
-- no normative limit but every real implementation has one;
-- 64 KiB is what the existing nginx \/ Apache defaults pick.
headerBlockCap :: Int
headerBlockCap = 64 * 1024

-- ---------------------------------------------------------------------------
-- Client side
-- ---------------------------------------------------------------------------

data UpgradeRequest = UpgradeRequest
  { upgradeMethod   :: !ByteString
    -- ^ Usually @\"GET\"@.
  , upgradeTarget   :: !ByteString
    -- ^ Request target (path \/ query).
  , upgradeHost     :: !ByteString
    -- ^ Value for the @Host@ header.
  , upgradeHeaders  :: !H.Headers
    -- ^ Headers to send. The helper validates that
    --   @Connection: upgrade@ and @Upgrade: \<token\>@ are
    --   present and rejects the request before going on the
    --   wire if they aren't.
  }

data UpgradeError
  = UpgradeMalformedStatus !ByteString
    -- ^ The response's status line didn't parse as
    --   @HTTP\/x.y \<code\> [reason]@.
  | UpgradeRejected !Int !ByteString
    -- ^ Server returned something other than 101.
  | UpgradeMissingConnectionUpgrade
    -- ^ Request did not include @Connection: upgrade@.
  | UpgradeMissingUpgradeToken
    -- ^ Request did not include an @Upgrade: \<token\>@ header.
  | UpgradeResponseMissingConnectionUpgrade !ByteString
    -- ^ 101 response missing @Connection: upgrade@. Carries
    --   the response head bytes for diagnostics.
  | UpgradeResponseMissingUpgradeToken !ByteString
    -- ^ 101 response did not echo an @Upgrade: \<token\>@.
  | UpgradeResponseProtocolMismatch !ByteString !ByteString
    -- ^ 101 response @Upgrade@ token doesn't match anything
    --   the client requested. First argument is what we asked
    --   for (comma-joined), second is what the server said.
  | UpgradeHeaderBlockTooLarge
    -- ^ Header block exceeded 'headerBlockCap' (64 KiB) without
    --   a @\\r\\n\\r\\n@ terminator. Almost always indicates a
    --   malformed or hostile peer.
  | UpgradeHeaderBlockTruncated
    -- ^ Peer closed the connection before sending the full
    --   header block.
  deriving stock (Show)

instance Exception UpgradeError

-- | Send an HTTP\/1.1 request with @Upgrade:@ over the supplied
-- socket. Reads the response header block; on @101@ returns the
-- socket plus any bytes already buffered after the header
-- terminator (callers should treat those as the first frame).
--
-- Validates that the request carries @Connection: upgrade@ and
-- @Upgrade: \<token\>@ (RFC 9110 §7.8); rejects the request
-- before the network with 'UpgradeMissingConnectionUpgrade' or
-- 'UpgradeMissingUpgradeToken' if either is missing. Validates
-- the 101 response in the same way and additionally checks that
-- the server's @Upgrade@ token matches one the client requested,
-- throwing 'UpgradeResponseProtocolMismatch' otherwise. Any
-- non-101 status throws 'UpgradeRejected'.
sendUpgrade :: NS.Socket -> UpgradeRequest -> IO (NS.Socket, ByteString)
sendUpgrade sock req = do
  -- Request-side validation.
  requestedProtos <- case lookupUpgradeTokens (upgradeHeaders req) of
    []  -> throwIO UpgradeMissingUpgradeToken
    xs  -> pure xs
  unless_ (hasConnectionUpgrade (upgradeHeaders req)) $
    throwIO UpgradeMissingConnectionUpgrade
  let reqBytes = BS.concat
        [ upgradeMethod req, " "
        , upgradeTarget req, " HTTP/1.1\r\n"
        , "Host: ", upgradeHost req, "\r\n"
        , renderHeaders (upgradeHeaders req)
        , "\r\n"
        ]
  NSB.sendAll sock reqBytes
  block <- readHeaderBlock sock BS.empty
  let (head_, rest) = splitHeaderBlock block
  ResponseHead { rhStatus = code, rhHeaders = respHdrs } <-
    case parseResponseHead head_ of
      Just r  -> pure r
      Nothing -> throwIO (UpgradeMalformedStatus head_)
  case code of
    101 -> do
      -- Response-side validation (RFC 9110 §7.8).
      unless_ (hasConnectionUpgrade respHdrs) $
        throwIO (UpgradeResponseMissingConnectionUpgrade head_)
      respProtos <- case lookupUpgradeTokens respHdrs of
        []  -> throwIO (UpgradeResponseMissingUpgradeToken head_)
        xs  -> pure xs
      unless_ (any (`elem` requestedProtos) respProtos) $
        throwIO
          (UpgradeResponseProtocolMismatch
             (BS.intercalate ", " requestedProtos)
             (BS.intercalate ", " respProtos))
      pure (sock, rest)
    _   -> throwIO (UpgradeRejected code head_)

-- ---------------------------------------------------------------------------
-- Server side
-- ---------------------------------------------------------------------------

-- | Server-side counterpart. Reads the upcoming request's header
-- block from @sock@, parses the request line + header list, and
-- returns both the structured 'RequestHead' and the raw bytes of
-- the head block (needed e.g. for WebSocket's
-- @Sec-WebSocket-Accept@ computation, which hashes the original
-- @Sec-WebSocket-Key@ verbatim).
--
-- The caller decides whether to accept the upgrade and, if so,
-- writes the @101 Switching Protocols@ response and any
-- handshake bytes into the supplied 'NS.Socket' itself; this
-- helper does not.
acceptUpgrade :: NS.Socket -> IO UpgradeAccepted
acceptUpgrade sock = do
  block <- readHeaderBlock sock BS.empty
  let (head_, leftover) = splitHeaderBlock block
  pure UpgradeAccepted
    { uaRequestHead = head_
    , uaParsed      = parseRequestHead head_
    , uaPrebuffered = leftover
    , uaSocket      = sock
    }

data UpgradeAccepted = UpgradeAccepted
  { uaRequestHead :: !ByteString
    -- ^ Raw request-head block, without the trailing @\\r\\n@.
    --   Preserved verbatim because some upgraded protocols (e.g.
    --   WebSocket) hash specific original bytes.
  , uaParsed      :: !(Maybe RequestHead)
    -- ^ Parsed request line + headers, or 'Nothing' if the bytes
    --   don't parse as a syntactic HTTP\/1.x request head.
  , uaPrebuffered :: !ByteString
    -- ^ Any bytes already received past the header terminator.
    --   Treat as the first frame.
  , uaSocket      :: !NS.Socket
  }

-- ---------------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------------

-- | A parsed HTTP\/1.x request head.
data RequestHead = RequestHead
  { reqMethod   :: !ByteString
  , reqTarget   :: !ByteString
  , reqVersion  :: !ByteString
  , reqHeaders  :: !H.Headers
  }
  deriving stock (Show)

-- | A parsed HTTP\/1.x response head.
data ResponseHead = ResponseHead
  { rhVersion :: !ByteString
  , rhStatus  :: !Int
  , rhReason  :: !ByteString
  , rhHeaders :: !H.Headers
  }
  deriving stock (Show)

parseRequestHead :: ByteString -> Maybe RequestHead
parseRequestHead bs = do
  (firstLine, rest) <- splitLine bs
  (m, ts, ver)      <- case BS.split 0x20 firstLine of
    [a, b, c] -> Just (a, b, c)
    _         -> Nothing
  hdrs <- parseHeaderLines rest
  pure RequestHead { reqMethod = m, reqTarget = ts, reqVersion = ver, reqHeaders = hdrs }

parseResponseHead :: ByteString -> Maybe ResponseHead
parseResponseHead bs = do
  (firstLine, rest) <- splitLine bs
  (ver, code, reason) <- case BS.break (== 0x20) firstLine of
    (v, after) | not (BS.null after) ->
      case BS.break (== 0x20) (BS.drop 1 after) of
        (c, after2) -> Just (v, c, BS.drop 1 after2)
    _ -> Nothing
  codeInt <- case BS8.readInt code of
    Just (n, leftover) | BS.null leftover -> Just n
    _                                     -> Nothing
  hdrs <- parseHeaderLines rest
  pure ResponseHead
    { rhVersion = ver
    , rhStatus  = codeInt
    , rhReason  = reason
    , rhHeaders = hdrs
    }

-- | @\"FIRST\\r\\nREST\"@ → @(\"FIRST\", \"REST\")@. Returns
-- 'Nothing' if there is no @\\r\\n@ in the input.
splitLine :: ByteString -> Maybe (ByteString, ByteString)
splitLine bs = case BS.breakSubstring "\r\n" bs of
  (a, b) | BS.null b -> Nothing
         | otherwise -> Just (a, BS.drop 2 b)

parseHeaderLines :: ByteString -> Maybe H.Headers
parseHeaderLines bs0 = go bs0 []
  where
    go bs acc = case splitLine bs of
      Nothing -> Just (reverse acc)
      Just (line, rest)
        | BS.null line -> Just (reverse acc)
        | otherwise    -> case BS.break (== 0x3A) line of
            (_, t) | BS.null t -> Nothing
            (n, t) ->
              let v = trimOws (BS.drop 1 t)
              in go rest ((CI.mk n, v) : acc)
    trimOws = BS.dropWhile isOws . BS.dropWhileEnd isOws
    isOws w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Wire helpers
-- ---------------------------------------------------------------------------

renderHeaders :: H.Headers -> ByteString
renderHeaders = BS.concat . map line
  where
    line (n, v) = CI.original n <> ": " <> v <> "\r\n"

readHeaderBlock :: NS.Socket -> ByteString -> IO ByteString
readHeaderBlock sock acc
  | "\r\n\r\n" `BS.isInfixOf` acc = pure acc
  | BS.length acc > headerBlockCap = throwIO UpgradeHeaderBlockTooLarge
  | otherwise = do
      chunk <- NSB.recv sock 4096
      if BS.null chunk
        then if "\r\n\r\n" `BS.isInfixOf` acc
               then pure acc
               else throwIO UpgradeHeaderBlockTruncated
        else readHeaderBlock sock (acc <> chunk)

-- | Split @head + \\r\\n\\r\\n + rest@.
splitHeaderBlock :: ByteString -> (ByteString, ByteString)
splitHeaderBlock bs = case BS.breakSubstring "\r\n\r\n" bs of
  (h, t) | BS.null t -> (h, BS.empty)
         | otherwise -> (h, BS.drop 4 t)

-- | Look up @Upgrade@ in the headers and return the
-- comma-separated tokens as a list (case-preserved, OWS-trimmed).
-- An absent or all-OWS value returns @[]@.
lookupUpgradeTokens :: H.Headers -> [ByteString]
lookupUpgradeTokens hdrs =
  let raws = H.lookupHeaders H.hUpgrade hdrs
      trim = BS.dropWhile isOws . BS.dropWhileEnd isOws
      isOws w = w == 0x20 || w == 0x09
      tokens = concatMap (map trim . BS.split 0x2C) raws
  in filter (not . BS.null) tokens

-- | RFC 9110 §7.6.1: @Connection@ is a comma-separated list of
-- option tokens; we accept @upgrade@ (case-insensitive) in any
-- position.
hasConnectionUpgrade :: H.Headers -> Bool
hasConnectionUpgrade hdrs =
  let raws = H.lookupHeaders H.hConnection hdrs
      trim = BS.dropWhile isOws . BS.dropWhileEnd isOws
      isOws w = w == 0x20 || w == 0x09
      tokens = concatMap (map trim . BS.split 0x2C) raws
  in any (\t -> CI.mk t == CI.mk "upgrade") tokens

unless_ :: Bool -> IO () -> IO ()
unless_ b act = if b then pure () else act
