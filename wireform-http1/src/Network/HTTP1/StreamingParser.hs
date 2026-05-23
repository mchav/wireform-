{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}

{- | Streaming HTTP\/1.x message parser built on the wireform
@Stream@ parser surface ('Wireform.Parser') and a magic-ring
'Wireform.Transport'.

The /classic/ HTTP\/1.x reader in this package
('Network.HTTP1.Parser' on top of 'Network.HTTP1.Internal.RecvBuffer')
pulls the whole header block into a contiguous slice and then walks it
with the SIMD scanners.  That works fine but it allocates one
'ByteString' per pull and copies under the hood whenever the block
wraps the recv buffer.

The streaming parser here walks the magic ring directly.  Bytes flow
from the socket into the ring with no userspace copy
('withRecvTransport' / 'withRecvBufTransport' use @recvBuf@); the
parser consumes them via 'anyWord8' / 'satisfyAscii' / 'takeBs' and
suspends on the IO manager when it runs out of data.

The parsed 'Request' \/ 'Response' fields ('Method', 'Headers') are
returned with their bytes copied out so they outlive the magic-ring's
tail advance — the caller can stash them on the heap, hand them to the
application, etc.  Only the body slot stays 'BodyEmpty'; the
connection layer attaches the appropriate framed body producer.
-}
module Network.HTTP1.StreamingParser
  ( -- * Errors
    StreamParseError (..)

    -- * Whole-request \/ whole-response parsers
  , requestHeadParser
  , responseHeadParser

    -- * Building blocks (exported for tests + reuse)
  , requestLineParser
  , statusLineParser
  , headerBlockParser
  , chunkSizeLineParser
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word (Word64, Word8)

import Wireform.Parser
  ( Parser
  , (<|>)
  , anyWord8
  , byteString
  , cut
  , empty
  , err
  , lookahead
  , satisfyAscii
  , skip
  , skipSatisfyAscii
  , takeBs
  , withAnyWord8
  , withSatisfyAscii
  )
import Wireform.Parser.Internal (Stream)

import Network.HTTP1.Headers (Header, Headers)
import Network.HTTP1.Method (Method, methodFromBytes)
import Network.HTTP1.Parser
  ( Framing
  , ParseError (..)
  , requestFraming
  , responseFraming
  )
import qualified Network.HTTP1.Parser as Classic
import Network.HTTP1.Status (Status (..))
import Network.HTTP1.Types
  ( Body (..)
  , RawTarget
  , Request (..)
  , Response (..)
  )
import Network.HTTP1.Version (Version (..))

-- | Wireform-parser-level errors thrown by 'cut'.
--
-- 'StreamParseClassic' is the existing 'Network.HTTP1.Parser.ParseError'
-- vocabulary: anything the structural validators in
-- "Network.HTTP1.Parser" already detect (bad request line, smuggling
-- vector, oversized chunk, missing Host, …).
--
-- 'StreamParseOversize' is raised when a single header line exceeds
-- the parser-supplied cap; it lets a server cut the connection
-- before the ring buffer grows without bound.
data StreamParseError
  = StreamParseClassic !ParseError
  | StreamParseOversize !Int
  deriving stock (Eq, Show)

------------------------------------------------------------------------
-- ASCII / RFC primitives
------------------------------------------------------------------------

-- RFC 9110 § 5.6.2 tchar (token character).
isTchar :: Word8 -> Bool
isTchar w =
     (w >= 0x30 && w <= 0x39)  -- 0-9
  || (w >= 0x41 && w <= 0x5A)  -- A-Z
  || (w >= 0x61 && w <= 0x7A)  -- a-z
  || w == 0x21 || w == 0x23 || w == 0x24 || w == 0x25 || w == 0x26
  || w == 0x27 || w == 0x2A || w == 0x2B || w == 0x2D || w == 0x2E
  || w == 0x5E || w == 0x5F || w == 0x60 || w == 0x7C || w == 0x7E
{-# INLINE isTchar #-}

-- A byte legal /inside/ the field value, including SP / HTAB
-- (which appear as inner whitespace; we trim leading and trailing).
isFieldByte :: Word8 -> Bool
isFieldByte w =
     (w >= 0x21 && w /= 0x7F)   -- field-vchar
  || w == 0x20 || w == 0x09     -- SP / HTAB
{-# INLINE isFieldByte #-}

-- A byte legal in the request target (loose validation: the strict
-- per-method check is delegated to 'Classic.parseRequestLine' via the
-- shared 'Classic.validateTarget'.  We reject only the four bytes
-- that absolutely cannot appear: NUL / SP / CR / LF).
isTargetByte :: Word8 -> Bool
isTargetByte w = w /= 0x00 && w /= 0x20 && w /= 0x0d && w /= 0x0a
{-# INLINE isTargetByte #-}

isHexDigit :: Word8 -> Bool
isHexDigit w =
     (w >= 0x30 && w <= 0x39)
  || (w >= 0x41 && w <= 0x46)
  || (w >= 0x61 && w <= 0x66)
{-# INLINE isHexDigit #-}

fromHexDigit :: Word8 -> Word64
fromHexDigit w
  | w >= 0x30 && w <= 0x39 = fromIntegral (w - 0x30)
  | w >= 0x41 && w <= 0x46 = fromIntegral (w - 0x37)
  | otherwise              = fromIntegral (w - 0x57)
{-# INLINE fromHexDigit #-}

------------------------------------------------------------------------
-- Tokens and bounded slices
------------------------------------------------------------------------

-- | Greedy take of 1+ bytes matching the predicate, with an upper
-- bound to keep a malicious / runaway peer from forcing unbounded
-- ring growth.
--
-- The implementation scans the run length under 'lookahead' (so the
-- cursor does not move) and then materialises the slice with a
-- single 'takeBs n'.  The wireform parser inlines those primitives
-- so the inner loop is a tight bounds-check + pointer bump.  The
-- primary perf win versus the classic byte-array-based parser comes
-- from the zero-copy slice (memory still lives in the ring).
takeWhile1Bounded
  :: Int                              -- ^ hard byte cap
  -> (Word8 -> Bool)                  -- ^ predicate
  -> Parser Stream StreamParseError ByteString
takeWhile1Bounded cap p = do
  n <- lookahead (countMatching cap p)
  if n == 0 then empty else takeBs n
{-# INLINE takeWhile1Bounded #-}

-- Count the run of matching bytes starting at the current position.
-- Consumes the matched bytes; callers that want the scan to be
-- non-destructive wrap this in 'lookahead'.
countMatching
  :: Int
  -> (Word8 -> Bool)
  -> Parser Stream StreamParseError Int
countMatching cap p = go 0
  where
    go !i
      | i >= cap = err (StreamParseOversize cap)
      | otherwise = do
          mw <- peekByte
          case mw of
            Nothing -> pure i
            Just w
              | p w -> skip 1 *> go (i + 1)
              | otherwise -> pure i

peekByte :: Parser Stream e (Maybe Word8)
peekByte = (Just <$> lookahead anyWord8) <|> pure Nothing

------------------------------------------------------------------------
-- CRLF / SP / OWS
------------------------------------------------------------------------

crlf :: Parser Stream StreamParseError ()
crlf = byteString "\r\n"

sp :: Parser Stream StreamParseError ()
sp = skipSatisfyAscii (== ' ')

skipOws :: Parser Stream StreamParseError ()
skipOws = loop
  where
    loop = (skipSatisfyAscii (\c -> c == ' ' || c == '\t') *> loop)
       <|> pure ()

------------------------------------------------------------------------
-- Header block
------------------------------------------------------------------------

-- | Maximum bytes any single header line is allowed to occupy
-- before we trip 'StreamParseOversize'.  Aligned with h2o's
-- 16-KiB default field-line cap.
defaultLineCap :: Int
defaultLineCap = 16 * 1024

-- | Parse a single @name@:@OWS@@value@@OWS@CRLF line.  Returns
-- name + value with bytes copied so the result outlives the next
-- ring-tail advance.
headerLineParser :: Parser Stream StreamParseError Header
headerLineParser = do
  !nameSlice <- takeWhile1Bounded defaultLineCap isTchar
                  `cut` StreamParseClassic ParseBadHeaderName
  skipSatisfyAscii (== ':')
    `cut` StreamParseClassic ParseBadHeaderName
  skipOws
  !valueSlice <- (takeWhile1Bounded defaultLineCap isFieldByte
                    <|> pure BS.empty)
                   `cut` StreamParseClassic ParseInvalidHeaderValue
  let !value = trimOws valueSlice
  -- Forbid CR / LF / NUL inside the trimmed value (smuggling guard;
  -- mirrors 'Classic.parseOneHeader').
  if BS.any forbidden value
    then err (StreamParseClassic ParseInvalidHeaderValue)
    else do
      crlf `cut` StreamParseClassic ParseInvalidHeaderValue
      -- Force the copy NOW; without the bangs the lazy thunks would
      -- still reference the ring's foreign pointer at @withMagicRing@
      -- teardown time and segfault on the next deref.
      let !nameCopy  = BS.copy nameSlice
          !valueCopy = BS.copy value
      pure (nameCopy, valueCopy)
  where
    forbidden b = b == 0x0d || b == 0x0a || b == 0x00

trimOws :: ByteString -> ByteString
trimOws = BSC.dropWhile isSp . BSC.dropWhileEnd isSp
  where
    isSp c = c == ' ' || c == '\t'

-- | Parse a header block terminated by a blank @\\r\\n@.
--
-- Rejects obs-fold (leading SP / HTAB on a continuation line) as
-- 'ParseInvalidHeaderValue' per RFC 9112 § 5.2.
headerBlockParser :: Parser Stream StreamParseError Headers
headerBlockParser = loop []
  where
    loop acc = do
      mw <- peekByte
      case mw of
        Just 0x0d -> do
          crlf `cut` StreamParseClassic ParseInvalidHeaderValue
          pure (reverse acc)
        Just w
          | w == 0x20 || w == 0x09 ->
              err (StreamParseClassic ParseInvalidHeaderValue)
          | otherwise -> do
              hdr <- headerLineParser
              loop (hdr : acc)
        Nothing ->
          err (StreamParseClassic ParseUnexpectedEof)

------------------------------------------------------------------------
-- Request / status lines
------------------------------------------------------------------------

versionParser :: Parser Stream StreamParseError Version
versionParser = do
  byteString "HTTP/"
  major <- satisfyAscii (\c -> c >= '0' && c <= '9')
  _     <- satisfyAscii (== '.')
  minor <- satisfyAscii (\c -> c >= '0' && c <= '9')
  case (major, minor) of
    ('1', '1') -> pure HTTP_1_1
    ('1', '0') -> pure HTTP_1_0
    _          -> err (StreamParseClassic ParseUnsupportedVersion)

-- | Parse @METHOD SP target SP HTTP\/X.Y CRLF@.  Method bytes are
-- folded through 'methodFromBytes' (which already copies for
-- 'MethodOther'); target bytes are copied out.
--
-- Performs the same per-method target validation
-- ('Classic.validateTarget') the existing parser does, so the
-- result is identical modulo zero-copy slice handling.
requestLineParser
  :: Parser Stream StreamParseError (Method, RawTarget, Version)
requestLineParser = do
  !methSlice <- takeWhile1Bounded 64 isTchar
                  `cut` StreamParseClassic ParseBadRequestLine
  sp `cut` StreamParseClassic ParseBadRequestLine
  let !meth = methodFromBytes methSlice
  !tgtSlice <- takeWhile1Bounded (4 * 1024) isTargetByte
                 `cut` StreamParseClassic ParseBadRequestLine
  sp `cut` StreamParseClassic ParseBadRequestLine
  !ver <- versionParser
  crlf `cut` StreamParseClassic ParseBadRequestLine
  let !target = BS.copy tgtSlice
  case Classic.validateTarget meth target of
    Right () -> pure $! (meth, target, ver)
    Left e   -> err (StreamParseClassic e)

-- | Parse @HTTP\/X.Y SP code SP reason CRLF@.
statusLineParser
  :: Parser Stream StreamParseError (Version, Status, ByteString)
statusLineParser = do
  !ver <- versionParser
  sp `cut` StreamParseClassic ParseBadStatusLine
  d0 <- digit
  d1 <- digit
  d2 <- digit
  let !code = d0 * 100 + d1 * 10 + d2
  sp `cut` StreamParseClassic ParseBadStatusLine
  !reasonSlice <- takeWhile1Bounded 1024 isReasonByte
                    <|> pure BS.empty
  crlf `cut` StreamParseClassic ParseBadStatusLine
  let !reasonCopy = BS.copy reasonSlice
  pure $! (ver, Status (fromIntegral code), reasonCopy)
  where
    digit :: Parser Stream StreamParseError Int
    digit = withSatisfyAscii (\c -> c >= '0' && c <= '9')
              (\c -> pure (fromEnum c - fromEnum '0'))
            `cut` StreamParseClassic ParseBadStatusLine
    isReasonByte w = w == 0x09 || (w >= 0x20 && w /= 0x7f)

------------------------------------------------------------------------
-- High-level: request / response head
------------------------------------------------------------------------

-- | Parse a full HTTP\/1.x request /head/ (request line +
-- header block, terminated by @CRLFCRLF@).  Body framing is
-- derived from the headers the same way 'Classic.parseRequest'
-- does, including the Host / target smuggling guards.
--
-- The returned 'Request' carries 'BodyEmpty'; the connection
-- layer attaches the framed body producer using the returned
-- 'Framing'.
requestHeadParser :: Parser Stream StreamParseError (Request, Framing)
requestHeadParser = do
  (meth, tgt, ver) <- requestLineParser
  hdrs <- headerBlockParser
  case Classic.validateHost ver hdrs of
    Left e   -> err (StreamParseClassic e)
    Right () -> pure ()
  case requestFraming meth ver hdrs of
    Left e        -> err (StreamParseClassic e)
    Right framing -> pure
      ( Request meth tgt ver hdrs BodyEmpty (pure [])
      , framing
      )

-- | Parse a full HTTP\/1.x response /head/.  Takes the request
-- method so 'responseFraming' can apply the HEAD / CONNECT
-- special cases.
responseHeadParser
  :: Method
  -> Parser Stream StreamParseError (Response, Framing)
responseHeadParser reqMethod = do
  (ver, st, _reason) <- statusLineParser
  hdrs <- headerBlockParser
  case responseFraming reqMethod ver st hdrs of
    Left e        -> err (StreamParseClassic e)
    Right framing -> pure
      ( Response st ver hdrs BodyEmpty (pure [])
      , framing
      )

------------------------------------------------------------------------
-- Chunked transfer-encoding chunk-size line
------------------------------------------------------------------------

-- | Parse one @chunk-size [ chunk-ext ] CRLF@ line.  Returns the
-- size in bytes (the caller follows with a @takeBs size@ for the
-- chunk-data and another CRLF).
--
-- Hex digits are bounded to 16 (the JVM / nginx default chunk-size
-- cap) and the extension grammar is intentionally permissive: we
-- accept anything in the field-vchar vocabulary plus SP / HTAB
-- inside an extension value, which matches what
-- 'Classic.parseChunkSize' allows.
chunkSizeLineParser :: Parser Stream StreamParseError Word64
chunkSizeLineParser = do
  d0 <- hexDigit `cut` StreamParseClassic ParseBadChunkHeader
  goDigits 1 (fromHexDigit d0)
  where
    hexDigit :: Parser Stream StreamParseError Word8
    hexDigit = withAnyWord8 $ \w ->
      if isHexDigit w then pure w else empty

    goDigits :: Int -> Word64 -> Parser Stream StreamParseError Word64
    goDigits !n !acc
      | n > 16 = err (StreamParseClassic ParseChunkTooLarge)
      | otherwise = do
          mw <- peekByte
          case mw of
            Just w
              | isHexDigit w -> do
                  _ <- anyWord8
                  goDigits (n + 1) (acc * 16 + fromHexDigit w)
            _ -> finishLine acc

    finishLine :: Word64 -> Parser Stream StreamParseError Word64
    finishLine !sz = do
      mw <- peekByte
      case mw of
        Just 0x3b -> do
          _ <- anyWord8
          skipExtensions
          crlf `cut` StreamParseClassic ParseBadChunkHeader
          pure sz
        Just 0x0d -> do
          crlf `cut` StreamParseClassic ParseBadChunkHeader
          pure sz
        Just _ -> err (StreamParseClassic ParseBadChunkHeader)
        Nothing -> err (StreamParseClassic ParseUnexpectedEof)

    skipExtensions :: Parser Stream StreamParseError ()
    skipExtensions = go 0
      where
        go !i
          | i > 4096 = err (StreamParseOversize 4096)
          | otherwise = do
              mw <- peekByte
              case mw of
                Just 0x0d -> pure ()
                Just w
                  | isFieldByte w || w == 0x3b || w == 0x3d || w == 0x22
                      -> skip 1 *> go (i + 1)
                  | otherwise ->
                      err (StreamParseClassic ParseBadChunkHeader)
                Nothing ->
                  err (StreamParseClassic ParseUnexpectedEof)
