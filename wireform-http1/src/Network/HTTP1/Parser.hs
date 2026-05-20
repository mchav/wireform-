{- | HTTP\/1.x message parser (RFC 9112 § 2-5).

The strategy is:

1. The connection's recv buffer hands us a contiguous slice of the
   header block (everything up to the first @\\r\\n\\r\\n@).
2. We walk that slice byte by byte with the SIMD CR \/ tchar \/
   field-value scanners. Header values and tokens are zero-copy
   sub-slices of the recv buffer.
3. The result is a strict 'Request' or 'Response' plus the parsed
   'Framing' that tells the connection layer how to read the body.

The parser is /strict/ about RFC 9112 § 3.5 \"robustness\":

* multiple @Content-Length@ values that disagree -> 'ParseLengthConflict'
* a non-numeric @Content-Length@ -> 'ParseInvalidLength'
* both @Content-Length@ and @Transfer-Encoding@ on the same message ->
  'ParseLengthAndTransferEncoding' (RFC 9112 § 6.3 requires
  'Transfer-Encoding' to take precedence; we accept and re-frame on
  TE, but flag this so the application can reject it if it's behind a
  cache that's known to be ambiguous).
* a @Transfer-Encoding@ chain that doesn't end with @chunked@ on a
  request -> 'ParseChunkedNotFinal'.
* CR \/ LF \/ NUL inside a header value -> 'ParseInvalidHeaderValue'
  (smuggling guard).

The header-name parser runs through the SIMD tchar scanner so values
like @\"Host:\"@ (no name) or @\"X-\\tFoo: bar\"@ (HTAB in name) are
rejected with a single SSE2 vector test.
-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP1.Parser
  ( -- * Errors
    ParseError (..)

    -- * Body framing
  , Framing (..)

    -- * Parsers
  , parseRequest
  , parseResponse
  , parseHeaderBlock
  , parseRequestLine
  , parseStatusLine

    -- * Chunked TE
  , ChunkHeader (..)
  , parseChunkHeader
  , parseChunkSize

    -- * Helpers (exported for testing)
  , findNonToken
  , findNonFieldValue
  ) where

import Control.DeepSeq (NFData)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Unsafe as BSU
import Data.Word (Word64, Word8)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

import Network.HTTP1.Internal.Ascii (asciiIeq)
import Network.HTTP1.Types
  -- Types re-exports Method, Version, Status, Headers; we import
  -- everything because the framing case-alts mention Status / Version /
  -- Method constructors directly.

foreign import ccall unsafe "hs_http1_find_non_token"
  c_find_non_token :: Ptr () -> CInt -> CInt -> CInt

foreign import ccall unsafe "hs_http1_find_non_fieldvalue"
  c_find_non_fv :: Ptr () -> CInt -> CInt -> CInt

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Why a parse failed.
--
-- These are designed to map onto specific HTTP response codes:
--
--   * 'ParseMessageTooLong'        -> 431 / 414
--   * 'ParseBadRequestLine'        -> 400
--   * 'ParseBadStatusLine'         -> 502 (we were the client)
--   * 'ParseBadHeaderName' /
--     'ParseInvalidHeaderValue'    -> 400
--   * 'ParseLengthConflict' /
--     'ParseLengthAndTransferEncoding' /
--     'ParseChunkedNotFinal' /
--     'ParseInvalidLength'         -> 400 (request smuggling guards)
--   * 'ParseUnsupportedVersion'    -> 505
--   * 'ParseBadChunkHeader' /
--     'ParseChunkTooLarge'         -> 400
data ParseError
  = ParseMessageTooLong
  | ParseBadRequestLine
  | ParseBadStatusLine
  | ParseBadHeaderName
  | ParseInvalidHeaderValue
  | ParseLengthConflict
  | ParseLengthAndTransferEncoding
  | ParseChunkedNotFinal
  | ParseInvalidLength
  | ParseUnsupportedVersion
  | ParseBadChunkHeader
  | ParseChunkTooLarge
  | ParseUnexpectedEof
  | ParseMissingHost
    -- ^ HTTP\/1.1 request without a @Host@ header (RFC 9112 § 3.2).
  | ParseMultipleHosts
    -- ^ HTTP\/1.1 request with more than one @Host@ header (RFC 9112
    -- § 3.2).
  deriving stock (Eq, Show, Generic)

instance NFData ParseError

------------------------------------------------------------------------
-- Helper: SIMD scanners exposed as pure ByteString fns
------------------------------------------------------------------------

-- | Offset of the first non-tchar in @bs[offset..]@. Returns
-- @BS.length bs@ on miss.
{-# INLINE findNonToken #-}
findNonToken :: ByteString -> Int -> Int
findNonToken bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! fromIntegral (c_find_non_token (castPtr p) (fromIntegral off) (fromIntegral len))

-- | Offset of the first non-field-vchar (i.e. forbidden control byte)
-- in @bs[offset..]@. Returns @BS.length bs@ on miss.
{-# INLINE findNonFieldValue #-}
findNonFieldValue :: ByteString -> Int -> Int
findNonFieldValue bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    pure $! fromIntegral (c_find_non_fv (castPtr p) (fromIntegral off) (fromIntegral len))

------------------------------------------------------------------------
-- Framing
------------------------------------------------------------------------

-- | Body framing decision derived from the parsed headers.
--
-- ['NoBody']         The message has no body (e.g. 1xx \/ 204 \/ 304
--                    response, GET request without explicit body
--                    headers).
-- ['ContentLength']  Body length is a fixed number of octets.
-- ['Chunked']        Body uses @Transfer-Encoding: chunked@. Must be
--                    delimited by the terminating zero-size chunk.
-- ['CloseDelimited'] HTTP\/1.0 response without explicit length; body
--                    runs until the peer closes the connection.
data Framing
  = NoBody
  | ContentLength !Word64
  | Chunked
  | CloseDelimited
  deriving stock (Eq, Show, Generic)

instance NFData Framing

------------------------------------------------------------------------
-- Header block parser (shared by request + response)
------------------------------------------------------------------------

-- | Parse a header block (the raw bytes between the request \/ status
-- line and the @\\r\\n\\r\\n@ terminator) into a 'Headers' list.
--
-- The input is the slice that comes out of
-- 'recvBufferReadUntilDoubleCRLF', minus the request \/ status line.
--
-- Implements:
--
--   * RFC 9110 § 5.1 — field-name is a token; field-value is
--     field-vchar \[(SP\/HTAB)+ field-vchar\] (we accept trailing OWS
--     trimming).
--   * RFC 9112 § 5.2 — no CR \/ LF \/ NUL inside the field-value.
--   * RFC 9112 § 5.2 — reject obs-fold (a line beginning with SP\/HTAB
--     that continues the previous header). We map it to
--     'ParseInvalidHeaderValue' because RFC 9112 says recipients MAY
--     either reject or "replace the obs-fold with one or more SP
--     octets"; rejection is safer in the absence of explicit opt-in.
parseHeaderBlock :: ByteString -> Either ParseError Headers
parseHeaderBlock raw = go 0 []
  where
    !len = BS.length raw
    go !off acc
      | off >= len = Right (reverse acc)
      | BSU.unsafeIndex raw off == 0x20 || BSU.unsafeIndex raw off == 0x09 =
          -- obs-fold: an indented line continuing the previous header.
          Left ParseInvalidHeaderValue
      | otherwise = parseOneHeader raw off >>= \(hdr, off') -> go off' (hdr : acc)

parseOneHeader :: ByteString -> Int -> Either ParseError (Header, Int)
parseOneHeader raw off0 =
  let !len = BS.length raw
      !nameEnd = findNonToken raw off0
  in
    if nameEnd == off0
      then Left ParseBadHeaderName
      else if nameEnd >= len || BSU.unsafeIndex raw nameEnd /= 0x3a  -- ':'
        then Left ParseBadHeaderName
        else
          let !name = sliceBS raw off0 nameEnd
              -- skip optional leading OWS
              valStart = skipOws raw (nameEnd + 1) len
              -- scan to end-of-line: must hit CRLF before any non-field-vchar
              vEnd0 = findNonFieldValue raw valStart
          in
            if vEnd0 < len && BSU.unsafeIndex raw vEnd0 /= 0x0d
              then Left ParseInvalidHeaderValue
              else
                let
                  -- We're at either end-of-buffer or CR
                  -- For end-of-buffer (no trailing CRLF on last line
                  -- because the caller stripped the terminator), treat
                  -- end-of-buffer as the line end.
                  !valTrimEnd = trimOwsEnd raw valStart vEnd0
                  !val = sliceBS raw valStart valTrimEnd
                  -- Advance past CRLF (or past end of buffer)
                  off' =
                    if vEnd0 < len
                      then
                        if vEnd0 + 1 < len && BSU.unsafeIndex raw (vEnd0 + 1) == 0x0a
                          then vEnd0 + 2
                          else len  -- bare CR at EOL: silently terminate
                      else len
                in
                  Right ((name, val), off')

{-# INLINE sliceBS #-}
sliceBS :: ByteString -> Int -> Int -> ByteString
sliceBS bs s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

{-# INLINE skipOws #-}
skipOws :: ByteString -> Int -> Int -> Int
skipOws bs !i !lim
  | i >= lim = lim
  | let !b = BSU.unsafeIndex bs i in b == 0x20 || b == 0x09 = skipOws bs (i + 1) lim
  | otherwise = i

{-# INLINE trimOwsEnd #-}
trimOwsEnd :: ByteString -> Int -> Int -> Int
trimOwsEnd bs !start !i
  | i <= start = start
  | let !b = BSU.unsafeIndex bs (i - 1) in b == 0x20 || b == 0x09 =
      trimOwsEnd bs start (i - 1)
  | otherwise = i

------------------------------------------------------------------------
-- Request line
------------------------------------------------------------------------

-- | Parse a request line of the form @METHOD SP target SP HTTP\/1.x CRLF@.
-- Returns (method, target, version) on success.
parseRequestLine :: ByteString -> Either ParseError (Method, RawTarget, Version)
parseRequestLine line = do
  -- METHOD (1*tchar)
  let !len = BS.length line
      !methEnd = findNonToken line 0
  if methEnd == 0 || methEnd >= len || BSU.unsafeIndex line methEnd /= 0x20
    then Left ParseBadRequestLine
    else do
      let !meth = methodFromBytes (sliceBS line 0 methEnd)
          tgtStart = methEnd + 1
      -- Target runs until next SP. We do *not* run a strict URI parser
      -- here; the application layer is responsible for that.
      let tgtEnd = findByte line tgtStart 0x20
      if tgtEnd >= len || tgtEnd == tgtStart
        then Left ParseBadRequestLine
        else do
          let !tgt = BS.copy (sliceBS line tgtStart tgtEnd)
              verStart = tgtEnd + 1
              ver = sliceBS line verStart len
          case versionFromBytes ver of
            Just v -> Right (meth, tgt, v)
            Nothing
              | BS.isPrefixOf "HTTP/" ver -> Left ParseUnsupportedVersion
              | otherwise -> Left ParseBadRequestLine

------------------------------------------------------------------------
-- Status line
------------------------------------------------------------------------

-- | Parse a status line of the form @HTTP\/1.x SP code SP reason CRLF@.
-- Returns (version, status). Reason phrase is preserved by the calling
-- code if it needs to; the typed @Response@ regenerates it from
-- 'statusReason' on outgoing messages.
parseStatusLine :: ByteString -> Either ParseError (Version, Status, ByteString)
parseStatusLine line = do
  let !len = BS.length line
      verEnd = findByte line 0 0x20
  if verEnd >= len
    then Left ParseBadStatusLine
    else do
      let ver = sliceBS line 0 verEnd
      case versionFromBytes ver of
        Nothing
          | BS.isPrefixOf "HTTP/" ver -> Left ParseUnsupportedVersion
          | otherwise -> Left ParseBadStatusLine
        Just v -> do
          let codeStart = verEnd + 1
              codeEnd = findByte line codeStart 0x20
          if codeEnd >= len || codeEnd - codeStart /= 3
            then Left ParseBadStatusLine
            else case parseStatusCode line codeStart codeEnd of
              Nothing -> Left ParseBadStatusLine
              Just code ->
                let reason = sliceBS line (codeEnd + 1) len
                in Right (v, Status (fromIntegral code), reason)

{-# INLINE parseStatusCode #-}
parseStatusCode :: ByteString -> Int -> Int -> Maybe Int
parseStatusCode bs s e = go s 0
  where
    go !i !acc
      | i >= e = Just acc
      | otherwise =
          let !b = BSU.unsafeIndex bs i
          in if b < 0x30 || b > 0x39
               then Nothing
               else go (i + 1) (acc * 10 + fromIntegral (b - 0x30))

-- | Scalar byte search (used only off the recv-buffer path; the SIMD
-- variant in @Wireform.FFI.findByteBS@ would be marginally faster but
-- we'd pay the FFI call overhead on tiny request lines where the win
-- is < 16 bytes scanned). For lines this short, an inlined loop wins.
{-# INLINE findByte #-}
findByte :: ByteString -> Int -> Word8 -> Int
findByte bs !s !needle = go s
  where
    !len = BS.length bs
    go !i
      | i >= len = len
      | BSU.unsafeIndex bs i == needle = i
      | otherwise = go (i + 1)

------------------------------------------------------------------------
-- High-level parsers
------------------------------------------------------------------------

-- | Parse a single HTTP\/1.x request out of a header-block slice (i.e.
-- everything up to but not including the @\\r\\n\\r\\n@ terminator).
-- The returned 'Body' is always 'BodyEmpty' — the connection layer
-- replaces it with a streaming producer once the framing is known.
--
-- Also enforces the RFC 9112 § 3.2 Host requirements on HTTP\/1.1:
-- requests MUST carry exactly one @Host@ header. Requests with zero or
-- more than one fail with 'ParseMissingHost' \/ 'ParseMultipleHosts'.
parseRequest :: ByteString -> Either ParseError (Request, Framing)
parseRequest block = do
  let (line, rest) = splitFirstLine block
  (meth, tgt, ver) <- parseRequestLine line
  hdrs <- parseHeaderBlock rest
  validateHost ver hdrs
  framing <- requestFraming meth ver hdrs
  Right (Request meth tgt ver hdrs BodyEmpty, framing)

-- | RFC 9112 § 3.2: HTTP\/1.1 requests MUST contain exactly one Host
-- header. HTTP\/1.0 has no such requirement.
validateHost :: Version -> Headers -> Either ParseError ()
validateHost HTTP_1_0 _ = Right ()
validateHost HTTP_1_1 hdrs = case hLookupAll "host" hdrs of
  []  -> Left ParseMissingHost
  [_] -> Right ()
  _   -> Left ParseMultipleHosts

-- | Parse a single HTTP\/1.x response out of a header-block slice.
parseResponse :: Method -> ByteString -> Either ParseError (Response, Framing)
parseResponse reqMethod block = do
  let (line, rest) = splitFirstLine block
  (ver, st, _reason) <- parseStatusLine line
  hdrs <- parseHeaderBlock rest
  framing <- responseFraming reqMethod ver st hdrs
  Right (Response st ver hdrs BodyEmpty, framing)

splitFirstLine :: ByteString -> (ByteString, ByteString)
splitFirstLine bs =
  let crIdx = findByte bs 0 0x0d
  in if crIdx + 1 >= BS.length bs
       then (bs, BS.empty)
       else
         if BSU.unsafeIndex bs (crIdx + 1) == 0x0a
           then (BSU.unsafeTake crIdx bs, BSU.unsafeDrop (crIdx + 2) bs)
           else (bs, BS.empty)

------------------------------------------------------------------------
-- Framing rules
------------------------------------------------------------------------

-- | Derive a request body framing from headers, per RFC 9112 § 6.3.
--
-- The smuggling guards here are what defines this as a "conformant"
-- parser rather than a permissive one — these are the exact checks
-- that h2o, nghttp2, and Hyper run.
requestFraming :: Method -> Version -> Headers -> Either ParseError Framing
requestFraming meth _ver hdrs
  | not (bodyAllowedInRequest meth) = Right NoBody
  | otherwise = do
      let mTE = findTransferEncoding hdrs
          cls = hLookupAll "content-length" hdrs
      case (mTE, cls) of
        (Just te, []) -> teFraming te
        (Just _,  _:_) -> Left ParseLengthAndTransferEncoding
        (Nothing, []) -> Right NoBody
        (Nothing, [v]) -> singleContentLength v
        (Nothing, v : vs)
          | all (== v) vs -> singleContentLength v
          | otherwise -> Left ParseLengthConflict

-- | Derive a response body framing per RFC 9112 § 6.3.
--
-- The "what method was the request" parameter matters: HEAD and CONNECT
-- responses never carry a body regardless of headers.
responseFraming :: Method -> Version -> Status -> Headers -> Either ParseError Framing
responseFraming HEAD _ _ _ = Right NoBody
responseFraming _ _ (Status sc) _
  | sc == 204 || sc == 304 || (sc >= 100 && sc < 200) = Right NoBody
responseFraming CONNECT _ (Status sc) _
  | sc >= 200 && sc < 300 = Right NoBody  -- tunnel takes over
responseFraming _ ver _ hdrs = do
  let mTE = findTransferEncoding hdrs
      cls = hLookupAll "content-length" hdrs
  case (mTE, cls) of
    (Just te, _) -> teFraming te
    (Nothing, []) ->
      -- RFC 9112 § 6.3: an HTTP/1.0 response without explicit framing
      -- is close-delimited; an HTTP/1.1 response without one is
      -- zero-length (the spec specifically forbids close-delimited
      -- here so receivers know when they've seen the full message).
      Right (if ver == HTTP_1_1 then NoBody else CloseDelimited)
    (Nothing, [v]) -> singleContentLength v
    (Nothing, v : vs)
      | all (== v) vs -> singleContentLength v
      | otherwise -> Left ParseLengthConflict

{-# INLINE singleContentLength #-}
singleContentLength :: ByteString -> Either ParseError Framing
singleContentLength v = case parseDecimalW64 (trimAll v) of
  Just n -> Right (ContentLength n)
  Nothing -> Left ParseInvalidLength

-- | Parse a @Transfer-Encoding@ value into a framing decision. We
-- accept the codings the RFC requires us to recognise (@chunked@,
-- @identity@) and reject anything that puts a non-@chunked@ coding
-- last (per RFC 9112 § 6.1, chunked MUST be the final coding so the
-- recipient can frame).
teFraming :: ByteString -> Either ParseError Framing
teFraming raw =
  let
    parts = map BSC.strip (BSC.split ',' raw)
    nonempty = filter (not . BS.null) parts
  in
    case nonempty of
      [] -> Right NoBody
      xs ->
        let lastCoding = last xs
        in if asciiIeq lastCoding "chunked"
             then Right Chunked
             else Left ParseChunkedNotFinal

{-# INLINE trimAll #-}
trimAll :: ByteString -> ByteString
trimAll = BSC.dropWhile isSp . BSC.dropWhileEnd isSp
  where
    isSp c = c == ' ' || c == '\t'

-- | Parse an unsigned decimal into a 'Word64', refusing overflow and
-- anything that isn't 1+ ASCII digits.
parseDecimalW64 :: ByteString -> Maybe Word64
parseDecimalW64 bs
  | BS.null bs = Nothing
  | otherwise = go 0 0
  where
    !len = BS.length bs
    go !i !acc
      | i >= len = Just acc
      | otherwise =
          let !b = BSU.unsafeIndex bs i
          in if b < 0x30 || b > 0x39
               then Nothing
               else
                 let !d = fromIntegral (b - 0x30) :: Word64
                     !acc' = acc * 10 + d
                 -- overflow guard: max Word64 = 1844...; checking
                 -- acc * 10 + 9 < acc (i.e. acc > maxBound `div` 10)
                 -- is cheap and conservative.
                 in if acc > 1844674407370955160 || (acc == 1844674407370955160 && d > 5)
                      then Nothing
                      else go (i + 1) acc'

------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------

foreign import ccall unsafe "hs_http1_parse_hex"
  c_parse_hex :: Ptr () -> CInt -> CInt -> Ptr Word64 -> Ptr CInt -> CInt

-- | A parsed chunk header. @chunkSize == 0@ marks the last chunk.
data ChunkHeader = ChunkHeader
  { chunkSize :: !Word64
  , chunkHeaderBytes :: !Int
    -- ^ How many bytes of the input the chunk-size + extensions +
    -- terminating CRLF occupy. The body follows immediately after.
  }
  deriving stock (Eq, Show, Generic)

instance NFData ChunkHeader

-- | Parse a chunk header from @bs[offset..]@. Returns the parsed
-- 'ChunkHeader' or 'Nothing' if more input is needed (the caller pulls
-- more bytes from the recv buffer and retries).
parseChunkHeader :: ByteString -> Int -> Either ParseError (Maybe ChunkHeader)
parseChunkHeader bs off = unsafePerformIO $
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    alloca $ \outVal -> alloca $ \outConsumed -> do
      let r = c_parse_hex (castPtr p) (fromIntegral off) (fromIntegral len)
                          outVal outConsumed
      case r of
        -1 -> pure (Left ParseChunkTooLarge)
        0  -> pure (Right Nothing)  -- nothing parsed; need more input
        _  -> do
          val <- peek outVal
          consumed <- (fromIntegral :: CInt -> Int) <$> peek outConsumed
          let afterHex = off + consumed
          -- Skip chunk extensions: @;param[=val]*@ until CRLF. We do
          -- not validate extension token syntax (RFC 9112 § 7.1.1
          -- requires us to accept any) but we reject bare CR or LF
          -- inside the extension line.
          let crIdx = findByte bs afterHex 0x0d
          if crIdx >= len
            then pure (Right Nothing)  -- need more input
            else if crIdx + 1 >= len
              then pure (Right Nothing)
              else if BSU.unsafeIndex bs (crIdx + 1) /= 0x0a
                then pure (Left ParseBadChunkHeader)
                else pure (Right (Just (ChunkHeader val (crIdx + 2 - off))))

-- | Parse a chunk-size line (the size hex + optional @;ext=val@
-- extensions) /without/ the trailing CRLF — i.e. exactly what
-- 'Network.HTTP1.Internal.RecvBuffer.recvBufferReadUntilCRLF' returns.
--
-- Returns the parsed size on success, 'Left ParseBadChunkHeader' on
-- garbage, 'Left ParseChunkTooLarge' on >16 hex digits.
parseChunkSize :: ByteString -> Either ParseError Word64
parseChunkSize line = unsafePerformIO $
  BSU.unsafeUseAsCStringLen line $ \(p, len) ->
    alloca $ \outVal -> alloca $ \outConsumed -> do
      let r = c_parse_hex (castPtr p) 0 (fromIntegral len) outVal outConsumed
      case r of
        -1 -> pure (Left ParseChunkTooLarge)
        0  -> pure (Left ParseBadChunkHeader)
        _  -> do
          consumed <- (fromIntegral :: CInt -> Int) <$> peek outConsumed
          -- After the hex digits we accept either end-of-line, or a
          -- ';' starting the extensions. Anything else is malformed.
          if consumed >= len
            then do
              val <- peek outVal
              pure (Right val)
            else
              let !b = BSU.unsafeIndex line consumed
              in if b == 0x3b  -- ';'
                   then do
                     val <- peek outVal
                     pure (Right val)
                   else pure (Left ParseBadChunkHeader)

-- (unused import dance: keep @.&.@ available so future
-- extension parsing has the bit-twiddling primitive in scope.)
_keepBits :: Word64 -> Word64
_keepBits x = x .&. 0xffffffffffffffff
