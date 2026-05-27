{- | Range request and partial-content helpers (RFC 9110 §14).

The wire grammar of @Range@ \/ @Content-Range@ \/ @Accept-Ranges@
lives in hermes ("Network.HTTP.Headers.Range",
"Network.HTTP.Headers.ContentRange",
"Network.HTTP.Headers.AcceptRanges"); this module is the
wireform-flavoured wrapper layer:

* The 'ByteRange' ADT mirrors hermes's @bytes-unit@ shape with the
  more ergonomic @from\/to\/suffix@ split that callers usually
  want.
* 'parseRange' \/ 'parseContentRange' delegate to the hermes
  parsers but project into wireform's vocabulary.
* 'parseAcceptRanges' surfaces the server's @Accept-Ranges@
  advertisement.
* 'parseMultipartByteranges' parses the @multipart\/byteranges@
  body shape (RFC 9110 §14.6) that servers use to return multiple
  ranges in a single 206 response.
* 'withRange' attaches a @Range@ header to a request.
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Range
  ( -- * Range request header
    ByteRange (..)
  , Range
  , byteRange
  , byteRangeFrom
  , byteRangeSuffix
  , rangeHeader
  , parseRange
    -- * Content-Range response header
  , ContentRange (..)
  , parseContentRange
  , parseContentRangeFull
  , renderContentRange
    -- * Accept-Ranges
  , AcceptRanges (..)
  , parseAcceptRanges
    -- * Multipart byteranges (RFC 9110 §14.6)
  , MultipartByterange (..)
  , parseMultipartByteranges
    -- * Request combinators
  , withRange
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word64)

import qualified Network.HTTP.Headers.AcceptRanges  as HAR
import qualified Network.HTTP.Headers.ContentRange  as HCR
import qualified Network.HTTP.Headers.Mason         as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Range         as HR
import qualified Network.HTTP.Types.Header          as H
import qualified Network.HTTP.Types.Status          as S

import Network.HTTP.Client.Request (Request, setHeader)

-- ---------------------------------------------------------------------------
-- Range request header
-- ---------------------------------------------------------------------------

-- | A single byte-range. Mirrors the wire grammar.
data ByteRange
  = ByteRange     !Word64 !Word64
    -- ^ @first-byte\/last-byte@, inclusive on both ends.
  | ByteRangeFrom !Word64
    -- ^ @first-byte-/@: from the given offset to the end of the
    --   resource.
  | ByteRangeSuffix !Word64
    -- ^ @-N@: the last N bytes of the resource.
  deriving stock (Eq, Show)

-- | A non-empty list of byte-ranges. The on-the-wire form is
-- @bytes=...,...@.
type Range = [ByteRange]

byteRange :: Word64 -> Word64 -> ByteRange
byteRange = ByteRange

byteRangeFrom :: Word64 -> ByteRange
byteRangeFrom = ByteRangeFrom

byteRangeSuffix :: Word64 -> ByteRange
byteRangeSuffix = ByteRangeSuffix

-- | Render a 'Range' as a complete @Range@ header value
-- (e.g. @bytes=0-1023,2048-@). Goes through the hermes builder so
-- the wire form matches the canonical grammar.
rangeHeader :: Range -> ByteString
rangeHeader rs = case NE.nonEmpty (map toHermes rs) of
  Nothing -> "bytes="
    -- Empty range is not strictly legal (the grammar mandates
    -- 1#range-spec), but produce a syntactically empty form so
    -- the caller's malformed input fails on the server side
    -- rather than here.
  Just ne -> M.toStrictByteString (HR.renderRange (HR.ByteRanges ne))
  where
    toHermes = \case
      ByteRange a b      -> HR.ByteRangeInt a (Just b)
      ByteRangeFrom a    -> HR.ByteRangeInt a Nothing
      ByteRangeSuffix n  -> HR.ByteRangeSuffix n

-- | Parse a @Range@ header value. Returns 'Nothing' if the unit
-- isn't @bytes@ or any range component is malformed.
parseRange :: ByteString -> Maybe Range
parseRange raw = case runParser HR.rangeParser raw of
  OK (HR.ByteRanges ne) leftover
    | BS.null (trim leftover) -> Just (map fromHermes (NE.toList ne))
  _ -> Nothing
  where
    fromHermes = \case
      HR.ByteRangeInt a (Just b) -> ByteRange a b
      HR.ByteRangeInt a Nothing  -> ByteRangeFrom a
      HR.ByteRangeSuffix n       -> ByteRangeSuffix n
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Content-Range
-- ---------------------------------------------------------------------------

-- | RFC 9110 §14.4: server's response to a satisfied range
-- request. Mirrors the original wireform shape; for the
-- unsatisfied-range form (the @*\/N@ payload used on 416) use
-- 'parseContentRangeFull'.
data ContentRange = ContentRange
  { crStart  :: !Word64
  , crEnd    :: !Word64
  , crTotal  :: !(Maybe Word64)
    -- ^ Total resource size. Servers may emit @*@ when the total
    --   length is unknown.
  }
  deriving stock (Eq, Show)

renderContentRange :: ContentRange -> ByteString
renderContentRange cr = M.toStrictByteString $
  HCR.renderContentRange (toHermes cr)
  where
    toHermes (ContentRange a b mTot) = HCR.ContentRange
      { HCR.contentRangeUnit = ST.fromString "bytes"
      , HCR.contentRangeResp = HCR.RangeRespSatisfied a b mTot
      }

-- | Parse a @Content-Range@ header value. Returns 'Nothing' for
-- both malformed input /and/ the unsatisfied form (@bytes *\/N@)
-- — that's the original behaviour and the right surface for the
-- common \"I expected a satisfied range\" callsite.
--
-- For full discrimination (including 416's unsatisfied form),
-- use 'parseContentRangeFull'.
parseContentRange :: ByteString -> Maybe ContentRange
parseContentRange raw = case parseContentRangeFull raw of
  Just (HCR.ContentRange _ (HCR.RangeRespSatisfied a b mTot)) ->
    Just (ContentRange a b mTot)
  _ -> Nothing

-- | Full @Content-Range@ parse including the unsatisfied form.
-- Returns 'Nothing' only when the value is syntactically broken.
parseContentRangeFull :: ByteString -> Maybe HCR.ContentRange
parseContentRangeFull raw = case runParser HCR.contentRangeParser raw of
  OK cr leftover
    | BS.null (trim leftover) -> Just cr
  _ -> Nothing
  where
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Accept-Ranges
-- ---------------------------------------------------------------------------

-- | The server-advertised @Accept-Ranges@ value (RFC 9110 §14.3).
data AcceptRanges
  = AcceptRangesNone
    -- ^ The server explicitly disabled range requests (the
    --   literal @"none"@).
  | AcceptRangesUnits ![ByteString]
    -- ^ Non-empty list of accepted range-units. Caller's job to
    --   check for @"bytes"@ before issuing a byte-range request.
  deriving stock (Eq, Show)

-- | Parse an @Accept-Ranges@ value. Returns 'Nothing' on
-- malformed input.
parseAcceptRanges :: ByteString -> Maybe AcceptRanges
parseAcceptRanges raw = case runParser HAR.acceptRangesParser raw of
  OK HAR.AcceptRangesNone leftover
    | BS.null (trim leftover) -> Just AcceptRangesNone
  OK (HAR.AcceptRangesUnits ne) leftover
    | BS.null (trim leftover) ->
        Just (AcceptRangesUnits (map (BS8.pack . ST.toString) (NE.toList ne)))
  _ -> Nothing
  where
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Multipart byteranges (RFC 9110 §14.6)
-- ---------------------------------------------------------------------------

-- | One part of a @multipart\/byteranges@ response payload (RFC
-- 9110 §14.6). Carries the part's @Content-Range@ (which tells
-- the caller which span of the resource the bytes correspond to),
-- the optional @Content-Type@, and the raw body bytes.
data MultipartByterange = MultipartByterange
  { mbRange       :: !ContentRange
  , mbContentType :: !(Maybe ByteString)
  , mbBody        :: !ByteString
  }
  deriving stock (Eq, Show)

-- | Parse a @multipart\/byteranges@ body. Takes the multipart
-- boundary (sans the leading @--@) and the raw body bytes; returns
-- 'Nothing' on malformed input.
--
-- Limitations: the parser ignores parts whose @Content-Range@
-- doesn't satisfy the 'parseContentRange' grammar, and it does
-- not attempt to honour 'Content-Transfer-Encoding'.
parseMultipartByteranges :: ByteString -> ByteString -> Maybe [MultipartByterange]
parseMultipartByteranges boundary body = do
  let dashBoundary = "--" <> boundary
      closing      = dashBoundary <> "--"
  -- Step 1: split on the boundary delimiter. The first piece
  -- before the first boundary is the optional preamble (we drop
  -- it); the last piece after the closing boundary is the
  -- optional epilogue (we drop that too).
  case splitOn dashBoundary body of
    []      -> Nothing
    (_pre : rest) ->
      let parts = takeWhile (not . isClosing closing) rest
          kept  = mapMaybe parseOnePart (map stripCRLFEdges parts)
      in Just kept
  where
    isClosing close chunk =
      "--" `BS.isPrefixOf` BS.dropWhile (\w -> w == 0x0D || w == 0x0A) chunk
        || close `BS.isPrefixOf` chunk
    stripCRLFEdges b =
      let b1 = dropPrefixCRLF b
          b2 = dropSuffixCRLF b1
      in b2
    dropPrefixCRLF b = case BS.uncons b of
      Just (0x0D, r) -> case BS.uncons r of
        Just (0x0A, r') -> r'
        _              -> r
      Just (0x0A, r) -> r
      _              -> b
    dropSuffixCRLF b = case BS.unsnoc b of
      Just (r, 0x0A) -> case BS.unsnoc r of
        Just (r', 0x0D) -> r'
        _              -> r
      _ -> b

parseOnePart :: ByteString -> Maybe MultipartByterange
parseOnePart raw = do
  let (rawHdrs, rawBody) = splitHeaderBody raw
      hdrLines  = filter (not . BS.null) (BS.split 0x0A (BS.map dropCR rawHdrs))
      hdrs      = mapMaybe parseHdrLine hdrLines
      mcr       = lookup "content-range" hdrs >>= parseContentRange
      mct       = lookup "content-type"  hdrs
  cr <- mcr
  pure MultipartByterange
    { mbRange       = cr
    , mbContentType = mct
    , mbBody        = rawBody
    }
  where
    dropCR 0x0D = 0x20
    dropCR w    = w
    parseHdrLine line = case BS.break (== 0x3A) line of
      (n, rest)
        | BS.null rest -> Nothing
        | otherwise    ->
            let trimOws  = BS.dropWhile  isOws . BS.dropWhileEnd isOws
                isOws w  = w == 0x20 || w == 0x09
                v        = trimOws (BS.drop 1 rest)
            in Just (BS.map asciiToLower n, v)
    asciiToLower w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

splitHeaderBody :: ByteString -> (ByteString, ByteString)
splitHeaderBody bs = case findCRLFCRLF bs of
  Just i -> (BS.take i bs, BS.drop (i + 4) bs)
  Nothing -> case findLFLF bs of
    Just i -> (BS.take i bs, BS.drop (i + 2) bs)
    Nothing -> (bs, BS.empty)
  where
    findCRLFCRLF = findSeq "\r\n\r\n"
    findLFLF     = findSeq "\n\n"

findSeq :: ByteString -> ByteString -> Maybe Int
findSeq needle haystack
  | BS.null needle = Nothing
  | otherwise = go 0
  where
    n = BS.length needle
    h = BS.length haystack
    go i
      | i + n > h                  = Nothing
      | BS.take n (BS.drop i haystack) == needle = Just i
      | otherwise                  = go (i + 1)

splitOn :: ByteString -> ByteString -> [ByteString]
splitOn needle haystack = case findSeq needle haystack of
  Nothing -> [haystack]
  Just i  ->
    BS.take i haystack
      : splitOn needle (BS.drop (i + BS.length needle) haystack)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x : xs) = case f x of
  Nothing -> mapMaybe f xs
  Just y  -> y : mapMaybe f xs

-- ---------------------------------------------------------------------------
-- Request combinator
-- ---------------------------------------------------------------------------

-- | Attach a @Range@ header to a request.
withRange :: Range -> Request a -> Request a
withRange r = setHeader H.hRange (rangeHeader r)

-- | Predicate for @206 Partial Content@ responses, kept here so
-- the same module that exposes 'ContentRange' also has the
-- companion status helper.
_status206 :: S.Status -> Bool
_status206 = (== S.status206)
