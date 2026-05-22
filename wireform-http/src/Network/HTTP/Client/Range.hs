{- | Range request and partial-content helpers (RFC 9110 \u00a714).

The unit is bytes; range-units other than @bytes@ are not supported
because that's what 99.9% of the wild uses (and what
@Accept-Ranges: bytes@ on a server response advertises).

A 'Range' value reads as a list of byte ranges. The high-level helpers
let you build the most common shapes (\"first N bytes\",
\"resume from offset\", \"last N bytes\") without thinking about the
spec syntax.
-}
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
  , renderContentRange
    -- * Request combinators
  , withRange
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.List (intersperse)
import Data.Word (Word64)

import qualified Network.HTTP.Types.Header as H

import Network.HTTP.Client.Request (Request, setHeader)

-- ---------------------------------------------------------------------------
-- Range header
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
-- (e.g. @bytes=0-1023,2048-@).
rangeHeader :: Range -> ByteString
rangeHeader rs = "bytes=" <> mconcat (intersperse "," (map renderOne rs))
  where
    renderOne (ByteRange a b)      = w a <> "-" <> w b
    renderOne (ByteRangeFrom a)    = w a <> "-"
    renderOne (ByteRangeSuffix n)  = "-" <> w n
    w = BS8.pack . show

-- | Parse a @Range@ header value. Returns 'Nothing' if the unit isn't
-- @bytes@ or any range component is malformed.
parseRange :: ByteString -> Maybe Range
parseRange raw0 = do
  rest <- BS.stripPrefix "bytes=" (trim raw0)
  let toks = filter (not . BS.null) (map trim (BS.split 0x2C rest))
  case toks of
    [] -> Nothing
    _  -> traverse parseOne toks
  where
    parseOne tok = case BS.break (== 0x2D) tok of
      (a, dash) | not (BS.null dash) -> case (BS.null a, BS.drop 1 dash) of
        (True, rest) -> ByteRangeSuffix <$> readW (trim rest)
        (False, rest)
          | BS.null rest -> ByteRangeFrom   <$> readW (trim a)
          | otherwise    -> ByteRange      <$> readW (trim a) <*> readW (trim rest)
      _ -> Nothing
    readW b = case BS8.readInteger b of
      Just (n, leftover) | BS.null leftover && n >= 0 -> Just (fromIntegral n)
      _ -> Nothing
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Content-Range
-- ---------------------------------------------------------------------------

-- | RFC 9110 \u00a714.4: server's response to a satisfied range request.
data ContentRange = ContentRange
  { crStart  :: !Word64
  , crEnd    :: !Word64
  , crTotal  :: !(Maybe Word64)
    -- ^ Total resource size. Servers may emit @*@ when the total
    --   length is unknown.
  }
  deriving stock (Eq, Show)

renderContentRange :: ContentRange -> ByteString
renderContentRange (ContentRange a b mTot) =
  let total = case mTot of
        Just t  -> w t
        Nothing -> "*"
      w :: Word64 -> ByteString
      w = BS8.pack . show
  in "bytes " <> w a <> "-" <> w b <> "/" <> total

-- | Parse a @Content-Range@ header value. Returns 'Nothing' for the
-- unsatisfied-range form (@bytes *\/N@) since that's about the
-- server having rejected the range, not about a satisfied response.
parseContentRange :: ByteString -> Maybe ContentRange
parseContentRange raw0 = do
  rest <- BS.stripPrefix "bytes " (trim raw0)
  let (rng, slashTotal) = BS.break (== 0x2F) rest
  totalBs <- BS.stripPrefix "/" slashTotal
  let (aBs, dashB) = BS.break (== 0x2D) (trim rng)
  bBs <- BS.stripPrefix "-" dashB
  a <- readW (trim aBs)
  b <- readW (trim bBs)
  let total = case BS.dropWhile isWS totalBs of
        "*" -> Just Nothing
        b' -> case readW b' of
          Just n  -> Just (Just n)
          Nothing -> Nothing
  case total of
    Just mTot -> Just (ContentRange a b mTot)
    Nothing   -> Nothing
  where
    readW :: ByteString -> Maybe Word64
    readW b = case BS8.readInteger b of
      Just (n, leftover) | BS.null leftover && n >= 0 -> Just (fromIntegral n)
      _ -> Nothing
    trim = BS.dropWhile isWS . BS.dropWhileEnd isWS
    isWS w = w == 0x20 || w == 0x09

-- ---------------------------------------------------------------------------
-- Request combinator
-- ---------------------------------------------------------------------------

-- | Attach a @Range@ header to a request.
withRange :: Range -> Request a -> Request a
withRange r = setHeader H.hRange (rangeHeader r)
