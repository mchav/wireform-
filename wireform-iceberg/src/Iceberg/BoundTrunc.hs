{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Lower- and upper-bound truncation for Iceberg manifest statistics.
--
-- Per Java's @MetricsModes.Truncate@ and PyIceberg's @bounds_helpers@, when
-- a 'Truncate' metrics mode is in effect the writer truncates strings\/
-- binaries to @N@ Unicode characters, then /rounds the upper bound up/ by
-- incrementing the last code point so that the truncated bound is still a
-- safe over-approximation. If the upper bound consists entirely of the
-- maximum code point, the upper-bound value is dropped entirely.
module Iceberg.BoundTrunc
  ( truncateLowerString
  , truncateUpperString
  , truncateLowerBytes
  , truncateUpperBytes
  ) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)

-- | Lower-bound truncation for strings: take the first @n@ characters.
truncateLowerString :: Int -> Text -> Text
truncateLowerString n t
  | n <= 0    = mempty
  | otherwise = T.take n t

-- | Upper-bound truncation for strings: take the first @n@ characters and
-- bump the last code point by one. If the entire @n@-char prefix is the
-- maximum code point (U+10FFFF), the bound becomes 'Nothing' (drop it).
truncateUpperString :: Int -> Text -> Maybe Text
truncateUpperString n t
  | n <= 0 = Nothing
  | T.length t <= n = Just t
  | otherwise =
      let prefix = T.take n t
          chars  = T.unpack prefix
       in case bumpLastChar chars of
            Just bumped -> Just (T.pack bumped)
            Nothing     -> Nothing

-- | Increment the trailing character; carry when the trailing character is
-- the Unicode maximum.
bumpLastChar :: String -> Maybe String
bumpLastChar = go . reverse
  where
    go [] = Nothing
    go (c : rest)
      | fromEnum c >= 0x10FFFF = case go rest of
          Just rest' -> Just (reverse (minBound : rest'))
          Nothing    -> Nothing
      | otherwise = Just (reverse (toEnum (fromEnum c + 1) : rest))

-- | Lower-bound truncation for byte strings: take the first @n@ bytes.
truncateLowerBytes :: Int -> ByteString -> ByteString
truncateLowerBytes n b
  | n <= 0    = BS.empty
  | otherwise = BS.take n b

-- | Upper-bound truncation for byte strings. Increments the last byte;
-- returns 'Nothing' when the entire prefix is @0xFF@.
truncateUpperBytes :: Int -> ByteString -> Maybe ByteString
truncateUpperBytes n b
  | n <= 0 = Nothing
  | BS.length b <= n = Just b
  | otherwise = case bumpLastByte (BS.take n b) of
      Just bumped -> Just bumped
      Nothing     -> Nothing

bumpLastByte :: ByteString -> Maybe ByteString
bumpLastByte b
  | BS.null b = Nothing
  | otherwise = go (BS.length b - 1)
  where
    go !i
      | i < 0 = Nothing
      | BS.index b i == 0xFF = go (i - 1)
      | otherwise =
          let prefix = BS.take i b
              middle = BS.singleton (BS.index b i + 1)
              suffix = BS.replicate (BS.length b - i - 1) 0
           in Just (prefix <> middle <> suffix)
