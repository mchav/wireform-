{- | Media types and content negotiation primitives.

The vocabulary here is small on purpose: a 'MediaType' is a parsed
@type\/subtype@ pair with parameters. 'HasMediaType' attaches a media
type to a phantom tag. 'Encode' and 'Decode' are open type classes
keyed on the tag: users define their own tags for protobuf, XML,
MessagePack, etc.

This module is library-style — no I\/O. The shipped tags (JSON,
plain text, form-urlencoded, octet-stream) live in
"Network.HTTP.Client.Media.JSON" and friends; that keeps aeson and
http-api-data out of the import graph until you actually want them.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Media
  ( -- * Media types
    MediaType (..)
  , mediaTypeBytes
  , parseMediaType
  , renderMediaType
  , matches
  , matchesAny
  , stripParameters
  , Quality (..)
  , maxQuality
  , minQuality
    -- * Open type-class machinery
  , HasMediaType (..)
  , Encode (..)
  , Decode (..)
  , DecodeError (..)
    -- * Header helpers
  , contentTypeOf
  , acceptHeaderValue
  ) where

import Control.Exception (Exception)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.ByteString (ByteString)
import Data.Char (toLower)
import qualified Data.List as List
import Data.String (IsString (..))

import qualified Network.HTTP.Types.Header as H

-- | A parsed @type\/subtype@ media type, plus any parameters that
-- came along on the @Content-Type@ header (e.g. @charset=utf-8@).
--
-- Comparison is case-insensitive on the type, subtype, and parameter
-- names; parameter /values/ are compared exactly.
data MediaType = MediaType
  { mtType       :: !ByteString
  , mtSubType    :: !ByteString
  , mtParameters :: ![(ByteString, ByteString)]
  }
  deriving stock (Eq, Show)

-- | @\"type\/subtype\"@ literals build a 'MediaType' via the 'IsString'
-- instance. Parameters are parsed if present.
instance IsString MediaType where
  fromString s = case parseMediaType (BS8.pack s) of
    Right mt -> mt
    Left _   -> MediaType (BS8.pack (map toLower s)) "" []

mediaTypeBytes :: MediaType -> ByteString
mediaTypeBytes = renderMediaType

-- | Parse a @Content-Type@ value, returning the media type and any
-- parameters. Lenient: unknown shapes produce a 'Left' but the only
-- check is that the value contains a single @\"\/\"@.
parseMediaType :: ByteString -> Either String MediaType
parseMediaType bs0 =
  let bs = BS.dropWhile (== 0x20) bs0
      (head_, rest) = BS.break (== 0x3B) bs  -- ';'
      (typ, sub0) = BS.break (== 0x2F) head_
  in case BS.uncons sub0 of
       Just (0x2F, sub) ->
         let typLower = BS8.map toLower (BS.dropWhileEnd (== 0x20) typ)
             subLower = BS8.map toLower (BS.dropWhileEnd (== 0x20) sub)
         in Right MediaType
              { mtType       = typLower
              , mtSubType    = subLower
              , mtParameters = parseParameters rest
              }
       _ -> Left ("malformed Content-Type: " <> BS8.unpack bs0)

parseParameters :: ByteString -> [(ByteString, ByteString)]
parseParameters = go . dropSemi
  where
    dropSemi b = case BS.uncons b of
      Just (0x3B, rest) -> BS.dropWhile (== 0x20) rest
      _                 -> BS.dropWhile (== 0x20) b
    go b
      | BS.null b = []
      | otherwise =
          let (chunk, rest) = BS.break (== 0x3B) b
              (k, v0) = BS.break (== 0x3D) chunk
              v = case BS.uncons v0 of
                    Just (0x3D, val) -> stripQuotes (BS.dropWhile (== 0x20) val)
                    _                -> ""
              k' = BS8.map toLower (BS.dropWhileEnd (== 0x20) k)
          in (k', v) : go (dropSemi rest)
    stripQuotes b = case BS.uncons b of
      Just (0x22, rest) ->
        case BS.unsnoc rest of
          Just (rest', 0x22) -> rest'
          _                  -> rest
      _ -> BS.dropWhileEnd (== 0x20) b

renderMediaType :: MediaType -> ByteString
renderMediaType m =
  let core = mtType m <> "/" <> mtSubType m
      params = mconcat
        [ "; " <> k <> "=" <> v | (k, v) <- mtParameters m, not (BS.null k) ]
  in core <> params

-- | Drop any media-type parameters, leaving the bare @type\/subtype@.
stripParameters :: MediaType -> MediaType
stripParameters m = m { mtParameters = [] }

-- | Match a media type against a target. Wildcards are honoured
-- according to RFC 9110: @*\/*@ matches anything, @text\/*@ matches
-- any @text\/...@, and @text\/plain@ matches exactly @text\/plain@.
-- Parameters on the candidate are ignored for matching.
matches :: MediaType -> MediaType -> Bool
matches candidate target =
  let candTy = mtType candidate
      candSub = mtSubType candidate
      tgtTy = mtType target
      tgtSub = mtSubType target
      tyOk  = candTy == tgtTy || tgtTy == "*"
      subOk = candSub == tgtSub || tgtSub == "*"
  in tyOk && subOk

matchesAny :: MediaType -> [MediaType] -> Bool
matchesAny c = any (matches c)

-- | RFC 9110 @q=...@ quality value, clamped to @[0,1]@.
newtype Quality = Quality { unQuality :: Double }
  deriving stock (Eq, Ord, Show)

maxQuality :: Quality
maxQuality = Quality 1.0

minQuality :: Quality
minQuality = Quality 0.0

-- ---------------------------------------------------------------------------
-- Open typeclass machinery
-- ---------------------------------------------------------------------------

-- | Associate a content-type tag with its media type. Tags are
-- typically uninhabited phantom types; the class is a way to project
-- the type-level tag onto a runtime media type.
--
-- > mediaType @JSON  ==  "application/json"
class HasMediaType tag where
  mediaType :: MediaType

class HasMediaType tag => Encode tag a where
  encode :: a -> ByteString

class HasMediaType tag => Decode tag a where
  decode :: ByteString -> Either DecodeError a

-- | A decode failure carrying the media type, the offending body
-- bytes (truncated for diagnostics by the caller if needed), and a
-- human-readable reason.
data DecodeError = DecodeError
  { decodeMediaType :: !MediaType
  , decodeMessage   :: !String
  }
  deriving stock (Show)

instance Exception DecodeError

-- ---------------------------------------------------------------------------
-- Header helpers
-- ---------------------------------------------------------------------------

-- | Extract a 'MediaType' from a header list. Returns
-- @application\/octet-stream@ if the header is absent (RFC 9110
-- § 8.3 default).
contentTypeOf :: H.Headers -> MediaType
contentTypeOf hdrs = case H.lookupHeader H.hContentType hdrs of
  Nothing -> MediaType "application" "octet-stream" []
  Just v  -> case parseMediaType v of
    Right mt -> mt
    Left _   -> MediaType "application" "octet-stream" []

-- | Render a list of @(media-type, quality)@ pairs as an @Accept@
-- header value. Quality 1.0 is omitted.
acceptHeaderValue :: [(MediaType, Quality)] -> ByteString
acceptHeaderValue = BS.intercalate ", " . List.map one
  where
    one (m, Quality q)
      | q >= 1.0  = renderMediaType m
      | otherwise = renderMediaType m <> "; q=" <> BS8.pack (formatQ q)
    formatQ q =
      let truncated = (fromIntegral (round (q * 1000) :: Int) :: Double) / 1000
      in show truncated
