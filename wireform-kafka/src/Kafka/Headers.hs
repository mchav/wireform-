{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Kafka.Headers
Description : Kafka record headers — typed wrapper around 'Vector (Text, ByteString)'
Copyright   : (c) 2025
License     : BSD-3-Clause

Kafka record headers are name-value pairs attached to a record. The
wire shape is a list of @(Text, ByteString)@ pairs, but at the
application level you usually want random-access lookup, structured
construction, and the option to thread a header list through several
interceptors without re-scanning it each time.

'Headers' is a thin newtype over 'Vector' @(Text, ByteString)@ that
preserves insertion order and provides O(n) lookup, O(1) append, and
O(n) replace. For the typical "five or ten headers per record" shape
this is faster and friendlier than @[(Text, ByteString)]@ while
keeping the wire shape unchanged.

= Recommended usage

@
import qualified Kafka.Headers as H

let hs = H.fromList
      [ ( \"trace-parent\", traceparent )
      , ( \"x-app-version\", appVersion )
      ]

case H.lookup \"trace-parent\" hs of
  Just bs -> ...
  Nothing -> ...
@
-}
module Kafka.Headers (
  -- * Type
  Headers,

  -- * Construction
  empty,
  fromList,
  fromPairs,
  singleton,

  -- * Inspection
  toList,
  toPairs,
  null,
  length,
  keys,
  lookup,
  lookupAll,
  member,

  -- * Mutation
  insert,
  insertText,
  replace,
  delete,

  -- * Composition
  append,
  concat,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Prelude hiding (concat, length, lookup, null)


-- | Ordered name-value pairs attached to a Kafka record.
newtype Headers = Headers {unHeaders :: Vector (Text, ByteString)}
  deriving stock (Eq, Show)


instance Semigroup Headers where
  Headers a <> Headers b = Headers (a V.++ b)


instance Monoid Headers where
  mempty = Headers V.empty


----------------------------------------------------------------------
-- Construction
----------------------------------------------------------------------

empty :: Headers
empty = Headers V.empty


fromList :: [(Text, ByteString)] -> Headers
fromList = Headers . V.fromList


{- | Alias for 'fromList' kept for callers that prefer the noun
"pairs".
-}
fromPairs :: [(Text, ByteString)] -> Headers
fromPairs = fromList


singleton :: Text -> ByteString -> Headers
singleton k v = Headers (V.singleton (k, v))


----------------------------------------------------------------------
-- Inspection
----------------------------------------------------------------------

toList :: Headers -> [(Text, ByteString)]
toList = V.toList . unHeaders


toPairs :: Headers -> [(Text, ByteString)]
toPairs = toList


null :: Headers -> Bool
null = V.null . unHeaders


length :: Headers -> Int
length = V.length . unHeaders


keys :: Headers -> [Text]
keys = V.toList . V.map fst . unHeaders


-- | Return the first header value with the given name, or 'Nothing'.
lookup :: Text -> Headers -> Maybe ByteString
lookup k = fmap snd . V.find ((== k) . fst) . unHeaders


-- | Return every value bound to the supplied name in insertion order.
lookupAll :: Text -> Headers -> [ByteString]
lookupAll k = V.toList . V.map snd . V.filter ((== k) . fst) . unHeaders


member :: Text -> Headers -> Bool
member k = V.any ((== k) . fst) . unHeaders


----------------------------------------------------------------------
-- Mutation
----------------------------------------------------------------------

{- | Append a header at the end. Duplicates are allowed; use 'replace'
to overwrite an existing entry.
-}
insert :: Text -> ByteString -> Headers -> Headers
insert k v (Headers hs) = Headers (V.snoc hs (k, v))


-- | Convenience: insert a UTF-8 text value.
insertText :: Text -> Text -> Headers -> Headers
insertText k v = insert k (TE.encodeUtf8 v)


{- | Replace every existing entry with name @k@ by a single new one,
preserving the position of the first occurrence. If @k@ is absent,
append.
-}
replace :: Text -> ByteString -> Headers -> Headers
replace k v (Headers hs) =
  let dropped = V.filter ((/= k) . fst) hs
  in case V.findIndex ((== k) . fst) hs of
       Nothing -> Headers (V.snoc hs (k, v))
       Just i ->
         let (before, after) = V.splitAt i dropped
         in Headers (before V.++ V.singleton (k, v) V.++ after)


-- | Drop every entry with the given name.
delete :: Text -> Headers -> Headers
delete k = Headers . V.filter ((/= k) . fst) . unHeaders


----------------------------------------------------------------------
-- Composition
----------------------------------------------------------------------

append :: Headers -> Headers -> Headers
append = (<>)


concat :: [Headers] -> Headers
concat = foldr append empty
