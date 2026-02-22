{-# LANGUAGE BangPatterns #-}
-- | Configurable field representations.
--
-- By default, proto string fields map to strict 'Text', bytes to strict
-- 'ByteString', and repeated fields to 'Vector'. This module lets you
-- override those choices per-field or per-message. For instance, a large
-- blob field might be better as lazy ByteString, identifiers as ShortText,
-- and a small repeated field as a plain list.
--
-- Usage with TH:
--
-- @
-- \$(loadProtoWithRep
--     (defaultLoadOpts { loRepConfig = defaultRepConfig
--         { rcFieldOverrides = Map.fromList
--             [ (("Person","name"), fieldRep { frString = ShortTextRep })
--             , (("Blob","data"), fieldRep { frBytes = LazyBytesRep })
--             , (("Config","tags"), fieldRep { frRepeated = ListRep })
--             ]
--         }
--     })
--     "path/to/file.proto")
-- @
module Proto.Repr
  ( -- * Representation choices
    StringRep (..)
  , BytesRep (..)
  , RepeatedRep (..)
  , OptionalRep (..)

    -- * Per-field configuration
  , FieldRep (..)
  , defaultFieldRep

    -- * Configuration table
  , RepConfig (..)
  , defaultRepConfig
  , lookupFieldRep

    -- * Encode adapters (used by TH-generated code)
  , encodeStrictText
  , encodeLazyText
  , encodeShortByteString
  , encodeHsString
  , encodeStrictBytes
  , encodeLazyBytes
  , encodeShortBytes
  , foldVector
  , foldList
  , foldSeq

    -- * Decode adapters
  , decodeToStrictText
  , decodeToLazyText
  , decodeToShortText
  , decodeToHsString
  , decodeToLazyBytes
  , decodeToShortBytes

    -- * Container operations
  , emptyVector, emptyList, emptySeq
  , snocVector, snocList, snocSeq
  , nullVector, nullList, nullSeq

    -- * Default values
  , emptyStrictText, emptyLazyText
  , emptyShortBytes, emptyHsString
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Short as SBS
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V

import Proto.Wire (WireType(..))
import Proto.Wire.Encode (putTag, putVarint, putLengthDelimited)

-- | How to represent proto @string@ fields.
data StringRep
  = StrictTextRep     -- ^ Data.Text.Text (default, zero-copy decode)
  | LazyTextRep       -- ^ Data.Text.Lazy.Text
  | ShortTextRep      -- ^ Data.Text.Short.ShortText (via ShortByteString, compact)
  | HsStringRep       -- ^ [Char] (convenient but slow)
  deriving stock (Show, Eq, Ord)

-- | How to represent proto @bytes@ fields.
data BytesRep
  = StrictBytesRep    -- ^ Data.ByteString.ByteString (default, zero-copy decode)
  | LazyBytesRep      -- ^ Data.ByteString.Lazy.ByteString
  | ShortBytesRep     -- ^ Data.ByteString.Short.ShortByteString (unpinned, GC-friendly)
  deriving stock (Show, Eq, Ord)

-- | How to represent proto @repeated@ fields.
data RepeatedRep
  = VectorRep         -- ^ Data.Vector.Vector (default, O(1) index)
  | ListRep           -- ^ [] (convenient, good fusion)
  | SeqRep            -- ^ Data.Sequence.Seq (O(log n) snoc, good for building)
  deriving stock (Show, Eq, Ord)

-- | How to represent proto optional/nullable fields.
data OptionalRep
  = MaybeRep          -- ^ Maybe a (default)
  | FieldPresenceRep  -- ^ Proto.FieldPresence.Field a (explicit presence tracking)
  deriving stock (Show, Eq, Ord)

-- | Representation choices for a single field.
data FieldRep = FieldRep
  { frString   :: !StringRep
  , frBytes    :: !BytesRep
  , frRepeated :: !RepeatedRep
  , frOptional :: !OptionalRep
  } deriving stock (Show, Eq, Ord)

-- | Sensible defaults: strict Text, strict ByteString, Vector, Maybe.
defaultFieldRep :: FieldRep
defaultFieldRep = FieldRep
  { frString   = StrictTextRep
  , frBytes    = StrictBytesRep
  , frRepeated = VectorRep
  , frOptional = MaybeRep
  }

-- | Configuration table mapping (message, field) pairs to representation choices.
data RepConfig = RepConfig
  { rcDefault          :: !FieldRep
    -- ^ Default representation for all fields.
  , rcMessageOverrides :: !(Map Text FieldRep)
    -- ^ Per-message override (applies to all fields in the message).
  , rcFieldOverrides   :: !(Map (Text, Text) FieldRep)
    -- ^ Per-field override. Key is (messageName, fieldName).
  } deriving stock (Show, Eq)

defaultRepConfig :: RepConfig
defaultRepConfig = RepConfig
  { rcDefault          = defaultFieldRep
  , rcMessageOverrides = Map.empty
  , rcFieldOverrides   = Map.empty
  }

-- | Look up the representation for a specific field, falling back through
-- message-level then default config.
lookupFieldRep :: Text -> Text -> RepConfig -> FieldRep
lookupFieldRep msgName fldName cfg =
  case Map.lookup (msgName, fldName) (rcFieldOverrides cfg) of
    Just rep -> rep
    Nothing  -> case Map.lookup msgName (rcMessageOverrides cfg) of
      Just rep -> rep
      Nothing  -> rcDefault cfg

-- Encode adapters: convert from the chosen representation to what
-- the wire encoder expects (strict Text / strict ByteString).

-- Specific encode functions for each string representation.
-- These are used by generated code (TH splices reference them via 'quotes).

-- | Encode strict Text (the default — no conversion needed).
encodeStrictText :: Int -> Text -> B.Builder
encodeStrictText fn t =
  putTag fn WireLengthDelimited <>
  let bs = TE.encodeUtf8 t in putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE encodeStrictText #-}

-- | Encode lazy Text.
encodeLazyText :: Int -> TL.Text -> B.Builder
encodeLazyText fn t =
  let bs = BL.toStrict (TLE.encodeUtf8 t)
  in putTag fn WireLengthDelimited <> putVarint (fromIntegral (BS.length bs)) <> B.byteString bs
{-# INLINE encodeLazyText #-}

-- | Encode ShortByteString (used for ShortText rep — stored as UTF-8 SBS).
encodeShortByteString :: Int -> SBS.ShortByteString -> B.Builder
encodeShortByteString fn sbs =
  let bs = SBS.fromShort sbs
  in putTag fn WireLengthDelimited <> putLengthDelimited bs
{-# INLINE encodeShortByteString #-}

-- | Encode String.
encodeHsString :: Int -> String -> B.Builder
encodeHsString fn s = encodeStrictText fn (T.pack s)
{-# INLINE encodeHsString #-}

-- | Encode strict ByteString (no conversion).
encodeStrictBytes :: Int -> ByteString -> B.Builder
encodeStrictBytes fn bs =
  putTag fn WireLengthDelimited <> putLengthDelimited bs
{-# INLINE encodeStrictBytes #-}

-- | Encode lazy ByteString.
encodeLazyBytes :: Int -> BL.ByteString -> B.Builder
encodeLazyBytes fn lbs = encodeStrictBytes fn (BL.toStrict lbs)
{-# INLINE encodeLazyBytes #-}

-- | Encode short ByteString.
encodeShortBytes :: Int -> SBS.ShortByteString -> B.Builder
encodeShortBytes = encodeShortByteString
{-# INLINE encodeShortBytes #-}

-- Decode adapters

-- | Decode to strict Text (zero-copy from wire).
decodeToStrictText :: ByteString -> Either String Text
decodeToStrictText bs = case TE.decodeUtf8' bs of
  Left _  -> Left "Invalid UTF-8"
  Right t -> Right t

-- | Decode to lazy Text.
decodeToLazyText :: ByteString -> Either String TL.Text
decodeToLazyText bs = case TE.decodeUtf8' bs of
  Left _  -> Left "Invalid UTF-8"
  Right t -> Right (TL.fromStrict t)

-- | Decode to ShortByteString (UTF-8 stored as SBS).
decodeToShortText :: ByteString -> SBS.ShortByteString
decodeToShortText = SBS.toShort

-- | Decode to Haskell String.
decodeToHsString :: ByteString -> Either String String
decodeToHsString bs = case TE.decodeUtf8' bs of
  Left _  -> Left "Invalid UTF-8"
  Right t -> Right (T.unpack t)

-- | Decode to lazy ByteString.
decodeToLazyBytes :: ByteString -> BL.ByteString
decodeToLazyBytes = BL.fromStrict

-- | Decode to ShortByteString.
decodeToShortBytes :: ByteString -> SBS.ShortByteString
decodeToShortBytes = SBS.toShort

-- Repeated field adapters

-- | Helper class: avoid writing the same fold for every container type.
-- Not exposed; used by generated code via the specific functions below.

-- Encode adapters for repeated fields with different container types.

-- | Fold over a Vector to encode each element.
foldVector :: (a -> B.Builder) -> V.Vector a -> B.Builder
foldVector f = V.foldl' (\acc v -> acc <> f v) mempty
{-# INLINE foldVector #-}

-- | Fold over a list to encode each element.
foldList :: (a -> B.Builder) -> [a] -> B.Builder
foldList f = go mempty
  where
    go !acc []     = acc
    go !acc (x:xs) = go (acc <> f x) xs
{-# INLINE foldList #-}

-- | Fold over a Seq to encode each element.
foldSeq :: (a -> B.Builder) -> Seq a -> B.Builder
foldSeq f = foldl' (\acc v -> acc <> f v) mempty . Foldable.toList
{-# INLINE foldSeq #-}


-- Decode: empty and snoc for each container type.

emptyVector :: V.Vector a
emptyVector = V.empty

emptyList :: [a]
emptyList = []

emptySeq :: Seq a
emptySeq = Seq.empty

snocVector :: V.Vector a -> a -> V.Vector a
snocVector = V.snoc
{-# INLINE snocVector #-}

snocList :: [a] -> a -> [a]
snocList xs x = xs ++ [x]
{-# INLINE snocList #-}

snocSeq :: Seq a -> a -> Seq a
snocSeq = (Seq.|>)
{-# INLINE snocSeq #-}


-- Functions for checking emptiness of each container type.

nullVector :: V.Vector a -> Bool
nullVector = V.null

nullList :: [a] -> Bool
nullList [] = True
nullList _  = False

nullSeq :: Seq a -> Bool
nullSeq = Seq.null

-- String emptiness checks for each representation.

emptyStrictText :: Text
emptyStrictText = T.empty

emptyLazyText :: TL.Text
emptyLazyText = TL.empty

emptyShortBytes :: SBS.ShortByteString
emptyShortBytes = SBS.empty

emptyHsString :: String
emptyHsString = ""
