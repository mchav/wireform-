{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Kafka.Serde
Description : Pluggable serialiser \/ deserialiser pairs
Copyright   : (c) 2025
License     : BSD-3-Clause

The Kafka equivalent of @org.apache.kafka.common.serialization.Serde<T>@:
a paired serialiser and deserialiser that the client can apply
record-by-record. The same 'Serde' values are used by both the
producer / consumer client and the streams DSL (the streams
'Kafka.Streams.Serde' module re-exports everything here).

Built-ins cover the common cases the Java client ships under
@Serdes@:

  * 'byteStringSerde' — opaque bytes (identity)
  * 'textSerde' — UTF-8 'Text'
  * 'utf8Serde' — UTF-8 'String' (allocates; prefer 'textSerde')
  * 'int32Serde' / 'int64Serde' \/ 'longSerde'
  * 'doubleSerde' / 'floatSerde'
  * 'voidSerde' — for keyless records
  * 'uuidSerde'
  * 'byteArraySerde'
  * 'jsonSerde' — Aeson 'ToJSON' \/ 'FromJSON' bridge

Constructors:

  * 'serde'         — total deserialise (no error reporting)
  * 'unsafeSerde'   — 'String'-error deserialise
  * 'imap'          — invariant map (compose serdes through an iso)
  * 'prefixedSerde' — tag with a single byte
  * 'lengthPrefixedSerde' — length-prefixed framing for composite
    values

Record helpers ('serializeRecord' / 'deserializeRecord') apply a
@(key, value)@ pair of serdes through @(Maybe k, v)@.
-}
module Kafka.Serde
  ( -- * Type
    Serde (..)
  , serde
  , unsafeSerde
  , imap
    -- * Built-ins
  , byteStringSerde
  , textSerde
  , utf8Serde
  , int32Serde
  , int64Serde
  , longSerde
  , doubleSerde
  , floatSerde
  , voidSerde
  , uuidSerde
  , byteArraySerde
  , jsonSerde
    -- * Combinators
  , prefixedSerde
  , lengthPrefixedSerde
    -- * Record helpers
  , serializeRecord
  , deserializeRecord
  ) where

import qualified Data.Aeson           as Aeson
import           Data.Bits            (shiftL, shiftR, (.|.))
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Int             (Int32, Int64)
import qualified Data.Text            as T
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TE
import qualified Data.UUID            as UUID
import           Data.UUID            (UUID)
import           Data.Word            (Word8, Word32, Word64)
import           GHC.Float            (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import           GHC.Generics         (Generic)

-- | Bidirectional codec.  'deserialize' returns 'Either' so callers
-- can route failures through a deserialisation handler.
data Serde a = Serde
  { serialize   :: a -> ByteString
  , deserialize :: ByteString -> Either String a
  }
  deriving stock (Generic)

-- | Convenience builder for a total deserialiser.
serde :: (a -> ByteString) -> (ByteString -> a) -> Serde a
serde s d = Serde s (Right . d)

-- | Convenience builder for a partial deserialiser whose error
-- channel is 'String'.
unsafeSerde :: (a -> ByteString) -> (ByteString -> Either String a) -> Serde a
unsafeSerde = Serde

-- | Invariant-map a 'Serde' through an iso @(b -> a, a -> b)@.
imap :: (b -> a) -> (a -> b) -> Serde a -> Serde b
imap toA fromA s = Serde
  { serialize   = serialize s . toA
  , deserialize = fmap fromA . deserialize s
  }

byteStringSerde :: Serde ByteString
byteStringSerde = serde id id

-- | UTF-8 'Text' serde. Mirrors @org.apache.kafka.common.serialization.StringSerializer@.
textSerde :: Serde Text
textSerde =
  Serde
    { serialize   = TE.encodeUtf8
    , deserialize = either (Left . show) Right . TE.decodeUtf8'
    }

utf8Serde :: Serde String
utf8Serde = imap T.pack T.unpack textSerde

voidSerde :: Serde ()
voidSerde =
  Serde
    { serialize   = const BS.empty
    , deserialize = \b ->
        if BS.null b
          then Right ()
          else Left "voidSerde: expected empty payload"
    }

-- | Big-endian 'Int32' serde. Matches @IntegerSerializer@ on the JVM
-- (which writes a 4-byte big-endian two's-complement value).
int32Serde :: Serde Int32
int32Serde = Serde encodeI32 decodeI32
  where
    encodeI32 :: Int32 -> ByteString
    encodeI32 n =
      let w = fromIntegral n :: Word32
       in BS.pack
            [ fromIntegral (w `shiftR` 24)
            , fromIntegral (w `shiftR` 16)
            , fromIntegral (w `shiftR`  8)
            , fromIntegral  w
            ]
    decodeI32 b
      | BS.length b /= 4 = Left "int32Serde: expected 4 bytes"
      | otherwise =
          let !w = (fromIntegral (BS.index b 0) `shiftL` 24)
                .|. (fromIntegral (BS.index b 1) `shiftL` 16)
                .|. (fromIntegral (BS.index b 2) `shiftL` 8)
                .|.  fromIntegral (BS.index b 3) :: Word32
           in Right (fromIntegral w)

-- | Big-endian 'Int64' serde. Matches @LongSerializer@ on the JVM.
int64Serde :: Serde Int64
int64Serde = Serde encodeI64 decodeI64
  where
    encodeI64 :: Int64 -> ByteString
    encodeI64 n =
      let w = fromIntegral n :: Word64
       in BS.pack
            [ fromIntegral (w `shiftR` 56)
            , fromIntegral (w `shiftR` 48)
            , fromIntegral (w `shiftR` 40)
            , fromIntegral (w `shiftR` 32)
            , fromIntegral (w `shiftR` 24)
            , fromIntegral (w `shiftR` 16)
            , fromIntegral (w `shiftR`  8)
            , fromIntegral  w
            ]
    decodeI64 b
      | BS.length b /= 8 = Left "int64Serde: expected 8 bytes"
      | otherwise =
          let bytes = BS.unpack b
              !w = foldl
                (\acc x -> (acc `shiftL` 8) .|. fromIntegral x)
                (0 :: Word64) bytes
           in Right (fromIntegral w)

-- | Alias for 'int64Serde' — matches Java's @LongSerializer@ /
-- @LongDeserializer@.
longSerde :: Serde Int64
longSerde = int64Serde

-- | IEEE-754 big-endian double. Matches @DoubleSerializer@ on the JVM.
doubleSerde :: Serde Double
doubleSerde = imap castDoubleToWord64 castWord64ToDouble word64Serde

-- | IEEE-754 big-endian float. Matches @FloatSerializer@ on the JVM.
floatSerde :: Serde Float
floatSerde = imap castFloatToWord32 castWord32ToFloat word32Serde

word64Serde :: Serde Word64
word64Serde = Serde
  (serialize int64Serde . fromIntegral)
  (fmap fromIntegral . deserialize int64Serde)

word32Serde :: Serde Word32
word32Serde = Serde
  (serialize int32Serde . fromIntegral)
  (fmap fromIntegral . deserialize int32Serde)

-- | Big-endian 16-byte UUID encoding (matches @UUIDSerializer@).
uuidSerde :: Serde UUID
uuidSerde = Serde
  { serialize   = BL.toStrict . UUID.toByteString
  , deserialize = \b ->
      case UUID.fromByteString (BL.fromStrict b) of
        Just u  -> Right u
        Nothing -> Left "uuidSerde: not a valid 16-byte UUID"
  }

-- | Alias for 'byteStringSerde' (matches Java's
-- @ByteArraySerializer@/@ByteArrayDeserializer@).
byteArraySerde :: Serde ByteString
byteArraySerde = byteStringSerde

-- | Aeson 'Aeson.ToJSON' / 'Aeson.FromJSON'-backed serde.
jsonSerde :: (Aeson.ToJSON a, Aeson.FromJSON a) => Serde a
jsonSerde = Serde
  { serialize   = BL.toStrict . Aeson.encode
  , deserialize = Aeson.eitherDecodeStrict
  }

-- | Length-prefix the value with a 32-bit big-endian byte-count, then
-- the serialised value. Useful for composite serdes (e.g. windowed
-- keys) that need a self-delimiting framing.
lengthPrefixedSerde :: Serde a -> Serde a
lengthPrefixedSerde inner = Serde
  { serialize = \a ->
      let payload = serialize inner a
          n       = fromIntegral (BS.length payload) :: Word32
       in BL.toStrict
            $ BB.toLazyByteString
            $ BB.word32BE n <> BB.byteString payload
  , deserialize = \b ->
      if BS.length b < 4
        then Left "lengthPrefixedSerde: truncated header"
        else
          let header = BS.take 4 b
              rest   = BS.drop 4 b
              hi a   = fromIntegral (BS.index header a) :: Word32
              n      = (hi 0 `shiftL` 24)
                   .|. (hi 1 `shiftL` 16)
                   .|. (hi 2 `shiftL` 8)
                   .|.  hi 3
           in if fromIntegral n /= BS.length rest
                then Left
                  $ "lengthPrefixedSerde: declared "
                  <> show n <> " bytes, payload had "
                  <> show (BS.length rest)
                else deserialize inner rest
  }

-- | Tag a serde with a static prefix byte. Useful for distinguishing
-- variants in store keyspaces.
prefixedSerde :: Word8 -> Serde a -> Serde a
prefixedSerde tag inner = Serde
  { serialize   = \a -> BS.cons tag (serialize inner a)
  , deserialize = \b ->
      case BS.uncons b of
        Nothing       -> Left "prefixedSerde: empty input"
        Just (t, rest)
          | t == tag  -> deserialize inner rest
          | otherwise -> Left
              $ "prefixedSerde: expected tag "
              <> show tag <> ", got " <> show t
  }

-- | Convenience: serialise a @(key, value)@ pair through a
-- key+value serde pair.
serializeRecord
  :: Serde k
  -> Serde v
  -> Maybe k
  -> v
  -> (Maybe ByteString, ByteString)
serializeRecord ks vs mk v =
  ( serialize ks <$> mk
  , serialize vs v
  )

deserializeRecord
  :: Serde k
  -> Serde v
  -> Maybe ByteString
  -> ByteString
  -> Either String (Maybe k, v)
deserializeRecord ks vs mkb vb = do
  k <- maybe (Right Nothing) (fmap Just . deserialize ks) mkb
  v <- deserialize vs vb
  Right (k, v)
