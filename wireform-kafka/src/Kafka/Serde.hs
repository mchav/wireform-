{-# LANGUAGE BangPatterns #-}
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
  * 'int16Serde' / 'int32Serde' / 'int64Serde' \/ 'longSerde'
  * 'word16Serde' / 'word32Serde' / 'word64Serde'
  * 'doubleSerde' / 'floatSerde'
  * 'voidSerde' — for keyless records
  * 'uuidSerde'
  * 'byteArraySerde'
  * 'jsonSerde' — Aeson 'ToJSON' \/ 'FromJSON' bridge

The numeric serdes use the GHC 'Data.Word.byteSwap16' /
'Data.Word.byteSwap32' \/ 'Data.Word.byteSwap64' primops paired
with a single unaligned word load / store — same shape the
protocol layer ("Kafka.Protocol.Wire") uses — so a 4-byte big-endian
write compiles to one @MOV@ + one @BSWAP@ on x86-64 (and one
@STR@ + one @REV@ on ARM64). On a big-endian host the byte-swap
is the identity, which 'targetByteOrder' constant-folds away.

For schema-driven typed serdes, see "Kafka.Serde.Proto" and
"Kafka.Serde.Avro".

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
  , mkSerde
  , unsafeSerde
  , imap
    -- * Type class
  --
  -- 'HasSerde' supplies the /default/ wire codec for a type. The
  -- streams DSL resolves serdes through this class at every
  -- type-changing operator (e.g. 'mapValues' picks up the new
  -- value's 'Serde' via @'HasSerde' v'@). For non-default
  -- encodings, every operator has a @'With'@ variant that
  -- accepts an explicit 'Serde' (or a wrapping config like
  -- 'Produced' \/ 'Repartitioned' \/ 'Grouped').
  , HasSerde (..)
    -- * Built-ins
  , byteStringSerde
  , textSerde
  , utf8Serde
  , int16Serde
  , int32Serde
  , int64Serde
  , longSerde
  , word16Serde
  , word32Serde
  , word64Serde
  , doubleSerde
  , floatSerde
  , voidSerde
  , uuidSerde
  , byteArraySerde
  , byteBufferSerde
  , bytesSerde
  , listSerde
  , jsonSerde
    -- * Combinators
  , prefixedSerde
  , lengthPrefixedSerde
    -- * Record helpers
  , serializeRecord
  , deserializeRecord
  ) where

import qualified Data.Aeson           as Aeson
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe   as BSU
import           Data.Int             (Int16, Int32, Int64)
import qualified Data.Text            as T
import           Data.Text            (Text)
import qualified Data.Text.Encoding   as TE
import qualified Data.UUID            as UUID
import           Data.UUID            (UUID)
import           Data.Word            (Word8, Word16, Word32, Word64, byteSwap16, byteSwap32, byteSwap64)
import           Foreign.Ptr          (Ptr, castPtr)
import           Foreign.Storable     (peek, poke)
import           GHC.ByteOrder        (ByteOrder (..), targetByteOrder)
import           GHC.Float            (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble)
import           GHC.Generics         (Generic)
import           GHC.IO               (unsafePerformIO)

-- | Bidirectional codec.  'deserialize' returns 'Either' so callers
-- can route failures through a deserialisation handler.
data Serde a = Serde
  { serialize   :: a -> ByteString
  , deserialize :: ByteString -> Either String a
  }
  deriving stock (Generic)

-- | Convenience builder for a total deserialiser.
mkSerde :: (a -> ByteString) -> (ByteString -> a) -> Serde a
mkSerde s d = Serde s (Right . d)

-- | Types whose default wire codec is implicit. Used by the
-- streams DSL to resolve serdes at every type-changing
-- operation. Provide an instance to make a type usable without
-- threading an explicit 'Serde' through every operator; for
-- alternative codecs over the same Haskell type, wrap with a
-- @newtype@ that supplies its own 'HasSerde' instance, or
-- reach for a @*With@ variant that takes an explicit 'Serde'.
class HasSerde a where
  serde :: Serde a

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
byteStringSerde = mkSerde id id

----------------------------------------------------------------------
-- HasSerde instances for the shipped built-in serdes
--
-- The streams DSL relies on these to resolve serdes implicitly at
-- type-changing operators. The instance set mirrors the @builtIn@
-- export list above; the orphan-instance rule keeps them in this
-- module rather than scattered through the codebase.
----------------------------------------------------------------------

instance HasSerde ByteString where serde = byteStringSerde
instance HasSerde Text       where serde = textSerde
instance HasSerde ()         where serde = voidSerde
instance HasSerde Int16      where serde = int16Serde
instance HasSerde Int32      where serde = int32Serde
instance HasSerde Int64      where serde = int64Serde
instance HasSerde Word16     where serde = word16Serde
instance HasSerde Word32     where serde = word32Serde
instance HasSerde Word64     where serde = word64Serde
instance HasSerde Double     where serde = doubleSerde
instance HasSerde Float      where serde = floatSerde
instance HasSerde UUID       where serde = uuidSerde

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

----------------------------------------------------------------------
-- Big-endian numeric primitives
--
-- These serdes use the GHC 'byteSwap16' \/ 'byteSwap32' \/
-- 'byteSwap64' primops (which compile to a single @bswap@
-- instruction on x86-64 and a @rev@ instruction on ARM64)
-- paired with a single unaligned word load / store.
--
-- The same pattern the protocol layer uses ("Kafka.Protocol.Wire")
-- — one MOV + one BSWAP instead of N byte loads + N shifts. The
-- target-endianness branch is a compile-time case on
-- 'targetByteOrder' so it constant-folds to either
--
--   * a plain 'peek' / 'poke'              (BE host: no-op)
--   * a 'peek' \/ 'poke' with 'byteSwap' (LE host: one instr)
--
-- at the call site.
----------------------------------------------------------------------

-- | Read a host-endian word from a 'ByteString' of the right
-- size. Uses 'Foreign.Storable.peek' on a 'Ptr Word_' aliased onto
-- the ByteString's payload; consistent with 'Kafka.Protocol.Wire'.
{-# INLINE peekHostWord32 #-}
peekHostWord32 :: ByteString -> Word32
peekHostWord32 !bs = unsafePerformIO $
  BSU.unsafeUseAsCString bs $ \p ->
    peek (castPtr p :: Ptr Word32)

{-# INLINE peekHostWord64 #-}
peekHostWord64 :: ByteString -> Word64
peekHostWord64 !bs = unsafePerformIO $
  BSU.unsafeUseAsCString bs $ \p ->
    peek (castPtr p :: Ptr Word64)

{-# INLINE peekHostWord16 #-}
peekHostWord16 :: ByteString -> Word16
peekHostWord16 !bs = unsafePerformIO $
  BSU.unsafeUseAsCString bs $ \p ->
    peek (castPtr p :: Ptr Word16)

-- | Write a host-endian word into a freshly allocated 'ByteString'.
{-# INLINE pokeHostWord32 #-}
pokeHostWord32 :: Word32 -> ByteString
pokeHostWord32 !w = BSI.unsafeCreate 4 $ \p ->
  poke (castPtr p :: Ptr Word32) w

{-# INLINE pokeHostWord64 #-}
pokeHostWord64 :: Word64 -> ByteString
pokeHostWord64 !w = BSI.unsafeCreate 8 $ \p ->
  poke (castPtr p :: Ptr Word64) w

{-# INLINE pokeHostWord16 #-}
pokeHostWord16 :: Word16 -> ByteString
pokeHostWord16 !w = BSI.unsafeCreate 2 $ \p ->
  poke (castPtr p :: Ptr Word16) w

-- | @hostToBE32 w@ flips the bytes of @w@ on a little-endian host
-- (one @bswap@ instruction); identity on a big-endian host
-- (compile-time constant fold).
{-# INLINE hostToBE32 #-}
hostToBE32 :: Word32 -> Word32
hostToBE32 = case targetByteOrder of
  BigEndian    -> id
  LittleEndian -> byteSwap32

{-# INLINE hostToBE64 #-}
hostToBE64 :: Word64 -> Word64
hostToBE64 = case targetByteOrder of
  BigEndian    -> id
  LittleEndian -> byteSwap64

{-# INLINE hostToBE16 #-}
hostToBE16 :: Word16 -> Word16
hostToBE16 = case targetByteOrder of
  BigEndian    -> id
  LittleEndian -> byteSwap16

-- | Big-endian 'Int32' serde. Matches @IntegerSerializer@ on the JVM
-- (which writes a 4-byte big-endian two's-complement value).
int32Serde :: Serde Int32
int32Serde = Serde
  { serialize   = pokeHostWord32 . hostToBE32 . fromIntegral
  , deserialize = \b -> if BS.length b /= 4
      then Left "int32Serde: expected 4 bytes"
      else Right $! fromIntegral (hostToBE32 (peekHostWord32 b))
  }

-- | Big-endian 'Int64' serde. Matches @LongSerializer@ on the JVM.
int64Serde :: Serde Int64
int64Serde = Serde
  { serialize   = pokeHostWord64 . hostToBE64 . fromIntegral
  , deserialize = \b -> if BS.length b /= 8
      then Left "int64Serde: expected 8 bytes"
      else Right $! fromIntegral (hostToBE64 (peekHostWord64 b))
  }

-- | Big-endian 'Int16' serde. Matches @ShortSerializer@ on the JVM.
int16Serde :: Serde Int16
int16Serde = Serde
  { serialize   = pokeHostWord16 . hostToBE16 . fromIntegral
  , deserialize = \b -> if BS.length b /= 2
      then Left "int16Serde: expected 2 bytes"
      else Right $! fromIntegral (hostToBE16 (peekHostWord16 b))
  }

-- | Alias for 'int64Serde' — matches Java's @LongSerializer@ /
-- @LongDeserializer@.
longSerde :: Serde Int64
longSerde = int64Serde

-- | IEEE-754 big-endian double. Matches @DoubleSerializer@ on the JVM.
-- Encoded as the raw 64-bit bit pattern via 'castDoubleToWord64'
-- (no allocation, no boxing).
doubleSerde :: Serde Double
doubleSerde = imap castDoubleToWord64 castWord64ToDouble word64Serde

-- | IEEE-754 big-endian float. Matches @FloatSerializer@ on the JVM.
floatSerde :: Serde Float
floatSerde = imap castFloatToWord32 castWord32ToFloat word32Serde

word64Serde :: Serde Word64
word64Serde = Serde
  { serialize   = pokeHostWord64 . hostToBE64
  , deserialize = \b -> if BS.length b /= 8
      then Left "word64Serde: expected 8 bytes"
      else Right $! hostToBE64 (peekHostWord64 b)
  }

word32Serde :: Serde Word32
word32Serde = Serde
  { serialize   = pokeHostWord32 . hostToBE32
  , deserialize = \b -> if BS.length b /= 4
      then Left "word32Serde: expected 4 bytes"
      else Right $! hostToBE32 (peekHostWord32 b)
  }

word16Serde :: Serde Word16
word16Serde = Serde
  { serialize   = pokeHostWord16 . hostToBE16
  , deserialize = \b -> if BS.length b /= 2
      then Left "word16Serde: expected 2 bytes"
      else Right $! hostToBE16 (peekHostWord16 b)
  }

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

-- | Alias for 'byteStringSerde'. Mirrors Java's
-- @ByteBufferSerializer@/@ByteBufferDeserializer@ — the Java
-- variant lets callers re-use a pre-allocated NIO @ByteBuffer@;
-- the Haskell side already owns immutable 'ByteString's, so the
-- shape collapses to the same codec.
byteBufferSerde :: Serde ByteString
byteBufferSerde = byteStringSerde

-- | Alias for 'byteStringSerde'. Mirrors Java's
-- @BytesSerializer@/@BytesDeserializer@ (Kafka's own opaque
-- byte-array wrapper).
bytesSerde :: Serde ByteString
bytesSerde = byteStringSerde

-- | Length-prefixed list serde. Each element is encoded by the
-- supplied @inner@ serde with a 4-byte big-endian length
-- prefix. Mirrors Java's @ListSerializer@/@ListDeserializer@.
--
-- The wire shape is: @count :: Int32 BE@ followed by @count@
-- elements, each prefixed by its own @len :: Int32 BE@.
listSerde :: Serde a -> Serde [a]
listSerde inner = Serde
  { serialize = \xs ->
      let !cnt = serialize int32Serde (fromIntegral (length xs))
          !els = BS.concat (map (lengthPrefixSerialize inner) xs)
       in BS.append cnt els
  , deserialize = \bs0 -> do
      (n, rest0) <- splitInt32 bs0
      goN (fromIntegral n) rest0 []
  }
  where
    lengthPrefixSerialize :: Serde a -> a -> ByteString
    lengthPrefixSerialize s a =
      let !bs = serialize s a
          !len = fromIntegral (BS.length bs) :: Int32
       in BS.append (serialize int32Serde len) bs

    splitInt32 bs
      | BS.length bs < 4 = Left "listSerde: truncated length prefix"
      | otherwise =
          let (h, t) = BS.splitAt 4 bs
           in case deserialize int32Serde h of
                Left e  -> Left e
                Right n -> Right (n, t)

    goN 0 _ acc = Right (reverse acc)
    goN _ bs _ | BS.null bs = Left "listSerde: truncated element"
    goN k bs acc = do
      (len, rest) <- splitInt32 bs
      let (h, t) = BS.splitAt (fromIntegral len) rest
      if BS.length h /= fromIntegral len
        then Left "listSerde: element shorter than length prefix"
        else case deserialize inner h of
          Left e  -> Left e
          Right x -> goN (k - 1) t (x : acc)

-- | Aeson 'Aeson.ToJSON' / 'Aeson.FromJSON'-backed serde.
jsonSerde :: (Aeson.ToJSON a, Aeson.FromJSON a) => Serde a
jsonSerde = Serde
  { serialize   = BL.toStrict . Aeson.encode
  , deserialize = Aeson.eitherDecodeStrict
  }

-- | Length-prefix the value with a 32-bit big-endian byte-count, then
-- the serialised value. Useful for composite serdes (e.g. windowed
-- keys) that need a self-delimiting framing.
--
-- Uses the same 'byteSwap32' + single-word load/store pattern as
-- the primitive serdes for the 4-byte header.
lengthPrefixedSerde :: Serde a -> Serde a
lengthPrefixedSerde inner = Serde
  { serialize = \a ->
      let payload = serialize inner a
          n       = fromIntegral (BS.length payload) :: Word32
       in pokeHostWord32 (hostToBE32 n) <> payload
  , deserialize = \b ->
      if BS.length b < 4
        then Left "lengthPrefixedSerde: truncated header"
        else
          let header = BS.take 4 b
              rest   = BS.drop 4 b
              n      = hostToBE32 (peekHostWord32 header)
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
