{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
-- | High-level decoding interface for protobuf messages.
--
-- This module provides the 'MessageDecode' typeclass and utilities for
-- decoding messages from 'ByteString'. Key performance characteristics:
--
-- * Zero-copy: bytes and string fields reference slices of the input
-- * Lazy submessage decoding: submessage bytes are captured but not parsed
--   until the field is accessed
-- * Packed repeated field support
-- * Efficient unknown field skipping
module Proto.Decode
  ( -- * Decoding typeclass
    MessageDecode (..)

    -- * Running decoders
  , decodeMessage

    -- * Field decoding helpers
  , decodeFieldVarint
  , decodeFieldSVarint32
  , decodeFieldSVarint64
  , decodeFieldFixed32
  , decodeFieldFixed64
  , decodeFieldFloat
  , decodeFieldDouble
  , decodeFieldBool
  , decodeFieldString
  , decodeFieldBytes
  , decodeFieldMessage
  , decodeFieldEnum

    -- * Packed repeated field decoding
  , decodePackedVarint
  , decodePackedFixed32
  , decodePackedFixed64
  , decodePackedFloat
  , decodePackedDouble
  , decodePackedSVarint32
  , decodePackedSVarint64

    -- * Submessage decoding
  , decodeSubmessage

    -- * Lazy submessage decoding
  , LazyMessage (..)
  , forceLazyMessage
  , decodeFieldLazyMessage

    -- * Unknown field preservation
  , UnknownField (..)
  , captureUnknownField
  , encodeUnknownFields

    -- * Re-exports for generated code
  , Decoder
  , DecodeResult (..)
  , DecodeError (..)
  , runDecoder
  , getVarint
  , getVarintSigned
  , getTagOr
  , getTag
  , skipField
  , getLengthDelimited
  , getFixed32
  , getFixed64
  , getFloat
  , getDouble
  , getText
  , getSVarint32
  , getSVarint64

    -- * Map entry decoding
  , decodeMapEntry

    -- * CPS failure
  , decodeFail

    -- * Unknown field sizes
  , unknownFieldsSize

    -- * Unboxed internal types (re-exported for generated code)
  , UMaybe (UJust, UNothing)
  , umaybe
  , getTagOrU

    -- * Three-way tag CPS (flattened, zero-allocation decode loop)
  , TagResult#
  , withTag

    -- * Combined three-way result type
  , DecRes# (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import Data.Int (Int32, Int64)
import Control.DeepSeq (NFData(..))
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as MVU
import Data.Word (Word32, Word64)
import Control.Monad.ST (runST)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Proto.Wire (Tag(..), WireType (..))
import Proto.Wire.Decode
import Proto.Wire.Encode (putTag, putVarint, putFixed32, putFixed64, putLengthDelimited, varintSize, tagSize)
import Proto.Wire.FFI (countPackedVarints, packedAllSingleByte)
import Proto.Wire.Result

-- | Typeclass for types that can be decoded from protobuf wire format.
class MessageDecode a where
  -- | Decode a message from a ByteString, starting from default field values.
  -- The decoder should consume all bytes of the submessage.
  messageDecoder :: Decoder a

-- | Decode a message from a strict 'ByteString'.
decodeMessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeMessage = runDecoder messageDecoder
{-# INLINE decodeMessage #-}

-- | Decode a varint value.
decodeFieldVarint :: Decoder Word64
decodeFieldVarint = getVarint
{-# INLINE decodeFieldVarint #-}

-- | Decode a sint32 value.
decodeFieldSVarint32 :: Decoder Int32
decodeFieldSVarint32 = getSVarint32
{-# INLINE decodeFieldSVarint32 #-}

-- | Decode a sint64 value.
decodeFieldSVarint64 :: Decoder Int64
decodeFieldSVarint64 = getSVarint64
{-# INLINE decodeFieldSVarint64 #-}

-- | Decode a fixed32 value.
decodeFieldFixed32 :: Decoder Word32
decodeFieldFixed32 = getFixed32
{-# INLINE decodeFieldFixed32 #-}

-- | Decode a fixed64 value.
decodeFieldFixed64 :: Decoder Word64
decodeFieldFixed64 = getFixed64
{-# INLINE decodeFieldFixed64 #-}

-- | Decode a float value.
decodeFieldFloat :: Decoder Float
decodeFieldFloat = getFloat
{-# INLINE decodeFieldFloat #-}

-- | Decode a double value.
decodeFieldDouble :: Decoder Double
decodeFieldDouble = getDouble
{-# INLINE decodeFieldDouble #-}

-- | Decode a bool value.
decodeFieldBool :: Decoder Bool
decodeFieldBool = do
  v <- getVarint
  pure (v /= 0)
{-# INLINE decodeFieldBool #-}

-- | Decode a string value (validated UTF-8, zero-copy).
decodeFieldString :: Decoder Text
decodeFieldString = getText
{-# INLINE decodeFieldString #-}

-- | Decode a bytes value (zero-copy).
decodeFieldBytes :: Decoder ByteString
decodeFieldBytes = getByteString
{-# INLINE decodeFieldBytes #-}

-- | Decode a submessage field. The length-delimited bytes are read and then
-- the submessage decoder is run on them.
decodeFieldMessage :: MessageDecode a => Decoder a
decodeFieldMessage = do
  bs <- getLengthDelimited
  case runDecoder messageDecoder bs of
    Left e  -> decodeFail (SubMessageError e)
    Right a -> pure a
{-# INLINE decodeFieldMessage #-}

-- | CPS-compatible failure: calls the error continuation.
decodeFail :: DecodeError -> Decoder a
decodeFail e = Decoder $ \_ _ -> (# | e #)
{-# INLINE decodeFail #-}

-- | Decode an enum field (as varint, then fromEnum).
decodeFieldEnum :: Enum a => Decoder a
decodeFieldEnum = toEnum . fromIntegral <$> getVarint
{-# INLINE decodeFieldEnum #-}

-- | Decode a submessage from raw bytes.
decodeSubmessage :: MessageDecode a => ByteString -> Either DecodeError a
decodeSubmessage = runDecoder messageDecoder
{-# INLINE decodeSubmessage #-}

-- Packed decoders: decode a length-delimited chunk containing multiple values.

-- | Decode packed varint values.
decodePackedVarint :: Decoder (VU.Vector Word64)
decodePackedVarint = do
  bs <- getLengthDelimited
  case decodeAllVarints bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedVarint #-}

-- | Decode all varints from a packed buffer.
--
-- Optimizations borrowed from hyperpb (mcyoung.xyz/2025/07/16/hyperpb):
--
--  1. SWAR pre-count: use the C SWAR routine to count terminator bytes
--     in one pass, then allocate the output vector to exact size upfront.
--     This avoids grow-and-copy or list-reversal overhead.
--
--  2. Single-byte zero-copy: when every varint is one byte (values 0-127),
--     each byte IS the value. We skip varint parsing entirely and just
--     widen bytes to Word64. This is the common case for enum fields,
--     small indices, and boolean-like repeated fields.
decodeAllVarints :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllVarints bs
  | BS.null bs = Right VU.empty
  | packedAllSingleByte bs =
      Right $! VU.generate (BS.length bs) (\i -> fromIntegral (BSU.unsafeIndex bs i))
  | otherwise =
      let !n = countPackedVarints bs
      in Right $! runST $ do
        mv <- MVU.unsafeNew n
        let len = BS.length bs
            go !idx !off
              | off >= len = pure ()
              | otherwise = case runDecoder' getVarint bs off of
                  DecodeOK v off' -> do
                    MVU.unsafeWrite mv idx v
                    go (idx + 1) off'
                  DecodeFail _ -> pure ()
        go 0 0
        VU.unsafeFreeze mv

-- | Decode packed fixed32 values.
decodePackedFixed32 :: Decoder (VU.Vector Word32)
decodePackedFixed32 = do
  bs <- getLengthDelimited
  case decodeAllFixed32 bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed32 #-}

-- | Decode packed fixed32 values. Count is known: byteLength / 4.
decodeAllFixed32 :: ByteString -> Either DecodeError (VU.Vector Word32)
decodeAllFixed32 bs
  | r /= 0    = Left (CustomError "packed fixed32: byte length not multiple of 4")
  | otherwise  = Right $ VU.generate n (\i -> readFixed32At bs (i * 4))
  where
    (!n, !r) = BS.length bs `quotRem` 4

readFixed32At :: ByteString -> Int -> Word32
readFixed32At (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word32)
{-# INLINE readFixed32At #-}

-- | Decode packed fixed64 values. Count is known: byteLength / 8.
decodePackedFixed64 :: Decoder (VU.Vector Word64)
decodePackedFixed64 = do
  bs <- getLengthDelimited
  case decodeAllFixed64 bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFixed64 #-}

decodeAllFixed64 :: ByteString -> Either DecodeError (VU.Vector Word64)
decodeAllFixed64 bs
  | r /= 0    = Left (CustomError "packed fixed64: byte length not multiple of 8")
  | otherwise  = Right $ VU.generate n (\i -> readFixed64At bs (i * 8))
  where
    (!n, !r) = BS.length bs `quotRem` 8

readFixed64At :: ByteString -> Int -> Word64
readFixed64At (BSI.BS fp _) off = unsafeDupablePerformIO $
  withForeignPtr fp $ \ptr ->
    peek (castPtr (ptr `plusPtr` off) :: Ptr Word64)
{-# INLINE readFixed64At #-}

-- | Decode packed float values. Count is known: byteLength / 4.
decodePackedFloat :: Decoder (VU.Vector Float)
decodePackedFloat = do
  bs <- getLengthDelimited
  case decodeAllFloat bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedFloat #-}

decodeAllFloat :: ByteString -> Either DecodeError (VU.Vector Float)
decodeAllFloat bs
  | r /= 0    = Left (CustomError "packed float: byte length not multiple of 4")
  | otherwise  = Right $ VU.generate n (\i -> castWord32ToFloat (readFixed32At bs (i * 4)))
  where
    (!n, !r) = BS.length bs `quotRem` 4

-- | Decode packed double values. Count is known: byteLength / 8.
decodePackedDouble :: Decoder (VU.Vector Double)
decodePackedDouble = do
  bs <- getLengthDelimited
  case decodeAllDouble bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedDouble #-}

decodeAllDouble :: ByteString -> Either DecodeError (VU.Vector Double)
decodeAllDouble bs
  | r /= 0    = Left (CustomError "packed double: byte length not multiple of 8")
  | otherwise  = Right $ VU.generate n (\i -> castWord64ToDouble (readFixed64At bs (i * 8)))
  where
    (!n, !r) = BS.length bs `quotRem` 8

-- | Decode packed sint32 values.
decodePackedSVarint32 :: Decoder (VU.Vector Int32)
decodePackedSVarint32 = do
  bs <- getLengthDelimited
  case decodeAllSVarint32 bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint32 #-}

decodeAllSVarint32 :: ByteString -> Either DecodeError (VU.Vector Int32)
decodeAllSVarint32 bs
  | BS.null bs = Right VU.empty
  | otherwise =
      let !n = countPackedVarints bs
      in Right $! runST $ do
        mv <- MVU.unsafeNew n
        let len = BS.length bs
            go !idx !off
              | off >= len = pure ()
              | otherwise = case runDecoder' getSVarint32 bs off of
                  DecodeOK v off' -> do
                    MVU.unsafeWrite mv idx v
                    go (idx + 1) off'
                  DecodeFail _ -> pure ()
        go 0 0
        VU.unsafeFreeze mv

-- | Decode packed sint64 values.
decodePackedSVarint64 :: Decoder (VU.Vector Int64)
decodePackedSVarint64 = do
  bs <- getLengthDelimited
  case decodeAllSVarint64 bs of
    Left e   -> decodeFail e
    Right vs -> pure vs
{-# INLINE decodePackedSVarint64 #-}

decodeAllSVarint64 :: ByteString -> Either DecodeError (VU.Vector Int64)
decodeAllSVarint64 bs
  | BS.null bs = Right VU.empty
  | otherwise =
      let !n = countPackedVarints bs
      in Right $! runST $ do
        mv <- MVU.unsafeNew n
        let len = BS.length bs
            go !idx !off
              | off >= len = pure ()
              | otherwise = case runDecoder' getSVarint64 bs off of
                  DecodeOK v off' -> do
                    MVU.unsafeWrite mv idx v
                    go (idx + 1) off'
                  DecodeFail _ -> pure ()
        go 0 0
        VU.unsafeFreeze mv

-- | A lazily-decoded submessage. The raw bytes are captured during the
-- parent message decode, but the actual submessage parsing is deferred
-- until 'forceLazyMessage' is called. This is a key performance
-- optimization from the Buf protobuf performance guide: if the consumer
-- never accesses a submessage field, the decode cost is zero.
data LazyMessage a = LazyMessage
  { lazyRawBytes :: !ByteString
  , lazyCached   :: ~(Either DecodeError a)
  }

instance Show a => Show (LazyMessage a) where
  show (LazyMessage bs _) = "LazyMessage (" <> show (BS.length bs) <> " bytes)"

instance Eq a => Eq (LazyMessage a) where
  a == b = lazyRawBytes a == lazyRawBytes b

-- | Force a lazy message, decoding the raw bytes.
forceLazyMessage :: LazyMessage a -> Either DecodeError a
forceLazyMessage = lazyCached
{-# INLINE forceLazyMessage #-}

-- | Decode a submessage field lazily: capture the bytes but defer parsing.
decodeFieldLazyMessage :: MessageDecode a => Decoder (LazyMessage a)
decodeFieldLazyMessage = do
  bs <- getLengthDelimited
  pure LazyMessage
    { lazyRawBytes = bs
    , lazyCached   = runDecoder messageDecoder bs
    }
{-# INLINE decodeFieldLazyMessage #-}

-- | An unknown field captured during decoding for round-trip preservation.
-- Storing unknown fields allows messages to pass through intermediaries
-- without losing data added by newer protocol versions.
data UnknownField
  = UnknownVarint    {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  | UnknownFixed64   {-# UNPACK #-} !Int {-# UNPACK #-} !Word64
  | UnknownFixed32   {-# UNPACK #-} !Int {-# UNPACK #-} !Word32
  | UnknownLenDelim  {-# UNPACK #-} !Int !ByteString
  deriving stock (Show, Eq)

instance NFData UnknownField where
  rnf (UnknownVarint a b) = rnf a `seq` rnf b
  rnf (UnknownFixed64 a b) = rnf a `seq` rnf b
  rnf (UnknownFixed32 a b) = rnf a `seq` rnf b
  rnf (UnknownLenDelim a b) = rnf a `seq` rnf b

-- | Capture an unknown field value during decoding.
captureUnknownField :: Int -> WireType -> Decoder UnknownField
captureUnknownField fn = \case
  WireVarint          -> UnknownVarint fn <$> getVarint
  Wire64Bit           -> UnknownFixed64 fn <$> getFixed64
  Wire32Bit           -> UnknownFixed32 fn <$> getFixed32
  WireLengthDelimited -> UnknownLenDelim fn <$> getLengthDelimited
  wt                  -> skipField wt >> decodeFail (CustomError ("Unsupported unknown wire type: " <> show wt))

-- | Compute the wire-format size of unknown fields.
unknownFieldsSize :: [UnknownField] -> Int
unknownFieldsSize = foldl' (\acc uf -> acc + unknownFieldSize uf) 0
  where
    unknownFieldSize (UnknownVarint fn val) =
      tagSize fn + varintSize val
    unknownFieldSize (UnknownFixed64 fn _) =
      tagSize fn + 8
    unknownFieldSize (UnknownFixed32 fn _) =
      tagSize fn + 4
    unknownFieldSize (UnknownLenDelim fn val) =
      tagSize fn + varintSize (fromIntegral (BS.length val)) + BS.length val

-- | Re-encode unknown fields for round-trip preservation.
encodeUnknownFields :: [UnknownField] -> B.Builder
encodeUnknownFields = foldMap encodeOne
  where
    encodeOne (UnknownVarint fn val) =
      putTag fn WireVarint <> putVarint val
    encodeOne (UnknownFixed64 fn val) =
      putTag fn Wire64Bit <> putFixed64 val
    encodeOne (UnknownFixed32 fn val) =
      putTag fn Wire32Bit <> putFixed32 val
    encodeOne (UnknownLenDelim fn val) =
      putTag fn WireLengthDelimited <> putLengthDelimited val

-- | Decode a map entry (key=field1, value=field2) from a length-delimited chunk.
decodeMapEntry :: Decoder k -> Decoder v -> k -> v -> Decoder (k, v)
decodeMapEntry decK decV = loop
  where
    loop !mk !mv = do
      mt <- getTagOr
      case mt of
        Nothing          -> pure (mk, mv)
        Just (Tag f wt') -> case f of
          1 -> do { kv <- decK; loop kv mv }
          2 -> do { vv <- decV; loop mk vv }
          _ -> skipField wt' >> loop mk mv
