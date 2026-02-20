{-# LANGUAGE BangPatterns #-}
-- | Low-level, high-performance wire format decoding primitives.
--
-- Uses direct 'ByteString' indexing for zero-copy access. Varint decoding
-- is unrolled for the common 1-2 byte case. All parsers operate on a
-- 'DecodeState' that tracks the current position in the input buffer.
module Proto.Wire.Decode
  ( -- * Decode monad
    DecodeResult (..)
  , DecodeError (..)

    -- * Varint decoding
  , getVarint
  , getVarintSigned
  , getSVarint32
  , getSVarint64

    -- * Fixed-width decoding
  , getFixed32
  , getFixed64
  , getFloat
  , getDouble

    -- * Length-delimited
  , getLengthDelimited
  , getByteString
  , getText

    -- * Tags
  , getTag
  , getTagOr

    -- * Skipping unknown fields
  , skipField

    -- * Running a decoder
  , runDecoder
  , Decoder (..)

    -- * ZigZag
  , unZigZag32
  , unZigZag64
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Proto.Wire (Tag (..), WireType (..), decodeTag)

data DecodeError
  = UnexpectedEnd
  | InvalidVarint
  | InvalidTag !Word64
  | InvalidWireType !Int
  | InvalidUtf8
  | NegativeLength
  | ExtraBytes
  | SubMessageError !DecodeError
  | CustomError !String
  deriving stock (Show, Eq)

data DecodeResult a
  = DecodeOK !a {-# UNPACK #-} !Int  -- result + new offset
  | DecodeFail !DecodeError
  deriving stock (Show)

-- | A decoder is a function from ByteString and offset to a DecodeResult.
-- We keep it as a newtype for composition.
newtype Decoder a = Decoder
  { runDecoder' :: ByteString -> Int -> DecodeResult a }

instance Functor Decoder where
  fmap f (Decoder g) = Decoder $ \bs off ->
    case g bs off of
      DecodeOK a off' -> DecodeOK (f a) off'
      DecodeFail e    -> DecodeFail e
  {-# INLINE fmap #-}

instance Applicative Decoder where
  pure a = Decoder $ \_ off -> DecodeOK a off
  {-# INLINE pure #-}
  Decoder f <*> Decoder g = Decoder $ \bs off ->
    case f bs off of
      DecodeOK fab off' -> case g bs off' of
        DecodeOK a off'' -> DecodeOK (fab a) off''
        DecodeFail e     -> DecodeFail e
      DecodeFail e -> DecodeFail e
  {-# INLINE (<*>) #-}

instance Monad Decoder where
  Decoder g >>= f = Decoder $ \bs off ->
    case g bs off of
      DecodeOK a off' -> runDecoder' (f a) bs off'
      DecodeFail e    -> DecodeFail e
  {-# INLINE (>>=) #-}

-- | Run a decoder on a ByteString.
runDecoder :: Decoder a -> ByteString -> Either DecodeError a
runDecoder (Decoder f) bs =
  case f bs 0 of
    DecodeOK a off
      | off == BS.length bs -> Right a
      | otherwise           -> Left ExtraBytes
    DecodeFail e -> Left e

-- | Decode a varint. Unrolled for 1-2 byte common case.
getVarint :: Decoder Word64
getVarint = Decoder $ \bs off ->
  let len = BS.length bs
  in if off >= len
     then DecodeFail UnexpectedEnd
     else
       let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
       in if b0 < 0x80
          then DecodeOK b0 (off + 1)
          else if off + 1 >= len
               then DecodeFail UnexpectedEnd
               else
                 let !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
                 in if b1 < 0x80
                    then DecodeOK ((b0 .&. 0x7F) .|. (b1 `shiftL` 7)) (off + 2)
                    else getVarintSlow bs off
{-# INLINE getVarint #-}

getVarintSlow :: ByteString -> Int -> DecodeResult Word64
getVarintSlow bs = go 0 0
  where
    len = BS.length bs
    go :: Word64 -> Int -> Int -> DecodeResult Word64
    go !acc !shift !off
      | shift > 63 = DecodeFail InvalidVarint
      | off >= len = DecodeFail UnexpectedEnd
      | otherwise  =
          let !b = BSU.unsafeIndex bs off
              !val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then DecodeOK val (off + 1)
             else go val (shift + 7) (off + 1)

-- | Decode a signed varint (int32/int64 in proto, two's complement).
getVarintSigned :: Decoder Int64
getVarintSigned = fromIntegral <$> getVarint
{-# INLINE getVarintSigned #-}

-- | Decode a sint32 (zigzag-encoded).
getSVarint32 :: Decoder Int32
getSVarint32 = unZigZag32 . fromIntegral <$> getVarint
{-# INLINE getSVarint32 #-}

-- | Decode a sint64 (zigzag-encoded).
getSVarint64 :: Decoder Int64
getSVarint64 = unZigZag64 <$> getVarint
{-# INLINE getSVarint64 #-}

-- | Reverse zigzag for 32-bit.
unZigZag32 :: Word32 -> Int32
unZigZag32 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag32 #-}

-- | Reverse zigzag for 64-bit.
unZigZag64 :: Word64 -> Int64
unZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag64 #-}

-- | Decode a fixed32 (little-endian).
getFixed32 :: Decoder Word32
getFixed32 = Decoder $ \bs off ->
  if off + 4 > BS.length bs
  then DecodeFail UnexpectedEnd
  else
    let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
        !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
        !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
        !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
        !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
    in DecodeOK val (off + 4)
{-# INLINE getFixed32 #-}

-- | Decode a fixed64 (little-endian).
getFixed64 :: Decoder Word64
getFixed64 = Decoder $ \bs off ->
  if off + 8 > BS.length bs
  then DecodeFail UnexpectedEnd
  else
    let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
        !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
        !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word64
        !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word64
        !b4 = fromIntegral (BSU.unsafeIndex bs (off + 4)) :: Word64
        !b5 = fromIntegral (BSU.unsafeIndex bs (off + 5)) :: Word64
        !b6 = fromIntegral (BSU.unsafeIndex bs (off + 6)) :: Word64
        !b7 = fromIntegral (BSU.unsafeIndex bs (off + 7)) :: Word64
        !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
           .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
    in DecodeOK val (off + 8)
{-# INLINE getFixed64 #-}

-- | Decode a float.
getFloat :: Decoder Float
getFloat = castWord32ToFloat <$> getFixed32
{-# INLINE getFloat #-}

-- | Decode a double.
getDouble :: Decoder Double
getDouble = castWord64ToDouble <$> getFixed64
{-# INLINE getDouble #-}

-- | Decode a length-delimited field: returns the raw bytes (zero-copy slice).
getLengthDelimited :: Decoder ByteString
getLengthDelimited = do
  len <- fromIntegral <$> getVarint
  Decoder $ \bs off ->
    if len < 0
    then DecodeFail NegativeLength
    else if off + len > BS.length bs
    then DecodeFail UnexpectedEnd
    else DecodeOK (BSU.unsafeTake len (BSU.unsafeDrop off bs)) (off + len)
{-# INLINE getLengthDelimited #-}

-- | Decode a bytes field (zero-copy).
getByteString :: Decoder ByteString
getByteString = getLengthDelimited
{-# INLINE getByteString #-}

-- | Decode a string field (validated UTF-8).
getText :: Decoder Text
getText = do
  bs <- getLengthDelimited
  case TE.decodeUtf8' bs of
    Left _  -> Decoder $ \_ _ -> DecodeFail InvalidUtf8
    Right t -> pure t
{-# INLINE getText #-}

-- | Decode a field tag.
getTag :: Decoder Tag
getTag = do
  w <- getVarint
  case decodeTag w of
    Just tag -> pure tag
    Nothing  -> Decoder $ \_ _ -> DecodeFail (InvalidTag w)
{-# INLINE getTag #-}

-- | Try to decode a tag, returning Nothing at end of input.
getTagOr :: Decoder (Maybe Tag)
getTagOr = Decoder $ \bs off ->
  if off >= BS.length bs
  then DecodeOK Nothing off
  else case runDecoder' getTag bs off of
    DecodeOK tag off' -> DecodeOK (Just tag) off'
    DecodeFail e      -> DecodeFail e
{-# INLINE getTagOr #-}

-- | Skip over a field value based on its wire type.
skipField :: WireType -> Decoder ()
skipField = \case
  WireVarint -> skipVarint
  Wire64Bit  -> skip 8
  WireLengthDelimited -> do
    len <- fromIntegral <$> getVarint
    skip len
  WireStartGroup -> skipGroup
  WireEndGroup   -> pure ()
  Wire32Bit  -> skip 4

skip :: Int -> Decoder ()
skip n = Decoder $ \bs off ->
  if off + n > BS.length bs
  then DecodeFail UnexpectedEnd
  else DecodeOK () (off + n)

skipVarint :: Decoder ()
skipVarint = Decoder $ \bs -> go bs
  where
    go bs !off
      | off >= BS.length bs = DecodeFail UnexpectedEnd
      | BSU.unsafeIndex bs off < 0x80 = DecodeOK () (off + 1)
      | otherwise = go bs (off + 1)

-- Skip a group (deprecated but needed for completeness).
skipGroup :: Decoder ()
skipGroup = do
  mt <- getTagOr
  case mt of
    Nothing -> Decoder $ \_ _ -> DecodeFail UnexpectedEnd
    Just tag -> case tagWireType tag of
      WireEndGroup -> pure ()
      wt           -> skipField wt >> skipGroup
  where
    tagWireType (Tag _ wt) = wt
