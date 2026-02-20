{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Low-level, high-performance wire format decoding primitives.
--
-- The decoder monad uses CPS (Continuation-Passing Style) to eliminate
-- allocation of intermediate result values on every bind. This is the
-- same technique used by high-performance parsing libraries like attoparsec.
--
-- Each decoder operation takes success and failure continuations directly,
-- so the happy path (which is the hot path) never allocates a sum type.
module Proto.Wire.Decode
  ( -- * Decode monad (CPS)
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

    -- * Low-level CPS access (for generated code)
  , runDecoder'
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

-- | Legacy result type, kept for compatibility with runDecoder'.
data DecodeResult a
  = DecodeOK !a {-# UNPACK #-} !Int
  | DecodeFail !DecodeError
  deriving stock (Show)

-- | CPS-transformed decoder monad.
--
-- Instead of returning an allocated DecodeResult on every >>=,
-- the decoder takes success and failure continuations. This means
-- the happy path (successful decode) never allocates intermediate
-- sum type constructors — the result flows directly through
-- continuation calls.
--
-- This is the Church encoding of the DecodeResult type:
-- instead of constructing DecodeOK/DecodeFail and pattern matching,
-- we pass the two "constructors" as function arguments.
newtype Decoder a = Decoder
  { unDecoder :: forall r.
       ByteString                   -- input buffer
    -> Int                          -- current offset
    -> (a -> Int -> r)              -- success continuation (value, new offset)
    -> (DecodeError -> r)           -- failure continuation
    -> r
  }

instance Functor Decoder where
  fmap f (Decoder g) = Decoder $ \bs off ok err ->
    g bs off (\a off' -> ok (f a) off') err
  {-# INLINE fmap #-}

instance Applicative Decoder where
  pure a = Decoder $ \_ off ok _ -> ok a off
  {-# INLINE pure #-}
  Decoder f <*> Decoder g = Decoder $ \bs off ok err ->
    f bs off (\fab off' -> g bs off' (\a off'' -> ok (fab a) off'') err) err
  {-# INLINE (<*>) #-}

instance Monad Decoder where
  Decoder g >>= f = Decoder $ \bs off ok err ->
    g bs off (\a off' -> unDecoder (f a) bs off' ok err) err
  {-# INLINE (>>=) #-}

-- | Run a decoder on a ByteString, producing Either.
runDecoder :: Decoder a -> ByteString -> Either DecodeError a
runDecoder (Decoder f) bs =
  f bs 0
    (\a off -> if off == BS.length bs then Right a else Left ExtraBytes)
    Left
{-# INLINE runDecoder #-}

-- | Run a decoder returning the legacy DecodeResult (for internal use).
runDecoder' :: Decoder a -> ByteString -> Int -> DecodeResult a
runDecoder' (Decoder f) bs off =
  f bs off DecodeOK DecodeFail
{-# INLINE runDecoder' #-}

-- FFI imports for C-optimized decode functions
-- C FFI declarations for SIMD-optimized decode functions.
-- These are used as fallback for complex varints; the inline Haskell
-- fast path handles the 1-2 byte common case without FFI overhead.

-- | Decode a varint using the C FFI fast path with SWAR optimization.
getVarint :: Decoder Word64
getVarint = Decoder $ \bs off ok err ->
  let len = BS.length bs
  in if off >= len
     then err UnexpectedEnd
     else
       -- Inline fast path for 1-byte varints (most common: tags, booleans, small ints)
       let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word64
       in if b0 < 0x80
          then ok b0 (off + 1)
          else if off + 1 >= len
               then err UnexpectedEnd
               else
                 let !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word64
                 in if b1 < 0x80
                    then ok ((b0 .&. 0x7F) .|. (b1 `shiftL` 7)) (off + 2)
                    else getVarintSlow bs off ok err
{-# INLINE getVarint #-}

-- Slow path for 3+ byte varints
getVarintSlow :: ByteString -> Int
  -> (Word64 -> Int -> r) -> (DecodeError -> r) -> r
getVarintSlow bs off ok err = go 0 0 off
  where
    len = BS.length bs
    go !acc !shift !pos
      | shift > 63 = err InvalidVarint
      | pos >= len = err UnexpectedEnd
      | otherwise  =
          let !b = BSU.unsafeIndex bs pos
              !val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then ok val (pos + 1)
             else go val (shift + 7) (pos + 1)
{-# INLINE getVarintSlow #-}

getVarintSigned :: Decoder Int64
getVarintSigned = fromIntegral <$> getVarint
{-# INLINE getVarintSigned #-}

getSVarint32 :: Decoder Int32
getSVarint32 = unZigZag32 . fromIntegral <$> getVarint
{-# INLINE getSVarint32 #-}

getSVarint64 :: Decoder Int64
getSVarint64 = unZigZag64 <$> getVarint
{-# INLINE getSVarint64 #-}

unZigZag32 :: Word32 -> Int32
unZigZag32 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag32 #-}

unZigZag64 :: Word64 -> Int64
unZigZag64 n = fromIntegral ((n `shiftR` 1) `xor` negate (n .&. 1))
{-# INLINE unZigZag64 #-}

-- | Decode a fixed32 (little-endian). Uses direct memory access.
getFixed32 :: Decoder Word32
getFixed32 = Decoder $ \bs off ok err ->
  if off + 4 > BS.length bs
  then err UnexpectedEnd
  else
    let !b0 = fromIntegral (BSU.unsafeIndex bs off) :: Word32
        !b1 = fromIntegral (BSU.unsafeIndex bs (off + 1)) :: Word32
        !b2 = fromIntegral (BSU.unsafeIndex bs (off + 2)) :: Word32
        !b3 = fromIntegral (BSU.unsafeIndex bs (off + 3)) :: Word32
        !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
    in ok val (off + 4)
{-# INLINE getFixed32 #-}

-- | Decode a fixed64 (little-endian). Uses direct memory access.
getFixed64 :: Decoder Word64
getFixed64 = Decoder $ \bs off ok err ->
  if off + 8 > BS.length bs
  then err UnexpectedEnd
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
    in ok val (off + 8)
{-# INLINE getFixed64 #-}

getFloat :: Decoder Float
getFloat = castWord32ToFloat <$> getFixed32
{-# INLINE getFloat #-}

getDouble :: Decoder Double
getDouble = castWord64ToDouble <$> getFixed64
{-# INLINE getDouble #-}

-- | Decode a length-delimited field. Zero-copy: returns a ByteString
-- slice of the input buffer.
getLengthDelimited :: Decoder ByteString
getLengthDelimited = Decoder $ \bs off ok err ->
  unDecoder getVarint bs off
    (\lenW off' ->
      let len = fromIntegral lenW
      in if len < 0
         then err NegativeLength
         else if off' + len > BS.length bs
         then err UnexpectedEnd
         else ok (BSU.unsafeTake len (BSU.unsafeDrop off' bs)) (off' + len))
    err
{-# INLINE getLengthDelimited #-}

getByteString :: Decoder ByteString
getByteString = getLengthDelimited
{-# INLINE getByteString #-}

getText :: Decoder Text
getText = Decoder $ \bs off ok err ->
  unDecoder getLengthDelimited bs off
    (\bytes off' ->
      case TE.decodeUtf8' bytes of
        Left _  -> err InvalidUtf8
        Right t -> ok t off')
    err
{-# INLINE getText #-}

getTag :: Decoder Tag
getTag = Decoder $ \bs off ok err ->
  unDecoder getVarint bs off
    (\w off' ->
      case decodeTag w of
        Just tag -> ok tag off'
        Nothing  -> err (InvalidTag w))
    err
{-# INLINE getTag #-}

-- | Try to decode a tag, returning Nothing at end-of-input.
-- CPS: the Nothing case flows directly to the success continuation.
getTagOr :: Decoder (Maybe Tag)
getTagOr = Decoder $ \bs off ok err ->
  if off >= BS.length bs
  then ok Nothing off
  else unDecoder getTag bs off (\tag off' -> ok (Just tag) off') err
{-# INLINE getTagOr #-}

-- | Skip over a field value based on its wire type.
skipField :: WireType -> Decoder ()
skipField = \case
  WireVarint -> skipVarint
  Wire64Bit  -> skip 8
  WireLengthDelimited -> Decoder $ \bs off ok err ->
    unDecoder getVarint bs off
      (\lenW off' ->
        let len = fromIntegral lenW
        in if off' + len > BS.length bs
           then err UnexpectedEnd
           else ok () (off' + len))
      err
  WireStartGroup -> skipGroup
  WireEndGroup   -> pure ()
  Wire32Bit  -> skip 4

skip :: Int -> Decoder ()
skip n = Decoder $ \bs off ok err ->
  if off + n > BS.length bs
  then err UnexpectedEnd
  else ok () (off + n)
{-# INLINE skip #-}

skipVarint :: Decoder ()
skipVarint = Decoder $ \bs off ok err ->
  let go !pos
        | pos >= BS.length bs = err UnexpectedEnd
        | BSU.unsafeIndex bs pos < 0x80 = ok () (pos + 1)
        | otherwise = go (pos + 1)
  in go off
{-# INLINE skipVarint #-}

skipGroup :: Decoder ()
skipGroup = Decoder $ \bs off ok err ->
  unDecoder getTagOr bs off
    (\mt off' -> case mt of
      Nothing -> err UnexpectedEnd
      Just (Tag _ WireEndGroup) -> ok () off'
      Just (Tag _ wt) -> unDecoder (skipField wt >> skipGroup) bs off' ok err)
    err
