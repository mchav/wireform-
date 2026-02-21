{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
-- | Low-level, high-performance wire format decoding primitives.
--
-- The decoder uses unboxed sums for the result type, avoiding heap
-- allocation for intermediate decode results. Each decode operation
-- returns @(# (# a, Int# #) | DecodeError #)@ — either a value with
-- a new offset (on the stack, not heap) or an error.
--
-- This approach is more robust than CPS: it doesn't depend on GHC
-- successfully inlining all continuations, and the result type can
-- be unboxed by GHC. On modern CPUs the branch prediction for the
-- success/failure case split is near-perfect since decoding almost
-- always succeeds.
module Proto.Wire.Decode
  ( -- * Decode result
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

    -- * Low-level access (for generated code)
  , runDecoder'

    -- * Unboxed internal variants (zero-allocation hot path)
  , UMaybe(UJust, UNothing)
  , umaybe
  , getTagOrU

    -- * Three-way tag result (flattened unboxed sum for the decode loop)
  , TagResult#
  , withTag

    -- * Non-throwing UTF-8 validation (C FFI, no catch#)
  , validateUtf8
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, xor)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Internal as BSI
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8, Word32, Word64)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Exts (Int#, Int(I#), (+#), (>=#), isTrue#)
import Control.DeepSeq (NFData(..))

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

instance NFData DecodeError where
  rnf UnexpectedEnd = ()
  rnf InvalidVarint = ()
  rnf (InvalidTag w) = rnf w
  rnf (InvalidWireType i) = rnf i
  rnf InvalidUtf8 = ()
  rnf NegativeLength = ()
  rnf ExtraBytes = ()
  rnf (SubMessageError e) = rnf e
  rnf (CustomError s) = rnf s

-- | Legacy result type, kept for compatibility with runDecoder'.
data DecodeResult a
  = DecodeOK !a {-# UNPACK #-} !Int
  | DecodeFail !DecodeError
  deriving stock (Show)

-- | Decoder monad using unboxed sums for the result.
-- Returns either (value, new_offset) or an error, with no heap
-- allocation for the result envelope.
newtype Decoder a = Decoder
  { runDecoder# :: ByteString -> Int# -> (# (# a, Int# #) | DecodeError #)
  }

instance Functor Decoder where
  fmap f (Decoder g) = Decoder $ \bs off -> case g bs off of
    (# (# a, off' #) | #) -> (# (# f a, off' #) | #)
    (# | e #)              -> (# | e #)
  {-# INLINE fmap #-}

instance Applicative Decoder where
  pure a = Decoder $ \_ off -> (# (# a, off #) | #)
  {-# INLINE pure #-}
  Decoder f <*> Decoder g = Decoder $ \bs off -> case f bs off of
    (# (# fab, off' #) | #) -> case g bs off' of
      (# (# a, off'' #) | #) -> (# (# fab a, off'' #) | #)
      (# | e #)               -> (# | e #)
    (# | e #)                -> (# | e #)
  {-# INLINE (<*>) #-}

instance Monad Decoder where
  Decoder g >>= f = Decoder $ \bs off -> case g bs off of
    (# (# a, off' #) | #) -> runDecoder# (f a) bs off'
    (# | e #)              -> (# | e #)
  {-# INLINE (>>=) #-}

-- | Run a decoder on a ByteString, producing Either.
runDecoder :: Decoder a -> ByteString -> Either DecodeError a
runDecoder (Decoder f) bs =
  case f bs 0# of
    (# (# a, off' #) | #)
      | I# off' == BS.length bs -> Right a
      | otherwise -> Left ExtraBytes
    (# | e #) -> Left e
{-# INLINE runDecoder #-}

-- | Run a decoder returning the legacy DecodeResult (for internal use).
runDecoder' :: Decoder a -> ByteString -> Int -> DecodeResult a
runDecoder' (Decoder f) bs (I# off) =
  case f bs off of
    (# (# a, off' #) | #) -> DecodeOK a (I# off')
    (# | e #)              -> DecodeFail e
{-# INLINE runDecoder' #-}

-- Helpers for offset arithmetic
bsLen :: ByteString -> Int#
bsLen bs = case BS.length bs of I# n -> n
{-# INLINE bsLen #-}

-- | Decode a varint. Inline fast path for 1-2 byte varints (most common).
getVarint :: Decoder Word64
getVarint = Decoder $ \bs off ->
  let len = bsLen bs
  in if isTrue# (off >=# len)
     then (# | UnexpectedEnd #)
     else
       let !b0 = fromIntegral (BSU.unsafeIndex bs (I# off)) :: Word64
       in if b0 < 0x80
          then (# (# b0, off +# 1# #) | #)
          else let off1 = off +# 1#
               in if isTrue# (off1 >=# len)
                  then (# | UnexpectedEnd #)
                  else
                    let !b1 = fromIntegral (BSU.unsafeIndex bs (I# off1)) :: Word64
                    in if b1 < 0x80
                       then (# (# (b0 .&. 0x7F) .|. (b1 `shiftL` 7), off +# 2# #) | #)
                       else getVarintSlow bs off
{-# INLINE getVarint #-}

getVarintSlow :: ByteString -> Int# -> (# (# Word64, Int# #) | DecodeError #)
getVarintSlow bs off0 = go 0 0 off0
  where
    len = bsLen bs
    go :: Word64 -> Int -> Int# -> (# (# Word64, Int# #) | DecodeError #)
    go !acc !shift !pos
      | shift > 63             = (# | InvalidVarint #)
      | isTrue# (pos >=# len) = (# | UnexpectedEnd #)
      | otherwise               =
          let !b = BSU.unsafeIndex bs (I# pos)
              !val = acc .|. ((fromIntegral b .&. 0x7F) `shiftL` shift)
          in if b < 0x80
             then (# (# val, pos +# 1# #) | #)
             else go val (shift + 7) (pos +# 1#)
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

-- | Decode a fixed32 (little-endian).
getFixed32 :: Decoder Word32
getFixed32 = Decoder $ \bs off ->
  if I# (off +# 4#) > BS.length bs
  then (# | UnexpectedEnd #)
  else
    let !i = I# off
        !b0 = fromIntegral (BSU.unsafeIndex bs i) :: Word32
        !b1 = fromIntegral (BSU.unsafeIndex bs (i + 1)) :: Word32
        !b2 = fromIntegral (BSU.unsafeIndex bs (i + 2)) :: Word32
        !b3 = fromIntegral (BSU.unsafeIndex bs (i + 3)) :: Word32
        !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
    in (# (# val, off +# 4# #) | #)
{-# INLINE getFixed32 #-}

-- | Decode a fixed64 (little-endian).
getFixed64 :: Decoder Word64
getFixed64 = Decoder $ \bs off ->
  if I# (off +# 8#) > BS.length bs
  then (# | UnexpectedEnd #)
  else
    let !i = I# off
        !b0 = fromIntegral (BSU.unsafeIndex bs i) :: Word64
        !b1 = fromIntegral (BSU.unsafeIndex bs (i + 1)) :: Word64
        !b2 = fromIntegral (BSU.unsafeIndex bs (i + 2)) :: Word64
        !b3 = fromIntegral (BSU.unsafeIndex bs (i + 3)) :: Word64
        !b4 = fromIntegral (BSU.unsafeIndex bs (i + 4)) :: Word64
        !b5 = fromIntegral (BSU.unsafeIndex bs (i + 5)) :: Word64
        !b6 = fromIntegral (BSU.unsafeIndex bs (i + 6)) :: Word64
        !b7 = fromIntegral (BSU.unsafeIndex bs (i + 7)) :: Word64
        !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)
           .|. (b4 `shiftL` 32) .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48) .|. (b7 `shiftL` 56)
    in (# (# val, off +# 8# #) | #)
{-# INLINE getFixed64 #-}

getFloat :: Decoder Float
getFloat = castWord32ToFloat <$> getFixed32
{-# INLINE getFloat #-}

getDouble :: Decoder Double
getDouble = castWord64ToDouble <$> getFixed64
{-# INLINE getDouble #-}

-- | Decode a length-delimited field. Zero-copy ByteString slice.
getLengthDelimited :: Decoder ByteString
getLengthDelimited = Decoder $ \bs off ->
  case runDecoder# getVarint bs off of
    (# (# lenW, off' #) | #) ->
      let !len = fromIntegral lenW :: Int
      in if len < 0
         then (# | NegativeLength #)
         else if I# off' + len > BS.length bs
         then (# | UnexpectedEnd #)
         else let !i = I# off'
              in (# (# BSU.unsafeTake len (BSU.unsafeDrop i bs), case i + len of I# r -> r #) | #)
    (# | e #) -> (# | e #)
{-# INLINE getLengthDelimited #-}

getByteString :: Decoder ByteString
getByteString = getLengthDelimited
{-# INLINE getByteString #-}

getText :: Decoder Text
getText = Decoder $ \bs off ->
  case runDecoder# getLengthDelimited bs off of
    (# (# bytes, off' #) | #) ->
      if validateUtf8 bytes
      then (# (# decodeUtf8 bytes, off' #) | #)
      else (# | InvalidUtf8 #)
    (# | e #) -> (# | e #)
{-# INLINE getText #-}

getTag :: Decoder Tag
getTag = Decoder $ \bs off ->
  case runDecoder# getVarint bs off of
    (# (# w, off' #) | #) ->
      case decodeTag w of
        Just tag -> (# (# tag, off' #) | #)
        Nothing  -> (# | InvalidTag w #)
    (# | e #) -> (# | e #)
{-# INLINE getTag #-}

-- | Try to decode a tag, returning Nothing at end-of-input.
getTagOr :: Decoder (Maybe Tag)
getTagOr = Decoder $ \bs off ->
  if isTrue# (off >=# bsLen bs)
  then (# (# Nothing, off #) | #)
  else case runDecoder# getTag bs off of
    (# (# tag, off' #) | #) -> (# (# Just tag, off' #) | #)
    (# | e #)               -> (# | e #)
{-# INLINE getTagOr #-}

-- | Skip over a field value based on its wire type.
skipField :: WireType -> Decoder ()
skipField = \case
  WireVarint -> skipVarint
  Wire64Bit  -> skip 8
  WireLengthDelimited -> Decoder $ \bs off ->
    case runDecoder# getVarint bs off of
      (# (# lenW, off' #) | #) ->
        let !len = fromIntegral lenW :: Int
        in if I# off' + len > BS.length bs
           then (# | UnexpectedEnd #)
           else (# (# (), case I# off' + len of I# r -> r #) | #)
      (# | e #) -> (# | e #)
  WireStartGroup -> skipGroup
  WireEndGroup   -> pure ()
  Wire32Bit  -> skip 4

skip :: Int -> Decoder ()
skip (I# n) = Decoder $ \bs off ->
  if I# (off +# n) > BS.length bs
  then (# | UnexpectedEnd #)
  else (# (# (), off +# n #) | #)
{-# INLINE skip #-}

skipVarint :: Decoder ()
skipVarint = Decoder $ \bs off0 ->
  let len = bsLen bs
      go !pos
        | isTrue# (pos >=# len) = (# | UnexpectedEnd #)
        | BSU.unsafeIndex bs (I# pos) < 0x80 = (# (# (), pos +# 1# #) | #)
        | otherwise = go (pos +# 1#)
  in go off0
{-# INLINE skipVarint #-}

skipGroup :: Decoder ()
skipGroup = Decoder $ \bs off ->
  case runDecoder# getTagOrU bs off of
    (# (# mt, off' #) | #) -> case mt of
      UNothing -> (# | UnexpectedEnd #)
      UJust (Tag _ WireEndGroup) -> (# (# (), off' #) | #)
      UJust (Tag _ wt) -> runDecoder# (skipField wt >> skipGroup) bs off'
    (# | e #) -> (# | e #)

-- | Unboxed optional for zero-allocation tag-or-EOF in the decode loop.
data UMaybe a = UMaybe (# (# #) | a #)

pattern UJust :: a -> UMaybe a
pattern UJust a = UMaybe (# | a #)

pattern UNothing :: UMaybe a
pattern UNothing = UMaybe (# (# #) | #)

{-# COMPLETE UJust, UNothing #-}

umaybe :: b -> (a -> b) -> UMaybe a -> b
umaybe def f (UMaybe x) = case x of
  (# (# #) | #) -> def
  (# | a #)     -> f a
{-# INLINE umaybe #-}

-- | Like 'getTagOr' but returns 'UMaybe' to avoid allocating a boxed Maybe.
getTagOrU :: Decoder (UMaybe Tag)
getTagOrU = Decoder $ \bs off ->
  if isTrue# (off >=# bsLen bs)
  then (# (# UNothing, off #) | #)
  else case runDecoder# getTag bs off of
    (# (# tag, off' #) | #) -> (# (# UJust tag, off' #) | #)
    (# | e #)               -> (# | e #)
{-# INLINE getTagOrU #-}

-- | Three-way unboxed result for the tag-or-EOF operation.
-- Flattens what would otherwise be two nested unboxed sums
-- (Decoder result × UMaybe) into a single three-way split.
--
-- * @(# (# #) | _ | _ #)@ — end of input (offset unchanged)
-- * @(# _ | (# Int#, Int#, Int# #) | _ #)@ — got a tag: field number, wire type, new offset
-- * @(# _ | _ | DecodeError #)@ — decode error
type TagResult# = (# (# #) | (# Int#, Int#, Int# #) | DecodeError #)

-- | CPS interface to the three-way tag result, specialized for decoder results.
-- Avoids constructing any intermediate value — the continuation is applied
-- directly to the unboxed field number and wire type.
withTag
  :: ByteString
  -> Int#
  -> (Int# -> (# (# a, Int# #) | DecodeError #))
  -> (Int# -> Int# -> Int# -> (# (# a, Int# #) | DecodeError #))
  -> (DecodeError -> (# (# a, Int# #) | DecodeError #))
  -> (# (# a, Int# #) | DecodeError #)
withTag bs off kEOF kTag kErr =
  if isTrue# (off >=# bsLen bs)
  then kEOF off
  else case runDecoder# getVarint bs off of
    (# (# w, off' #) | #) ->
      case decodeTagParts w of
        (# fn, wt #) -> kTag fn wt off'
    (# | e #) -> kErr e
{-# INLINE withTag #-}

-- | Decode tag into unboxed field number and wire type.
decodeTagParts :: Word64 -> (# Int#, Int# #)
decodeTagParts w =
  let fn = fromIntegral (w `shiftR` 3) :: Int
      wt = fromIntegral (w .&. 0x07) :: Int
  in case fn of
    I# fn# -> case wt of
      I# wt# -> (# fn#, wt# #)
{-# INLINE decodeTagParts #-}

-- | C FFI UTF-8 validator. Returns 1 for valid, 0 for invalid.
foreign import ccall unsafe "hs_proto_validate_utf8"
  c_validate_utf8 :: Ptr Word8 -> Int -> Int

-- | Validate UTF-8 without exceptions. Uses C FFI for the validation,
-- then constructs Text only when valid. This avoids the catch#-based
-- exception path in Data.Text.Encoding.decodeUtf8' which Core showed
-- allocates Right/Left constructors in the hot decode path.
validateUtf8 :: ByteString -> Bool
validateUtf8 bs =
  case BSI.toForeignPtr bs of
    (fptr, off, len) ->
      unsafeDupablePerformIO $
        withForeignPtr fptr $ \ptr ->
          let !r = c_validate_utf8 (ptr `plusPtr` off) len
          in pure (r == 1)
{-# INLINE validateUtf8 #-}

