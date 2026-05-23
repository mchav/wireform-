{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Fast streaming parser with flatparse-grade inner-loop performance.
module Wireform.Parser
  ( -- * Parser type
    Parser

    -- * Byte primitives
  , anyWord8, anyWord8_
  , anyWord16, anyWord32, anyWord64
  , anyWord16le, anyWord32le, anyWord64le
  , anyWord16be, anyWord32be, anyWord64be
  , anyWord16_, anyWord32_, anyWord64_
  , anyFloatle, anyFloatbe, anyDoublele, anyDoublebe

    -- * CPS byte primitives (avoid intermediate boxing)
  , withAnyWord8
  , withAnyWord16, withAnyWord32, withAnyWord64
  , withSatisfyAscii

    -- * Byte matching
  , word8
  , byteString

    -- * ByteString operations
  , takeBs, takeBsCopy, skip
  , takeRest, skipBack

    -- * Signed integers
  , anyInt8
  , anyInt16, anyInt32, anyInt64
  , anyInt16le, anyInt32le, anyInt64le
  , anyInt16be, anyInt32be, anyInt64be

    -- * UTF-8 characters
  , anyChar, anyChar_
  , satisfy, satisfy_
  , satisfyAscii, satisfyAscii_
  , skipSatisfyAscii
  , fusedSatisfy

    -- * UTF-8 characters (additional)
  , anyAsciiChar, skipAnyAsciiChar

    -- * Character classes
  , isDigit, isLatinLetter, isGreekLetter

    -- * ASCII numeric
  , anyAsciiDecimalWord, anyAsciiDecimalInt
  , anyAsciiDecimalInteger
  , anyAsciiHexWord, anyAsciiHexInt

    -- * Control flow
  , (<|>), empty
  , branch, lookahead, fails, try
  , optional, optional_
  , many_, some_
  , many, some
  , skipMany, skipSome
  , withOption
  , chainl, chainr
  , notFollowedBy
  , isolate

    -- * Error handling
  , err, cut, cutting
  , ensureNOrEof
  , withError

    -- * Position and span
  , Pos (..), Span (..)
  , getPos, withSpan, byteStringOf, spanToByteString
  , withByteString, inSpan, subPos

    -- * Marks
  , Mark, mark, restore, release

    -- * Low-level
  , ensureN#, checkpoint, eof
  , atEnd, remaining

    -- * Re-exports
  , module Wireform.Parser.Error
  ) where

import Data.Bits ((.&.), shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Foreign.Ptr (Ptr (..), plusPtr, minusPtr, castPtr)
import GHC.ForeignPtr (ForeignPtr (..))
import Foreign.Storable (Storable (..))
import GHC.Exts
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.IO (IO (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import GHC.Word (Word8 (..), Word16 (..), Word32 (..), Word64 (..))
import GHC.Int (Int (..))

import Wireform.Parser.Internal
import Wireform.Parser.Error
import Wireform.Parser.Position
import Wireform.Parser.Mark

------------------------------------------------------------------------
-- Internal: withEnsure# (bounds check then run continuation)
------------------------------------------------------------------------

-- | Ensure @n#@ bytes available, then run the body.
--
-- Fast path (1 register comparison, no memory access).
-- Semi-fast path: re-read 'peEndPtr' to catch stale @eob@ after a
-- prior streaming resume; avoids the NOINLINE 'ensureNSlow' call.
-- Slow path: genuine suspension via 'onEnsureFail'.
withEnsure# :: forall m e a. ParserMode m => Int# -> Parser m e a -> Parser m e a
withEnsure# n# (Parser p) = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> p env eob s st
    _  -> case readEnd# env st of
      (# st', eob' #) -> case n# <=# minusAddr# eob' s of
        1# -> p env eob' s st'
        _  -> case onEnsureFail @m env eob' s n# st' of
          (# st'', OK# _ s'' #) -> case readEnd# env st'' of
            (# st''', eob'' #) -> p env eob'' s'' st'''
          (# st'', x #) -> (# st'', unsafeCoerce# x #)
{-# INLINE withEnsure# #-}

------------------------------------------------------------------------
-- CPS byte primitives (match flatparse's withAnyWord8 etc)
------------------------------------------------------------------------

-- | Read one byte and pass it to the continuation.  Most efficient
-- form — avoids allocating an intermediate @Word8@ on the heap.
withAnyWord8 :: ParserMode m => (Word8 -> Parser m e r) -> Parser m e r
withAnyWord8 p = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
    w# -> runParser# (p (W8# w#)) env eob (plusAddr# s 1#) st
{-# INLINE withAnyWord8 #-}

withAnyWord16 :: ParserMode m => (Word16 -> Parser m e r) -> Parser m e r
withAnyWord16 p = withEnsure# 2# $ Parser \env eob s st ->
  case indexWord16OffAddr# s 0# of
    w# -> runParser# (p (W16# w#)) env eob (plusAddr# s 2#) st
{-# INLINE withAnyWord16 #-}

withAnyWord32 :: ParserMode m => (Word32 -> Parser m e r) -> Parser m e r
withAnyWord32 p = withEnsure# 4# $ Parser \env eob s st ->
  case indexWord32OffAddr# s 0# of
    w# -> runParser# (p (W32# w#)) env eob (plusAddr# s 4#) st
{-# INLINE withAnyWord32 #-}

withAnyWord64 :: ParserMode m => (Word64 -> Parser m e r) -> Parser m e r
withAnyWord64 p = withEnsure# 8# $ Parser \env eob s st ->
  case indexWord64OffAddr# s 0# of
    w# -> runParser# (p (W64# w#)) env eob (plusAddr# s 8#) st
{-# INLINE withAnyWord64 #-}

------------------------------------------------------------------------
-- Non-CPS byte primitives
------------------------------------------------------------------------

anyWord8 :: ParserMode m => Parser m e Word8
anyWord8 = withAnyWord8 pure
{-# INLINE anyWord8 #-}

anyWord8_ :: ParserMode m => Parser m e ()
anyWord8_ = withEnsure# 1# $ Parser \env eob s st ->
  (# st, OK# () (plusAddr# s 1#) #)
{-# INLINE anyWord8_ #-}

anyWord16 :: ParserMode m => Parser m e Word16
anyWord16 = withAnyWord16 pure
{-# INLINE anyWord16 #-}

anyWord32 :: ParserMode m => Parser m e Word32
anyWord32 = withAnyWord32 pure
{-# INLINE anyWord32 #-}

anyWord64 :: ParserMode m => Parser m e Word64
anyWord64 = withAnyWord64 pure
{-# INLINE anyWord64 #-}

-- Skip variants
anyWord16_ :: ParserMode m => Parser m e ()
anyWord16_ = withEnsure# 2# $ Parser \env eob s st -> (# st, OK# () (plusAddr# s 2#) #)
{-# INLINE anyWord16_ #-}

anyWord32_ :: ParserMode m => Parser m e ()
anyWord32_ = withEnsure# 4# $ Parser \env eob s st -> (# st, OK# () (plusAddr# s 4#) #)
{-# INLINE anyWord32_ #-}

anyWord64_ :: ParserMode m => Parser m e ()
anyWord64_ = withEnsure# 8# $ Parser \env eob s st -> (# st, OK# () (plusAddr# s 8#) #)
{-# INLINE anyWord64_ #-}

------------------------------------------------------------------------
-- Endianness variants
------------------------------------------------------------------------

#if defined(WORDS_BIGENDIAN)
anyWord16le = withAnyWord16 (pure . byteSwap16)
anyWord32le = withAnyWord32 (pure . byteSwap32)
anyWord64le = withAnyWord64 (pure . byteSwap64)
anyWord16be = anyWord16
anyWord32be = anyWord32
anyWord64be = anyWord64
#else
anyWord16le :: ParserMode m => Parser m e Word16
anyWord16le = anyWord16
{-# INLINE anyWord16le #-}
anyWord32le :: ParserMode m => Parser m e Word32
anyWord32le = anyWord32
{-# INLINE anyWord32le #-}
anyWord64le :: ParserMode m => Parser m e Word64
anyWord64le = anyWord64
{-# INLINE anyWord64le #-}
anyWord16be :: ParserMode m => Parser m e Word16
anyWord16be = withAnyWord16 (pure . byteSwap16)
{-# INLINE anyWord16be #-}
anyWord32be :: ParserMode m => Parser m e Word32
anyWord32be = withAnyWord32 (pure . byteSwap32)
{-# INLINE anyWord32be #-}
anyWord64be :: ParserMode m => Parser m e Word64
anyWord64be = withAnyWord64 (pure . byteSwap64)
{-# INLINE anyWord64be #-}
#endif

------------------------------------------------------------------------
-- Floating-point
------------------------------------------------------------------------

anyFloatle :: ParserMode m => Parser m e Float
anyFloatle = castWord32ToFloat <$> anyWord32le
{-# INLINE anyFloatle #-}
anyFloatbe :: ParserMode m => Parser m e Float
anyFloatbe = castWord32ToFloat <$> anyWord32be
{-# INLINE anyFloatbe #-}
anyDoublele :: ParserMode m => Parser m e Double
anyDoublele = castWord64ToDouble <$> anyWord64le
{-# INLINE anyDoublele #-}
anyDoublebe :: ParserMode m => Parser m e Double
anyDoublebe = castWord64ToDouble <$> anyWord64be
{-# INLINE anyDoublebe #-}

------------------------------------------------------------------------
-- Byte matching
------------------------------------------------------------------------

word8 :: ParserMode m => Word8 -> Parser m e ()
word8 (W8# expected) = withEnsure# 1# $ Parser \env eob s st ->
  case eqWord8# (indexWord8OffAddr# s 0#) expected of
    1# -> (# st, OK# () (plusAddr# s 1#) #)
    _  -> (# st, Fail# #)
{-# INLINE word8 #-}

-- | Match a literal 'ByteString'.  Uses word-at-a-time comparison.
byteString :: ParserMode m => ByteString -> Parser m e ()
byteString bs = Parser \env eob s st ->
  let !(BSI.BS bsfp len@(I# len#)) = bs
  in case len# <=# minusAddr# eob s of
       1# -> case memcmpAddr# s (bsAddr# bs) len# of
               0# -> (# st, OK# () (plusAddr# s len#) #)
               _  -> (# st, Fail# #)
       _ -> case ensureNSlow env eob s len# st of
              (# st', OK# _ s' #) -> case readEnd# env st' of
                (# st'', eob' #) ->
                  case memcmpAddr# s' (bsAddr# bs) len# of
                    0# -> (# st'', OK# () (plusAddr# s' len#) #)
                    _  -> (# st'', Fail# #)
              (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE byteString #-}

bsAddr# :: ByteString -> Addr#
bsAddr# (BSI.BS (ForeignPtr addr _) _) = addr
{-# INLINE bsAddr# #-}

memcmpAddr# :: Addr# -> Addr# -> Int# -> Int#
memcmpAddr# a b n = case unsafeDupablePerformIO (c_memcmp (Ptr a) (Ptr b) (fromIntegral (I# n))) of
  r -> if r == 0 then 0# else 1#
{-# INLINE memcmpAddr# #-}

foreign import ccall unsafe "memcmp"
  c_memcmp :: Ptr Word8 -> Ptr Word8 -> Int -> IO Int

------------------------------------------------------------------------
-- ByteString operations
------------------------------------------------------------------------

-- | Take @n@ bytes as a zero-copy slice.
--
-- For 'parseByteString': the returned 'ByteString' is a zero-copy
-- slice of the input (identical to flatparse's @take@).
--
-- For ring-backed streaming: the returned 'ByteString' holds a
-- reference to the ring's backing memory.  The slice is valid as
-- long as the ring exists (managed by 'withMagicRing' / 'withRecvTransport').
-- If the ring is freed while the 'ByteString' is still alive, the
-- slice becomes a dangling pointer.  In practice this is safe because
-- 'runParser' / 'runParserLoop' run within the transport's scope.
takeBs :: ParserMode m => Int -> Parser m e ByteString
takeBs (I# n#) = withEnsure# n# $ Parser \env eob s st ->
  let !bs = BSI.BS (ForeignPtr s (peBackingFp env)) (I# n#)
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBs #-}

-- | Take @n@ bytes, always copying.  Use when you need the result
-- to outlive the transport's scope.
takeBsCopy :: ParserMode m => Int -> Parser m e ByteString
takeBsCopy (I# n#) = withEnsure# n# $ Parser \env eob s st ->
  let !bs = unsafeDupablePerformIO (BSI.create (I# n#) \dst -> BSI.memcpy dst (Ptr s) (I# n#))
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBsCopy #-}

skip :: ParserMode m => Int -> Parser m e ()
skip (I# n#) = withEnsure# n# $ Parser \env eob s st ->
  (# st, OK# () (plusAddr# s n#) #)
{-# INLINE skip #-}

-- | Consume all remaining bytes in the current window.
takeRest :: ParserMode m => Parser m e ByteString
takeRest = Parser \env eob s st ->
  let !len = I# (minusAddr# eob s)
      !bs = if len <= 0
            then BS.empty
            else BSI.BS (ForeignPtr s (peBackingFp env)) len
  in (# st, OK# bs eob #)
{-# INLINE takeRest #-}

-- | Skip backward @n@ bytes (takes a positive integer).
--
-- In the ring buffer context, you can only skip back to bytes that
-- haven't been overwritten — i.e., bytes between the consumer tail
-- and the current position.  For 'parseByteString' this is the
-- entire input.  For streaming, it's bounded by whatever the driver
-- hasn't advanced the tail past (at minimum, the start of the
-- current 'runParser' invocation).
--
-- Fails if @n@ would move before the start of the current parse
-- run (the @peInitCur@ boundary).  Does NOT check the ring tail
-- directly — the driver guarantees tail <= initCur.
skipBack :: ParserMode m => Int -> Parser m e ()
skipBack (I# n#) = Parser \env eob s st ->
  let !target = plusAddr# s (negateInt# n#)
      !(Ptr initCur) = peInitCur env
  in case leAddr# initCur target of
       1# -> (# st, OK# () target #)
       _  -> (# st, Fail# #)
{-# INLINE skipBack #-}

------------------------------------------------------------------------
-- Signed integers
------------------------------------------------------------------------

anyInt8 :: ParserMode m => Parser m e Int8
anyInt8 = fromIntegral <$> anyWord8
{-# INLINE anyInt8 #-}

anyInt16 :: ParserMode m => Parser m e Int16
anyInt16 = fromIntegral <$> anyWord16
{-# INLINE anyInt16 #-}
anyInt32 :: ParserMode m => Parser m e Int32
anyInt32 = fromIntegral <$> anyWord32
{-# INLINE anyInt32 #-}
anyInt64 :: ParserMode m => Parser m e Int64
anyInt64 = fromIntegral <$> anyWord64
{-# INLINE anyInt64 #-}

anyInt16le :: ParserMode m => Parser m e Int16
anyInt16le = fromIntegral <$> anyWord16le
{-# INLINE anyInt16le #-}
anyInt32le :: ParserMode m => Parser m e Int32
anyInt32le = fromIntegral <$> anyWord32le
{-# INLINE anyInt32le #-}
anyInt64le :: ParserMode m => Parser m e Int64
anyInt64le = fromIntegral <$> anyWord64le
{-# INLINE anyInt64le #-}

anyInt16be :: ParserMode m => Parser m e Int16
anyInt16be = fromIntegral <$> anyWord16be
{-# INLINE anyInt16be #-}
anyInt32be :: ParserMode m => Parser m e Int32
anyInt32be = fromIntegral <$> anyWord32be
{-# INLINE anyInt32be #-}
anyInt64be :: ParserMode m => Parser m e Int64
anyInt64be = fromIntegral <$> anyWord64be
{-# INLINE anyInt64be #-}

------------------------------------------------------------------------
-- UTF-8 / ASCII
------------------------------------------------------------------------

-- | Parse any UTF-8 character.
anyChar :: ParserMode m => Parser m e Char
anyChar = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
      c1 -> case leWord8# c1 (wordToWord8# 0x7F##) of
        1# -> (# st, OK# (C# (chr# (word2Int# (word8ToWord# c1)))) (plusAddr# s 1#) #)
        _  -> case leWord8# c1 (wordToWord8# 0xBF##) of
          1# -> (# st, Fail# #)
          _  -> case leWord8# c1 (wordToWord8# 0xDF##) of
            1# -> case 2# <=# minusAddr# eob s of
              1# -> case indexWord8OffAddr# s 1# of
                c2 ->
                  let !cp = ((word2Int# (word8ToWord# c1) -# 0xC0#) `uncheckedIShiftL#` 6#)
                        +# (word2Int# (word8ToWord# c2) -# 0x80#)
                  in (# st, OK# (C# (chr# cp)) (plusAddr# s 2#) #)
              _ -> (# st, Fail# #)
            _  -> case leWord8# c1 (wordToWord8# 0xEF##) of
              1# -> case 3# <=# minusAddr# eob s of
                1# -> case indexWord8OffAddr# s 1# of { c2 ->
                       case indexWord8OffAddr# s 2# of { c3 ->
                  let !cp = ((word2Int# (word8ToWord# c1) -# 0xE0#) `uncheckedIShiftL#` 12#)
                        +# ((word2Int# (word8ToWord# c2) -# 0x80#) `uncheckedIShiftL#` 6#)
                        +# (word2Int# (word8ToWord# c3) -# 0x80#)
                  in (# st, OK# (C# (chr# cp)) (plusAddr# s 3#) #)
                  }}
                _ -> (# st, Fail# #)
              _  -> case 4# <=# minusAddr# eob s of
                1# -> case indexWord8OffAddr# s 1# of { c2 ->
                       case indexWord8OffAddr# s 2# of { c3 ->
                       case indexWord8OffAddr# s 3# of { c4 ->
                  let !cp = ((word2Int# (word8ToWord# c1) -# 0xF0#) `uncheckedIShiftL#` 18#)
                        +# ((word2Int# (word8ToWord# c2) -# 0x80#) `uncheckedIShiftL#` 12#)
                        +# ((word2Int# (word8ToWord# c3) -# 0x80#) `uncheckedIShiftL#` 6#)
                        +# (word2Int# (word8ToWord# c4) -# 0x80#)
                  in (# st, OK# (C# (chr# cp)) (plusAddr# s 4#) #)
                  }}}
                _ -> (# st, Fail# #)
{-# INLINE anyChar #-}

anyChar_ :: ParserMode m => Parser m e ()
anyChar_ = () <$ anyChar
{-# INLINE anyChar_ #-}

-- | CPS variant: parse an ASCII char and pass to continuation.
withSatisfyAscii :: ParserMode m => (Char -> Bool) -> (Char -> Parser m e r) -> Parser m e r
withSatisfyAscii f p = withEnsure# 1# $ Parser \env eob s st ->
  case indexCharOffAddr# s 0# of
      c1 -> case leChar# c1 '\x7F'# of
        1# -> let !ch = C# c1
              in if f ch
                 then runParser# (p ch) env eob (plusAddr# s 1#) st
                 else (# st, Fail# #)
        _  -> (# st, Fail# #)
{-# INLINE withSatisfyAscii #-}

-- | Parse an ASCII character matching a predicate.
satisfyAscii :: ParserMode m => (Char -> Bool) -> Parser m e Char
satisfyAscii f = withSatisfyAscii f pure
{-# INLINE satisfyAscii #-}

satisfyAscii_ :: ParserMode m => (Char -> Bool) -> Parser m e ()
satisfyAscii_ f = withSatisfyAscii f (\_ -> pure ())
{-# INLINE satisfyAscii_ #-}

skipSatisfyAscii :: ParserMode m => (Char -> Bool) -> Parser m e ()
skipSatisfyAscii f = withEnsure# 1# $ Parser \env eob s st ->
  case indexCharOffAddr# s 0# of
      c1 -> case leChar# c1 '\x7F'# of
        1# -> if f (C# c1)
              then (# st, OK# () (plusAddr# s 1#) #)
              else (# st, Fail# #)
        _  -> (# st, Fail# #)
{-# INLINE skipSatisfyAscii #-}

-- | Parse any ASCII character (< 0x80).
anyAsciiChar :: ParserMode m => Parser m e Char
anyAsciiChar = satisfyAscii (const True)
{-# INLINE anyAsciiChar #-}

-- | Skip one ASCII character.
skipAnyAsciiChar :: ParserMode m => Parser m e ()
skipAnyAsciiChar = skipSatisfyAscii (const True)
{-# INLINE skipAnyAsciiChar #-}

-- | Parse a UTF-8 character matching a predicate.
satisfy :: ParserMode m => (Char -> Bool) -> Parser m e Char
satisfy f = do
  c <- anyChar
  if f c then pure c else empty
{-# INLINE satisfy #-}

satisfy_ :: ParserMode m => (Char -> Bool) -> Parser m e ()
satisfy_ f = () <$ satisfy f
{-# INLINE satisfy_ #-}

-- | Fused satisfy: uses the ASCII fast path when the byte is < 0x80,
-- the full UTF-8 path otherwise.  Two separate predicates for each case.
fusedSatisfy :: forall m e. ParserMode m => (Char -> Bool) -> (Word8 -> Bool) -> Parser m e Char
fusedSatisfy charPred bytePred = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
      w -> case leWord8# w (wordToWord8# 0x7F##) of
        1# -> let !ch = C# (chr# (word2Int# (word8ToWord# w)))
              in if charPred ch
                 then (# st, OK# ch (plusAddr# s 1#) #)
                 else (# st, Fail# #)
        _  -> case runParser# (anyChar @m) env eob s st of
                (# st', OK# c s' #) -> if charPred c then (# st', OK# c s' #) else (# st', Fail# #)
                x                    -> x
{-# INLINE fusedSatisfy #-}

------------------------------------------------------------------------
-- Character classes
------------------------------------------------------------------------

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

isLatinLetter :: Char -> Bool
isLatinLetter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isLatinLetter #-}

------------------------------------------------------------------------
-- ASCII numeric
------------------------------------------------------------------------

anyAsciiDecimalWord :: ParserMode m => Parser m e Word
anyAsciiDecimalWord = withEnsure# 1# $ Parser \env eob s st ->
  case indexWord8OffAddr# s 0# of
      c -> case leWord8# (wordToWord8# 0x30##) c of
        0# -> (# st, Fail# #)
        _  -> case leWord8# c (wordToWord8# 0x39##) of
          0# -> (# st, Fail# #)
          _  -> let !d0 = W# (word8ToWord# c) - 0x30
                in goDecWord eob (plusAddr# s 1#) st d0
{-# INLINE anyAsciiDecimalWord #-}

goDecWord :: Addr# -> Addr# -> State# RealWorld -> Word -> StRes# e Word
goDecWord eob s st !acc =
  case eqAddr# eob s of
    1# -> (# st, OK# acc s #)
    _  -> case indexWord8OffAddr# s 0# of
      c -> case leWord8# (wordToWord8# 0x30##) c of
        0# -> (# st, OK# acc s #)
        _  -> case leWord8# c (wordToWord8# 0x39##) of
          0# -> (# st, OK# acc s #)
          _  -> goDecWord eob (plusAddr# s 1#) st
                  (acc * 10 + (W# (word8ToWord# c) - 0x30))
{-# NOINLINE goDecWord #-}

anyAsciiDecimalInt :: ParserMode m => Parser m e Int
anyAsciiDecimalInt = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInt #-}

------------------------------------------------------------------------
-- Control flow
------------------------------------------------------------------------

empty :: ParserMode m => Parser m e a
empty = Parser \env eob s st -> (# st, Fail# #)
{-# INLINE empty #-}

infixr 6 <|>

(<|>) :: ParserMode m => Parser m e a -> Parser m e a -> Parser m e a
(<|>) (Parser f) (Parser g) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #) -> g env eob s st'
    x                -> x
{-# INLINE[1] (<|>) #-}

{-# RULES "wireform/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r) #-}

branch :: ParserMode m => Parser m e a -> Parser m e b -> Parser m e b -> Parser m e b
branch (Parser p) (Parser t) (Parser f) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# _ s' #) -> t env eob s' st'
    (# st', Fail# #)    -> f env eob s st'
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE branch #-}

lookahead :: ParserMode m => Parser m e a -> Parser m e a
lookahead (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', OK# a _ #) -> (# st', OK# a s #)
    x                   -> x
{-# INLINE lookahead #-}

fails :: ParserMode m => Parser m e a -> Parser m e ()
fails (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', OK# _ _ #) -> (# st', Fail# #)
    (# st', Fail# #)   -> (# st', OK# () s #)
    (# st', x #)       -> (# st', unsafeCoerce# x #)
{-# INLINE fails #-}

try :: ParserMode m => Parser m e a -> Parser m e a
try (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Err# _ #) -> (# st', Fail# #)
    x                  -> x
{-# INLINE try #-}

optional :: ParserMode m => Parser m e a -> Parser m e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

optional_ :: ParserMode m => Parser m e a -> Parser m e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}

withOption :: ParserMode m => Parser m e a -> (a -> Parser m e b) -> Parser m e b -> Parser m e b
withOption (Parser p) just (Parser nothing) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# a s' #) -> runParser# (just a) env eob s' st'
    (# st', Fail# #)    -> nothing env eob s st'
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE withOption #-}

many_ :: ParserMode m => Parser m e a -> Parser m e ()
many_ (Parser f) = Parser go where
  go env eob s st = case f env eob s st of
    (# st', OK# _ s' #) -> go env eob s' st'
    (# st', Fail# #)    -> (# st', OK# () s #)
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE many_ #-}

some_ :: ParserMode m => Parser m e a -> Parser m e ()
some_ p = p *> many_ p
{-# INLINE some_ #-}

skipMany :: ParserMode m => Parser m e a -> Parser m e ()
skipMany = many_
{-# INLINE skipMany #-}

skipSome :: ParserMode m => Parser m e a -> Parser m e ()
skipSome = some_
{-# INLINE skipSome #-}

-- | Run @p@ zero or more times, collecting results into a list.
many :: ParserMode m => Parser m e a -> Parser m e [a]
many (Parser f) = Parser go where
  go env eob s st = case f env eob s st of
    (# st', OK# a s' #) -> case go env eob s' st' of
      (# st'', OK# as s'' #) -> (# st'', OK# (a : as) s'' #)
      x                      -> x
    (# st', Fail# #)    -> (# st', OK# [] s #)
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE many #-}

-- | Run @p@ one or more times, collecting results.
some :: ParserMode m => Parser m e a -> Parser m e [a]
some p = (:) <$> p <*> many p
{-# INLINE some #-}

-- | Succeed iff @p@ fails. Does not consume input.
notFollowedBy :: ParserMode m => Parser m e a -> Parser m e ()
notFollowedBy = fails
{-# INLINE notFollowedBy #-}

-- | Run a parser on exactly @n@ bytes. The inner parser must consume
-- all @n@ bytes or the overall parse fails.
isolate :: ParserMode m => Int -> Parser m e a -> Parser m e a
isolate (I# n#) (Parser p) = withEnsure# n# $ Parser \env eob s st ->
  let !isolEnd = plusAddr# s n#
  in case p env isolEnd s st of
       (# st', OK# a s' #) -> case eqAddr# s' isolEnd of
         1# -> (# st', OK# a s' #)
         _  -> (# st', Fail# #)
       (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE isolate #-}

------------------------------------------------------------------------
-- Error handling
------------------------------------------------------------------------

-- | Catch an @Err#@ and handle it.
withError :: ParserMode m => (e -> Parser m e a) -> Parser m e a -> Parser m e a
withError handler (Parser f) = Parser \env eob s st ->
  case f env eob s st of
    (# st', Err# e #) -> runParser# (handler e) env eob s st'
    x                  -> x
{-# INLINE withError #-}

err :: ParserMode m => e -> Parser m e a
err e = Parser \env eob s st -> (# st, Err# e #)
{-# INLINE err #-}

cut :: ParserMode m => Parser m e a -> e -> Parser m e a
cut (Parser f) e = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #) -> (# st', Err# e #)
    x                -> x
{-# INLINE cut #-}

cutting :: ParserMode m => Parser m e a -> e -> (e -> e -> e) -> Parser m e a
cutting (Parser f) e merge = Parser \env eob s st ->
  case f env eob s st of
    (# st', Fail# #)  -> (# st', Err# e #)
    (# st', Err# e' #) -> (# st', Err# (merge e' e) #)
    x                   -> x
{-# INLINE cutting #-}

------------------------------------------------------------------------
-- EOF
------------------------------------------------------------------------

-- | Succeed iff at end of input.
-- In streaming mode, this may suspend to check if more data is coming.
-- Uses ensureN# 1 internally: if ensure fails (no more data), we're at EOF.
eof :: forall m e. ParserMode m => Parser m e ()
eof = Parser \env eob s st ->
  case eqAddr# eob s of
    1# -> case onEnsureFail @m env eob s 1# st of
            (# st', OK# _ _ #) -> (# st', Fail# #)  -- data arrived
            (# st', Fail# #)   -> (# st', OK# () s #)  -- genuine EOF
            (# st', x #)       -> (# st', unsafeCoerce# x #)
    _  -> (# st, Fail# #)
{-# INLINE eof #-}

-- | Non-consuming check: 'True' if at end of input.
-- In streaming mode, may suspend to determine.
atEnd :: ParserMode m => Parser m e Bool
atEnd = (eof *> pure True) <|> pure False
{-# INLINE atEnd #-}

-- | Number of bytes remaining in the current window (cheap, no suspend).
remaining :: ParserMode m => Parser m e Int
remaining = Parser \env eob s st ->
  (# st, OK# (I# (minusAddr# eob s)) s #)
{-# INLINE remaining #-}

------------------------------------------------------------------------
-- Chain combinators
------------------------------------------------------------------------

-- | Left-associative chain.
-- @chainl f p q@ parses @p@ then zero or more @q@, folding left with @f@.
chainl :: ParserMode m => (b -> a -> b) -> Parser m e b -> Parser m e a -> Parser m e b
chainl f (Parser p) (Parser q) = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# b s' #) -> chainlGo f q env eob s' st' b
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE chainl #-}

chainlGo :: forall e b a. (b -> a -> b)
         -> (ParserEnv -> Addr# -> Addr# -> State# RealWorld -> StRes# e a)
         -> ParserEnv -> Addr# -> Addr# -> State# RealWorld -> b -> StRes# e b
chainlGo f q env eob s st !acc =
  case q env eob s st of
    (# st', OK# a s' #) -> chainlGo f q env eob s' st' (f acc a)
    (# st', Fail# #)    -> (# st', OK# acc s #)
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# NOINLINE chainlGo #-}

-- | Right-associative chain.
-- @chainr f p q@ parses zero or more @p@, then @q@ at the end,
-- folding right with @f@.
chainr :: ParserMode m => (a -> b -> b) -> Parser m e a -> Parser m e b -> Parser m e b
chainr f (Parser p) (Parser q) = Parser go where
  go env eob s st =
    case p env eob s st of
      (# st', OK# a s' #) -> case go env eob s' st' of
        (# st'', OK# b s'' #) -> (# st'', OK# (f a b) s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', Fail# #) -> q env eob s st'
      (# st', x #)     -> (# st', unsafeCoerce# x #)
{-# INLINE chainr #-}

------------------------------------------------------------------------
-- Additional error handling
------------------------------------------------------------------------

-- | Like 'ensureN#' but on EOF, errors with a specific error
-- instead of failing recoverably.
ensureNOrEof :: ParserMode m => Int -> e -> Parser m e ()
ensureNOrEof n e = ensureN# (case n of I# n# -> n#) <|> err e
{-# INLINE ensureNOrEof #-}

------------------------------------------------------------------------
-- Additional position combinators
------------------------------------------------------------------------

-- | Parse something and return both the result and the bytes consumed.
withByteString :: ParserMode m => Parser m e a -> (a -> ByteString -> Parser m e b) -> Parser m e b
withByteString (Parser p) f = Parser \env eob s st ->
  case p env eob s st of
    (# st', OK# a s' #) ->
      let !len = I# (minusAddr# s' s)
          !bs  = BSI.BS (ForeignPtr s (peBackingFp env)) len
      in runParser# (f a bs) env eob s' st'
    (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withByteString #-}

------------------------------------------------------------------------
-- Additional character classes
------------------------------------------------------------------------

isGreekLetter :: Char -> Bool
isGreekLetter c = (c >= '\x0391' && c <= '\x03A9') || (c >= '\x03B1' && c <= '\x03C9')
{-# INLINE isGreekLetter #-}

------------------------------------------------------------------------
-- Additional ASCII numeric
------------------------------------------------------------------------

anyAsciiHexWord :: ParserMode m => Parser m e Word
anyAsciiHexWord = withEnsure# 1# $ Parser \env eob s st ->
  case hexDigit (indexWord8OffAddr# s 0#) of
      (# | (# #) #) -> (# st, Fail# #)
      (# (# d #) | #) -> goHexWord eob (plusAddr# s 1#) st (W# (word8ToWord# d))
{-# INLINE anyAsciiHexWord #-}

hexDigit :: Word8# -> (# (# Word8# #) | (# #) #)
hexDigit w
  | isTrue# (leWord8# (wordToWord8# 0x30##) w) , isTrue# (leWord8# w (wordToWord8# 0x39##))
    = (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x30##) #) | #)
  | isTrue# (leWord8# (wordToWord8# 0x41##) w) , isTrue# (leWord8# w (wordToWord8# 0x46##))
    = (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x37##) #) | #)
  | isTrue# (leWord8# (wordToWord8# 0x61##) w) , isTrue# (leWord8# w (wordToWord8# 0x66##))
    = (# (# wordToWord8# (word8ToWord# w `minusWord#` 0x57##) #) | #)
  | otherwise = (# | (# #) #)
{-# INLINE hexDigit #-}

goHexWord :: Addr# -> Addr# -> State# RealWorld -> Word -> StRes# e Word
goHexWord eob s st !acc =
  case eqAddr# eob s of
    1# -> (# st, OK# acc s #)
    _  -> case hexDigit (indexWord8OffAddr# s 0#) of
      (# | (# #) #)     -> (# st, OK# acc s #)
      (# (# d #) | #) -> goHexWord eob (plusAddr# s 1#) st
                            (acc * 16 + W# (word8ToWord# d))
{-# NOINLINE goHexWord #-}

anyAsciiHexInt :: ParserMode m => Parser m e Int
anyAsciiHexInt = fromIntegral <$> anyAsciiHexWord
{-# INLINE anyAsciiHexInt #-}

anyAsciiDecimalInteger :: ParserMode m => Parser m e Integer
anyAsciiDecimalInteger = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInteger #-}
