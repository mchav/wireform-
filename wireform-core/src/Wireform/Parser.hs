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

    -- * UTF-8 characters
  , anyChar, anyChar_
  , satisfyAscii, satisfyAscii_
  , skipSatisfyAscii

    -- * Character classes
  , isDigit, isLatinLetter, isGreekLetter

    -- * ASCII numeric
  , anyAsciiDecimalWord, anyAsciiDecimalInt
  , anyAsciiHexWord

    -- * Control flow
  , (<|>), empty
  , branch, lookahead, fails, try
  , optional, optional_
  , many_, some_
  , skipMany, skipSome
  , withOption
  , chainl, chainr

    -- * Error handling
  , err, cut, cutting
  , ensureNOrEof

    -- * Position and span
  , Pos (..), Span (..)
  , getPos, withSpan, byteStringOf, spanToByteString
  , withByteString

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
-- The body receives the current @s@ (which may have been updated
-- by ensureNSlow on the slow path).
withEnsure# :: Int# -> Parser e a -> Parser e a
withEnsure# n# (Parser p) = Parser \tag env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> p tag env eob s st
    _  -> case ensureNSlow tag env eob s n# st of
            (# st', OK# _ s' #) -> case readEnd# env st' of
              (# st'', eob' #) -> p tag env eob' s' st''
            (# st', x #) -> (# st', unsafeCoerce# x #)
{-# INLINE withEnsure# #-}

------------------------------------------------------------------------
-- CPS byte primitives (match flatparse's withAnyWord8 etc)
------------------------------------------------------------------------

-- | Read one byte and pass it to the continuation.  Most efficient
-- form — avoids allocating an intermediate @Word8@ on the heap.
withAnyWord8 :: (Word8 -> Parser e r) -> Parser e r
withAnyWord8 p = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case indexWord8OffAddr# s 0# of
            w# -> runParser# (p (W8# w#)) tag env eob (plusAddr# s 1#) st
{-# INLINE withAnyWord8 #-}

withAnyWord16 :: (Word16 -> Parser e r) -> Parser e r
withAnyWord16 p = withEnsure# 2# $ Parser \tag env eob s st ->
  case indexWord16OffAddr# s 0# of
    w# -> runParser# (p (W16# w#)) tag env eob (plusAddr# s 2#) st
{-# INLINE withAnyWord16 #-}

withAnyWord32 :: (Word32 -> Parser e r) -> Parser e r
withAnyWord32 p = withEnsure# 4# $ Parser \tag env eob s st ->
  case indexWord32OffAddr# s 0# of
    w# -> runParser# (p (W32# w#)) tag env eob (plusAddr# s 4#) st
{-# INLINE withAnyWord32 #-}

withAnyWord64 :: (Word64 -> Parser e r) -> Parser e r
withAnyWord64 p = withEnsure# 8# $ Parser \tag env eob s st ->
  case indexWord64OffAddr# s 0# of
    w# -> runParser# (p (W64# w#)) tag env eob (plusAddr# s 8#) st
{-# INLINE withAnyWord64 #-}

------------------------------------------------------------------------
-- Non-CPS byte primitives
------------------------------------------------------------------------

anyWord8 :: Parser e Word8
anyWord8 = withAnyWord8 pure
{-# INLINE anyWord8 #-}

anyWord8_ :: Parser e ()
anyWord8_ = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> (# st, OK# () (plusAddr# s 1#) #)
{-# INLINE anyWord8_ #-}

anyWord16 :: Parser e Word16
anyWord16 = withAnyWord16 pure
{-# INLINE anyWord16 #-}

anyWord32 :: Parser e Word32
anyWord32 = withAnyWord32 pure
{-# INLINE anyWord32 #-}

anyWord64 :: Parser e Word64
anyWord64 = withAnyWord64 pure
{-# INLINE anyWord64 #-}

-- Skip variants
anyWord16_ :: Parser e ()
anyWord16_ = withEnsure# 2# $ Parser \tag env eob s st -> (# st, OK# () (plusAddr# s 2#) #)
{-# INLINE anyWord16_ #-}

anyWord32_ :: Parser e ()
anyWord32_ = withEnsure# 4# $ Parser \tag env eob s st -> (# st, OK# () (plusAddr# s 4#) #)
{-# INLINE anyWord32_ #-}

anyWord64_ :: Parser e ()
anyWord64_ = withEnsure# 8# $ Parser \tag env eob s st -> (# st, OK# () (plusAddr# s 8#) #)
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
anyWord16le :: Parser e Word16
anyWord16le = anyWord16
{-# INLINE anyWord16le #-}
anyWord32le :: Parser e Word32
anyWord32le = anyWord32
{-# INLINE anyWord32le #-}
anyWord64le :: Parser e Word64
anyWord64le = anyWord64
{-# INLINE anyWord64le #-}
anyWord16be :: Parser e Word16
anyWord16be = withAnyWord16 (pure . byteSwap16)
{-# INLINE anyWord16be #-}
anyWord32be :: Parser e Word32
anyWord32be = withAnyWord32 (pure . byteSwap32)
{-# INLINE anyWord32be #-}
anyWord64be :: Parser e Word64
anyWord64be = withAnyWord64 (pure . byteSwap64)
{-# INLINE anyWord64be #-}
#endif

------------------------------------------------------------------------
-- Floating-point
------------------------------------------------------------------------

anyFloatle :: Parser e Float
anyFloatle = castWord32ToFloat <$> anyWord32le
{-# INLINE anyFloatle #-}
anyFloatbe :: Parser e Float
anyFloatbe = castWord32ToFloat <$> anyWord32be
{-# INLINE anyFloatbe #-}
anyDoublele :: Parser e Double
anyDoublele = castWord64ToDouble <$> anyWord64le
{-# INLINE anyDoublele #-}
anyDoublebe :: Parser e Double
anyDoublebe = castWord64ToDouble <$> anyWord64be
{-# INLINE anyDoublebe #-}

------------------------------------------------------------------------
-- Byte matching
------------------------------------------------------------------------

word8 :: Word8 -> Parser e ()
word8 (W8# expected) = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case eqWord8# (indexWord8OffAddr# s 0#) expected of
            1# -> (# st, OK# () (plusAddr# s 1#) #)
            _  -> (# st, Fail# #)
{-# INLINE word8 #-}

-- | Match a literal 'ByteString'.  Uses word-at-a-time comparison.
byteString :: ByteString -> Parser e ()
byteString bs = Parser \tag env eob s st ->
  let !(BSI.BS bsfp len@(I# len#)) = bs
  in case len# <=# minusAddr# eob s of
       1# -> case memcmpAddr# s (bsAddr# bs) len# of
               0# -> (# st, OK# () (plusAddr# s len#) #)
               _  -> (# st, Fail# #)
       _ -> case ensureNSlow tag env eob s len# st of
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
takeBs :: Int -> Parser e ByteString
takeBs (I# n#) = withEnsure# n# $ Parser \tag env eob s st ->
  let !bs = BSI.BS (ForeignPtr s (peBackingFp env)) (I# n#)
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBs #-}

-- | Take @n@ bytes, always copying.  Use when you need the result
-- to outlive the transport's scope.
takeBsCopy :: Int -> Parser e ByteString
takeBsCopy (I# n#) = withEnsure# n# $ Parser \tag env eob s st ->
  let !bs = unsafeDupablePerformIO (BSI.create (I# n#) \dst -> BSI.memcpy dst (Ptr s) (I# n#))
  in (# st, OK# bs (plusAddr# s n#) #)
{-# INLINE takeBsCopy #-}

skip :: Int -> Parser e ()
skip (I# n#) = withEnsure# n# $ Parser \tag env eob s st ->
  (# st, OK# () (plusAddr# s n#) #)
{-# INLINE skip #-}

------------------------------------------------------------------------
-- UTF-8 / ASCII
------------------------------------------------------------------------

-- | Parse any UTF-8 character.
anyChar :: Parser e Char
anyChar = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case indexWord8OffAddr# s 0# of
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

anyChar_ :: Parser e ()
anyChar_ = () <$ anyChar
{-# INLINE anyChar_ #-}

-- | CPS variant: parse an ASCII char and pass to continuation.
withSatisfyAscii :: (Char -> Bool) -> (Char -> Parser e r) -> Parser e r
withSatisfyAscii f p = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case indexCharOffAddr# s 0# of
      c1 -> case leChar# c1 '\x7F'# of
        1# -> let !ch = C# c1
              in if f ch
                 then runParser# (p ch) tag env eob (plusAddr# s 1#) st
                 else (# st, Fail# #)
        _  -> (# st, Fail# #)
{-# INLINE withSatisfyAscii #-}

-- | Parse an ASCII character matching a predicate.
satisfyAscii :: (Char -> Bool) -> Parser e Char
satisfyAscii f = withSatisfyAscii f pure
{-# INLINE satisfyAscii #-}

satisfyAscii_ :: (Char -> Bool) -> Parser e ()
satisfyAscii_ f = withSatisfyAscii f (\_ -> pure ())
{-# INLINE satisfyAscii_ #-}

skipSatisfyAscii :: (Char -> Bool) -> Parser e ()
skipSatisfyAscii f = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case indexCharOffAddr# s 0# of
      c1 -> case leChar# c1 '\x7F'# of
        1# -> if f (C# c1)
              then (# st, OK# () (plusAddr# s 1#) #)
              else (# st, Fail# #)
        _  -> (# st, Fail# #)
{-# INLINE skipSatisfyAscii #-}

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

anyAsciiDecimalWord :: Parser e Word
anyAsciiDecimalWord = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case indexWord8OffAddr# s 0# of
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

anyAsciiDecimalInt :: Parser e Int
anyAsciiDecimalInt = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInt #-}

------------------------------------------------------------------------
-- Control flow
------------------------------------------------------------------------

empty :: Parser e a
empty = Parser \tag env eob s st -> (# st, Fail# #)
{-# INLINE empty #-}

infixr 6 <|>

(<|>) :: Parser e a -> Parser e a -> Parser e a
(<|>) (Parser f) (Parser g) = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', Fail# #) -> g tag env eob s st'
    x                -> x
{-# INLINE[1] (<|>) #-}

{-# RULES "wireform/reassoc-alt" forall l m r. (l <|> m) <|> r = l <|> (m <|> r) #-}

branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch (Parser p) (Parser t) (Parser f) = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# _ s' #) -> t tag env eob s' st'
    (# st', Fail# #)    -> f tag env eob s st'
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE branch #-}

lookahead :: Parser e a -> Parser e a
lookahead (Parser f) = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', OK# a _ #) -> (# st', OK# a s #)
    x                   -> x
{-# INLINE lookahead #-}

fails :: Parser e a -> Parser e ()
fails (Parser f) = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', OK# _ _ #) -> (# st', Fail# #)
    (# st', Fail# #)   -> (# st', OK# () s #)
    (# st', x #)       -> (# st', unsafeCoerce# x #)
{-# INLINE fails #-}

try :: Parser e a -> Parser e a
try (Parser f) = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', Err# _ #) -> (# st', Fail# #)
    x                  -> x
{-# INLINE try #-}

optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

optional_ :: Parser e a -> Parser e ()
optional_ p = (() <$ p) <|> pure ()
{-# INLINE optional_ #-}

withOption :: Parser e a -> (a -> Parser e b) -> Parser e b -> Parser e b
withOption (Parser p) just (Parser nothing) = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# a s' #) -> runParser# (just a) tag env eob s' st'
    (# st', Fail# #)    -> nothing tag env eob s st'
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE withOption #-}

many_ :: Parser e a -> Parser e ()
many_ (Parser f) = Parser go where
  go tag env eob s st = case f tag env eob s st of
    (# st', OK# _ s' #) -> go tag env eob s' st'
    (# st', Fail# #)    -> (# st', OK# () s #)
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE many_ #-}

some_ :: Parser e a -> Parser e ()
some_ p = p *> many_ p
{-# INLINE some_ #-}

skipMany :: Parser e a -> Parser e ()
skipMany = many_
{-# INLINE skipMany #-}

skipSome :: Parser e a -> Parser e ()
skipSome = some_
{-# INLINE skipSome #-}

------------------------------------------------------------------------
-- Error handling
------------------------------------------------------------------------

err :: e -> Parser e a
err e = Parser \tag env eob s st -> (# st, Err# e #)
{-# INLINE err #-}

cut :: Parser e a -> e -> Parser e a
cut (Parser f) e = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', Fail# #) -> (# st', Err# e #)
    x                -> x
{-# INLINE cut #-}

cutting :: Parser e a -> e -> (e -> e -> e) -> Parser e a
cutting (Parser f) e merge = Parser \tag env eob s st ->
  case f tag env eob s st of
    (# st', Fail# #)  -> (# st', Err# e #)
    (# st', Err# e' #) -> (# st', Err# (merge e' e) #)
    x                   -> x
{-# INLINE cutting #-}

------------------------------------------------------------------------
-- EOF
------------------------------------------------------------------------

eof :: Parser e ()
eof = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, OK# () s #)
    _  -> (# st, Fail# #)
{-# INLINE eof #-}

-- | Non-consuming check: 'True' if at end of input.
atEnd :: Parser e Bool
atEnd = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, OK# True s #)
    _  -> (# st, OK# False s #)
{-# INLINE atEnd #-}

-- | Number of bytes remaining in the current window (cheap, no suspend).
remaining :: Parser e Int
remaining = Parser \tag env eob s st ->
  (# st, OK# (I# (minusAddr# eob s)) s #)
{-# INLINE remaining #-}

------------------------------------------------------------------------
-- Chain combinators
------------------------------------------------------------------------

-- | Left-associative chain.
-- @chainl f p q@ parses @p@ then zero or more @q@, folding left with @f@.
chainl :: (b -> a -> b) -> Parser e b -> Parser e a -> Parser e b
chainl f (Parser p) (Parser q) = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# b s' #) -> chainlGo f q tag env eob s' st' b
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# INLINE chainl #-}

chainlGo :: forall e r b a. (b -> a -> b)
         -> (forall r'. PromptTag# (Step e r') -> ParserEnv -> Addr# -> Addr# -> State# RealWorld -> StRes# e a)
         -> PromptTag# (Step e r) -> ParserEnv -> Addr# -> Addr# -> State# RealWorld -> b -> StRes# e b
chainlGo f q tag env eob s st !acc =
  case q tag env eob s st of
    (# st', OK# a s' #) -> chainlGo f q tag env eob s' st' (f acc a)
    (# st', Fail# #)    -> (# st', OK# acc s #)
    (# st', x #)        -> (# st', unsafeCoerce# x #)
{-# NOINLINE chainlGo #-}

-- | Right-associative chain.
-- @chainr f p q@ parses zero or more @p@, then @q@ at the end,
-- folding right with @f@.
chainr :: (a -> b -> b) -> Parser e a -> Parser e b -> Parser e b
chainr f (Parser p) (Parser q) = Parser go where
  go tag env eob s st =
    case p tag env eob s st of
      (# st', OK# a s' #) -> case go tag env eob s' st' of
        (# st'', OK# b s'' #) -> (# st'', OK# (f a b) s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', Fail# #) -> q tag env eob s st'
      (# st', x #)     -> (# st', unsafeCoerce# x #)
{-# INLINE chainr #-}

------------------------------------------------------------------------
-- Additional error handling
------------------------------------------------------------------------

-- | Like 'ensureN#' but on EOF, errors with a specific error
-- instead of failing recoverably.
ensureNOrEof :: Int -> e -> Parser e ()
ensureNOrEof n e = ensureN# (case n of I# n# -> n#) <|> err e
{-# INLINE ensureNOrEof #-}

------------------------------------------------------------------------
-- Additional position combinators
------------------------------------------------------------------------

-- | Parse something and return both the result and the bytes consumed.
withByteString :: Parser e a -> (a -> ByteString -> Parser e b) -> Parser e b
withByteString (Parser p) f = Parser \tag env eob s st ->
  case p tag env eob s st of
    (# st', OK# a s' #) ->
      let !len = I# (minusAddr# s' s)
          !bs  = BSI.BS (ForeignPtr s (peBackingFp env)) len
      in runParser# (f a bs) tag env eob s' st'
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

anyAsciiHexWord :: Parser e Word
anyAsciiHexWord = Parser \tag env eob s st ->
  case eqAddr# eob s of
    1# -> (# st, Fail# #)
    _  -> case hexDigit (indexWord8OffAddr# s 0#) of
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
