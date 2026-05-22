{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE CPP #-}

-- | Fast streaming parser with flatparse-grade performance.
--
-- The parser operates over a magic ring buffer and supports transparent
-- suspension when more data is needed.  The suspension mechanism uses
-- GHC's delimited continuation primops (@prompt#@ / @control0#@, GHC 9.6+).
--
-- = Entry points
--
-- * 'runParser' — parse once from a 'Transport' (streaming).
-- * 'runParserLoop' — parse repeatedly until EOF or callback stop.
-- * 'parseByteString' — parse a whole 'ByteString' (non-streaming, flatparse-equivalent).
--
-- = Combinators
--
-- The API is a near-complete port of @FlatParse.Basic@.  See the
-- individual combinators for streaming-specific differences.
module Wireform.Parser
  ( -- * Parser type
    Parser

    -- * Byte primitives
  , anyWord8
  , anyWord8_
  , anyInt8

    -- * Multi-byte (native endianness)
  , anyWord16
  , anyWord32
  , anyWord64
  , anyInt16
  , anyInt32
  , anyInt64

    -- * Multi-byte (little-endian)
  , anyWord16le
  , anyWord32le
  , anyWord64le
  , anyInt16le
  , anyInt32le
  , anyInt64le

    -- * Multi-byte (big-endian)
  , anyWord16be
  , anyWord32be
  , anyWord64be
  , anyInt16be
  , anyInt32be
  , anyInt64be

    -- * Skip variants
  , anyWord16_
  , anyWord32_
  , anyWord64_

    -- * Floating-point
  , anyFloatle, anyFloatbe
  , anyDoublele, anyDoublebe

    -- * Byte matching
  , word8
  , bytes

    -- * ByteString operations
  , takeBs
  , takeRef
  , skip
  , takeRest

    -- * UTF-8 characters
  , anyChar
  , anyChar_
  , anyCharASCII
  , anyCharASCII_
  , satisfy
  , satisfy_
  , satisfyASCII
  , satisfyASCII_
  , char

    -- * Character classes
  , isDigit
  , isLatinLetter

    -- * ASCII numeric
  , anyAsciiDecimalWord
  , anyAsciiDecimalInt

    -- * Control flow
  , (<|>)
  , empty
  , branch
  , lookahead
  , fails
  , try
  , optional
  , optional_
  , many_
  , some_

    -- * Error handling
  , err
  , cut
  , cutting

    -- * Position and span
  , Pos (..)
  , Span (..)
  , getPos
  , withSpan
  , byteStringOf

    -- * Marks
  , Mark
  , mark
  , restore
  , release

    -- * Low-level
  , ensureN
  , checkpoint

    -- * EOF
  , eof

    -- * Re-exports
  , module Wireform.Parser.Error
  ) where

import Control.Monad (when)
import Data.Bits ((.&.), shiftL, shiftR, xor)
import qualified Data.Bits as Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (chr, ord)
import Data.IORef
import Data.Word
import Data.Int
import Foreign.Ptr (Ptr, plusPtr, minusPtr, castPtr)
import Foreign.Storable (Storable (..))
import GHC.Exts (PromptTag#)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)

import Wireform.Parser.Internal
import Wireform.Parser.Error
import Wireform.Parser.Position
import Wireform.Parser.Mark

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Read n bytes, ensuring they are available first.
-- After ensureN succeeds, the data at cur is guaranteed valid.
withEnsure :: forall e a. Int -> (Ptr Word8 -> IO a) -> Parser e a
withEnsure !n readFn = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= n
    then do
      !a <- readFn cur
      pure (OK a (cur `plusPtr` n))
    else do
      r <- ensureNSlow tag env cur n
      case r of
        OK () newCur -> do
          !a <- readFn newCur
          pure (OK a (newCur `plusPtr` n))
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE withEnsure #-}

withEnsure_ :: Int -> Parser e ()
withEnsure_ !n = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= n
    then pure (OK () (cur `plusPtr` n))
    else do
      r <- ensureNSlow tag env cur n
      case r of
        OK () newCur -> pure (OK () (newCur `plusPtr` n))
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE withEnsure_ #-}

------------------------------------------------------------------------
-- Byte primitives
------------------------------------------------------------------------

anyWord8 :: Parser e Word8
anyWord8 = withEnsure 1 peek
{-# INLINE anyWord8 #-}

anyWord8_ :: Parser e ()
anyWord8_ = withEnsure_ 1
{-# INLINE anyWord8_ #-}

anyInt8 :: Parser e Int8
anyInt8 = withEnsure 1 (peek . castPtr)
{-# INLINE anyInt8 #-}

------------------------------------------------------------------------
-- Multi-byte, native endianness
------------------------------------------------------------------------

anyWord16 :: Parser e Word16
anyWord16 = withEnsure 2 (peek . castPtr)
{-# INLINE anyWord16 #-}

anyWord32 :: Parser e Word32
anyWord32 = withEnsure 4 (peek . castPtr)
{-# INLINE anyWord32 #-}

anyWord64 :: Parser e Word64
anyWord64 = withEnsure 8 (peek . castPtr)
{-# INLINE anyWord64 #-}

anyInt16 :: Parser e Int16
anyInt16 = withEnsure 2 (peek . castPtr)
{-# INLINE anyInt16 #-}

anyInt32 :: Parser e Int32
anyInt32 = withEnsure 4 (peek . castPtr)
{-# INLINE anyInt32 #-}

anyInt64 :: Parser e Int64
anyInt64 = withEnsure 8 (peek . castPtr)
{-# INLINE anyInt64 #-}

------------------------------------------------------------------------
-- Little-endian
------------------------------------------------------------------------

anyWord16le :: Parser e Word16
anyWord16le = withEnsure 2 \p -> fromLE16 <$> peek (castPtr p)
{-# INLINE anyWord16le #-}

anyWord32le :: Parser e Word32
anyWord32le = withEnsure 4 \p -> fromLE32 <$> peek (castPtr p)
{-# INLINE anyWord32le #-}

anyWord64le :: Parser e Word64
anyWord64le = withEnsure 8 \p -> fromLE64 <$> peek (castPtr p)
{-# INLINE anyWord64le #-}

anyInt16le :: Parser e Int16
anyInt16le = fromIntegral <$> anyWord16le
{-# INLINE anyInt16le #-}

anyInt32le :: Parser e Int32
anyInt32le = fromIntegral <$> anyWord32le
{-# INLINE anyInt32le #-}

anyInt64le :: Parser e Int64
anyInt64le = fromIntegral <$> anyWord64le
{-# INLINE anyInt64le #-}

------------------------------------------------------------------------
-- Big-endian
------------------------------------------------------------------------

anyWord16be :: Parser e Word16
anyWord16be = withEnsure 2 \p -> fromBE16 <$> peek (castPtr p)
{-# INLINE anyWord16be #-}

anyWord32be :: Parser e Word32
anyWord32be = withEnsure 4 \p -> fromBE32 <$> peek (castPtr p)
{-# INLINE anyWord32be #-}

anyWord64be :: Parser e Word64
anyWord64be = withEnsure 8 \p -> fromBE64 <$> peek (castPtr p)
{-# INLINE anyWord64be #-}

anyInt16be :: Parser e Int16
anyInt16be = fromIntegral <$> anyWord16be
{-# INLINE anyInt16be #-}

anyInt32be :: Parser e Int32
anyInt32be = fromIntegral <$> anyWord32be
{-# INLINE anyInt32be #-}

anyInt64be :: Parser e Int64
anyInt64be = fromIntegral <$> anyWord64be
{-# INLINE anyInt64be #-}

------------------------------------------------------------------------
-- Skip variants
------------------------------------------------------------------------

anyWord16_ :: Parser e ()
anyWord16_ = withEnsure_ 2
{-# INLINE anyWord16_ #-}

anyWord32_ :: Parser e ()
anyWord32_ = withEnsure_ 4
{-# INLINE anyWord32_ #-}

anyWord64_ :: Parser e ()
anyWord64_ = withEnsure_ 8
{-# INLINE anyWord64_ #-}

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

-- | Match a specific byte; fail if mismatch.
word8 :: Word8 -> Parser e ()
word8 !expected = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= 1
    then do
      !w <- peek cur
      if w == expected
        then pure (OK () (cur `plusPtr` 1))
        else pure Fail
    else do
      r <- ensureNSlow tag env cur 1
      case r of
        OK () newCur -> do
          !w <- peek newCur
          if w == expected
            then pure (OK () (newCur `plusPtr` 1))
            else pure Fail
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE word8 #-}

-- | Match a literal byte sequence; fail on mismatch.
bytes :: ByteString -> Parser e ()
bytes !bs = Parser \tag env cur -> do
  let !len = BS.length bs
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= len
    then matchBytes bs cur len
    else do
      r <- ensureNSlow tag env cur len
      case r of
        OK () newCur -> matchBytes bs newCur len
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE bytes #-}

matchBytes :: ByteString -> Ptr Word8 -> Int -> IO (Res e ())
matchBytes bs ptr len =
  BSU.unsafeUseAsCStringLen bs \(bsPtr, _) -> do
    eq <- BSI.memcmp (castPtr ptr) (castPtr bsPtr) len
    if eq == 0
      then pure (OK () (ptr `plusPtr` len))
      else pure Fail

------------------------------------------------------------------------
-- ByteString operations
------------------------------------------------------------------------

-- | Consume @n@ bytes and return a copy.
takeBs :: Int -> Parser e ByteString
takeBs !n = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= n
    then do
      bs <- copyFromRing cur n
      pure (OK bs (cur `plusPtr` n))
    else do
      r <- ensureNSlow tag env cur n
      case r of
        OK () newCur -> do
          bs <- copyFromRing newCur n
          pure (OK bs (newCur `plusPtr` n))
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE takeBs #-}

-- | Zero-copy reference into the ring.  Caller must consume before
-- the tail advances past these bytes.
takeRef :: Int -> Parser e (Ptr Word8, Int)
takeRef !n = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= n
    then pure (OK (cur, n) (cur `plusPtr` n))
    else do
      r <- ensureNSlow tag env cur n
      case r of
        OK () newCur -> pure (OK (newCur, n) (newCur `plusPtr` n))
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE takeRef #-}

-- | Skip @n@ bytes without copying.
skip :: Int -> Parser e ()
skip !n = withEnsure_ n
{-# INLINE skip #-}

-- | Consume all remaining bytes (copies).
-- Only meaningful in non-streaming mode or after framing.
takeRest :: Parser e ByteString
takeRest = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !len = end `minusPtr` cur
  if len <= 0
    then pure (OK BS.empty cur)
    else do
      bs <- copyFromRing cur len
      pure (OK bs (cur `plusPtr` len))

copyFromRing :: Ptr Word8 -> Int -> IO ByteString
copyFromRing src len = BSI.create len \dst -> BSI.memcpy dst src len

------------------------------------------------------------------------
-- UTF-8 character primitives
------------------------------------------------------------------------

anyChar :: Parser e Char
anyChar = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= 1
    then decodeUtf8 tag env cur end avail
    else do
      r <- ensureNSlow tag env cur 1
      case r of
        OK () newCur -> do
          end' <- readIORef (peEndRef env)
          let !avail' = end' `minusPtr` newCur
          decodeUtf8 tag env newCur end' avail'
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE anyChar #-}

decodeUtf8 :: forall e r. PromptTag# (Step e r)
           -> ParserEnv -> Ptr Word8 -> Ptr Word8 -> Int -> IO (Res e Char)
decodeUtf8 tag env cur end avail = do
  (b0 :: Word8) <- peek cur
  if b0 < 0x80
    then pure (OK (chr (fromIntegral b0)) (cur `plusPtr` 1))
    else if b0 < 0xC0
      then pure Fail
      else if b0 < 0xE0
        then do
          ensure2 tag env cur 2 \p -> do
            (b1 :: Word8) <- peek (p `plusPtr` 1)
            let !c = ((fromIntegral b0 .&. 0x1F) `shiftL` 6)
                   + (fromIntegral b1 .&. 0x3F) :: Int
            pure (OK (chr c) (p `plusPtr` 2))
        else if b0 < 0xF0
          then do
            ensure2 tag env cur 3 \p -> do
              (b1 :: Word8) <- peek (p `plusPtr` 1)
              (b2 :: Word8) <- peek (p `plusPtr` 2)
              let !c = ((fromIntegral b0 .&. 0x0F) `shiftL` 12)
                     + ((fromIntegral b1 .&. 0x3F) `shiftL` 6)
                     + (fromIntegral b2 .&. 0x3F) :: Int
              pure (OK (chr c) (p `plusPtr` 3))
          else do
            ensure2 tag env cur 4 \p -> do
              (b1 :: Word8) <- peek (p `plusPtr` 1)
              (b2 :: Word8) <- peek (p `plusPtr` 2)
              (b3 :: Word8) <- peek (p `plusPtr` 3)
              let !c = ((fromIntegral b0 .&. 0x07) `shiftL` 18)
                     + ((fromIntegral b1 .&. 0x3F) `shiftL` 12)
                     + ((fromIntegral b2 .&. 0x3F) `shiftL` 6)
                     + (fromIntegral b3 .&. 0x3F) :: Int
              pure (OK (chr c) (p `plusPtr` 4))
  where
    ensure2 :: PromptTag# (Step e r) -> ParserEnv -> Ptr Word8 -> Int
            -> (Ptr Word8 -> IO (Res e Char)) -> IO (Res e Char)
    ensure2 t e c n cont
      | avail >= n = cont c
      | otherwise  = do
          r <- ensureNSlow t e c n
          case r of
            OK () newCur -> cont newCur
            Fail  -> pure Fail
            Err err' -> pure (Err err')
    {-# INLINE ensure2 #-}

anyChar_ :: Parser e ()
anyChar_ = anyChar *> pure ()
{-# INLINE anyChar_ #-}

anyCharASCII :: Parser e Char
anyCharASCII = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= 1
    then do
      !b <- peek cur
      if b < 0x80
        then pure (OK (chr (fromIntegral b)) (cur `plusPtr` 1))
        else pure Fail
    else do
      r <- ensureNSlow tag env cur 1
      case r of
        OK () newCur -> do
          !b <- peek newCur
          if b < 0x80
            then pure (OK (chr (fromIntegral b)) (newCur `plusPtr` 1))
            else pure Fail
        Fail  -> pure Fail
        Err e -> pure (Err e)
{-# INLINE anyCharASCII #-}

anyCharASCII_ :: Parser e ()
anyCharASCII_ = anyCharASCII *> pure ()
{-# INLINE anyCharASCII_ #-}

satisfy :: (Char -> Bool) -> Parser e Char
satisfy f = do
  c <- anyChar
  if f c then pure c else Parser \tag env cur -> pure Fail
{-# INLINE satisfy #-}

satisfy_ :: (Char -> Bool) -> Parser e ()
satisfy_ f = satisfy f *> pure ()
{-# INLINE satisfy_ #-}

satisfyASCII :: (Char -> Bool) -> Parser e Char
satisfyASCII f = do
  c <- anyCharASCII
  if f c then pure c else Parser \tag env cur -> pure Fail
{-# INLINE satisfyASCII #-}

satisfyASCII_ :: (Char -> Bool) -> Parser e ()
satisfyASCII_ f = satisfyASCII f *> pure ()
{-# INLINE satisfyASCII_ #-}

char :: Char -> Parser e ()
char c
  | ord c < 0x80 = word8 (fromIntegral (ord c))
  | otherwise     = satisfy_ (== c)
{-# INLINE char #-}

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
anyAsciiDecimalWord = do
  !d0 <- satisfyASCII isDigit
  go (fromIntegral (ord d0 - ord '0'))
  where
    go !acc = Parser \tag env cur -> do
      end <- readIORef (peEndRef env)
      if cur `minusPtr` end >= 0
        then pure (OK acc cur)
        else do
          !b <- peek cur
          if b >= 0x30 && b <= 0x39
            then unParser (go (acc * 10 + fromIntegral (b - 0x30))) tag env (cur `plusPtr` 1)
            else pure (OK acc cur)
{-# INLINE anyAsciiDecimalWord #-}

anyAsciiDecimalInt :: Parser e Int
anyAsciiDecimalInt = fromIntegral <$> anyAsciiDecimalWord
{-# INLINE anyAsciiDecimalInt #-}

------------------------------------------------------------------------
-- Control flow combinators
------------------------------------------------------------------------

empty :: Parser e a
empty = Parser \tag env cur -> pure Fail
{-# INLINE empty #-}

infixr 3 <|>

-- | Try the left parser; if it fails (Fail, not Err), restore position
-- and try the right parser.
(<|>) :: Parser e a -> Parser e a -> Parser e a
Parser p <|> Parser q = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK a cur' -> pure (OK a cur')
    Fail      -> q tag env cur  -- restore to original cur
    Err e     -> pure (Err e)
{-# INLINE (<|>) #-}

-- | @branch p t f@: if @p@ succeeds, run @t@; else run @f@.
branch :: Parser e a -> Parser e b -> Parser e b -> Parser e b
branch p t f = Parser \tag env cur -> do
  r <- unParser p tag env cur
  case r of
    OK _ cur' -> unParser t tag env cur'
    Fail      -> unParser f tag env cur
    Err e     -> pure (Err e)
{-# INLINE branch #-}

-- | Lookahead: run without consuming on success.
lookahead :: Parser e a -> Parser e a
lookahead (Parser p) = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK a _cur' -> pure (OK a cur)  -- restore cur
    Fail       -> pure Fail
    Err e      -> pure (Err e)
{-# INLINE lookahead #-}

-- | Negative lookahead: succeed iff @p@ fails.
fails :: Parser e a -> Parser e ()
fails (Parser p) = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK _ _  -> pure Fail
    Fail    -> pure (OK () cur)
    Err e   -> pure (Err e)
{-# INLINE fails #-}

-- | On Err, convert to Fail (re-allow backtracking).
try :: Parser e a -> Parser e a
try (Parser p) = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK a c -> pure (OK a c)
    Fail   -> pure Fail
    Err _  -> pure Fail
{-# INLINE try #-}

optional :: Parser e a -> Parser e (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
{-# INLINE optional #-}

optional_ :: Parser e a -> Parser e ()
optional_ p = (p *> pure ()) <|> pure ()
{-# INLINE optional_ #-}

-- | Skip-many: run @p@ repeatedly until failure, discarding results.
many_ :: Parser e a -> Parser e ()
many_ p = go
  where
    go = (p *> go) <|> pure ()
{-# INLINE many_ #-}

-- | Some: run @p@ at least once, then skip the rest.
some_ :: Parser e a -> Parser e ()
some_ p = p *> many_ p
{-# INLINE some_ #-}

------------------------------------------------------------------------
-- Error handling
------------------------------------------------------------------------

-- | Throw an unrecoverable error. Bypasses @\<|\>@ backtracking.
err :: e -> Parser e a
err e = Parser \tag env cur -> pure (Err e)
{-# INLINE err #-}

-- | If the inner parser fails with Fail, convert to Err with the given error.
cut :: Parser e a -> e -> Parser e a
cut (Parser p) e = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK a c -> pure (OK a c)
    Fail   -> pure (Err e)
    Err e' -> pure (Err e')
{-# INLINE cut #-}

-- | Like 'cut' but with a merge function for combining errors.
cutting :: Parser e a -> e -> (e -> e -> e) -> Parser e a
cutting (Parser p) e merge = Parser \tag env cur -> do
  r <- p tag env cur
  case r of
    OK a c  -> pure (OK a c)
    Fail    -> pure (Err e)
    Err e'  -> pure (Err (merge e' e))
{-# INLINE cutting #-}

------------------------------------------------------------------------
-- EOF
------------------------------------------------------------------------

-- | Succeed iff at end of input.
-- In streaming mode, this may suspend to determine whether more data
-- is coming.  If the transport has reported EOF and cur == end,
-- this succeeds.  Otherwise fails.
eof :: Parser e ()
eof = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  if cur `minusPtr` end < 0
    then pure Fail  -- there's data remaining
    else do
      r <- ensureNSlow tag env cur 1
      case r of
        OK () _newCur -> pure Fail  -- data arrived, not at EOF
        Fail          -> pure (OK () cur)  -- genuine EOF
        Err e         -> pure (Err e)
{-# INLINE eof #-}

------------------------------------------------------------------------
-- Byte-order helpers (compile to bswap or noop on x86_64)
------------------------------------------------------------------------

#if defined(WORDS_BIGENDIAN)
fromLE16 :: Word16 -> Word16
fromLE16 = byteSwap16
fromLE32 :: Word32 -> Word32
fromLE32 = byteSwap32
fromLE64 :: Word64 -> Word64
fromLE64 = byteSwap64
fromBE16 :: Word16 -> Word16
fromBE16 = id
fromBE32 :: Word32 -> Word32
fromBE32 = id
fromBE64 :: Word64 -> Word64
fromBE64 = id
#else
fromLE16 :: Word16 -> Word16
fromLE16 = id
{-# INLINE fromLE16 #-}
fromLE32 :: Word32 -> Word32
fromLE32 = id
{-# INLINE fromLE32 #-}
fromLE64 :: Word64 -> Word64
fromLE64 = id
{-# INLINE fromLE64 #-}
fromBE16 :: Word16 -> Word16
fromBE16 = byteSwap16
{-# INLINE fromBE16 #-}
fromBE32 :: Word32 -> Word32
fromBE32 = byteSwap32
{-# INLINE fromBE32 #-}
fromBE64 :: Word64 -> Word64
fromBE64 = byteSwap64
{-# INLINE fromBE64 #-}
#endif
