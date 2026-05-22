{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Core parser types, mirroring flatparse's representation for
-- identical inner-loop performance.
--
-- The parser threads @Addr#@ pointers and @State# RealWorld@ directly
-- (no IO wrapper, no boxed Ptr).  The result is an unboxed sum
-- matching flatparse's @Res#@.
--
-- Streaming suspension via @control0#@ is confined to 'ensureNSlow',
-- which is @NOINLINE@ and never fires on the hot path for whole-input
-- parsing.
module Wireform.Parser.Internal
  ( -- * Parser type
    Parser (..)

    -- * Result types
  , type Res#
  , type StRes#
  , pattern OK#
  , pattern Fail#
  , pattern Err#

    -- * Step / Resume (driver protocol)
  , Step (..)
  , Resume (..)

    -- * Parser environment
  , ParserEnv (..)
  , curToPos

    -- * Suspension / checkpoint primitives
  , ensureN#
  , ensureNSlow
  , checkpoint

    -- * End-pointer access
  , readEnd#
  , writeEnd#
  , readEnd
  , writeEnd
  ) where

import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtrContents (..))
import GHC.IO (IO (..))
import GHC.Word (Word64 (..))

------------------------------------------------------------------------
-- Result types — unboxed, identical to flatparse
------------------------------------------------------------------------

-- | Parser result. We drop the State# token from the result type
-- since it's threaded implicitly by IO.  The parser function signature
-- still takes State# and must thread it, but Res# is just the
-- unboxed sum of outcomes.
type Res# e a =
  (#
    (# a, Addr# #)
  | (# #)
  | (# e #)
  #)

pattern OK# :: a -> Addr# -> Res# e a
pattern OK# a s = (# (# a, s #) | | #)

pattern Fail# :: Res# e a
pattern Fail# = (# | (# #) | #)

pattern Err# :: e -> Res# e a
pattern Err# e = (# | | (# e #) #)

{-# COMPLETE OK#, Fail#, Err# #-}

------------------------------------------------------------------------
-- Parser environment (carries mutable end pointer for streaming)
------------------------------------------------------------------------

-- | Shared context for a parse run. The mutable end pointer is the
-- only thing that changes during streaming; everything else is
-- constant for the duration of a @runParser@ call.
data ParserEnv = ParserEnv
  { peEndPtr   :: {-# UNPACK #-} !(Ptr (Ptr Word8))
    -- ^ Mutable end pointer (single deref to read, updated on resume)
  , peBaseAddr :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Ring base address
  , peMask     :: {-# UNPACK #-} !Int
    -- ^ @ringSize - 1@
  , peStartPos :: {-# UNPACK #-} !Word64
    -- ^ Absolute start position of this parse run
  , peInitCur  :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Initial cur address (for absolute-position computation)
  , peBackingFp :: !ForeignPtrContents
    -- ^ Keeps the backing memory alive for zero-copy 'ByteString'
    -- slices.  For 'parseByteString' this is the input BS's own
    -- 'ForeignPtrContents'.  For ring-backed streaming this is
    -- a finalizer that prevents the ring from being freed while
    -- any slice survives.
  }

{-# INLINE curToPos #-}
curToPos :: ParserEnv -> Addr# -> Word64
curToPos env cur =
  let !(Ptr base) = peInitCur env
      !off = I# (minusAddr# cur base)
  in peStartPos env + fromIntegral off

{-# INLINE readEnd# #-}
readEnd# :: ParserEnv -> State# RealWorld -> (# State# RealWorld, Addr# #)
readEnd# env s =
  let !(Ptr p) = peEndPtr env
  in case readAddrOffAddr# p 0# s of
    (# s', a #) -> (# s', a #)

{-# INLINE writeEnd# #-}
writeEnd# :: ParserEnv -> Addr# -> State# RealWorld -> State# RealWorld
writeEnd# env val s =
  let !(Ptr p) = peEndPtr env
  in writeAddrOffAddr# p 0# val s

-- Boxed wrappers for driver code
readEnd :: ParserEnv -> IO (Ptr Word8)
readEnd env = IO \s -> case readEnd# env s of
  (# s', a #) -> (# s', Ptr a #)
{-# INLINE readEnd #-}

writeEnd :: ParserEnv -> Ptr Word8 -> IO ()
writeEnd env (Ptr a) = IO \s -> (# writeEnd# env a s, () #)
{-# INLINE writeEnd #-}

------------------------------------------------------------------------
-- Step / Resume (driver protocol)
------------------------------------------------------------------------

data Step e r
  = StepDone    {-# UNPACK #-} !Word64 r
  | StepFail    {-# UNPACK #-} !Word64
  | StepErr     {-# UNPACK #-} !Word64 e
  | StepSuspend {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Int
                !(Resume e r)
  | StepCheckpoint {-# UNPACK #-} !Word64 !(Resume e r)

data Resume e r = Resume
  { resumeContinue :: !(Ptr Word8 -> Ptr Word8 -> IO (Step e r))
  , resumeEof      :: !(IO (Step e r))
  }

------------------------------------------------------------------------
-- The Parser newtype — flatparse-shaped
------------------------------------------------------------------------

-- | A parser consuming bytes from @(cur, eob)@ pointers.
--
-- The representation mirrors flatparse exactly:
-- @ForeignPtrContents -> Addr# (eob) -> Addr# (cur) -> State# -> Res#@
--
-- The @ParserEnv@ and @PromptTag#@ are additional parameters for
-- streaming support.  For whole-input parsing ('parseByteString'),
-- the PromptTag# is allocated but @ensureNSlow@ is never reached.
-- | Parser result with state token.
type StRes# e a = (# State# RealWorld, Res# e a #)

newtype Parser e a = Parser
  { runParser# :: forall r.
                  PromptTag# (Step e r)
               -> ParserEnv
               -> Addr#         -- eob (end of buffer / current end)
               -> Addr#         -- cur (current position)
               -> State# RealWorld
               -> StRes# e a
  }

instance Functor (Parser e) where
  fmap f (Parser g) = Parser \tag env eob s st ->
    case g tag env eob s st of
      (# st', OK# a s' #) -> let !b = f a in (# st', OK# b s' #)
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE fmap #-}

  a' <$ Parser g = Parser \tag env eob s st ->
    case g tag env eob s st of
      (# st', OK# _ s' #) -> (# st', OK# a' s' #)
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (<$) #-}

instance Applicative (Parser e) where
  pure !a = Parser \tag env eob s st -> (# st, OK# a s #)
  {-# INLINE pure #-}

  Parser ff <*> Parser fa = Parser \tag env eob s st ->
    case ff tag env eob s st of
      (# st', OK# f s' #) -> case fa tag env eob s' st' of
        (# st'', OK# a s'' #) -> let !b = f a in (# st'', OK# b s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*>) #-}

  Parser fa *> Parser fb = Parser \tag env eob s st ->
    case fa tag env eob s st of
      (# st', OK# _ s' #) -> fb tag env eob s' st'
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (*>) #-}

  Parser fa <* Parser fb = Parser \tag env eob s st ->
    case fa tag env eob s st of
      (# st', OK# a s' #) -> case fb tag env eob s' st' of
        (# st'', OK# _ s'' #) -> (# st'', OK# a s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*) #-}

instance Monad (Parser e) where
  Parser fa >>= f = Parser \tag env eob s st ->
    case fa tag env eob s st of
      (# st', OK# a s' #) -> runParser# (f a) tag env eob s' st'
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail (Parser e) where
  fail _ = Parser \tag env eob s st -> (# st, Fail# #)
  {-# INLINE fail #-}

------------------------------------------------------------------------
-- ensureN#: bounds check + suspension
------------------------------------------------------------------------

-- | Require @n#@ bytes available from @cur@.
-- Fast path: single comparison, no memory access to mutable state.
-- Slow path: suspends to the driver via @control0#@.
ensureN# :: Int# -> Parser e ()
ensureN# n# = Parser \tag env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> (# st, OK# () s #)
    _  -> ensureNSlow tag env eob s n# st
{-# INLINE ensureN# #-}

ensureNSlow :: forall e r. PromptTag# (Step e r)
            -> ParserEnv
            -> Addr# -> Addr# -> Int#
            -> State# RealWorld
            -> StRes# e ()
ensureNSlow tag env eob s n# st =
  let !pos    = curToPos env s
      !needed = I# n#
  in control0# tag
       (\k st1 ->
         let resume = Resume
               { resumeContinue = \(Ptr newCur) (Ptr newEnd) ->
                   IO \st2 ->
                     let st3 = writeEnd# env newEnd st2
                     in prompt# tag (\st4 -> k (\st5 -> (# st5, OK# () newCur #)) st4) st3
               , resumeEof =
                   IO \st2 -> prompt# tag (\st3 -> k (\st4 -> (# st4, Fail# #)) st3) st2
               }
         in (# st1, StepSuspend pos needed resume #)
       ) st
{-# NOINLINE ensureNSlow #-}

------------------------------------------------------------------------
-- checkpoint
------------------------------------------------------------------------

checkpoint :: Parser e ()
checkpoint = Parser \tag env eob s st0 ->
  let !pos = curToPos env s
  in control0# tag
       (\k st1 ->
         let resume = Resume
               { resumeContinue = \(Ptr newCur) _ ->
                   IO \st2 -> prompt# tag (\st3 -> k (\st4 -> (# st4, OK# () newCur #)) st3) st2
               , resumeEof =
                   IO \st2 -> prompt# tag (\st3 -> k (\st4 -> (# st4, Fail# #)) st3) st2
               }
         in (# st1, StepCheckpoint pos resume #)
       ) st0
