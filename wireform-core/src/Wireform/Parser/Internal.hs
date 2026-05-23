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
{-# LANGUAGE AllowAmbiguousTypes #-}

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

    -- * Mode types and class
  , Pure, Stream
  , ParserMode (..)

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

    -- * Tag storage
  , tagToAny
  , pePromptTag

    -- * End-pointer access
  , readEnd#
  , writeEnd#
  , readEnd
  , writeEnd
  ) where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import Data.Kind (Type)
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
-- Parser mode (compile-time dispatch for ensure slow path)
------------------------------------------------------------------------

-- | Whole-input mode.  When bounds check fails, just return 'Fail#'.
-- Identical codegen to flatparse — zero suspension overhead.
data Pure

-- | Streaming mode.  When bounds check fails, suspend via @control0#@
-- to wait for more data from the transport.
data Stream

-- | Type class dispatching the ensure slow path.
-- Resolved at compile time via specialization.
class ParserMode (m :: Type) where
  onEnsureFail :: ParserEnv -> Addr# -> Addr# -> Int#
               -> State# RealWorld -> StRes# e ()

instance ParserMode Pure where
  onEnsureFail _env _eob _s _n st = (# st, Fail# #)
  {-# INLINE onEnsureFail #-}

instance ParserMode Stream where
  onEnsureFail = ensureNSlow
  {-# INLINE onEnsureFail #-}

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
    -- ^ Keeps the backing memory alive for zero-copy slices.
  , peTag      :: Any
    -- ^ PromptTag# stored as Any via unsafeCoerce#.
    -- Only meaningful for streaming mode.
  }

-- | Recover the typed PromptTag# from the env. Only call from ensureNSlow.
{-# INLINE pePromptTag #-}
pePromptTag :: forall e r. ParserEnv -> PromptTag# (Step e r)
pePromptTag env = unsafeCoerce# (peTag env)

-- | Store a PromptTag# into an Any-typed field.
{-# INLINE tagToAny #-}
tagToAny :: PromptTag# a -> Any
tagToAny t = unsafeCoerce# t

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

-- | The parser type.  Parameterized by mode @m@ ('Pure' or 'Stream'),
-- error type @e@, and result type @a@.
--
-- Four args on the hot path: env, eob, cur, state.  The mode @m@ is
-- phantom — it controls which 'ParserMode' instance is used for
-- bounds-check failures, resolved at compile time.
newtype Parser (m :: Type) e a = Parser
  { runParser# :: ParserEnv
               -> Addr#         -- eob (end of buffer / current end)
               -> Addr#         -- cur (current position)
               -> State# RealWorld
               -> StRes# e a
  }

instance Functor (Parser m e) where
  fmap f (Parser g) = Parser \env eob s st ->
    case g env eob s st of
      (# st', OK# a s' #) -> let !b = f a in (# st', OK# b s' #)
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE fmap #-}

  a' <$ Parser g = Parser \env eob s st ->
    case g env eob s st of
      (# st', OK# _ s' #) -> (# st', OK# a' s' #)
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (<$) #-}

instance Applicative (Parser m e) where
  pure !a = Parser \env eob s st -> (# st, OK# a s #)
  {-# INLINE pure #-}

  Parser ff <*> Parser fa = Parser \env eob s st ->
    case ff env eob s st of
      (# st', OK# f s' #) -> case fa env eob s' st' of
        (# st'', OK# a s'' #) -> let !b = f a in (# st'', OK# b s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*>) #-}

  Parser fa *> Parser fb = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# _ s' #) -> fb env eob s' st'
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (*>) #-}

  Parser fa <* Parser fb = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# a s' #) -> case fb env eob s' st' of
        (# st'', OK# _ s'' #) -> (# st'', OK# a s'' #)
        (# st'', x #)         -> (# st'', unsafeCoerce# x #)
      (# st', x #) -> (# st', unsafeCoerce# x #)
  {-# INLINE (<*) #-}

instance Monad (Parser m e) where
  Parser fa >>= f = Parser \env eob s st ->
    case fa env eob s st of
      (# st', OK# a s' #) -> runParser# (f a) env eob s' st'
      (# st', x #)        -> (# st', unsafeCoerce# x #)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFail (Parser m e) where
  fail _ = Parser \env eob s st -> (# st, Fail# #)
  {-# INLINE fail #-}

instance ParserMode m => Alternative (Parser m e) where
  empty = Parser \env eob s st -> (# st, Fail# #)
  {-# INLINE empty #-}
  (Parser f) <|> (Parser g) = Parser \env eob s st ->
    case f env eob s st of
      (# st', Fail# #) -> g env eob s st'
      x                -> x
  {-# INLINE (<|>) #-}

instance ParserMode m => MonadPlus (Parser m e)

------------------------------------------------------------------------
-- ensureN#: bounds check + suspension
------------------------------------------------------------------------

-- | Require @n#@ bytes available from @cur@.
-- Fast path: single comparison, no memory access to mutable state.
-- Slow path: suspends to the driver via @control0#@.
ensureN# :: forall m e. ParserMode m => Int# -> Parser m e ()
ensureN# n# = Parser \env eob s st ->
  case n# <=# minusAddr# eob s of
    1# -> (# st, OK# () s #)
    _  -> onEnsureFail @m env eob s n# st
{-# INLINE ensureN# #-}

-- | Streaming slow path.
--
-- Before suspending via @control0#@, re-read the mutable end pointer.
-- After a prior resume, @>>=@ threads the stale @eob@ that was captured
-- in its closure, so the fast-path comparison in 'ensureN#' fails even
-- though new data is already available in the ring.  The re-read here
-- catches that case and avoids a redundant suspension round-trip.
ensureNSlow :: ParserEnv
            -> Addr# -> Addr# -> Int#
            -> State# RealWorld
            -> StRes# e ()
ensureNSlow env _eob s n# st =
  case readEnd# env st of
    (# st0, eob' #) ->
      case n# <=# minusAddr# eob' s of
        1# -> (# st0, OK# () s #)
        _  ->
          let tag :: PromptTag# (Step Any Any)
              tag = unsafeCoerce# (peTag env) in
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
               ) st0
{-# NOINLINE ensureNSlow #-}

------------------------------------------------------------------------
-- checkpoint
------------------------------------------------------------------------

-- | Checkpoint is only meaningful in streaming mode.
checkpoint :: Parser Stream e ()
checkpoint = Parser \env eob s st0 ->
  let !pos = curToPos env s
      tag :: PromptTag# (Step Any Any)
      tag = unsafeCoerce# (peTag env)
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
