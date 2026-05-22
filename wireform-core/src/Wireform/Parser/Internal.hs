{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

-- | Core parser types and the suspension primitive.
--
-- Not intended for direct import.  Use "Wireform.Parser" instead.
module Wireform.Parser.Internal
  ( -- * Parser type
    Parser (..)

    -- * Result type
  , Res (..)

    -- * Step / Resume (driver protocol)
  , Step (..)
  , Resume (..)

    -- * Parser environment
  , ParserEnv (..)
  , curToPos

    -- * Suspension primitive
  , ensureN
  , ensureNSlow
  , checkpoint
  ) where

import Control.Monad (ap)
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import GHC.Exts
  ( PromptTag#, prompt#, control0#
  , State#, RealWorld
  )
import GHC.IO (IO (..))

------------------------------------------------------------------------
-- Parser result (what sub-parsers return to each other)
------------------------------------------------------------------------

-- | Direct return modes of a parser.
data Res e a
  = OK !a {-# UNPACK #-} !(Ptr Word8)
    -- ^ Success with value and updated cur pointer
  | Fail
    -- ^ Recoverable failure; @\<|\>@ may try an alternative
  | Err !e
    -- ^ Unrecoverable error (after cut\/commit)

------------------------------------------------------------------------
-- Parser environment
------------------------------------------------------------------------

-- | Read/write context shared by all parsers in a single @runParser@ call.
data ParserEnv = ParserEnv
  { peEndRef   :: {-# UNPACK #-} !(IORef (Ptr Word8))
    -- ^ Mutable end pointer; updated by the driver on resumption.
  , peBaseAddr :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Base address of the magic ring.
  , peMask     :: {-# UNPACK #-} !Int
    -- ^ @ringSize - 1@ for modular indexing.
  , peStartPos :: {-# UNPACK #-} !Word64
    -- ^ Absolute stream position at parse-run start.
  , peInitCur  :: {-# UNPACK #-} !(Ptr Word8)
    -- ^ Initial cur pointer; together with 'peStartPos', lets us convert
    -- pointer offsets to absolute positions.
  }

{-# INLINE curToPos #-}
curToPos :: ParserEnv -> Ptr Word8 -> Word64
curToPos env cur =
  peStartPos env + fromIntegral (cur `minusPtr` peInitCur env)

------------------------------------------------------------------------
-- Step / Resume (driver protocol)
------------------------------------------------------------------------

-- | Outcome of a @prompt#@ frame.  The driver matches on this after
-- each parser round trip.
--
-- @e@ is the user error type; @r@ is the final result type of the
-- enclosing @runParser@ call.
data Step e r
  = StepDone    {-# UNPACK #-} !Word64 r
  | StepFail    {-# UNPACK #-} !Word64
  | StepErr     {-# UNPACK #-} !Word64 e
  | StepSuspend {-# UNPACK #-} !Word64
                {-# UNPACK #-} !Int
                !(Resume e r)
  | StepCheckpoint {-# UNPACK #-} !Word64 !(Resume e r)

-- | Suspended continuation.  Invoke exactly one method, exactly once.
data Resume e r = Resume
  { resumeContinue :: !(Ptr Word8 -> Ptr Word8 -> IO (Step e r))
  , resumeEof      :: !(IO (Step e r))
  }

------------------------------------------------------------------------
-- The Parser newtype
------------------------------------------------------------------------

-- | A parser consuming bytes from a pointer window inside a magic ring.
--
-- The prompt tag is threaded explicitly (via @forall r@) so that
-- suspension can type-check without parameterizing the environment
-- on the final result type.
newtype Parser e a = Parser
  { unParser :: forall r.
                PromptTag# (Step e r)
             -> ParserEnv
             -> Ptr Word8     -- cur
             -> IO (Res e a)
  }

instance Functor (Parser e) where
  fmap f (Parser p) = Parser \tag env cur -> do
    r <- p tag env cur
    pure $ case r of
      OK a cur' -> OK (f a) cur'
      Fail      -> Fail
      Err e     -> Err e
  {-# INLINE fmap #-}

instance Applicative (Parser e) where
  pure a = Parser \tag env cur -> pure (OK a cur)
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Parser e) where
  Parser p >>= f = Parser \tag env cur -> do
    r <- p tag env cur
    case r of
      OK a cur' -> unParser (f a) tag env cur'
      Fail      -> pure Fail
      Err e     -> pure (Err e)
  {-# INLINE (>>=) #-}

instance MonadFail (Parser e) where
  fail _ = Parser \tag env cur -> pure Fail
  {-# INLINE fail #-}

------------------------------------------------------------------------
-- ensureN: the core suspension primitive
------------------------------------------------------------------------

-- | Require at least @n@ bytes available from the current position.
-- If enough bytes are in the window, returns immediately.
-- Otherwise suspends via @control0#@ and the driver waits for data.
ensureN :: Int -> Parser e ()
ensureN !n = Parser \tag env cur -> do
  end <- readIORef (peEndRef env)
  let !avail = end `minusPtr` cur
  if avail >= n
    then pure (OK () cur)
    else ensureNSlow tag env cur n
{-# INLINE ensureN #-}

-- | Slow path: suspend to the driver.  After resumption, the driver
-- has guaranteed at least @needed@ bytes are available from @cur@.
-- On EOF, returns 'Fail' (recoverable, so @\<|\>@ can handle it).
ensureNSlow :: forall e r. PromptTag# (Step e r)
            -> ParserEnv -> Ptr Word8 -> Int
            -> IO (Res e ())
ensureNSlow tag env cur needed = IO \s0 ->
  let !pos = curToPos env cur
  in control0# tag
       (\k s1 ->
         let resume = Resume
               { resumeContinue = \newCur newEnd -> do
                   writeIORef (peEndRef env) newEnd
                   IO \s2 -> prompt# tag
                     (\s3 -> k (unIO (pure (OK () newCur))) s3)
                     s2
               , resumeEof = do
                   IO \s2 -> prompt# tag
                     (\s3 -> k (unIO (pure Fail)) s3)
                     s2
               }
         in (# s1, StepSuspend pos needed resume #)
       ) s0
  where
    unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
    unIO (IO f) = f
{-# NOINLINE ensureNSlow #-}

------------------------------------------------------------------------
-- checkpoint: release consumed bytes mid-parse
------------------------------------------------------------------------

-- | Release consumed bytes back to the producer.  The driver advances
-- the tail to the current position (respecting marks) and immediately
-- resumes.  No transport interaction.
checkpoint :: Parser e ()
checkpoint = Parser \tag env cur -> IO \s0 ->
  let !pos = curToPos env cur
  in control0# tag
       (\k s1 ->
         let resume = Resume
               { resumeContinue = \newCur _ ->
                   IO \s2 -> prompt# tag
                     (\s3 -> k (unIO (pure (OK () newCur))) s3)
                     s2
               , resumeEof =
                   IO \s2 -> prompt# tag
                     (\s3 -> k (unIO (pure Fail)) s3)
                     s2
               }
         in (# s1, StepCheckpoint pos resume #)
       ) s0
  where
    unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
    unIO (IO f) = f
