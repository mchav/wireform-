{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Wireform.Parser.Internal
  ( Parser (..)
  , Res (..)
  , Step (..)
  , Resume (..)
  , ParserEnv (..)
  , curToPos
  , ensureN
  , ensureNSlow
  , checkpoint
  , readEnd
  , writeEnd
  ) where

import Control.Monad (ap)
import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Ptr (Ptr, plusPtr, minusPtr)
import Foreign.Storable (peek, poke)
import Foreign.Marshal.Alloc (mallocBytes, free)
import GHC.Exts
  ( PromptTag#, prompt#, control0#
  , State#, RealWorld
  )
import GHC.IO (IO (..))

------------------------------------------------------------------------
-- Parser result
------------------------------------------------------------------------

data Res e a
  = OK !a {-# UNPACK #-} !(Ptr Word8)
  | Fail
  | Err !e

------------------------------------------------------------------------
-- Parser environment
------------------------------------------------------------------------

-- | The end pointer is stored as a @Ptr (Ptr Word8)@ — a single
-- pointer dereference to read, versus IORef which goes through
-- MutableVar# and a heap object.
data ParserEnv = ParserEnv
  { peEndPtr   :: {-# UNPACK #-} !(Ptr (Ptr Word8))
    -- ^ Mutable end pointer (one machine word, single deref to read)
  , peBaseAddr :: {-# UNPACK #-} !(Ptr Word8)
  , peMask     :: {-# UNPACK #-} !Int
  , peStartPos :: {-# UNPACK #-} !Word64
  , peInitCur  :: {-# UNPACK #-} !(Ptr Word8)
  }

{-# INLINE readEnd #-}
readEnd :: ParserEnv -> IO (Ptr Word8)
readEnd env = peek (peEndPtr env)

{-# INLINE writeEnd #-}
writeEnd :: ParserEnv -> Ptr Word8 -> IO ()
writeEnd env = poke (peEndPtr env)

{-# INLINE curToPos #-}
curToPos :: ParserEnv -> Ptr Word8 -> Word64
curToPos env cur =
  peStartPos env + fromIntegral (cur `minusPtr` peInitCur env)

------------------------------------------------------------------------
-- Step / Resume
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
-- Parser
------------------------------------------------------------------------

newtype Parser e a = Parser
  { unParser :: forall r.
                PromptTag# (Step e r)
             -> ParserEnv
             -> Ptr Word8
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
-- ensureN
------------------------------------------------------------------------

ensureN :: Int -> Parser e ()
ensureN !n = Parser \tag env cur -> do
  end <- readEnd env
  let !avail = end `minusPtr` cur
  if avail >= n
    then pure (OK () cur)
    else ensureNSlow tag env cur n
{-# INLINE ensureN #-}

ensureNSlow :: forall e r. PromptTag# (Step e r)
            -> ParserEnv -> Ptr Word8 -> Int
            -> IO (Res e ())
ensureNSlow tag env cur needed = IO \s0 ->
  let !pos = curToPos env cur
  in control0# tag
       (\k s1 ->
         let resume = Resume
               { resumeContinue = \newCur newEnd -> do
                   writeEnd env newEnd
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
-- checkpoint
------------------------------------------------------------------------

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
