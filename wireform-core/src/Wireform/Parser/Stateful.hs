{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExplicitNamespaces #-}

-- | Stateful parser variant with a reader environment @r@ and mutable
-- state @s@, following flatparse's @FlatParse.Stateful@ design.
--
-- The state is stored in a heap-allocated @MutVar#@ for uniform
-- representation (state values may be of arbitrary type).  Reader
-- is passed as a plain boxed value.
--
-- All combinators from "Wireform.Parser" have stateful counterparts
-- with the same names.  The basic parser is recovered as
-- @ParserS () () e a@.
module Wireform.Parser.Stateful
  ( -- * Parser type
    ParserS (..)

    -- * Running
  , runParserS
  , parseByteStringS

    -- * Reader operations
  , ask, asks, local

    -- * State operations
  , get, put, modify, modify'

    -- * Lifting basic parsers
  , liftParser

    -- * Re-exports (combinators work identically)
  , module Wireform.Parser
  ) where

import Data.IORef
import Data.Word (Word8, Word64)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr (..), plusPtr, minusPtr, castPtr)
import Foreign.Storable (poke)
import GHC.Exts
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (..))
import GHC.IO (IO (..))
import qualified Data.ByteString.Internal as BSI
import Foreign.ForeignPtr (withForeignPtr)
import Control.Exception (bracket)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Wireform.Parser
import Wireform.Parser.Internal
  ( Parser (..)
  , ParserEnv (..), Step (..), Resume (..)
  , type Res#, type StRes#
  , pattern OK#, pattern Fail#, pattern Err#
  , readEnd, writeEnd, readEnd#, writeEnd#, curToPos
  , ensureNSlow
  )
import Wireform.Parser.Error
import Wireform.Parser.Driver (LoopControl (..))

------------------------------------------------------------------------
-- Stateful parser type
------------------------------------------------------------------------

-- | Parser with reader context @r@ and mutable state @s@.
-- The state is stored in an @IORef@ (boxed mutable variable).
newtype ParserS r s e a = ParserS
  { runParserS# :: forall result.
                   PromptTag# (Step e result)
                -> ParserEnv
                -> r                    -- reader
                -> IORef s              -- mutable state
                -> Addr#                -- eob
                -> Addr#                -- cur
                -> State# RealWorld
                -> StRes# e a
  }

instance Functor (ParserS r s e) where
  fmap f (ParserS g) = ParserS \tag env r st eob s rw ->
    case g tag env r st eob s rw of
      (# rw', OK# a s' #) -> let !b = f a in (# rw', OK# b s' #)
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE fmap #-}

instance Applicative (ParserS r s e) where
  pure !a = ParserS \tag env r st eob s rw -> (# rw, OK# a s #)
  {-# INLINE pure #-}
  ParserS ff <*> ParserS fa = ParserS \tag env r st eob s rw ->
    case ff tag env r st eob s rw of
      (# rw', OK# f s' #) -> case fa tag env r st eob s' rw' of
        (# rw'', OK# a s'' #) -> let !b = f a in (# rw'', OK# b s'' #)
        (# rw'', x #)         -> (# rw'', unsafeCoerce# x #)
      (# rw', x #) -> (# rw', unsafeCoerce# x #)
  {-# INLINE (<*>) #-}
  ParserS fa *> ParserS fb = ParserS \tag env r st eob s rw ->
    case fa tag env r st eob s rw of
      (# rw', OK# _ s' #) -> fb tag env r st eob s' rw'
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE (*>) #-}

instance Monad (ParserS r s e) where
  ParserS fa >>= f = ParserS \tag env r st eob s rw ->
    case fa tag env r st eob s rw of
      (# rw', OK# a s' #) -> runParserS# (f a) tag env r st eob s' rw'
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

------------------------------------------------------------------------
-- Reader operations
------------------------------------------------------------------------

ask :: ParserS r s e r
ask = ParserS \tag env r st eob s rw -> (# rw, OK# r s #)
{-# INLINE ask #-}

asks :: (r -> a) -> ParserS r s e a
asks f = ParserS \tag env r st eob s rw -> let !a = f r in (# rw, OK# a s #)
{-# INLINE asks #-}

local :: (r -> r) -> ParserS r s e a -> ParserS r s e a
local f (ParserS p) = ParserS \tag env r st eob s rw ->
  p tag env (f r) st eob s rw
{-# INLINE local #-}

------------------------------------------------------------------------
-- State operations
------------------------------------------------------------------------

get :: ParserS r s e s
get = ParserS \tag env r stRef eob s rw ->
  case readMutVar# (unsafeCoerce# stRef) rw of
    (# rw', val #) -> (# rw', OK# val s #)
{-# INLINE get #-}

put :: s -> ParserS r s e ()
put !v = ParserS \tag env r stRef eob s rw ->
  case writeMutVar# (unsafeCoerce# stRef) v rw of
    rw' -> (# rw', OK# () s #)
{-# INLINE put #-}

modify :: (s -> s) -> ParserS r s e ()
modify f = ParserS \tag env r stRef eob s rw ->
  case readMutVar# (unsafeCoerce# stRef) rw of
    (# rw', val #) -> case writeMutVar# (unsafeCoerce# stRef) (f val) rw' of
      rw'' -> (# rw'', OK# () s #)
{-# INLINE modify #-}

modify' :: (s -> s) -> ParserS r s e ()
modify' f = ParserS \tag env r stRef eob s rw ->
  case readMutVar# (unsafeCoerce# stRef) rw of
    (# rw', val #) -> let !val' = f val in
      case writeMutVar# (unsafeCoerce# stRef) val' rw' of
        rw'' -> (# rw'', OK# () s #)
{-# INLINE modify' #-}

------------------------------------------------------------------------
-- Lifting basic parsers
------------------------------------------------------------------------

-- | Lift a basic 'W.Parser' into a 'ParserS'.
-- The lifted parser ignores the reader and state.
liftParser :: Parser e a -> ParserS r s e a
liftParser (Parser p) = ParserS \tag env _r _st eob s rw ->
  p tag env eob s rw
{-# INLINE liftParser #-}

------------------------------------------------------------------------
-- Running
------------------------------------------------------------------------

-- | Run a stateful parser against a 'ByteString'.
runParserS :: ParserS r s e a -> r -> s -> BSI.ByteString
           -> Either (ParseError e) (a, s)
runParserS p r s0 b = unsafeDupablePerformIO $ do
  let !(BSI.BS (ForeignPtr buf# fp) (I# len#)) = b
      !end# = plusAddr# buf# len#

  stRef <- newIORef s0

  withForeignPtr (ForeignPtr buf# fp) \_ ->
    bracket (mallocBytes 8) free \endPtr -> do
      poke (castPtr endPtr :: Ptr (Ptr Word8)) (Ptr end#)

      let env = ParserEnv
            { peEndPtr    = castPtr endPtr
            , peBaseAddr  = Ptr buf#
            , peMask      = maxBound
            , peStartPos  = 0
            , peInitCur   = Ptr buf#
            , peBackingFp = fp
            }

      result <- IO \s0' -> case newPromptTag# s0' of
        (# s1, (tag :: PromptTag# (Step e a)) #) ->
          let body :: State# RealWorld -> (# State# RealWorld, Step e a #)
              body rw = case runParserS# p tag env r stRef end# buf# rw of
                (# rw', OK# a _cur #) -> (# rw', StepDone 0 a #)
                (# rw', Fail# #)      -> (# rw', StepFail 0 #)
                (# rw', Err# e #)     -> (# rw', StepErr 0 e #)
          in case prompt# tag body s1 of
               (# s2, step #) -> (# s2, classifyStep step #)

      case result of
        Right a -> do
          finalState <- readIORef stRef
          pure (Right (a, finalState))
        Left e -> pure (Left e)
  where
    classifyStep (StepDone _ a)       = Right a
    classifyStep (StepFail pos)       = Left (ParseFail pos)
    classifyStep (StepErr pos e)      = Left (ParseErr pos e)
    classifyStep (StepSuspend _ _ r)  = unsafeDupablePerformIO (resumeEof r >>= pure . classifyStep)
    classifyStep (StepCheckpoint _ r) = unsafeDupablePerformIO (resumeEof r >>= pure . classifyStep)

-- | Alias for 'runParserS'.
parseByteStringS :: ParserS r s e a -> r -> s -> BSI.ByteString
                 -> Either (ParseError e) (a, s)
parseByteStringS = runParserS
