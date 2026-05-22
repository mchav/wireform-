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
-- State is stored in a raw @MutVar#@ — no IORef boxing overhead.
-- Reader is a plain boxed value.
module Wireform.Parser.Stateful
  ( ParserS (..)

    -- * Running
  , runParserS
  , parseByteStringS

    -- * Reader operations
  , ask, asks, local

    -- * State operations
  , get, put, modify, modify'

    -- * Lifting basic parsers
  , liftParser

    -- * Re-exports
  , module Wireform.Parser
  ) where

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
  , readEnd, writeEnd, curToPos, tagToAny
  )
import Wireform.Parser.Error

------------------------------------------------------------------------
-- Stateful parser type
------------------------------------------------------------------------

-- | Parser with reader @r@ and mutable state @s@ stored in a raw
-- @MutVar#@.  One pointer deref per 'get'/'put', no IORef boxing.
newtype ParserS r s e a = ParserS
  { runParserS# :: forall result.
                   PromptTag# (Step e result)
                -> ParserEnv
                -> r
                -> MutVar# RealWorld s
                -> Addr#
                -> Addr#
                -> State# RealWorld
                -> StRes# e a
  }

instance Functor (ParserS r s e) where
  fmap f (ParserS g) = ParserS \tag env r mv eob s rw ->
    case g tag env r mv eob s rw of
      (# rw', OK# a s' #) -> let !b = f a in (# rw', OK# b s' #)
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE fmap #-}

instance Applicative (ParserS r s e) where
  pure !a = ParserS \tag env r mv eob s rw -> (# rw, OK# a s #)
  {-# INLINE pure #-}
  ParserS ff <*> ParserS fa = ParserS \tag env r mv eob s rw ->
    case ff tag env r mv eob s rw of
      (# rw', OK# f s' #) -> case fa tag env r mv eob s' rw' of
        (# rw'', OK# a s'' #) -> let !b = f a in (# rw'', OK# b s'' #)
        (# rw'', x #)         -> (# rw'', unsafeCoerce# x #)
      (# rw', x #) -> (# rw', unsafeCoerce# x #)
  {-# INLINE (<*>) #-}
  ParserS fa *> ParserS fb = ParserS \tag env r mv eob s rw ->
    case fa tag env r mv eob s rw of
      (# rw', OK# _ s' #) -> fb tag env r mv eob s' rw'
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE (*>) #-}

instance Monad (ParserS r s e) where
  ParserS fa >>= f = ParserS \tag env r mv eob s rw ->
    case fa tag env r mv eob s rw of
      (# rw', OK# a s' #) -> runParserS# (f a) tag env r mv eob s' rw'
      (# rw', x #)        -> (# rw', unsafeCoerce# x #)
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}

------------------------------------------------------------------------
-- Reader operations
------------------------------------------------------------------------

ask :: ParserS r s e r
ask = ParserS \tag env r mv eob s rw -> (# rw, OK# r s #)
{-# INLINE ask #-}

asks :: (r -> a) -> ParserS r s e a
asks f = ParserS \tag env r mv eob s rw -> let !a = f r in (# rw, OK# a s #)
{-# INLINE asks #-}

local :: (r -> r) -> ParserS r s e a -> ParserS r s e a
local f (ParserS p) = ParserS \tag env r mv eob s rw ->
  p tag env (f r) mv eob s rw
{-# INLINE local #-}

------------------------------------------------------------------------
-- State operations (raw MutVar#, no IORef)
------------------------------------------------------------------------

get :: ParserS r s e s
get = ParserS \tag env r mv eob s rw ->
  case readMutVar# mv rw of
    (# rw', val #) -> (# rw', OK# val s #)
{-# INLINE get #-}

put :: s -> ParserS r s e ()
put !v = ParserS \tag env r mv eob s rw ->
  case writeMutVar# mv v rw of
    rw' -> (# rw', OK# () s #)
{-# INLINE put #-}

modify :: (s -> s) -> ParserS r s e ()
modify f = ParserS \tag env r mv eob s rw ->
  case readMutVar# mv rw of
    (# rw', val #) -> case writeMutVar# mv (f val) rw' of
      rw'' -> (# rw'', OK# () s #)
{-# INLINE modify #-}

modify' :: (s -> s) -> ParserS r s e ()
modify' f = ParserS \tag env r mv eob s rw ->
  case readMutVar# mv rw of
    (# rw', val #) -> let !val' = f val in
      case writeMutVar# mv val' rw' of
        rw'' -> (# rw'', OK# () s #)
{-# INLINE modify' #-}

------------------------------------------------------------------------
-- Lifting basic parsers
------------------------------------------------------------------------

liftParser :: Parser e a -> ParserS r s e a
liftParser (Parser p) = ParserS \tag env _r _mv eob s rw ->
  p env eob s rw
{-# INLINE liftParser #-}

------------------------------------------------------------------------
-- Running
------------------------------------------------------------------------

runParserS :: ParserS r s e a -> r -> s -> BSI.ByteString
           -> Either (ParseError e) (a, s)
runParserS p r s0 b = unsafeDupablePerformIO $ do
  let !(BSI.BS (ForeignPtr buf# fp) (I# len#)) = b
      !end# = plusAddr# buf# len#

  withForeignPtr (ForeignPtr buf# fp) \_ ->
    bracket (mallocBytes 8) free \endPtr -> do
      poke (castPtr endPtr :: Ptr (Ptr Word8)) (Ptr end#)

      IO \rw0 -> case newMutVar# s0 rw0 of
        (# rw1, mv #) -> case newPromptTag# rw1 of
          (# rw2, (tag :: PromptTag# (Step e a)) #) ->
            let env = ParserEnv
                  { peEndPtr    = castPtr endPtr
                  , peBaseAddr  = Ptr buf#
                  , peMask      = maxBound
                  , peStartPos  = 0
                  , peInitCur   = Ptr buf#
                  , peBackingFp = fp
                  , peTag       = tagToAny tag
                  }
                body :: State# RealWorld -> (# State# RealWorld, Step e a #)
                body rw = case runParserS# p tag env r mv end# buf# rw of
                  (# rw', OK# a _cur #) -> (# rw', StepDone 0 a #)
                  (# rw', Fail# #)      -> (# rw', StepFail 0 #)
                  (# rw', Err# e #)     -> (# rw', StepErr 0 e #)
            in case prompt# tag body rw2 of
                 (# rw3, step #) -> case readMutVar# mv rw3 of
                   (# rw4, finalState #) ->
                     (# rw4, classifyStep step finalState #)
  where
    classifyStep (StepDone _ a) s       = Right (a, s)
    classifyStep (StepFail pos) _       = Left (ParseFail pos)
    classifyStep (StepErr pos e) _      = Left (ParseErr pos e)
    classifyStep (StepSuspend _ _ res) s = unsafeDupablePerformIO $
      resumeEof res >>= \step -> pure (classifyStep step s)
    classifyStep (StepCheckpoint _ res) s = unsafeDupablePerformIO $
      resumeEof res >>= \step -> pure (classifyStep step s)

parseByteStringS :: ParserS r s e a -> r -> s -> BSI.ByteString
                 -> Either (ParseError e) (a, s)
parseByteStringS = runParserS
