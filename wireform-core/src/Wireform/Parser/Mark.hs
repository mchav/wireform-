{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module Wireform.Parser.Mark (
  Mark (..),
  mark,
  restore,
  release,
) where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Foreign.Ptr (plusPtr)
import GHC.Exts
import Wireform.Parser.Internal


newtype Mark = Mark {unMark :: Word64}
  deriving stock (Eq, Ord, Show)


mark :: Parser m e Mark
mark = Parser \env _ s st ->
  case curToPos env s st of
    (# st', pos #) -> (# st', OK# (Mark pos) s #)
{-# INLINE mark #-}


restore :: Mark -> Parser m e ()
restore (Mark pos) = Parser \env _ _ st ->
  let !offset = fromIntegral pos .&. peMask env
      !(Ptr newCur) = peBaseAddr env `plusPtr` offset
  in (# st, OK# () newCur #)
{-# INLINE restore #-}


release :: Mark -> Parser m e ()
release _ = Parser \_ _ s st -> (# st, OK# () s #)
{-# INLINE release #-}
