{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}

module Wireform.Parser.Mark
  ( Mark (..)
  , mark, restore, release
  ) where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Foreign.Ptr (Ptr (..), plusPtr)
import GHC.Exts

import Wireform.Parser.Internal

newtype Mark = Mark { unMark :: Word64 }
  deriving stock (Eq, Ord, Show)

mark :: Parser e Mark
mark = Parser \env eob s st ->
  (# st, OK# (Mark (curToPos env s)) s #)
{-# INLINE mark #-}

restore :: Mark -> Parser e ()
restore (Mark pos) = Parser \env eob s st ->
  let !offset = fromIntegral pos .&. peMask env
      !(Ptr newCur) = peBaseAddr env `plusPtr` offset
  in (# st, OK# () newCur #)
{-# INLINE restore #-}

release :: Mark -> Parser e ()
release _ = Parser \env eob s st -> (# st, OK# () s #)
{-# INLINE release #-}
