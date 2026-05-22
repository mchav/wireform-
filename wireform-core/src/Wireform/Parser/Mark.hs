{-# LANGUAGE BlockArguments #-}

module Wireform.Parser.Mark
  ( Mark (..)
  , mark
  , restore
  , release
  ) where

import Data.Bits ((.&.))
import Data.Word (Word64)
import Foreign.Ptr (plusPtr)

import Wireform.Parser.Internal

-- | A saved position in the byte stream.
-- Marks survive suspensions trivially — they're just absolute positions.
newtype Mark = Mark { unMark :: Word64 }
  deriving stock (Eq, Ord, Show)

-- | Save the current position as a mark.
mark :: Parser e Mark
mark = Parser \tag env cur ->
  pure (OK (Mark (curToPos env cur)) cur)
{-# INLINE mark #-}

-- | Restore the parser to a previously-saved mark.
restore :: Mark -> Parser e ()
restore (Mark pos) = Parser \tag env _cur -> do
  let !offset = fromIntegral pos .&. peMask env
      !newCur = peBaseAddr env `plusPtr` offset
  pure (OK () newCur)
{-# INLINE restore #-}

-- | Release a mark.  Currently a no-op at the parser level.
release :: Mark -> Parser e ()
release _ = Parser \tag _ cur -> pure (OK () cur)
{-# INLINE release #-}
