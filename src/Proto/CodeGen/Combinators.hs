-- | Shared helpers for the code-generation pipeline.
--
-- __THIS MODULE IS INTERNAL TO hs-proto.  DO NOT DEPEND ON IT.__
module Proto.CodeGen.Combinators
  ( -- * Text → Doc helpers
    txt
  , tshow

    -- * Structural helpers
  , braceBlock
  , instanceHead
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter

-- | Emit a 'Text' literal into the generated Haskell source.
-- Replaces the ubiquitous @pretty ("..." :: Text)@ pattern.
txt :: Text -> Doc ann
txt = pretty
{-# INLINE txt #-}

-- | @T.pack . show@ — convenient for embedding numbers / enum tags.
tshow :: Show a => a -> Text
tshow = T.pack . show
{-# INLINE tshow #-}

-- | Format a list of field docs as a brace-delimited block:
--
-- @
-- { field1
-- , field2
-- }
-- @
braceBlock :: [Doc ann] -> Doc ann
braceBlock [] = txt "{ }"
braceBlock (f:fs) =
  vsep (txt "{ " <> f : fmap (\x -> txt ", " <> x) fs)
  <> line <> txt "}"

-- | @instance C T where@
instanceHead :: Text -> Text -> Doc ann
instanceHead cls ty = txt "instance " <> txt cls <> txt " " <> txt ty <> txt " where"
