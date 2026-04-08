-- | Convenience re-exports for XML serialization.
--
-- @
-- import qualified Wireform.XML as XML
-- @
module Wireform.XML
  ( module XML.Value
  , module XML.SAX
  , module XML.Decode
  , module XML.Encode
  , module XML.Path
  , module XML.Class
  , module XML.DSL
  ) where

import XML.Value
import XML.SAX
import XML.Decode
import XML.Encode
import XML.Path
import XML.Class
import XML.DSL hiding (textContent, textNodes, commentNodes)
