-- | User-extensible attribute handler registration for Microsoft Bond.
--
-- Bond schemas support attributes (annotations) on structs and fields.
-- This module allows users to register custom 'AttributeHandler's that
-- optionally transform field types and emit extra code during Bond
-- code generation.
module Bond.Registry
  ( -- * Registry
    BondRegistry (..)
  , defaultBondRegistry
    -- * Attribute handlers
  , AttributeHandler (..)
  , registerBondAttribute
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Handler for a Bond attribute.  Can transform the Haskell type
-- of the field and emit extra lines of code.
data AttributeHandler = AttributeHandler
  { hTransformType :: !(Text -> Text)
  , hExtraCode     :: !(Text -> Maybe Text -> [Text])
  }

-- | Registry of custom Bond attribute handlers.
data BondRegistry = BondRegistry
  { brAttributeHandlers :: !(Map Text AttributeHandler)
  }

instance Semigroup BondRegistry where
  a <> b = BondRegistry
    { brAttributeHandlers = brAttributeHandlers a <> brAttributeHandlers b
    }

instance Monoid BondRegistry where
  mempty = BondRegistry Map.empty

-- | Default (empty) Bond registry.
defaultBondRegistry :: BondRegistry
defaultBondRegistry = mempty

-- | Register an attribute handler.
registerBondAttribute :: Text -> AttributeHandler -> BondRegistry -> BondRegistry
registerBondAttribute name handler reg =
  reg { brAttributeHandlers = Map.insert name handler (brAttributeHandlers reg) }
