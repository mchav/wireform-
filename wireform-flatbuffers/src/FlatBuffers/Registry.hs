-- | User-extensible metadata handler registration for FlatBuffers.
--
-- FlatBuffers schemas support metadata (attributes) on table fields.
-- This module allows users to register custom 'MetadataHandler's that
-- optionally transform field types and emit extra code during FlatBuffers
-- code generation.
module FlatBuffers.Registry
  ( -- * Registry
    FlatBuffersRegistry (..)
  , defaultFlatBuffersRegistry
    -- * Metadata handlers
  , MetadataHandler (..)
  , registerFlatBuffersMetadata
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Handler for a FlatBuffers metadata attribute.  Can transform the
-- Haskell type of the field and emit extra lines of code.
data MetadataHandler = MetadataHandler
  { hTransformType :: !(Text -> Text)
  , hExtraCode     :: !(Text -> Maybe Text -> [Text])
  }

-- | Registry of custom FlatBuffers metadata handlers.
data FlatBuffersRegistry = FlatBuffersRegistry
  { frMetadataHandlers :: !(Map Text MetadataHandler)
  }

instance Semigroup FlatBuffersRegistry where
  a <> b = FlatBuffersRegistry
    { frMetadataHandlers = frMetadataHandlers a <> frMetadataHandlers b
    }

instance Monoid FlatBuffersRegistry where
  mempty = FlatBuffersRegistry Map.empty

-- | Default (empty) FlatBuffers registry.
defaultFlatBuffersRegistry :: FlatBuffersRegistry
defaultFlatBuffersRegistry = mempty

-- | Register a metadata handler.
registerFlatBuffersMetadata :: Text -> MetadataHandler -> FlatBuffersRegistry -> FlatBuffersRegistry
registerFlatBuffersMetadata name handler reg =
  reg { frMetadataHandlers = Map.insert name handler (frMetadataHandlers reg) }
