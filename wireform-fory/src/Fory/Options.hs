-- | Encoder / decoder options for Apache Fory.
--
-- Both records mirror the constructor flags @pyfory.Fory@ takes:
-- the most useful ones for us are @ref_tracking@ (turn on the
-- shared-reference protocol) and @meta_share@ (TypeDef sidecar
-- mode for compatible structs).
module Fury.Options
  ( EncodeOptions (..)
  , defaultEncodeOptions
  , DecodeOptions (..)
  , defaultDecodeOptions
  , StructRegistry
  , emptyStructRegistry
  , registerStruct
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Fury.Struct (StructSchema, ssNamespace, ssTypename)

-- ---------------------------------------------------------------------------
-- Struct registry
-- ---------------------------------------------------------------------------

-- | A registry mapping @(namespace, type_name)@ to its struct
-- schema. Both encoder and decoder consult the same registry to
-- emit / accept the pyfory-compatible @NAMED_STRUCT@ wire layout.
type StructRegistry = HashMap (Text, Text) StructSchema

emptyStructRegistry :: StructRegistry
emptyStructRegistry = HM.empty

registerStruct :: StructSchema -> StructRegistry -> StructRegistry
registerStruct sch =
  HM.insert (ssNamespace sch, ssTypename sch) sch

-- ---------------------------------------------------------------------------
-- Options
-- ---------------------------------------------------------------------------

-- | Options controlling what the encoder emits on the wire.
data EncodeOptions = EncodeOptions
  { eoRefTracking    :: !Bool
    -- ^ When 'True', emit per-slot reference flags
    -- (@NULL@ \/ @REF_VALUE@ \/ @REF@) for every object value
    -- (lists, sets, maps, structs, primitive arrays) and use
    -- the @TRACKING_REF@ bit in @collect_flag@ for same-type
    -- collections so that repeated occurrences of the same
    -- subtree become @REF@ back-references on the wire. When
    -- 'False' (the default), reference flags appear only at the
    -- top level and around explicit 'Fury.Value.RefVal' nodes.
  , eoStructRegistry :: !StructRegistry
    -- ^ Schemas keyed by @(namespace, type_name)@. When the
    -- encoder sees a 'Fury.Value.RegisteredStructVal' whose
    -- @(namespace, type_name)@ is present, it emits the
    -- pyfory-compatible @NAMED_STRUCT@ wire layout. Defaults to
    -- the empty registry.
  } deriving (Eq, Show)

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
  { eoRefTracking    = False
  , eoStructRegistry = emptyStructRegistry
  }

-- | Options controlling what the decoder expects.
data DecodeOptions = DecodeOptions
  { doRefTracking    :: !Bool
    -- ^ When 'True', the decoder expects every object value to
    -- be preceded by a reference flag and the
    -- @TRACKING_REF@-aware collection layout. Must match the
    -- producer's 'eoRefTracking' setting.
  , doStructRegistry :: !StructRegistry
    -- ^ When the decoder sees a @NAMED_STRUCT@ tag and looks up
    -- a matching schema in this registry, it parses the
    -- pyfory-compatible wire layout (4-byte hash + fields in
    -- canonical order). If the lookup fails, it falls back to
    -- the in-package self-describing layout produced by
    -- 'Fury.Value.StructVal'.
  } deriving (Eq, Show)

defaultDecodeOptions :: DecodeOptions
defaultDecodeOptions = DecodeOptions
  { doRefTracking    = False
  , doStructRegistry = emptyStructRegistry
  }
