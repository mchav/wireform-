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
  ) where

-- | Options controlling what the encoder emits on the wire.
data EncodeOptions = EncodeOptions
  { eoRefTracking :: !Bool
    -- ^ When 'True', emit per-slot reference flags
    -- (@NULL@ \/ @REF_VALUE@ \/ @REF@) for every object value
    -- (lists, sets, maps, structs, primitive arrays) and use
    -- the @TRACKING_REF@ bit in @collect_flag@ for same-type
    -- collections so that repeated occurrences of the same
    -- subtree become @REF@ back-references on the wire. When
    -- 'False' (the default), reference flags appear only at the
    -- top level and around explicit 'Fury.Value.RefVal' nodes.
  } deriving (Eq, Show)

defaultEncodeOptions :: EncodeOptions
defaultEncodeOptions = EncodeOptions
  { eoRefTracking = False
  }

-- | Options controlling what the decoder expects.
data DecodeOptions = DecodeOptions
  { doRefTracking :: !Bool
    -- ^ When 'True', the decoder expects every object value to
    -- be preceded by a reference flag and the
    -- @TRACKING_REF@-aware collection layout. Must match the
    -- producer's 'eoRefTracking' setting.
  } deriving (Eq, Show)

defaultDecodeOptions :: DecodeOptions
defaultDecodeOptions = DecodeOptions
  { doRefTracking = False
  }
