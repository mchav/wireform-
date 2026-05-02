-- | Annotation-driven Template Haskell deriver core, shared across
-- every wireform format package.
--
-- This module re-exports the public surface so that downstream
-- deriver packages (@wireform-proto@, @wireform-cbor@,
-- @wireform-msgpack@, @wireform-thrift@, and the 18 newer per-format
-- derivers) can depend on a single import:
--
-- @
-- import Wireform.Derive
-- @
--
-- The Aeson deriver is bundled here as 'Wireform.Derive.Aeson' rather
-- than living in a separate package; it doubles as the canonical
-- worked example for adding a new backend on top of this core.
--
-- == Usage
--
-- 1. Annotate types and fields with 'Modifier's via @ANN@ pragmas.
-- 2. In a deriver splice, call 'reifyTypeInfo' for the data type and
--    'reifyModifierInfoFor' / 'reifyModifierInfo' for each affected
--    'Name' to obtain a backend-resolved 'ModifierInfo'.
-- 3. Use 'renderRenameKey' or 'renderWireKey' to splice the
--    appropriate 'Text' (or runtime expression) for each field's wire
--    key.
--
-- See "Wireform.Derive.Aeson" for a complete worked example.
module Wireform.Derive
  ( -- * Backends
    module Wireform.Derive.Backend

    -- * Name styles
  , module Wireform.Derive.NameStyle

    -- * Modifiers
  , module Wireform.Derive.Modifier

    -- * Type reflection
  , module Wireform.Derive.TypeInfo

    -- * Resolution
  , module Wireform.Derive.ModifierInfo

    -- * Backend extension vocabulary
    --
    -- | Per-backend modifiers without modifying the core ADT. See
    -- 'Wireform.Derive.Extension' for the rationale.
  , module Wireform.Derive.Extension
  ) where

import Wireform.Derive.Backend
import Wireform.Derive.Extension
import Wireform.Derive.Modifier
import Wireform.Derive.ModifierInfo
import Wireform.Derive.NameStyle
import Wireform.Derive.TypeInfo
