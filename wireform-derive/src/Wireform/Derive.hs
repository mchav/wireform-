-- | Annotation-driven Template Haskell deriver core, shared across
-- every wireform format package.
--
-- This module re-exports the public surface so that downstream
-- deriver packages (@wireform-proto@, @wireform-cbor@,
-- @wireform-msgpack@, @wireform-thrift@, @wireform-derive-aeson@) can
-- depend on a single import:
--
-- @
-- import Wireform.Derive
-- @
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
-- See @Wireform.Derive.Aeson@ for a complete worked example.
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
  ) where

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.ModifierInfo
import Wireform.Derive.NameStyle
import Wireform.Derive.TypeInfo
