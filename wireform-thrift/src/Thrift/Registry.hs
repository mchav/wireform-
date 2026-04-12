-- | User-extensible annotation handler registration for Thrift.
--
-- Thrift's extension mechanism is annotations on fields and structs.
-- This module allows users to register custom handlers that transform
-- types and emit extra code during Thrift code generation.
module Thrift.Registry
  ( -- * Registry
    ThriftRegistry (..)
  , defaultThriftRegistry
    -- * Field annotation handlers
  , FieldAnnotationHandler (..)
  , registerFieldAnnotation
    -- * Struct annotation handlers
  , StructAnnotationHandler (..)
  , registerStructAnnotation
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Handler for a Thrift field annotation.  When a field carries an
-- annotation matching a registered handler, the handler can transform
-- the field's Haskell type and emit extra lines of code.
data FieldAnnotationHandler = FieldAnnotationHandler
  { fahTransformType :: !(Text -> Text)
  , fahExtraCode     :: !(Text -> Text -> [Text])
  }

-- | Handler for a Thrift struct-level annotation.  Can add extra
-- deriving clauses and emit extra lines of code.
data StructAnnotationHandler = StructAnnotationHandler
  { sahExtraDerivations :: !(Text -> [Text])
  , sahExtraCode        :: !(Text -> Text -> [Text])
  }

-- | Registry of custom annotation handlers for Thrift code generation.
data ThriftRegistry = ThriftRegistry
  { trFieldAnnotations  :: !(Map Text FieldAnnotationHandler)
  , trStructAnnotations :: !(Map Text StructAnnotationHandler)
  }

instance Semigroup ThriftRegistry where
  a <> b = ThriftRegistry
    { trFieldAnnotations  = trFieldAnnotations a  <> trFieldAnnotations b
    , trStructAnnotations = trStructAnnotations a <> trStructAnnotations b
    }

instance Monoid ThriftRegistry where
  mempty = ThriftRegistry Map.empty Map.empty

-- | Default (empty) Thrift registry with no custom annotation handlers.
defaultThriftRegistry :: ThriftRegistry
defaultThriftRegistry = mempty

-- | Register a field annotation handler.
registerFieldAnnotation :: Text -> FieldAnnotationHandler -> ThriftRegistry -> ThriftRegistry
registerFieldAnnotation name handler reg =
  reg { trFieldAnnotations = Map.insert name handler (trFieldAnnotations reg) }

-- | Register a struct annotation handler.
registerStructAnnotation :: Text -> StructAnnotationHandler -> ThriftRegistry -> ThriftRegistry
registerStructAnnotation name handler reg =
  reg { trStructAnnotations = Map.insert name handler (trStructAnnotations reg) }
