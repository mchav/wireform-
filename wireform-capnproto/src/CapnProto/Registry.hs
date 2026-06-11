{- | User-extensible annotation handler registration for Cap'n Proto.

Cap'n Proto schemas support annotations on fields and declarations.
This module allows users to register custom 'AnnotationHandler's that
optionally transform field types and emit extra code during Cap'n Proto
code generation.
-}
module CapnProto.Registry (
  -- * Registry
  CapnProtoRegistry (..),
  defaultCapnProtoRegistry,

  -- * Annotation handlers
  AnnotationHandler (..),
  registerCapnProtoAnnotation,
) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)


{- | Handler for a Cap'n Proto annotation.  Can transform the Haskell
type and emit extra lines of code.
-}
data AnnotationHandler = AnnotationHandler
  { hTransformType :: !(Text -> Text)
  , hExtraCode :: !(Text -> Maybe Text -> [Text])
  }


-- | Registry of custom Cap'n Proto annotation handlers.
data CapnProtoRegistry = CapnProtoRegistry
  { crAnnotationHandlers :: !(Map Text AnnotationHandler)
  }


instance Semigroup CapnProtoRegistry where
  a <> b =
    CapnProtoRegistry
      { crAnnotationHandlers = crAnnotationHandlers a <> crAnnotationHandlers b
      }


instance Monoid CapnProtoRegistry where
  mempty = CapnProtoRegistry Map.empty


-- | Default (empty) Cap'n Proto registry.
defaultCapnProtoRegistry :: CapnProtoRegistry
defaultCapnProtoRegistry = mempty


-- | Register an annotation handler.
registerCapnProtoAnnotation :: Text -> AnnotationHandler -> CapnProtoRegistry -> CapnProtoRegistry
registerCapnProtoAnnotation name handler reg =
  reg {crAnnotationHandlers = Map.insert name handler (crAnnotationHandlers reg)}
