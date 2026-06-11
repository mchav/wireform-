{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Typed, per-backend modifier extensions.

This module is the answer to the question: \"how does a brand-new
backend define its own annotation vocabulary without modifying the
core 'Modifier' ADT?\"

The core ADT exposes one open slot — 'Wireform.Derive.Modifier.ModCustom' —
a @('Text', 'ByteString')@ pair where the 'Text' identifies the
vocabulary and the 'ByteString' is an opaque payload. Backends were
always free to use it directly, but the ergonomics were poor: every
backend had to invent its own serialization story.

The 'BackendModifier' typeclass standardises that story:

1. The backend declares its modifier type with derived 'Show' /
   'Read' instances and provides a stable 'backendModifierTag'.
2. Users embed the typed value into an annotation via 'extension'.
3. The backend's TH deriver pulls the typed value back out via
   'lookupExtension' or 'lookupExtensions'.

Serialization uses @show@\/@read@ so we incur no extra package
dependencies. The payload only ever lives in @.hi@ files at
compile time, so the verbosity is a non-issue.

== Example: an Iceberg-only modifier

@
module Iceberg.Derive.Modifiers where

import Wireform.Derive.Extension
import Wireform.Derive.Modifier (Modifier)

data IcebergFieldOpt
  = PartitionColumn
  | OptimisticTransform !Text
  deriving stock (Eq, Show, Read, Typeable)

instance BackendModifier IcebergFieldOpt where
  backendModifierTag _ = "wireform-iceberg.field-opt"

partition :: Modifier
partition = extension PartitionColumn
@

and the deriver does:

@
mi <- reifyModifierInfoFor backendIceberg fieldName
case lookupExtension @IcebergFieldOpt mi of
  Just PartitionColumn        -> ...
  Just (OptimisticTransform t)-> ...
  Nothing                     -> defaultBehaviour
@
-}
module Wireform.Derive.Extension (
  -- * Typed payloads
  BackendModifier (..),

  -- * Embedding
  extension,

  -- * Reading back
  lookupExtension,
  lookupExtensions,
  hasExtension,
) where

import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Text.Read (readMaybe)
import Wireform.Derive.Modifier (Modifier (..), customModifier)
import Wireform.Derive.ModifierInfo (ModifierInfo (..))


{- | Types that may be embedded into a 'Modifier' as a backend-specific
extension payload.

The 'Show' representation is what gets persisted into the @.hi@
file via the underlying @ANN@ pragma; the 'Read' instance reverses
it at splice time. 'Typeable' is required so derivers can assert
the expected vocabulary on extraction.

The 'backendModifierTag' is a globally-unique key for the
extension. Convention: @\"wireform-\<backend\>.\<concept\>\"@ so
two backends that happen to use the same concept name (e.g.
@\"partition\"@) do not collide.
-}
class (Eq a, Show a, Read a, Typeable a) => BackendModifier a where
  backendModifierTag :: Proxy a -> Text


{- | Lift a typed value into a 'Modifier'. The result can be attached
via an @ANN@ pragma like any other modifier.

@
{\-\# ANN myField (extension PartitionColumn) \#-\}
@
-}
extension :: forall a. BackendModifier a => a -> Modifier
extension a =
  customModifier
    (backendModifierTag (Proxy @a))
    (show a)


{- | Read back the first 'BackendModifier' of the requested type
attached to a name. Returns 'Nothing' if no annotation tagged for
this type was present, or if the payload failed to parse.

The latter case (parse failure) most often indicates that the
backend's modifier vocabulary changed in an incompatible way; in
that case derivers may want to call 'lookupExtensions' instead and
decide their own policy.
-}
lookupExtension
  :: forall a
   . BackendModifier a
  => ModifierInfo
  -> Maybe a
lookupExtension mi =
  case lookupExtensions @a mi of
    (a : _) -> Just a
    [] -> Nothing


{- | Like 'lookupExtension' but returns every successfully-decoded
value of the requested type. Order matches the order in which @ANN@
pragmas were processed.
-}
lookupExtensions
  :: forall a
   . BackendModifier a
  => ModifierInfo
  -> [a]
lookupExtensions mi =
  let key = backendModifierTag (Proxy @a)
  in case Map.lookup key (miCustom mi) of
       Nothing -> []
       Just ms -> mapMaybe decodeOne (reverse ms)
  where
    -- 'miCustom' uses 'Map.insertWith (++)' which prepends new
    -- modifiers to the head; 'reverse' here puts them back in
    -- declaration order so per-call iteration matches user intent.
    decodeOne :: Modifier -> Maybe a
    decodeOne (ModCustom _ s) = readMaybe s
    decodeOne _ = Nothing


{- | True iff at least one 'BackendModifier' of the requested type is
attached to the name.
-}
hasExtension
  :: forall a
   . BackendModifier a
  => ModifierInfo
  -> Bool
hasExtension mi =
  let key = backendModifierTag (Proxy @a)
  in case Map.lookup key (miCustom mi) of
       Nothing -> False
       Just _ -> True
