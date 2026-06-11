{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.State.Ref
Description : Typed references to state stores (Riffle §6)

Today's state-store API is stringly-typed:

@
getStateStore :: ProcessorContext -> StoreName -> IO (Maybe AnyStateStore)
@

and the processor body has to pattern-match the
'AnyStateStore' constructor and trust that the @k@ and @v@
type variables it casts to are the same ones the topology
builder attached at the other end of the wire. Get the cast
wrong and you get a silent 'unsafeCoerce' bug at runtime.

Riffle §6 adds a thin typed wrapper: 'StoreRef' carries the
store kind (KV / window / session) at the type level along
with the key and value types. Creating a 'StoreRef' threads a
builder-side declaration through to the processor's
'getKVStoreRef' (etc.) call; mismatched types are a compile
error.

The stringly-typed @processStream@ \/ @getStateStore@ APIs
remain unchanged. 'StoreRef' is a strict opt-in.
-}
module Kafka.Streams.State.Ref (
  -- * Reference type
  StoreKind (..),
  StoreRef (..),
  SomeStoreRef (..),
  someStoreRefName,

  -- * Typed lookup
  getKVStoreRef,
  getWindowStoreRef,
  getSessionStoreRef,

  -- * Helpers
  storeRefOfBuilder,
  kvRefOfBuilder,
  windowRefOfBuilder,
  sessionRefOfBuilder,
) where

import Kafka.Streams.Processor (
  ProcessorContext,
 )
import Kafka.Streams.Processor qualified as Processor
import Kafka.Streams.State.Store (
  AnyStateStore (..),
  KeyValueStore,
  SessionStore,
  StoreBuilderKV,
  StoreBuilderS,
  StoreBuilderW,
  StoreName,
  WindowStore,
 )
import Kafka.Streams.State.Store qualified as Store
import Unsafe.Coerce (unsafeCoerce)


----------------------------------------------------------------------
-- Reference type
----------------------------------------------------------------------

{- | What flavour of store a 'StoreRef' refers to. Carried at
the type level so the lookup helpers can return the right
store record without ambiguity.
-}
data StoreKind
  = SKKV
  | SKWindow
  | SKSession


{- | A typed reference to a state store. The phantom @k@ and @v@
pin the store's key and value types; the @kind@ pins the
store flavour. The wrapped 'StoreName' is the runtime
identity (matches what the topology builder registered).
-}
newtype StoreRef (kind :: StoreKind) k v = StoreRef
  { storeRefName :: StoreName
  }


{- | Existential wrapper used when a list of refs must be
heterogeneous — e.g. @processStreamRefs@ taking refs of
different kinds and types.
-}
data SomeStoreRef where
  SomeKVRef :: !(StoreRef 'SKKV k v) -> SomeStoreRef
  SomeWindowRef :: !(StoreRef 'SKWindow k v) -> SomeStoreRef
  SomeSessionRef :: !(StoreRef 'SKSession k v) -> SomeStoreRef


-- | Project an existential ref to its 'StoreName'.
someStoreRefName :: SomeStoreRef -> StoreName
someStoreRefName = \case
  SomeKVRef r -> storeRefName r
  SomeWindowRef r -> storeRefName r
  SomeSessionRef r -> storeRefName r


----------------------------------------------------------------------
-- Typed lookup
----------------------------------------------------------------------

{- | Resolve a KV store ref to a concrete 'KeyValueStore'. The
runtime returns 'Nothing' if the topology never attached a
store under that name (e.g. the processor was reused across
two topologies where one forgot the declaration). On a type
mismatch — which the wrapping declaration is designed to
prevent — this would be 'Nothing' instead of a coerce-bug,
because 'AnyStateStore' tagged the wrong kind.
-}
getKVStoreRef
  :: forall k v
   . ProcessorContext
  -> StoreRef 'SKKV k v
  -> IO (Maybe (KeyValueStore k v))
getKVStoreRef ctx (StoreRef nm) = do
  mAny <- Processor.getStateStore ctx nm
  pure $ case mAny of
    Just (AnyKeyValueStore s) -> Just (unsafeCoerce s)
    -- The 'unsafeCoerce' here is sound because the only way a
    -- caller can obtain a @StoreRef \'SKKV k v@ is through
    -- 'kvRefOfBuilder' / 'storeRefOfBuilder', both of which
    -- carry the @StoreBuilderKV k v@ type into the phantom.
    _ -> Nothing


getWindowStoreRef
  :: forall k v
   . ProcessorContext
  -> StoreRef 'SKWindow k v
  -> IO (Maybe (WindowStore k v))
getWindowStoreRef ctx (StoreRef nm) = do
  mAny <- Processor.getStateStore ctx nm
  pure $ case mAny of
    Just (AnyWindowStore s) -> Just (unsafeCoerce s)
    _ -> Nothing


getSessionStoreRef
  :: forall k v
   . ProcessorContext
  -> StoreRef 'SKSession k v
  -> IO (Maybe (SessionStore k v))
getSessionStoreRef ctx (StoreRef nm) = do
  mAny <- Processor.getStateStore ctx nm
  pure $ case mAny of
    Just (AnySessionStore s) -> Just (unsafeCoerce s)
    _ -> Nothing


----------------------------------------------------------------------
-- Builder helpers
----------------------------------------------------------------------

{- | Mint a typed 'StoreRef' from a 'StoreName'. Use this only
when you already know the name and types match (e.g. a unit
test that wires both ends).
-}
storeRefOfBuilder :: StoreName -> StoreRef kind k v
storeRefOfBuilder = StoreRef


{- | Derive a typed KV ref from a 'StoreBuilderKV'. Pair with
'Topo.addStateStoreKV' on the topology side; the returned
ref can be passed to a processor that consumes it via
'getKVStoreRef' with the same @k@ and @v@.
-}
kvRefOfBuilder :: StoreBuilderKV k v -> StoreRef 'SKKV k v
kvRefOfBuilder b = StoreRef (Store.sbKvName b)


windowRefOfBuilder :: StoreBuilderW k v -> StoreRef 'SKWindow k v
windowRefOfBuilder b = StoreRef (Store.sbWName b)


sessionRefOfBuilder :: StoreBuilderS k v -> StoreRef 'SKSession k v
sessionRefOfBuilder b = StoreRef (Store.sbSName b)
