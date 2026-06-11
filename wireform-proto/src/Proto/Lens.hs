{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | Optional van Laarhoven lenses for protobuf message fields.

These lenses are __not__ the primary interface — plain record access
and 'Proto.Schema.HasField' get\/set are the default. Import this
module only when you want to use lens combinators.

The lenses are compatible with both @lens@ and @microlens@ since they
use the van Laarhoven encoding directly (no dependency on either package).

@
import Proto.Lens (field)

-- Get a field:
view (field \@\"seconds\") timestamp

-- Set a field:
set (field \@\"seconds\") 42 timestamp

-- Modify:
over (field \@\"seconds\") (+1) timestamp

-- Compose:
view (field \@\"inner\" . field \@\"name\") nested
@

The 'field' function produces a 'Lens'' that works with any lens library.
-}
module Proto.Lens (
  -- * Generic field lens
  field,

  -- * Van Laarhoven lens type (no dependency on lens/microlens)
  Lens',
  Getting,
  ASetter,

  -- * Operators (standalone, no lens dependency needed)
  view,
  set,
  over,
  (^.),
  (.~),
  (%~),
  (&),
) where

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))
import Proto.Schema (HasField (..))


{- | A van Laarhoven lens. Compatible with @lens@, @microlens@, @optics@
(via adapter), and any library that understands @Functor f => (a -> f a) -> s -> f s@.
-}
type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s


-- | A getter (read-only lens).
type Getting r s a = (a -> Const r a) -> s -> Const r s


-- | A setter.
type ASetter s a = (a -> Identity a) -> s -> Identity s


{- | Produce a lens for a proto field, identified by its type-level name.

@
field \@\"seconds\" :: Lens' Timestamp Int64
field \@\"name\"    :: Lens' Person Text
@

This works for any message type that has a 'HasField' instance for the
given field name (i.e., all generated message types).
-}
field :: forall name msg a f. (HasField msg name a, Functor f) => (a -> f a) -> msg -> f msg
field k msg = fmap (\a' -> setField @msg @name a' msg) (k (getField @msg @name msg))
{-# INLINE field #-}


{- | Extract the field value through a getter.

@view (field \@\"seconds\") ts == getField \@Timestamp \@\"seconds\" ts@
-}
view :: Getting a s a -> s -> a
view l s = getConst (l Const s)
{-# INLINE view #-}


-- | Set a field to a specific value.
set :: ASetter s a -> a -> s -> s
set l a s = runIdentity (l (const (Identity a)) s)
{-# INLINE set #-}


-- | Modify a field with a function.
over :: ASetter s a -> (a -> a) -> s -> s
over l f s = runIdentity (l (Identity . f) s)
{-# INLINE over #-}


-- | Infix view.
(^.) :: s -> Getting a s a -> a
s ^. l = view l s
{-# INLINE (^.) #-}


infixl 8 ^.


-- | Infix set.
(.~) :: ASetter s a -> a -> s -> s
l .~ a = set l a
{-# INLINE (.~) #-}


infixr 4 .~


-- | Infix over (modify).
(%~) :: ASetter s a -> (a -> a) -> s -> s
l %~ f = over l f
{-# INLINE (%~) #-}


infixr 4 %~


{- | Reverse application (flip ($)), for chaining setters.
Re-exported for convenience; equivalent to Data.Function.(&).
-}
(&) :: a -> (a -> b) -> b
x & f = f x
{-# INLINE (&) #-}


infixl 1 &
