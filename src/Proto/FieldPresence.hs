{-# LANGUAGE BangPatterns #-}
-- | Field presence tracking for proto3 optional fields.
--
-- In proto3, scalar fields with the @optional@ keyword have explicit
-- presence semantics: the field can distinguish between "not set" and
-- "set to the default value". Without @optional@, scalar fields use
-- implicit presence (default value means "not set").
--
-- This module provides types and utilities for tracking field presence
-- in generated code.
--
-- Example: a proto3 optional int32 field can be:
--
-- * Not set: 'Absent'
-- * Set to 0: @'Present' 0@
-- * Set to 42: @'Present' 42@
--
-- Without optional, the field is just 'Int32', and 0 means "not set".
module Proto.FieldPresence
  ( -- * Presence-aware field type
    Field (..)
  , isPresent
  , isAbsent
  , fieldValue
  , fieldWithDefault
  , fromMaybeField
  , toMaybeField

    -- * HasField-style access
  , setField
  , clearField
  ) where

-- | A field with explicit presence tracking.
-- This is used for proto3 @optional@ scalar fields.
data Field a
  = Absent
  | Present !a
  deriving stock (Show, Eq, Ord, Functor)

instance Applicative Field where
  pure = Present
  Absent <*> _        = Absent
  _ <*> Absent        = Absent
  Present f <*> Present a = Present (f a)

instance Monad Field where
  Absent >>= _    = Absent
  Present a >>= f = f a

isPresent :: Field a -> Bool
isPresent (Present _) = True
isPresent Absent      = False

isAbsent :: Field a -> Bool
isAbsent Absent    = True
isAbsent (Present _) = False

-- | Get the field value, or the proto default if absent.
fieldValue :: a -> Field a -> a
fieldValue def Absent      = def
fieldValue _   (Present a) = a

-- | Get the field value with a specified default.
fieldWithDefault :: a -> Field a -> a
fieldWithDefault = fieldValue

-- | Convert from Maybe.
fromMaybeField :: Maybe a -> Field a
fromMaybeField Nothing  = Absent
fromMaybeField (Just a) = Present a

-- | Convert to Maybe.
toMaybeField :: Field a -> Maybe a
toMaybeField Absent      = Nothing
toMaybeField (Present a) = Just a

-- | Set a field to a value.
setField :: a -> Field a
setField = Present

-- | Clear a field (mark as absent).
clearField :: Field a
clearField = Absent
