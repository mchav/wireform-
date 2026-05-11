{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

{- HLINT ignore "Use newtype instead of data" -}

-- | Fixtures for the auto-detecting 'Proto.Derive.deriveProto'
-- entry point. Each record relies on type-shape detection to pick
-- 'FKRepeated' / 'FKMap' / 'FKMaybe' / 'FKBare' from the field's
-- Haskell type alone — the only annotation present is the
-- mandatory @tag N@.
--
-- Together with "Test.Proto.Derive.AutoInstances", this file
-- proves that users no longer have to go through the IDL bridge
-- ('deriveProtoFromTranslated') just to use a 'Vector', 'Map', or
-- sum type in a record.
module Test.Proto.Derive.AutoTypes
  ( -- * Auto-detected repeated, optional, enum
    AutoColor (..)
  , AutoCard (..)

    -- * Auto-detected map field
  , AutoTagged (..)

    -- * Auto-detected oneof
  , AutoChoice (..)
  , AutoEnvelope (..)

    -- * Auto-detected packed scalar (Vector Int32)
  , AutoPackedNums (..)
  ) where

import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Wireform.Derive (tag)

-- | A C-style enum: every constructor is nullary, so
-- 'TypeShapeEnum' picks it up and the deriver routes through
-- 'PFEnum' automatically.
data AutoColor = AutoRed | AutoGreen | AutoBlue
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

-- | One record exercises three of the auto-detect paths at once:
-- a singular scalar (FKBare), an optional enum (FKMaybe wrapping
-- a sum-of-nullary, which the deriver upgrades to PFEnum after
-- stripping the Maybe), and a list-backed repeated string
-- (FKRepeated RepList ModeUnpacked — strings are non-packable).
data AutoCard = AutoCard
  { autoCardId    :: !Int32
  , autoCardLabel :: !(Maybe AutoColor)
  , autoCardNotes :: ![Text]
  } deriving stock (Show, Eq, Generic)

{-# ANN type AutoCard ("AutoCard" :: String) #-}
{-# ANN autoCardId    (tag 1) #-}
{-# ANN autoCardLabel (tag 2) #-}
{-# ANN autoCardNotes (tag 3) #-}

-- | A record with a strict 'Map.Map' field. The deriver sniffs the
-- outer constructor and routes through 'FKMap', inferring the
-- map-key encoding from the key type ('Text' -> 'MapKeyString').
data AutoTagged = AutoTagged
  { autoName  :: !Text
  , autoAttrs :: !(Map Text Text)
  } deriving stock (Show, Eq, Generic)

{-# ANN type AutoTagged ("AutoTagged" :: String) #-}
{-# ANN autoName  (tag 1) #-}
{-# ANN autoAttrs (tag 2) #-}

-- | A sum where every constructor has exactly one argument and a
-- per-constructor @tag N@ annotation. The deriver routes this
-- through 'FKOneof' /without/ the user having to construct a
-- 'TranslatedOneofVariant' list explicitly.
data AutoChoice
  = AutoUrl  !Text
  | AutoSeed !Int32
  deriving stock (Show, Eq, Generic)

{-# ANN AutoUrl  (tag 6) #-}
{-# ANN AutoSeed (tag 8) #-}

-- | Carrier record for the auto-detected oneof. The
-- @autoEnvChoice@ field's type is @Maybe AutoChoice@; the deriver
-- strips the @Maybe@, reifies @AutoChoice@, sees it's a sum where
-- every constructor has one argument and a @tag@ annotation, and
-- emits an 'FKOneof' kind for the field.
data AutoEnvelope = AutoEnvelope
  { autoEnvLabel  :: !Text
  , autoEnvChoice :: !(Maybe AutoChoice)
  } deriving stock (Show, Eq, Generic)

{-# ANN type AutoEnvelope ("AutoEnvelope" :: String) #-}
{-# ANN autoEnvLabel (tag 1) #-}

-- | A record with a 'V.Vector' of a packable scalar. The deriver
-- defaults this to packed encoding (proto3 spec); the encoded
-- bytes should be a single length-delimited block, not one
-- record per element.
data AutoPackedNums = AutoPackedNums
  { autoPackedTag  :: !Text
  , autoPackedNums :: !(V.Vector Int32)
  } deriving stock (Show, Eq, Generic)

{-# ANN type AutoPackedNums ("AutoPackedNums" :: String) #-}
{-# ANN autoPackedTag  (tag 1) #-}
{-# ANN autoPackedNums (tag 2) #-}
