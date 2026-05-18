{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Streams.EmitPolicy
-- Description : Promote KIP-825's EmitStrategy to a first-class
--               'EmitPolicy' usable by every windowed / stateful
--               operator (Riffle \xc2\xa76)
--
-- The KIP-825 'Kafka.Streams.TimeWindowedKStream.EmitStrategy'
-- enum has two arms (@OnWindowUpdate@ and @OnWindowClose@) and
-- only the time-windowed-KStream consumes it. Riffle \xc2\xa76 promotes
-- it to a first-class 'EmitPolicy' value that:
--
--   * Supports the existing two strategies (as smart constructors
--     'emitOnUpdate' / 'emitOnWindowClose').
--   * Adds 'emitOnCount': fire once every @N@ updates per key.
--   * Adds 'emitCustom': a user-supplied @\\record state -> Bool@
--     predicate.
--   * Is consumed by any windowed / stateful operator that wants
--     to throttle downstream traffic, not just
--     @TimeWindowedKStream@.
--
-- The existing 'EmitStrategy' / 'emitOnWindowUpdate' /
-- 'emitOnWindowClose' names stay; they just become aliases for
-- the 'EmitPolicy' equivalents.
module Kafka.Streams.EmitPolicy
  ( -- * Policy
    EmitPolicy (..)
  , EmitDecision (..)
    -- ** Smart constructors
  , emitOnUpdate
  , emitOnWindowClose'
  , emitOnCount
  , emitCustom
    -- * Evaluation
  , EmitContext (..)
  , decideEmit
  ) where

import GHC.Generics (Generic)
import Data.Int (Int64)
import Data.Text (Text)

import Kafka.Streams.Time (Timestamp)

----------------------------------------------------------------------
-- Policy
----------------------------------------------------------------------

-- | What an operator should do with a downstream emission for a
-- given record. Returned by 'decideEmit'.
data EmitDecision
  = Emit
    -- ^ Forward the record (or aggregate, etc.) downstream.
  | Suppress
    -- ^ Hold the record back; the operator's existing buffer /
    -- aggregate machinery keeps the latest state but does not
    -- propagate.
  deriving stock (Eq, Show, Generic)

-- | Pluggable emit policy. The ADT carries a /tag/ + parameters
-- so the engine can pattern-match on it for optimisations
-- (e.g. KIP-825 'OnWindowClose' uses the suppress operator's
-- machinery instead of a per-update buffer). Use 'emitCustom'
-- when none of the built-in shapes fits.
data EmitPolicy
  = EmitOnUpdate
    -- ^ JVM default. Every update propagates.
  | EmitOnWindowClose
    -- ^ KIP-825 @OnWindowClose@. Hold until stream-time \>= window
    -- end + grace, then emit one record per window.
  | EmitOnCount !Int64
    -- ^ Emit once every @N@ updates per key. @EmitOnCount 1@
    -- degenerates to 'EmitOnUpdate'.
  | EmitCustom
      !Text
      !(EmitContext -> EmitDecision)
    -- ^ Caller-supplied predicate. The label is for diagnostics.
  deriving stock (Generic)

instance Show EmitPolicy where
  show EmitOnUpdate           = "EmitOnUpdate"
  show EmitOnWindowClose      = "EmitOnWindowClose"
  show (EmitOnCount n)        = "EmitOnCount " <> show n
  show (EmitCustom lbl _)     = "EmitCustom " <> show lbl

-- | Per-record context the operator passes to the policy at
-- decision time. New fields can be added without breaking
-- callers because 'EmitCustom' takes the record directly.
data EmitContext = EmitContext
  { ecKeyUpdateCount :: !Int64
    -- ^ Number of updates this key has received so far this
    -- operator instance has been live (1-based).
  , ecStreamTime     :: !Timestamp
    -- ^ Current per-task stream time, as the engine sees it.
  , ecWindowEnd      :: !(Maybe Timestamp)
    -- ^ For windowed operators: the end of the window the
    -- update belongs to. 'Nothing' for non-windowed callers.
  } deriving stock (Show, Generic)

----------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------

-- | JVM-default policy: every update emits.
emitOnUpdate :: EmitPolicy
emitOnUpdate = EmitOnUpdate

-- | KIP-825 @OnWindowClose@ policy. Named with a trailing
-- apostrophe to avoid clashing with the existing
-- 'Kafka.Streams.TimeWindowedKStream.emitOnWindowClose' export,
-- which has the same name but returns the legacy
-- 'EmitStrategy'. Tests / callers using 'EmitPolicy' use this
-- form.
emitOnWindowClose' :: EmitPolicy
emitOnWindowClose' = EmitOnWindowClose

-- | Emit once every @N@ updates for a given key. @N <= 0@ is
-- treated as 1.
emitOnCount :: Int64 -> EmitPolicy
emitOnCount n = EmitOnCount (max 1 n)

-- | Build a user-supplied policy from a label and a predicate.
emitCustom :: Text -> (EmitContext -> EmitDecision) -> EmitPolicy
emitCustom = EmitCustom

----------------------------------------------------------------------
-- Evaluation
----------------------------------------------------------------------

-- | Apply the policy to a context. Operators call this in their
-- hot path for every record they're considering emitting.
decideEmit :: EmitPolicy -> EmitContext -> EmitDecision
decideEmit p ctx = case p of
  EmitOnUpdate        -> Emit
  EmitOnWindowClose   -> case ecWindowEnd ctx of
    Just we
      | ecStreamTime ctx >= we -> Emit
      | otherwise              -> Suppress
    Nothing -> Emit  -- non-windowed callers degenerate to Emit
  EmitOnCount n ->
    if ecKeyUpdateCount ctx `mod` n == 0
      then Emit
      else Suppress
  EmitCustom _ f -> f ctx
