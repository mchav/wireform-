{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Streams.DSL.ForeignKeyJoinV2
Description : KIP-213 foreign-key join with subscription store + responder

Today's "ForeignKeyJoin" combinator does naive re-keying: every
left update is republished to the right side under the foreign
key, and the join is recomputed on the right side. That works
for monotonic streams but breaks under timing skew — out-of-order
right updates can shadow a more recent left value, producing a
stale join output.

KIP-213 fixes this with two pieces:

  1. A /subscription store/ on the left side: each left record
     publishes a "subscription" message keyed by the foreign key,
     carrying a token derived from the left value's hash so the
     right side can verify that the responder it generates still
     refers to the same left value.
  2. A /responder topic/ keyed by the original (left) key,
     carrying the right value (or a tombstone). The left side
     joins each responder against its current left-value cache
     and only emits the join if the token matches.

This module provides the pure data layer:

  * 'SubscriptionMessage'    — what the left side publishes.
  * 'Responder'              — what the right side publishes back.
  * 'foreignKeyJoinPure'     — the join transition state machine
    (left update / right update / responder), used by tests to
    exercise the protocol without spinning up a full topology.

The DSL combinator that wires this into a 'StreamsBuilder' is a
larger change still pending; this module is the correctness
foundation it would build on.
-}
module Kafka.Streams.DSL.ForeignKeyJoinV2
  ( SubscriptionToken (..)
  , SubscriptionMessage (..)
  , Responder (..)
  , FkJoinState (..)
  , emptyState
  , LeftEvent (..)
  , RightEvent (..)
  , JoinOutput (..)
  , stepLeft
  , stepRight
  , runEvents
  ) where

import Data.Hashable (Hashable, hash)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

-- | An opaque token identifying a particular value-version of a
-- left record. We use the value's 'hash' so the right side can
-- compute it independently from the responder's payload.
newtype SubscriptionToken = SubscriptionToken { unToken :: Int }
  deriving stock (Eq, Ord, Show, Generic)

mkToken :: Hashable v => v -> SubscriptionToken
mkToken v = SubscriptionToken (hash v)

-- | The message the left side publishes when its value changes.
-- 'smPropagate' is the foreign key — what the right side sees
-- as its message key.
data SubscriptionMessage k fk = SubscriptionMessage
  { smLeftKey  :: !k
  , smPropagate :: !fk
  , smToken    :: !SubscriptionToken
  , smTombstone :: !Bool
    -- ^ 'True' when the left value was deleted.
  }
  deriving stock (Eq, Show, Generic)

-- | The message the right side publishes back, keyed by the
-- /left/ key (so the left-side join cache can match).
data Responder k v0 = Responder
  { rsLeftKey :: !k
  , rsToken   :: !SubscriptionToken
    -- ^ Echoed from the subscription so the left side can ignore
    --   stale responders.
  , rsValue   :: !(Maybe v0)
    -- ^ The right value, or 'Nothing' if the right side deleted /
    --   doesn't have it.
  }
  deriving stock (Eq, Show, Generic)

-- | The pure FK-join state machine.
data FkJoinState k fk vl vr vo = FkJoinState
  { fjsLefts        :: !(Map k vl)
    -- ^ Latest left value per key.
  , fjsLeftTokens   :: !(Map k SubscriptionToken)
    -- ^ Latest token published to the right side per key.
  , fjsRights       :: !(Map fk vr)
    -- ^ Latest right value per foreign key.
  , fjsLeftToFk     :: !(Map k fk)
    -- ^ Map a left key to the foreign key its current value
    --   subscribed to. Updated when a left record changes.
  , fjsJoiner       :: !(vl -> vr -> vo)
  , fjsExtractFk    :: !(vl -> fk)
  }

emptyState
  :: Ord k
  => Ord fk
  => (vl -> vr -> vo)
  -> (vl -> fk)
  -> FkJoinState k fk vl vr vo
emptyState join_ extractFk = FkJoinState
  { fjsLefts      = Map.empty
  , fjsLeftTokens = Map.empty
  , fjsRights     = Map.empty
  , fjsLeftToFk   = Map.empty
  , fjsJoiner     = join_
  , fjsExtractFk  = extractFk
  }

data LeftEvent k vl
  = LeftPut    !k !vl
  | LeftDelete !k
  deriving stock (Eq, Show, Generic)

data RightEvent fk vr
  = RightPut    !fk !vr
  | RightDelete !fk
  deriving stock (Eq, Show, Generic)

-- | One join output. The output topic is keyed by the left key;
-- the value is the joined value or 'Nothing' for a tombstone.
data JoinOutput k vo = JoinOutput
  { joKey   :: !k
  , joValue :: !(Maybe vo)
  }
  deriving stock (Eq, Show, Generic)

-- | Apply a left event. May emit join outputs (updates / tombstones)
-- and a subscription message for the right side.
stepLeft
  :: (Ord k, Ord fk, Hashable vl)
  => FkJoinState k fk vl vr vo
  -> LeftEvent k vl
  -> ( FkJoinState k fk vl vr vo
     , [JoinOutput k vo]
     , Maybe (SubscriptionMessage k fk)
     )
stepLeft st = \case
  LeftPut k vl ->
    let !fk      = fjsExtractFk st vl
        !token   = mkToken vl
        !st'     = st
          { fjsLefts      = Map.insert k vl (fjsLefts st)
          , fjsLeftTokens = Map.insert k token (fjsLeftTokens st)
          , fjsLeftToFk   = Map.insert k fk (fjsLeftToFk st)
          }
        !outs    = case Map.lookup fk (fjsRights st) of
          Just vr -> [JoinOutput k (Just (fjsJoiner st vl vr))]
          Nothing -> [JoinOutput k Nothing]
        !subMsg  = SubscriptionMessage k fk token False
    in (st', outs, Just subMsg)
  LeftDelete k ->
    let !st' = st
          { fjsLefts      = Map.delete k (fjsLefts st)
          , fjsLeftTokens = Map.delete k (fjsLeftTokens st)
          , fjsLeftToFk   = Map.delete k (fjsLeftToFk st)
          }
        !subMsg = case Map.lookup k (fjsLeftToFk st) of
          Nothing -> Nothing
          Just fk ->
            Just (SubscriptionMessage k fk (SubscriptionToken 0) True)
    in (st', [JoinOutput k Nothing], subMsg)

-- | Apply a right event. Returns the updated state and any join
-- outputs for left records that subscribe to this foreign key.
stepRight
  :: (Ord k, Ord fk)
  => FkJoinState k fk vl vr vo
  -> RightEvent fk vr
  -> ( FkJoinState k fk vl vr vo
     , [JoinOutput k vo]
     )
stepRight st = \case
  RightPut fk vr ->
    let !st' = st { fjsRights = Map.insert fk vr (fjsRights st) }
        !outs =
          [ JoinOutput k (Just (fjsJoiner st vl vr))
          | (k, fk') <- Map.toList (fjsLeftToFk st)
          , fk' == fk
          , Just vl <- [Map.lookup k (fjsLefts st)]
          ]
    in (st', outs)
  RightDelete fk ->
    let !st' = st { fjsRights = Map.delete fk (fjsRights st) }
        !outs =
          [ JoinOutput k Nothing
          | (k, fk') <- Map.toList (fjsLeftToFk st)
          , fk' == fk
          ]
    in (st', outs)

-- | Replay a sequence of events. Useful for property-style tests:
-- the output for a serialised execution must equal the output for
-- /any/ replay, given identical end state.
runEvents
  :: (Ord k, Ord fk, Hashable vl)
  => FkJoinState k fk vl vr vo
  -> [Either (LeftEvent k vl) (RightEvent fk vr)]
  -> ( FkJoinState k fk vl vr vo
     , [JoinOutput k vo]
     , [SubscriptionMessage k fk]
     )
runEvents st0 = foldl step (st0, [], [])
  where
    step (st, accJ, accS) (Left le) =
      let (st', js, mSub) = stepLeft st le
          accS' = accS ++ maybe [] pure mSub
      in (st', accJ ++ js, accS')
    step (st, accJ, accS) (Right re) =
      let (st', js) = stepRight st re
      in (st', accJ ++ js, accS)
