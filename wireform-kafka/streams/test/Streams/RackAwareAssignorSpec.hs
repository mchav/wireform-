{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

{- | KIP-925 rack-aware partition assignor.

Verifies that the cost-aware placement actually keeps
/like-racked/ tasks together (saving cross-rack traffic) and
spreads standbys across racks (preserving failure-domain
diversity) — without breaking the cooperative-sticky
balance invariants the existing 'assign' satisfies.
-}
module Streams.RackAwareAssignorSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Kafka.Streams.Processor (TaskId (..))
import Kafka.Streams.Runtime.Assignor
import Test.Syd


m :: Int -> MemberId
m i = MemberId (T.pack ("m" <> show i))


t :: Int -> TaskId
t i = TaskId 0 (fromIntegral i)


tests :: Spec
tests =
  describe "Rack-aware assignor (KIP-925)" $
    sequence_
      [ same_rack_wins_among_lightest
      , rack_info_empty_equals_plain_assign
      , standbys_prefer_different_rack
      ]


----------------------------------------------------------------------
-- 1. Same-rack tie-break
----------------------------------------------------------------------

same_rack_wins_among_lightest :: Spec
same_rack_wins_among_lightest =
  it "Lightest-loaded members in matching rack are preferred over off-rack" $ do
    let members = Set.fromList [m 0, m 1, m 2]
        -- 1 task. Two members are lightest-loaded (everyone
        -- starts at 0), so the rack tie-breaker decides.
        tasks = Set.fromList [t 0]
        ri =
          RackInfo
            { memberRack =
                Map.fromList
                  [ (m 0, "rack-a")
                  , (m 1, "rack-b")
                  , (m 2, "rack-b")
                  ]
            , taskRacks =
                Map.singleton
                  (t 0)
                  (Set.singleton "rack-b")
            }
    let na =
          assignRackAware
            members
            tasks
            0
            Map.empty
            ri
            defaultRackAwareCost
    -- The task should land on m01 or m02 (rack-b), not m00.
    let owner =
          head
            [ memb
            | (memb, asg) <- Map.toList na
            , Set.member (t 0) asg.active
            ]
    (if (owner `elem` [m 1, m 2]) then pure () else expectationFailure ("task landed on " <> show owner <> " not a rack-b member"))


----------------------------------------------------------------------
-- 2. Empty rack info = same as plain assign
----------------------------------------------------------------------

rack_info_empty_equals_plain_assign :: Spec
rack_info_empty_equals_plain_assign =
  it "assignRackAware with empty RackInfo: identical to assign" $ do
    let members = Set.fromList [m 0, m 1]
        tasks = Set.fromList [t i | i <- [0 .. 3]]
        ri = RackInfo Map.empty Map.empty
        a = assign members tasks 0 Map.empty
        b =
          assignRackAware
            members
            tasks
            0
            Map.empty
            ri
            defaultRackAwareCost
    -- Active sets must match member-by-member. Standby sets
    -- can differ (the rack-aware standby placement sorts by
    -- (load, cost, idx) which can pick a different
    -- deterministic order); we don't compare standbys here.
    Map.map (\ta -> ta.active) a `shouldBe` Map.map (\ta -> ta.active) b


----------------------------------------------------------------------
-- 3. Standbys prefer different rack
----------------------------------------------------------------------

standbys_prefer_different_rack :: Spec
standbys_prefer_different_rack =
  it "Standby for a task placed in rack X prefers a member in rack Y" $ do
    let members = Set.fromList [m 0, m 1, m 2, m 3]
        tasks = Set.fromList [t 0]
        ri =
          RackInfo
            { memberRack =
                Map.fromList
                  [ (m 0, "rack-a")
                  , (m 1, "rack-a")
                  , (m 2, "rack-b")
                  , (m 3, "rack-b")
                  ]
            , taskRacks =
                Map.singleton
                  (t 0)
                  (Set.singleton "rack-a")
            }
    let na =
          assignRackAware
            members
            tasks
            1
            Map.empty
            ri
            defaultRackAwareCost
        ownersOf field =
          [ mem
          | (mem, asg) <- Map.toList na
          , Set.member (t 0) (field asg)
          ]
        actives = ownersOf (\ta -> ta.active)
        standbys = ownersOf (\ta -> ta.standby)
    -- Active should land in rack-a (cheapest traffic for the
    -- task's partitions in rack-a).
    actives `shouldBe` [m 0]
    -- Standby should land in rack-b (the non-overlap cost
    -- pushes it away from rack-a where the active lives).
    case standbys of
      [s] ->
        ( if (s `elem` [m 2, m 3])
            then pure ()
            else
              expectationFailure
                ( "standby landed on "
                    <> show s
                    <> " not a rack-b member"
                )
        )
      _ ->
        error
          ( "expected exactly one standby, got "
              <> show standbys
          )
