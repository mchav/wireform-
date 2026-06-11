{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the KIP-368 helper functions in
@Kafka.Network.Auth.SASL@: 'effectiveReauthDeadlineMs' and
'reauthRequiredAtMs'.
-}
module Network.SaslReauthSpec (tests) where

import Kafka.Network.Auth.SASL qualified as SASL
import Test.Syd


tests :: Spec
tests =
  describe "SASL re-auth (KIP-368)" $
    sequence_
      [ describe "effectiveReauthDeadlineMs" $
          sequence_
            [ it
                "no deadlines configured -> Nothing"
                d_no_deadlines
            , it
                "broker lifetime only -> now + lifetime"
                d_broker_only
            , it
                "client max only -> now + max"
                d_client_only
            , it
                "both set -> now + min(broker, client)"
                d_both_set
            , it
                "broker > client -> client wins"
                d_broker_larger
            , it
                "broker < client -> broker wins"
                d_broker_smaller
            ]
      , describe "reauthRequiredAtMs" $
          sequence_
            [ it
                "Nothing deadline -> never"
                r_never
            , it
                "deadline far in the future -> not required"
                r_far_future
            , it
                "deadline within the safety margin -> required"
                r_within_margin
            , it
                "deadline already passed -> required"
                r_passed
            , it
                "safety margin >= 1 second"
                r_min_margin
            ]
      ]


----------------------------------------------------------------------
-- effectiveReauthDeadlineMs
----------------------------------------------------------------------

d_no_deadlines :: IO ()
d_no_deadlines =
  SASL.effectiveReauthDeadlineMs 1000 0 0 `shouldBe` Nothing


d_broker_only :: IO ()
d_broker_only =
  SASL.effectiveReauthDeadlineMs 1000 60_000 0 `shouldBe` Just 61_000


d_client_only :: IO ()
d_client_only =
  SASL.effectiveReauthDeadlineMs 1000 0 30_000 `shouldBe` Just 31_000


d_both_set :: IO ()
d_both_set =
  SASL.effectiveReauthDeadlineMs 1000 60_000 30_000 `shouldBe` Just 31_000


d_broker_larger :: IO ()
d_broker_larger =
  SASL.effectiveReauthDeadlineMs 1000 90_000 30_000 `shouldBe` Just 31_000


d_broker_smaller :: IO ()
d_broker_smaller =
  SASL.effectiveReauthDeadlineMs 1000 15_000 60_000 `shouldBe` Just 16_000


----------------------------------------------------------------------
-- reauthRequiredAtMs
----------------------------------------------------------------------

r_never :: IO ()
r_never =
  SASL.reauthRequiredAtMs 1_000_000 Nothing `shouldBe` False


r_far_future :: IO ()
r_far_future =
  -- now=1000, deadline=1_000_000 -> remaining 999_000ms,
  -- margin = max 1000 (1_000_000/10) = 100_000ms;
  -- 999_000 > 100_000 so no reauth yet.
  SASL.reauthRequiredAtMs 1000 (Just 1_000_000) `shouldBe` False


r_within_margin :: IO ()
r_within_margin =
  -- now=950_000, deadline=1_000_000 -> remaining 50_000ms,
  -- margin = max 1000 (1_000_000/10) = 100_000ms;
  -- 50_000 <= 100_000 so reauth required.
  SASL.reauthRequiredAtMs 950_000 (Just 1_000_000) `shouldBe` True


r_passed :: IO ()
r_passed =
  -- Already past the deadline.
  SASL.reauthRequiredAtMs 2_000_000 (Just 1_000_000) `shouldBe` True


r_min_margin :: IO ()
r_min_margin = do
  -- Even very small deadlines get a 1 second floor on the margin
  -- so we don't burn the connection on a tiny rounding error.
  -- Deadline = 5000 ms, /10 = 500, but max gives 1000.
  -- now = 4500, remaining = 500 <= 1000 -> required.
  SASL.reauthRequiredAtMs 4500 (Just 5000) `shouldBe` True
  -- now = 3500, remaining = 1500 > 1000 -> not yet.
  SASL.reauthRequiredAtMs 3500 (Just 5000) `shouldBe` False
