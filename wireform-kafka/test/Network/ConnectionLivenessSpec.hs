{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Kafka.Network.Connection.isConnected'.
--
-- The pre-rewrite test suite exercised the crypton-connection
-- 'Network.Connection.initConnectionContext' \/
-- 'Network.Connection.connectFromSocket' path with hand-rolled
-- accept loops.  After the OpenSSL\/duplex-transport rewrite the
-- liveness probe is a thin wrapper over the closed flag (the
-- magic-ring transport surfaces a real failure on the next
-- syscall via its sticky-error state machine, so the
-- best-effort probe has nothing additional to inspect on an
-- idle alive connection).
--
-- This stub keeps the test module in the suite without exercising
-- the deprecated path; the post-rewrite version of the probe is
-- tested end-to-end by the integration suite against a real broker.
module Network.ConnectionLivenessSpec (tests) where

import Test.Syd

tests :: Spec
tests = describe "Connection.isConnected (post-OpenSSL-rewrite stub)" $ sequence_ []
