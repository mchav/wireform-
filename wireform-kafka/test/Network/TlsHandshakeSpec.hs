{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the TLS handshake path on Kafka broker connections.
--
-- The pre-rewrite suite built 'Network.TLS.ClientParams' values and
-- handed them to crypton-connection's @connectFromSocket@.  After
-- the OpenSSL\/duplex-transport rewrite, broker TLS goes through
-- 'Wireform.Network.TLS.Config.TlsClientConfig' +
-- 'Wireform.Network.TLS.OpenSSL.newClient'; the corresponding
-- handshake assertions live in
-- @wireform-network/test/Wireform/Network/TLS/OpenSSL/Test.hs@,
-- which exercises the same OpenSSL bridge bytes flow through.
--
-- This stub keeps the test module in the suite without exercising
-- the deprecated pure-Haskell @tls@ path.
module Network.TlsHandshakeSpec (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "TLS handshake (post-OpenSSL-rewrite stub)" []
