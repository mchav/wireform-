{-|
Module      : Main
Description : librdkafka-conformance test suite entry point

Runs the porting of librdkafka's @tests/@ directory into Haskell.
See @docs/LIBRDKAFKA_CONFORMANCE.md@ for the full catalog of which
of librdkafka's 168 tests we have ported, which we have intentionally
skipped (and why), and which are blocked on us building infrastructure
that librdkafka has but we do not (notably: an in-process Kafka mock
broker analogous to @rd_kafka_mock_cluster_new@).

Each ported test corresponds 1-to-1 to a numbered @tests\/NNNN-...c@
or @tests\/NNNN-...cpp@ file in librdkafka, so it is easy to compare
the two implementations side by side.
-}
module Main (main) where

import Test.Tasty

import qualified Conformance.T0000.Unittests
import qualified Conformance.T0004.Conf
import qualified Conformance.T0006.Symbols
import qualified Conformance.T0017.Compression
import qualified Conformance.T0043.NoConnection
import qualified Conformance.T0072.Headers
import qualified Conformance.T0080.AdminUt
import qualified Conformance.T0086.PurgeLocal
import qualified Conformance.T0095.AllBrokersDown
import qualified Conformance.T0103.TransactionsLocal
import qualified Conformance.T0142.Reauthentication
import qualified Conformance.T0144.IdempotenceMock

main :: IO ()
main = defaultMain $ testGroup "librdkafka conformance"
  -- Tests are grouped by the librdkafka test file they correspond to.
  -- Test number prefix matches librdkafka's NNNN- convention so the
  -- two suites can be diffed by `ls`.
  [ Conformance.T0000.Unittests.tests
  , Conformance.T0004.Conf.tests
  , Conformance.T0006.Symbols.tests
  , Conformance.T0017.Compression.tests
  , Conformance.T0043.NoConnection.tests
  , Conformance.T0072.Headers.tests
  , Conformance.T0080.AdminUt.tests
  , Conformance.T0086.PurgeLocal.tests
  , Conformance.T0095.AllBrokersDown.tests
  , Conformance.T0103.TransactionsLocal.tests
  , Conformance.T0142.Reauthentication.tests
  , Conformance.T0144.IdempotenceMock.tests
  ]
