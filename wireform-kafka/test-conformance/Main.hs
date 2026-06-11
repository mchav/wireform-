{- |
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

import Conformance.T0000.Unittests qualified
import Conformance.T0004.Conf qualified
import Conformance.T0006.Symbols qualified
import Conformance.T0009.MockCluster qualified
import Conformance.T0017.Compression qualified
import Conformance.T0031.GetOffsetsMock qualified
import Conformance.T0043.NoConnection qualified
import Conformance.T0072.Headers qualified
import Conformance.T0080.AdminUt qualified
import Conformance.T0086.PurgeLocal qualified
import Conformance.T0095.AllBrokersDown qualified
import Conformance.T0103.TransactionsLocal qualified
import Conformance.T0142.Reauthentication qualified
import Conformance.T0144.IdempotenceMock qualified
import Conformance.T0145.PauseResumeMock qualified
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "librdkafka conformance" $
      sequence_
        -- Tests are grouped by the librdkafka test file they correspond to.
        -- Test number prefix matches librdkafka's NNNN- convention so the
        -- two suites can be diffed by `ls`.
        [ Conformance.T0000.Unittests.tests
        , Conformance.T0004.Conf.tests
        , Conformance.T0006.Symbols.tests
        , Conformance.T0009.MockCluster.tests
        , Conformance.T0017.Compression.tests
        , Conformance.T0031.GetOffsetsMock.tests
        , Conformance.T0043.NoConnection.tests
        , Conformance.T0072.Headers.tests
        , Conformance.T0080.AdminUt.tests
        , Conformance.T0086.PurgeLocal.tests
        , Conformance.T0095.AllBrokersDown.tests
        , Conformance.T0103.TransactionsLocal.tests
        , Conformance.T0142.Reauthentication.tests
        , Conformance.T0144.IdempotenceMock.tests
        , Conformance.T0145.PauseResumeMock.tests
        ]
