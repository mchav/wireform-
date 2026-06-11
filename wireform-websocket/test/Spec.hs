module Main (main) where

import Test.Echo qualified
import Test.Frame qualified
import Test.Handshake qualified
import Test.PerMessageDeflate qualified
import Test.Syd
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)
import Test.URI qualified


main :: IO ()
-- The Echo suite spins up loopback websocket servers; like the other
-- socket-driven suites it isn't safe under sydtest's default
-- per-capability parallelism, so run synchronously (as hspec/tasty did).
main =
  sydTestWith defaultSettings {settingThreads = Synchronous} $
    describe "wireform-websocket" $
      sequence_
        [ Test.Frame.tests
        , Test.Handshake.tests
        , Test.URI.tests
        , Test.PerMessageDeflate.tests
        , Test.Echo.tests
        ]
