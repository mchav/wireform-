module Main (main) where

import Test.Syd
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)

import qualified Test.Frame
import qualified Test.Handshake
import qualified Test.URI
import qualified Test.Echo
import qualified Test.PerMessageDeflate

main :: IO ()
-- The Echo suite spins up loopback websocket servers; like the other
-- socket-driven suites it isn't safe under sydtest's default
-- per-capability parallelism, so run synchronously (as hspec/tasty did).
main = sydTestWith defaultSettings {settingThreads = Synchronous} $ describe "wireform-websocket" $ sequence_
  [ Test.Frame.tests
  , Test.Handshake.tests
  , Test.URI.tests
  , Test.PerMessageDeflate.tests
  , Test.Echo.tests
  ]
