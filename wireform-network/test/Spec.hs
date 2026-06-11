module Main where

import Test.Syd
import Test.Syd.OptParse (Settings (..), Threads (..), defaultSettings)
import Wireform.Network.TLS.OpenSSL.Test qualified as TLS
import Wireform.Network.Transport.Receive.Test qualified as Receive
import Wireform.Network.Transport.Roundtrip.Test qualified as Roundtrip


main :: IO ()
main =
  -- These suites exercise loopback sockets, OpenSSL FFI, and the
  -- magic-ring transport, none of which are safe to drive from
  -- multiple threads at once. hspec ran them sequentially; sydtest
  -- defaults to one thread per capability, so pin it to synchronous.
  sydTestWith defaultSettings {settingThreads = Synchronous} $ do
    Receive.spec
    TLS.spec
    Roundtrip.spec
