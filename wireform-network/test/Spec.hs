module Main where

import Test.Hspec
import qualified Wireform.Network.TLS.OpenSSL.Test as TLS
import qualified Wireform.Network.Transport.Receive.Test as Receive

main :: IO ()
main = hspec $ do
  Receive.spec
  TLS.spec
