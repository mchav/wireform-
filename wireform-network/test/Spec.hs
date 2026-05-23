module Main where

import Test.Hspec
import qualified Wireform.Network.TLS.OpenSSL.Test as TLS
import qualified Wireform.Network.Transport.Recv.Test as Recv

main :: IO ()
main = hspec $ do
  Recv.spec
  TLS.spec
