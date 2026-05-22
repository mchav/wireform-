module Main where

import Test.Hspec
import qualified Wireform.Network.Transport.Recv.Test as Recv

main :: IO ()
main = hspec Recv.spec
