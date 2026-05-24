module Main where

import Test.Hspec
import qualified Wireform.Ring.Test as Ring
import qualified Wireform.Parser.Test as Parser
import qualified Wireform.Transport.SendTest as Send

main :: IO ()
main = hspec $ do
  Ring.spec
  Parser.spec
  Send.spec
