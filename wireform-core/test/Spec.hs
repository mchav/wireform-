module Main where

import Test.Hspec
import qualified Wireform.Ring.Test as Ring
import qualified Wireform.Parser.Test as Parser
import qualified Wireform.Transport.SendTest as Send
import qualified Wireform.Base64.Test as Base64

main :: IO ()
main = hspec $ do
  Ring.spec
  Parser.spec
  Send.spec
  Base64.spec
