module Main (main) where

import Test.Tasty (defaultMain)

import qualified Test.TLS

main :: IO ()
main = defaultMain Test.TLS.tests
