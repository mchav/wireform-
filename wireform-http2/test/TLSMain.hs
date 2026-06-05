module Main (main) where

import Test.Syd

import qualified Test.TLS

main :: IO ()
main = sydTest Test.TLS.tests
