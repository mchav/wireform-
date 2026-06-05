module Main (main) where

import Test.Syd

import qualified Test.Hermes.AcceptCharset
import qualified Test.Hermes.AcceptEncoding
import qualified Test.Hermes.AcceptRanges
import qualified Test.Hermes.ContentRange
import qualified Test.Hermes.ProxyAuthenticate
import qualified Test.Hermes.Range
import qualified Test.Hermes.RenderingUtil
import qualified Test.Hermes.WWWAuthenticate

main :: IO ()
main = sydTest $ describe "hermes" $ sequence_
  [ Test.Hermes.AcceptCharset.tests
  , Test.Hermes.AcceptEncoding.tests
  , Test.Hermes.AcceptRanges.tests
  , Test.Hermes.ContentRange.tests
  , Test.Hermes.ProxyAuthenticate.tests
  , Test.Hermes.Range.tests
  , Test.Hermes.RenderingUtil.tests
  , Test.Hermes.WWWAuthenticate.tests
  ]
