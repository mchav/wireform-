{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty

import qualified Test.HPACK
import qualified Test.Frame
import qualified Test.Connection
#ifndef WIREFORM_HTTP2_NO_TLS_TESTS
import qualified Test.TLS
#endif

main :: IO ()
main = defaultMain $ testGroup "wireform-http2"
  [ Test.HPACK.tests
  , Test.Frame.tests
  , Test.Connection.tests
#ifndef WIREFORM_HTTP2_NO_TLS_TESTS
  , Test.TLS.tests
#endif
  ]
