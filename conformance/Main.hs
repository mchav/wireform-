-- | Conformance test runner for hs-proto.
--
-- Usage: conformance-test-runner ./hs-proto-conformance
module Main where

import Proto.Conformance

main :: IO ()
main = conformanceMain handleConformanceRequest
