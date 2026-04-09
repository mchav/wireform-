-- | Conformance test runner for wireform.
--
-- Usage: conformance-test-runner ./wireform-conformance
module Main where

import Proto.Conformance

main :: IO ()
main = conformanceMain handleConformanceRequest
