{-# LANGUAGE OverloadedStrings #-}
module Test.Marker (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import qualified Wireform.Stats.Marker as Mk

tests :: TestTree
tests = testGroup "Marker"
  [ testCase "parseRegions: no markers"     noMarkersCase
  , testCase "parseRegions: round-trip"     roundTripCase
  , testCase "rewriteMarkers: replace one"  replaceOne
  , testCase "rewriteMarkers: idempotent"   idempotent
  , testCase "rewriteMarkers: leaves unmanaged keys alone" leaveUnknown
  , testCase "rewriteMarkers: doesn't drop content"        preserveOutside
  , testCase "markersIn: order"             markersInOrder
  ]

doc :: Text
doc = T.unlines
  [ "header"
  , ""
  , "<!-- BEGIN_AUTOGEN tests -->"
  , "old test summary"
  , "<!-- END_AUTOGEN tests -->"
  , ""
  , "middle"
  , ""
  , "<!-- BEGIN_AUTOGEN bench:foo -->"
  , "old bench"
  , "<!-- END_AUTOGEN bench:foo -->"
  , ""
  , "footer"
  ]

noMarkersCase :: Assertion
noMarkersCase = do
  let plain = "no markers here\nat all\n"
  Mk.rewriteMarkers Map.empty plain @?= plain

roundTripCase :: Assertion
roundTripCase = Mk.renderRegions (Mk.parseRegions doc) @?= doc

replaceOne :: Assertion
replaceOne = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "new test summary")]
      result   = Mk.rewriteMarkers reps doc
  -- New body present
  assertBool "new body present"
    ("new test summary" `T.isInfixOf` result)
  -- Old body gone
  assertBool "old body gone"
    (not ("old test summary" `T.isInfixOf` result))
  -- Marker lines preserved
  assertBool "BEGIN preserved"
    ("<!-- BEGIN_AUTOGEN tests -->" `T.isInfixOf` result)
  assertBool "END preserved"
    ("<!-- END_AUTOGEN tests -->" `T.isInfixOf` result)
  -- Other marker untouched
  assertBool "other body untouched"
    ("old bench" `T.isInfixOf` result)
  -- Surrounding text untouched
  assertBool "header untouched" ("header"  `T.isInfixOf` result)
  assertBool "middle untouched" ("middle"  `T.isInfixOf` result)
  assertBool "footer untouched" ("footer"  `T.isInfixOf` result)

idempotent :: Assertion
idempotent = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "stable body")]
      once     = Mk.rewriteMarkers reps doc
      twice    = Mk.rewriteMarkers reps once
  twice @?= once

leaveUnknown :: Assertion
leaveUnknown = do
  let unknownKey = mustKey "not-in-doc"
      reps       = Map.fromList [(unknownKey, "anything")]
  Mk.rewriteMarkers reps doc @?= doc

preserveOutside :: Assertion
preserveOutside = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "x")]
      result   = Mk.rewriteMarkers reps doc
  assertBool "header line preserved verbatim" ("header\n" `T.isInfixOf` result)
  assertBool "blank line before middle preserved" ("\nmiddle\n" `T.isInfixOf` result)
  assertBool "footer line at end preserved" (T.takeEnd 7 result == "footer\n")

markersInOrder :: Assertion
markersInOrder = do
  Mk.markersIn doc @?= [mustKey "tests", mustKey "bench:foo"]

mustKey :: Text -> Mk.MarkerKey
mustKey t = case Mk.markerKey t of
  Right k -> k
  Left  e -> error ("invalid key in test: " <> T.unpack t <> " (" <> e <> ")")
