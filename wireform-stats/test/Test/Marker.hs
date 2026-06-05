{-# LANGUAGE OverloadedStrings #-}
module Test.Marker (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import qualified Wireform.Stats.Marker as Mk

tests :: Spec
tests = describe "Marker" $ sequence_
  [ it "parseRegions: no markers"     noMarkersCase
  , it "parseRegions: round-trip"     roundTripCase
  , it "rewriteMarkers: replace one"  replaceOne
  , it "rewriteMarkers: idempotent"   idempotent
  , it "rewriteMarkers: leaves unmanaged keys alone" leaveUnknown
  , it "rewriteMarkers: doesn't drop content"        preserveOutside
  , it "markersIn: order"             markersInOrder
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
  Mk.rewriteMarkers Map.empty plain `shouldBe` plain

roundTripCase :: Assertion
roundTripCase = Mk.renderRegions (Mk.parseRegions doc) `shouldBe` doc

replaceOne :: Assertion
replaceOne = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "new test summary")]
      result   = Mk.rewriteMarkers reps doc
  -- New body present
  ("new test summary" `T.isInfixOf` result) `shouldBe` True
  -- Old body gone
  (not ("old test summary" `T.isInfixOf` result)) `shouldBe` True
  -- Marker lines preserved
  ("<!-- BEGIN_AUTOGEN tests -->" `T.isInfixOf` result) `shouldBe` True
  ("<!-- END_AUTOGEN tests -->" `T.isInfixOf` result) `shouldBe` True
  -- Other marker untouched
  ("old bench" `T.isInfixOf` result) `shouldBe` True
  -- Surrounding text untouched
  ("header"  `T.isInfixOf` result) `shouldBe` True
  ("middle"  `T.isInfixOf` result) `shouldBe` True
  ("footer"  `T.isInfixOf` result) `shouldBe` True

idempotent :: Assertion
idempotent = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "stable body")]
      once     = Mk.rewriteMarkers reps doc
      twice    = Mk.rewriteMarkers reps once
  twice `shouldBe` once

leaveUnknown :: Assertion
leaveUnknown = do
  let unknownKey = mustKey "not-in-doc"
      reps       = Map.fromList [(unknownKey, "anything")]
  Mk.rewriteMarkers reps doc `shouldBe` doc

preserveOutside :: Assertion
preserveOutside = do
  let testsKey = mustKey "tests"
      reps     = Map.fromList [(testsKey, "x")]
      result   = Mk.rewriteMarkers reps doc
  ("header\n" `T.isInfixOf` result) `shouldBe` True
  ("\nmiddle\n" `T.isInfixOf` result) `shouldBe` True
  (T.takeEnd 7 result == "footer\n") `shouldBe` True

markersInOrder :: Assertion
markersInOrder = do
  Mk.markersIn doc `shouldBe` [mustKey "tests", mustKey "bench:foo"]

mustKey :: Text -> Mk.MarkerKey
mustKey t = case Mk.markerKey t of
  Right k -> k
  Left  e -> error ("invalid key in test: " <> T.unpack t <> " (" <> e <> ")")
