{-# LANGUAGE OverloadedStrings #-}
module Test.Test (tests) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Test.Syd

import qualified Wireform.Stats.Test as Tst

tests :: Spec
tests = describe "Test" $ sequence_
  [ it "parses a tasty-style JUnit XML doc" parseSimple
  , it "summaryToTestLine: clean run"        cleanLine
  , it "summaryToTestLine: with failures"    failureLine
  ]

junitDoc :: Text
junitDoc = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<testsuites>"
  , "  <testsuite name=\"unit\" tests=\"3\" failures=\"0\" errors=\"0\" skipped=\"0\" time=\"0.250\">"
  , "    <testcase name=\"a\" classname=\"unit\" time=\"0.100\"/>"
  , "    <testcase name=\"b\" classname=\"unit\" time=\"0.080\"/>"
  , "    <testcase name=\"c\" classname=\"unit\" time=\"0.070\"/>"
  , "  </testsuite>"
  , "  <testsuite name=\"properties\" tests=\"5\" failures=\"1\" errors=\"0\" skipped=\"0\" time=\"1.000\">"
  , "    <testcase name=\"p1\" classname=\"properties\" time=\"0.300\"/>"
  , "    <testcase name=\"p2\" classname=\"properties\" time=\"0.200\"/>"
  , "    <testcase name=\"p3\" classname=\"properties\" time=\"0.200\">"
  , "      <failure type=\"counterexample\">boom</failure>"
  , "    </testcase>"
  , "    <testcase name=\"p4\" classname=\"properties\" time=\"0.150\"/>"
  , "    <testcase name=\"p5\" classname=\"properties\" time=\"0.150\"/>"
  , "  </testsuite>"
  , "</testsuites>"
  ]

parseSimple :: IO ()
parseSimple =
  case Tst.parseJUnit (BS8.pack (T.unpack junitDoc)) of
    Left err -> expectationFailure err
    Right ts -> do
      Tst.tsTotal    ts `shouldBe` 8
      Tst.tsPassed   ts `shouldBe` 7
      Tst.tsFailures ts `shouldBe` 1
      Tst.tsErrors   ts `shouldBe` 0
      Tst.tsSkipped  ts `shouldBe` 0
      length (Tst.tsSuites ts) `shouldBe` 2

cleanLine :: IO ()
cleanLine = do
  let s = Tst.TestSummary 10 10 0 0 0 1.5
        [ Tst.SuiteSummary "unit" 6 0 0 0 0.5
        , Tst.SuiteSummary "props" 4 0 0 0 1.0
        ]
      line = Tst.summaryToTestLine s
  ("10 tests passing" `T.isInfixOf` line) `shouldBe` True
  ("2 categories"     `T.isInfixOf` line) `shouldBe` True

failureLine :: IO ()
failureLine = do
  let s = Tst.TestSummary 10 7 2 1 0 1.5
        [ Tst.SuiteSummary "unit" 10 2 1 0 1.5
        ]
      line = Tst.summaryToTestLine s
  ("7 passing" `T.isInfixOf` line) `shouldBe` True
  ("2 failures" `T.isInfixOf` line) `shouldBe` True
