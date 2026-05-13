{-# LANGUAGE OverloadedStrings #-}
module Test.Test (tests) where

import qualified Data.ByteString.Char8 as BS8
import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import qualified Wireform.Stats.Test as Tst

tests :: TestTree
tests = testGroup "Test"
  [ testCase "parses a tasty-style JUnit XML doc" parseSimple
  , testCase "summaryToTestLine: clean run"        cleanLine
  , testCase "summaryToTestLine: with failures"    failureLine
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

parseSimple :: Assertion
parseSimple =
  case Tst.parseJUnit (BS8.pack (T.unpack junitDoc)) of
    Left err -> assertFailure err
    Right ts -> do
      Tst.tsTotal    ts @?= 8
      Tst.tsPassed   ts @?= 7
      Tst.tsFailures ts @?= 1
      Tst.tsErrors   ts @?= 0
      Tst.tsSkipped  ts @?= 0
      length (Tst.tsSuites ts) @?= 2

cleanLine :: Assertion
cleanLine = do
  let s = Tst.TestSummary 10 10 0 0 0 1.5
        [ Tst.SuiteSummary "unit" 6 0 0 0 0.5
        , Tst.SuiteSummary "props" 4 0 0 0 1.0
        ]
      line = Tst.summaryToTestLine s
  assertBool "mentions count"     ("10 tests passing" `T.isInfixOf` line)
  assertBool "mentions categories" ("2 categories"     `T.isInfixOf` line)

failureLine :: Assertion
failureLine = do
  let s = Tst.TestSummary 10 7 2 1 0 1.5
        [ Tst.SuiteSummary "unit" 10 2 1 0 1.5
        ]
      line = Tst.summaryToTestLine s
  assertBool "carries pass count" ("7 passing" `T.isInfixOf` line)
  assertBool "mentions failures"  ("2 failures" `T.isInfixOf` line)
