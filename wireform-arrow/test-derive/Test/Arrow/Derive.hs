{-# LANGUAGE OverloadedStrings #-}

module Test.Arrow.Derive (tests) where

import qualified Data.Vector as V
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Arrow.Derive (hasTable)
import Arrow.Record (Table, decodeTable, encodeTable, tableSchema)
import Arrow.Types (Field (..), Schema (..))

import Test.Arrow.Derive.Instances (outcomeIsRejected)
import Test.Arrow.Derive.Types

tests :: TestTree
tests = testGroup "Arrow.Derive"
  [ recordTests
  , newtypeTests
  , maybeTests
  , coercedTests
  , spliceFailureTests
  ]

recordTests :: TestTree
recordTests = testGroup "record"
  [ testCase "schema honours rename + renameStyle and drops skipped column" $ do
      let sch = tableSchema (hasTable :: Table Profile)
          names = V.toList (V.map fieldName (arrowFields sch))
      names @?= ["name", "profile_age", "email"]

  , testCase "round-trip restores skipped fields from defaults" $ do
      let rows = V.fromList
            [ Profile "Alice" 30 "alice@x" "secret"
            , Profile "Bob"   25 "bob@x"   "hidden"
            ]
          (sch, cols) = encodeTable hasTable rows
      assertBool "three columns emitted"
        (V.length cols == 3)
      case decodeTable hasTable sch cols of
        Right rs -> do
          V.length rs @?= V.length rows
          let r0 = V.head rs
              r1 = V.last rs
          profileName    r0 @?= "Alice"
          profileAge     r0 @?= 30
          profileEmail   r0 @?= "alice@x"
          profilePrivate r0 @?= defaultPrivate
          profileName    r1 @?= "Bob"
          profileAge     r1 @?= 25
          profileEmail   r1 @?= "bob@x"
          profilePrivate r1 @?= defaultPrivate
        Left e -> fail e
  ]

newtypeTests :: TestTree
newtypeTests = testGroup "newtype-passthrough"
  [ testCase "WithTag round-trips Tag column via passthrough instance" $ do
      let rows = V.fromList
            [ WithTag (Tag 7) "alpha"
            , WithTag (Tag 9) "beta"
            ]
          (sch, cols) = encodeTable hasTable rows
          names       = V.toList (V.map fieldName (arrowFields sch))
      -- Arrow's idiomatic name style is snake_case, applied
      -- automatically when no explicit rename is set.
      names @?= ["wt_id", "wt_name"]
      V.length cols @?= 2
      case decodeTable hasTable sch cols of
        Right rs -> rs @?= rows
        Left  e  -> fail e
  ]

maybeTests :: TestTree
maybeTests = testGroup "Maybe-fields"
  [ testCase "Maybe Text column reflected in schema as nullable" $ do
      let sch = tableSchema (hasTable :: Table Event)
          flds = V.toList (arrowFields sch)
      map fieldName     flds @?= ["event_id", "event_note"]
      map fieldNullable flds @?= [False, True]

  , testCase "Maybe Text round-trips with Just / Nothing values" $ do
      let rows = V.fromList
            [ Event 1 (Just "first")
            , Event 2 Nothing
            , Event 3 (Just "third")
            ]
          (sch, cols) = encodeTable hasTable rows
      case decodeTable hasTable sch cols of
        Right rs -> rs @?= rows
        Left  e  -> fail e
  ]

coercedTests :: TestTree
coercedTests = testGroup "coerced"
  [ testCase "coerced field uses underlying-type schema and round-trips" $ do
      let sch = tableSchema (hasTable :: Table Result)
          names = V.toList (V.map fieldName (arrowFields sch))
      names @?= ["result_name", "result_score"]
      let rows = V.fromList
            [ Result "alpha" (Score 100)
            , Result "beta"  (Score 200)
            ]
          (sch', cols) = encodeTable hasTable rows
      case decodeTable hasTable sch' cols of
        Right rs -> rs @?= rows
        Left  e  -> fail e
  ]

spliceFailureTests :: TestTree
spliceFailureTests = testGroup "splice-time-rejection"
  [ testCase "deriveArrow ''Outcome (a sum type) is rejected at splice time" $
      assertBool
        "outcomeIsRejected was False — the splice unexpectedly succeeded"
        outcomeIsRejected
  ]
