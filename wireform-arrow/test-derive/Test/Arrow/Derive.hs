{-# LANGUAGE OverloadedStrings #-}

module Test.Arrow.Derive (tests) where

import Arrow.Derive (hasTable)
import Arrow.Record (Table, decodeTable, encodeTable, tableSchema)
import Arrow.Types (Field (..), Schema (..))
import Data.Vector qualified as V
import Test.Arrow.Derive.Instances (outcomeIsRejected)
import Test.Arrow.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "Arrow.Derive" $
    sequence_
      [ recordTests
      , newtypeTests
      , maybeTests
      , coercedTests
      , spliceFailureTests
      ]


recordTests :: Spec
recordTests =
  describe "record" $
    sequence_
      [ it "schema honours rename + renameStyle and drops skipped column" $ do
          let sch = tableSchema (hasTable :: Table Profile)
              names = V.toList (V.map fieldName (arrowFields sch))
          names `shouldBe` ["name", "profile_age", "email"]
      , it "round-trip restores skipped fields from defaults" $ do
          let rows =
                V.fromList
                  [ Profile "Alice" 30 "alice@x" "secret"
                  , Profile "Bob" 25 "bob@x" "hidden"
                  ]
              (sch, cols) = encodeTable hasTable rows
          (V.length cols == 3) `shouldBe` True
          case decodeTable hasTable sch cols of
            Right rs -> do
              V.length rs `shouldBe` V.length rows
              let r0 = V.head rs
                  r1 = V.last rs
              profileName r0 `shouldBe` "Alice"
              profileAge r0 `shouldBe` 30
              profileEmail r0 `shouldBe` "alice@x"
              profilePrivate r0 `shouldBe` defaultPrivate
              profileName r1 `shouldBe` "Bob"
              profileAge r1 `shouldBe` 25
              profileEmail r1 `shouldBe` "bob@x"
              profilePrivate r1 `shouldBe` defaultPrivate
            Left e -> expectationFailure e
      ]


newtypeTests :: Spec
newtypeTests =
  describe "newtype-passthrough" $
    sequence_
      [ it "WithTag round-trips Tag column via passthrough instance" $ do
          let rows =
                V.fromList
                  [ WithTag (Tag 7) "alpha"
                  , WithTag (Tag 9) "beta"
                  ]
              (sch, cols) = encodeTable hasTable rows
              names = V.toList (V.map fieldName (arrowFields sch))
          -- Arrow's idiomatic name style is snake_case, applied
          -- automatically when no explicit rename is set.
          names `shouldBe` ["wt_id", "wt_name"]
          V.length cols `shouldBe` 2
          case decodeTable hasTable sch cols of
            Right rs -> rs `shouldBe` rows
            Left e -> expectationFailure e
      ]


maybeTests :: Spec
maybeTests =
  describe "Maybe-fields" $
    sequence_
      [ it "Maybe Text column reflected in schema as nullable" $ do
          let sch = tableSchema (hasTable :: Table Event)
              flds = V.toList (arrowFields sch)
          map fieldName flds `shouldBe` ["event_id", "event_note"]
          map fieldNullable flds `shouldBe` [False, True]
      , it "Maybe Text round-trips with Just / Nothing values" $ do
          let rows =
                V.fromList
                  [ Event 1 (Just "first")
                  , Event 2 Nothing
                  , Event 3 (Just "third")
                  ]
              (sch, cols) = encodeTable hasTable rows
          case decodeTable hasTable sch cols of
            Right rs -> rs `shouldBe` rows
            Left e -> expectationFailure e
      ]


coercedTests :: Spec
coercedTests =
  describe "coerced" $
    sequence_
      [ it "coerced field uses underlying-type schema and round-trips" $ do
          let sch = tableSchema (hasTable :: Table Result)
              names = V.toList (V.map fieldName (arrowFields sch))
          names `shouldBe` ["result_name", "result_score"]
          let rows =
                V.fromList
                  [ Result "alpha" (Score 100)
                  , Result "beta" (Score 200)
                  ]
              (sch', cols) = encodeTable hasTable rows
          case decodeTable hasTable sch' cols of
            Right rs -> rs `shouldBe` rows
            Left e -> expectationFailure e
      ]


spliceFailureTests :: Spec
spliceFailureTests =
  describe "splice-time-rejection" $
    sequence_
      [ it "deriveArrow ''Outcome (a sum type) is rejected at splice time" $
          (outcomeIsRejected) `shouldBe` True
      ]
