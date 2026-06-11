{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.CSV.Derive (tests) where

import CSV.Class qualified as C
import CSV.Derive qualified as CD
import Data.Vector qualified as V
import Test.CSV.Derive.Instances ()
import Test.CSV.Derive.Types
import Test.Syd


tests :: Spec
tests =
  describe "CSV.Derive" $
    sequence_
      [ it "row encoding drops skipped fields" $
          C.toCSVRow (Person "Alice" 30 "a@x" "private")
            `shouldBe` V.fromList ["Alice", "30", "a@x"]
      , it "round-trip applies defaults for skipped" $
          case C.fromCSVRow (V.fromList ["Bob", "25", "b@y"]) of
            Right p -> do
              personName p `shouldBe` "Bob"
              personAge p `shouldBe` 25
              personEmail p `shouldBe` "b@y"
              personNotes p `shouldBe` defaultNotes
            Left e -> expectationFailure e
      , it "csvHeaderFor uses renames, drops skipped" $
          $(CD.csvHeaderFor ''Person)
            `shouldBe` V.fromList ["name", "person_age", "email"]
      , it "missing cell yields Left" $
          case C.fromCSVRow (V.fromList ["Bob", "25"]) :: Either String Person of
            Left _ -> pure ()
            Right p -> expectationFailure ("unexpected " ++ show p)
      ]
