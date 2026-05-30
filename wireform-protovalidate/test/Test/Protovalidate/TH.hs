{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the compile-time validator generator: the @.proto@'s rules are
-- read at compile time and every predicate is compiled to Haskell.
module Test.Protovalidate.TH (tests) where

import Data.List (sort)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import CEL (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Protovalidate
import Protovalidate.TH (compileMessageValidator)
import Test.Protovalidate.UserProto (eventProto, userProto)

-- Generated at compile time; each rule's CEL is compiled to Haskell.
userValidator :: Value -> [Violation]
userValidator = $(compileMessageValidator userProto "User")

-- Exercises compiled timestamp/duration literal bounds and enum defined_only.
eventValidator :: Value -> [Violation]
eventValidator = $(compileMessageValidator eventProto "Event")

msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])

ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId

tests :: TestTree
tests =
  testGroup
    "compile-time validator (Protovalidate.TH)"
    [ testCase "valid message: no violations" $
        userValidator
          ( msg
              [ ("id", VString "abc")
              , ("age", VUInt 30)
              , ("email", VString "alice@example.com")
              ]
          )
          @?= []
    , testCase "invalid message: all compiled predicates fire" $
        ids
          ( userValidator
              ( msg
                  [ ("id", VString "")
                  , ("age", VUInt 200)
                  , ("email", VString "bad")
                  ]
              )
          )
          @?= sort ["string.min_len", "uint32.lte", "string.email", "id_required_with_age"]
    , testCase "compiled time bounds + enum defined_only: valid" $
        eventValidator
          ( msg
              [ ("at", VTimestamp (Timestamp 2000 0))
              , ("ttl", VDuration (Duration 30 0))
              , ("kind", VInt 1)
              ]
          )
          @?= []
    , testCase "compiled time bounds + enum defined_only: violations" $
        ids
          ( eventValidator
              ( msg
                  [ ("at", VTimestamp (Timestamp 500 0))
                  , ("ttl", VDuration (Duration 90 0))
                  , ("kind", VInt 9)
                  ]
              )
          )
          @?= sort ["timestamp.gt", "duration.lte", "enum.defined_only"]
    ]
