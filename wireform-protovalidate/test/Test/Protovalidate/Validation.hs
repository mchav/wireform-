{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end validation tests: standard rules, custom CEL, and message-level
-- CEL, mirroring the protovalidate @User@ example.
module Test.Protovalidate.Validation (tests) where

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import CEL (Value (..), celMapFromList)
import Protovalidate

-- Build a message value from named fields.
msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])

mustCompile :: Text -> Text -> Text -> Constraint
mustCompile cid m src = either (error . show) id (mkConstraint cid m src)

ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId

userRules :: MessageRules
userRules =
  messageRules
    [ ("id", fieldRules KString [uuid])
    , ("age", fieldRules KUint32 [lteV (VUInt 150)])
    , ("email", fieldRules KString [email])
    , ("first_name", fieldRules KString [maxLen 64])
    , ("last_name", fieldRules KString [maxLen 64])
    ]
    [ mustCompile
        "first_name_requires_last_name"
        "last_name must be present if first_name is present"
        "!has(this.first_name) || has(this.last_name)"
    ]

tests :: TestTree
tests =
  testGroup
    "validation"
    [ testCase "valid user has no violations" $
        validate
          ( msg
              [ ("id", VString "12345678-1234-1234-1234-123456789abc")
              , ("age", VUInt 30)
              , ("email", VString "alice@example.com")
              , ("first_name", VString "Alice")
              , ("last_name", VString "Smith")
              ]
          )
          userRules
          @?= []
    , testCase "invalid user reports each failing rule" $
        ids
          ( validate
              ( msg
                  [ ("id", VString "not-a-uuid")
                  , ("age", VUInt 200)
                  , ("email", VString "not-an-email")
                  , ("first_name", VString "Alice")
                  ]
              )
              userRules
          )
          @?= sort ["string.uuid", "uint32.lte", "string.email", "first_name_requires_last_name"]
    , testCase "string length rules" $
        ids (validate (msg [("name", VString "ab")]) lenRules)
          @?= ["string.min_len"]
    , testCase "numeric comparison rules" $
        ids (validate (msg [("n", VInt 5)]) numRules)
          @?= sort ["int64.gt", "int64.lte"]
    , testCase "in / not_in" $
        ids (validate (msg [("color", VString "purple")]) setRules)
          @?= ["string.in"]
    , testCase "repeated unique + min_items" $
        ids (validate (msg [("tags", VList (V.fromList [VString "a", VString "a"]))]) repeatedRules)
          @?= sort ["repeated.min_items", "repeated.unique"]
    , testCase "required field absent" $
        ids (validate (msg []) requiredRules)
          @?= ["required"]
    , testCase "required field present" $
        validate (msg [("token", VString "abc")]) requiredRules @?= []
    , testCase "ignore_empty skips empty value" $
        validate (msg [("name", VString "")]) ignoreEmptyRules @?= []
    , testCase "nested message validation reports nested path" $
        map violationFieldPath (validate (msg [("profile", msg [("email", VString "bad")])]) nestedRules)
          @?= ["profile.email"]
    , testCase "field-level custom CEL" $
        ids (validate (msg [("n", VInt 7)]) customRules)
          @?= ["n.even"]
    , testCase "ip / hostname formats" $
        validate (msg [("host", VString "192.168.0.1"), ("name", VString "example.com")]) hostRules
          @?= []
    ]
  where
    lenRules = messageRules [("name", fieldRules KString [minLen 3, maxLen 10])] []
    numRules = messageRules [("n", fieldRules KInt64 [gtV (VInt 10), lteV (VInt 3)])] []
    setRules =
      messageRules
        [("color", fieldRules KString [inV [VString "red", VString "green", VString "blue"]])]
        []
    repeatedRules = messageRules [("tags", fieldRules KRepeated [minItems 3, unique])] []
    requiredRules = messageRules [("token", emptyFieldRules {frRequired = True})] []
    ignoreEmptyRules =
      messageRules [("name", (fieldRules KString [minLen 3]) {frIgnoreEmpty = True})] []
    nestedRules =
      messageRules
        [("profile", emptyFieldRules {frMessage = Just (messageRules [("email", fieldRules KString [email])] [])})]
        []
    customRules =
      messageRules
        [("n", emptyFieldRules {frCustom = [mustCompile "n.even" "n must be even" "this % 2 == 0"]})]
        []
    hostRules =
      messageRules
        [ ("host", fieldRules KString [ip])
        , ("name", fieldRules KString [hostname])
        ]
        []
