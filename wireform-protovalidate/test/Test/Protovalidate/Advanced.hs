{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the advanced protovalidate features: time-relative timestamp
-- rules, map key/value rules, enum @defined_only@, oneof @required@,
-- @well_known_regex@, and predefined constraints.
module Test.Protovalidate.Advanced (tests) where

import Data.List (sort)
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit

import CEL (Duration (..), Timestamp (..), Value (..), celMapFromList)
import Protovalidate
import Protovalidate.Constraint (mkConstraint)

msg :: [(Text, Value)] -> Value
msg fs = VMap (celMapFromList [(VString k, v) | (k, v) <- fs])

cmap :: [(Value, Value)] -> Value
cmap = VMap . celMapFromList

ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId

mustC :: Text -> Text -> Text -> Constraint
mustC i m e = either (error . show) id (mkConstraint i m e)

tests :: TestTree
tests =
  testGroup
    "advanced rules"
    [ testGroup
        "time-relative timestamp rules (validateAt)"
        [ testCase "lt_now passes for past, fails for future" $ do
            validateAt now (msg [("t", VTimestamp (Timestamp 500 0))]) ltNowRules @?= []
            ids (validateAt now (msg [("t", VTimestamp (Timestamp 1500 0))]) ltNowRules)
              @?= ["timestamp.lt_now"]
        , testCase "gt_now passes for future" $
            validateAt now (msg [("t", VTimestamp (Timestamp 1500 0))]) gtNowRules @?= []
        , testCase "within tolerates the configured duration" $ do
            validateAt now (msg [("t", VTimestamp (Timestamp 1050 0))]) withinRules @?= []
            ids (validateAt now (msg [("t", VTimestamp (Timestamp 1200 0))]) withinRules)
              @?= ["timestamp.within"]
        ]
    , testGroup
        "map key / value rules"
        [ testCase "valid map" $
            validate (msg [("m", cmap [(VString "ab", VInt 1)])]) mapRules @?= []
        , testCase "bad key reports at m[key]" $
            map (\v -> (violationFieldPath v, violationConstraintId v))
              (validate (msg [("m", cmap [(VString "a", VInt 1)])]) mapRules)
              @?= [("m[a]", "string.min_len")]
        , testCase "bad value reports at m[key]" $
            map (\v -> (violationFieldPath v, violationConstraintId v))
              (validate (msg [("m", cmap [(VString "ab", VInt 0)])]) mapRules)
              @?= [("m[ab]", "int32.gt")]
        ]
    , testGroup
        "enum defined_only"
        [ testCase "defined value passes" $
            validate (msg [("e", VInt 1)]) enumRules @?= []
        , testCase "undefined value fails" $
            ids (validate (msg [("e", VInt 5)]) enumRules) @?= ["enum.defined_only"]
        ]
    , testGroup
        "oneof required"
        [ testCase "one member present passes" $
            validate (msg [("a", VInt 1)]) oneofRules @?= []
        , testCase "no member present fails" $
            ids (validate (msg []) oneofRules) @?= ["kind"]
        ]
    , testGroup
        "well_known_regex"
        [ testCase "valid HTTP header name" $
            validate (msg [("h", VString "Content-Type")]) wkrRules @?= []
        , testCase "invalid header name" $
            ids (validate (msg [("h", VString "bad header")]) wkrRules) @?= ["string.well_known_regex"]
        ]
    , testGroup
        "predefined constraints (rule binding)"
        [ testCase "satisfied" $
            validate (msg [("n", VInt 20)]) predefRules @?= []
        , testCase "violated" $
            ids (validate (msg [("n", VInt 5)]) predefRules) @?= ["n.min_rule"]
        ]
    , testGroup
        "schema extraction of map key/value rules"
        [ testCase "valid map from .proto rules" $
            validate (msg [("scores", cmap [(VString "ab", VInt 5)]), ("a", VString "x")]) extractedMapRules @?= []
        , testCase "bad key + value from .proto rules" $
            ids (validate (msg [("scores", cmap [(VString "x", VInt 0)]), ("a", VString "x")]) extractedMapRules)
              @?= sort ["string.min_len", "int32.gt"]
        , testCase "oneof required extracted from .proto" $ do
            validate (msg [("scores", cmap [(VString "ab", VInt 5)]), ("a", VString "x")]) extractedMapRules @?= []
            ids (validate (msg [("scores", cmap [(VString "ab", VInt 5)])]) extractedMapRules) @?= ["choice"]
        ]
    ]
  where
    now = Timestamp 1000 0
    ltNowRules = messageRules [("t", fieldRules KTimestamp [("lt_now", VBool True)])] []
    gtNowRules = messageRules [("t", fieldRules KTimestamp [("gt_now", VBool True)])] []
    withinRules = messageRules [("t", fieldRules KTimestamp [("within", VDuration (Duration 100 0))])] []
    mapRules =
      messageRules
        [("m", mapValues (fieldRules KInt32 [gtV (VInt 0)]) (mapKeys (fieldRules KString [minLen 2]) (fieldRules KMap [])))]
        []
    enumRules =
      messageRules [("e", emptyFieldRules {frKind = Just KEnum, frCustom = [definedOnly [0, 1, 2]]})] []
    oneofRules = messageRules [] [oneofRequired "kind" ["a", "b"]]
    wkrRules =
      messageRules [("h", emptyFieldRules {frKind = Just KString, frCustom = [wellKnownRegex 1 True]})] []
    predefRules =
      messageRules
        [("n", emptyFieldRules {frKind = Just KInt32, frPredefined = [predefined (mustC "n.min_rule" "too small" "this >= rule") (VInt 10)]})]
        []
    extractedMapRules =
      case parseProtoRules mapProto of
        Left e -> error (show e)
        Right rs -> maybe (error "no message M") id (lookup "M" rs)
    mapProto =
      "syntax = \"proto3\";\n\
      \package t;\n\
      \message M {\n\
      \  map<string, int32> scores = 1 [\n\
      \    (buf.validate.field).map.keys.string.min_len = 2,\n\
      \    (buf.validate.field).map.values.int32.gt = 0\n\
      \  ];\n\
      \  oneof choice {\n\
      \    option (buf.validate.oneof).required = true;\n\
      \    string a = 2;\n\
      \    string b = 3;\n\
      \  }\n\
      \}\n"
