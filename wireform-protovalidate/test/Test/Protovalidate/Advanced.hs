{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the advanced protovalidate features: time-relative timestamp
-- rules, map key/value rules, enum @defined_only@, oneof @required@,
-- @well_known_regex@, and predefined constraints.
module Test.Protovalidate.Advanced (tests) where

import Data.List (sort)
import Data.Text (Text)
import Test.Syd

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

tests :: Spec
tests =
  describe
    "advanced rules" $ sequence_
    [ describe
        "time-relative timestamp rules (validateAt)" $ sequence_
        [ it "lt_now passes for past, fails for future" $ do
            validateAt now (msg [("t", VTimestamp (Timestamp 500 0))]) ltNowRules `shouldBe` []
            ids (validateAt now (msg [("t", VTimestamp (Timestamp 1500 0))]) ltNowRules)
              `shouldBe` ["timestamp.lt_now"]
        , it "gt_now passes for future" $
            validateAt now (msg [("t", VTimestamp (Timestamp 1500 0))]) gtNowRules `shouldBe` []
        , it "within tolerates the configured duration" $ do
            validateAt now (msg [("t", VTimestamp (Timestamp 1050 0))]) withinRules `shouldBe` []
            ids (validateAt now (msg [("t", VTimestamp (Timestamp 1200 0))]) withinRules)
              `shouldBe` ["timestamp.within"]
        ]
    , describe
        "map key / value rules" $ sequence_
        [ it "valid map" $
            validate (msg [("m", cmap [(VString "ab", VInt 1)])]) mapRules `shouldBe` []
        , it "bad key reports at m[key]" $
            map (\v -> (violationFieldPath v, violationConstraintId v))
              (validate (msg [("m", cmap [(VString "a", VInt 1)])]) mapRules)
              `shouldBe` [("m[a]", "string.min_len")]
        , it "bad value reports at m[key]" $
            map (\v -> (violationFieldPath v, violationConstraintId v))
              (validate (msg [("m", cmap [(VString "ab", VInt 0)])]) mapRules)
              `shouldBe` [("m[ab]", "int32.gt")]
        ]
    , describe
        "enum defined_only" $ sequence_
        [ it "defined value passes" $
            validate (msg [("e", VInt 1)]) enumRules `shouldBe` []
        , it "undefined value fails" $
            ids (validate (msg [("e", VInt 5)]) enumRules) `shouldBe` ["enum.defined_only"]
        ]
    , describe
        "oneof required" $ sequence_
        [ it "one member present passes" $
            validate (msg [("a", VInt 1)]) oneofRules `shouldBe` []
        , it "no member present fails" $
            ids (validate (msg []) oneofRules) `shouldBe` ["kind"]
        ]
    , describe
        "well_known_regex" $ sequence_
        [ it "valid HTTP header name" $
            validate (msg [("h", VString "Content-Type")]) wkrRules `shouldBe` []
        , it "invalid header name" $
            ids (validate (msg [("h", VString "bad header")]) wkrRules) `shouldBe` ["string.well_known_regex"]
        ]
    , describe
        "predefined constraints (rule binding)" $ sequence_
        [ it "satisfied" $
            validate (msg [("n", VInt 20)]) predefRules `shouldBe` []
        , it "violated" $
            ids (validate (msg [("n", VInt 5)]) predefRules) `shouldBe` ["n.min_rule"]
        ]
    , describe
        "schema extraction of map key/value rules" $ sequence_
        [ it "valid map from .proto rules" $
            validate (msg [("scores", cmap [(VString "ab", VInt 5)]), ("a", VString "x")]) extractedMapRules `shouldBe` []
        , it "bad key + value from .proto rules" $
            ids (validate (msg [("scores", cmap [(VString "x", VInt 0)]), ("a", VString "x")]) extractedMapRules)
              `shouldBe` sort ["string.min_len", "int32.gt"]
        , it "oneof required extracted from .proto" $ do
            validate (msg [("scores", cmap [(VString "ab", VInt 5)]), ("a", VString "x")]) extractedMapRules `shouldBe` []
            ids (validate (msg [("scores", cmap [(VString "ab", VInt 5)])]) extractedMapRules) `shouldBe` ["choice"]
        ]
    , describe
        "schema extraction of enum/regex/time bounds" $ sequence_
        [ it "enum.defined_only resolves declared values" $ do
            validate (msg [("c", VInt 1)]) (rulesFor "E" enumProto) `shouldBe` []
            ids (validate (msg [("c", VInt 5)]) (rulesFor "E" enumProto)) `shouldBe` ["enum.defined_only"]
        , it "well_known_regex resolves the header-name pattern" $ do
            validate (msg [("name", VString "Content-Type")]) (rulesFor "H" enumProto) `shouldBe` []
            ids (validate (msg [("name", VString "bad name")]) (rulesFor "H" enumProto))
              `shouldBe` ["string.well_known_regex"]
        , it "timestamp.gt / duration.lte message-literal bounds" $ do
            validate (msg [("t", VTimestamp (Timestamp 2000 0)), ("d", VDuration (Duration 5 0))]) (rulesFor "T" enumProto)
              `shouldBe` []
            ids (validate (msg [("t", VTimestamp (Timestamp 500 0)), ("d", VDuration (Duration 9 0))]) (rulesFor "T" enumProto))
              `shouldBe` sort ["timestamp.gt", "duration.lte"]
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
    rulesFor name src =
      case parseProtoRules src of
        Left e -> error (show e)
        Right rs -> maybe (error ("no message " <> show name)) id (lookup name rs)
    extractedMapRules = rulesFor "M" mapProto
    enumProto =
      "syntax = \"proto3\";\n\
      \package t;\n\
      \enum Color { RED = 0; GREEN = 1; BLUE = 2; }\n\
      \message E {\n\
      \  Color c = 1 [(buf.validate.field).enum.defined_only = true];\n\
      \}\n\
      \message H {\n\
      \  string name = 1 [(buf.validate.field).string.well_known_regex = KNOWN_REGEX_HTTP_HEADER_NAME];\n\
      \}\n\
      \message T {\n\
      \  google.protobuf.Timestamp t = 1 [(buf.validate.field).timestamp.gt = { seconds: 1000 }];\n\
      \  google.protobuf.Duration d = 2 [(buf.validate.field).duration.lte = { seconds: 5 }];\n\
      \}\n"
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
