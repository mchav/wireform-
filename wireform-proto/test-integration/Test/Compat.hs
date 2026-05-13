module Test.Compat (compatTests) where

import Data.Text (Text)
import qualified Data.Text as T
import Test.Tasty
import Test.Tasty.HUnit

import Proto.AST
import Proto.Parser
import Proto.Compat

compatTests :: TestTree
compatTests = testGroup "Schema Compatibility"
  [ testGroup "BACKWARD compatibility"
      [ testCase "identical schemas are compatible" $ do
          let schema = parseOrDie simpleSchema
          assertCompatible (checkBackward schema schema)

      , testCase "adding optional field is backward compatible" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
          assertCompatible (checkBackward new old)

      , testCase "removing field without reserving breaks backward" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
          assertIncompatible (checkBackward new old)
          assertHasRule "FIELD_REMOVED_NOT_RESERVED" (checkBackward new old)

      , testCase "removing field with reservation is backward compatible" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; reserved 2; }"
                ]
          assertCompatible (checkBackward new old)

      , testCase "changing field type (incompatible wire) breaks backward" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { int32 value = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string value = 1; }"
                ]
          assertIncompatible (checkBackward new old)
          assertHasRule "FIELD_TYPE_CHANGED_INCOMPATIBLE" (checkBackward new old)

      , testCase "changing between wire-compatible types warns" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { int32 value = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { int64 value = 1; }"
                ]
          let result = checkBackward new old
          assertCompatible result
          assertHasRule "FIELD_TYPE_CHANGED_COMPATIBLE" result

      , testCase "adding required field breaks backward" $ do
          -- Simulate required by checking the rule directly
          let fd = FieldDef (Just Required) (FTScalar SInt32) "user_id" (FieldNumber 2) []
              oldMsg = MessageDef "Msg" [MEField (FieldDef Nothing (FTScalar SString) "name" (FieldNumber 1) [])]
              newMsg = MessageDef "Msg" [MEField (FieldDef Nothing (FTScalar SString) "name" (FieldNumber 1) []), MEField fd]
          assertIncompatible (checkMessageCompat BackwardDir "Msg" newMsg oldMsg)
          assertHasRule "REQUIRED_FIELD_ADDED" (checkMessageCompat BackwardDir "Msg" newMsg oldMsg)
      ]

  , testGroup "FORWARD compatibility"
      [ testCase "adding field is forward compatible (old ignores new)" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
          assertCompatible (checkForward new old)

      , testCase "removing field is forward compatible" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
          assertCompatible (checkForward new old)

      , testCase "type change (incompatible wire) breaks forward" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { int32 value = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { double value = 1; }"
                ]
          assertIncompatible (checkForward new old)
      ]

  , testGroup "FULL compatibility"
      [ testCase "adding optional field is full compatible" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
          assertCompatible (checkFull new old)

      , testCase "removing field without reserving breaks full" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
          assertIncompatible (checkFull new old)
      ]

  , testGroup "Enum compatibility"
      [ testCase "adding enum value warns for forward" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ACTIVE = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ACTIVE = 1; INACTIVE = 2; }"
                ]
          let result = checkForward new old
          assertCompatible result
          assertHasRule "ENUM_VALUE_ADDED" result

      , testCase "removing enum value breaks backward" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ACTIVE = 1; INACTIVE = 2; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ACTIVE = 1; }"
                ]
          assertIncompatible (checkBackward new old)
          assertHasRule "ENUM_VALUE_REMOVED" (checkBackward new old)

      , testCase "renaming enum value warns" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ACTIVE = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "enum Status { UNKNOWN = 0; ENABLED = 1; }"
                ]
          assertHasRule "ENUM_VALUE_RENAMED" (checkBackward new old)
      ]

  , testGroup "Transitive compatibility"
      [ testCase "checkCompatAll with multiple versions" $ do
          let v1 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              v2 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; }"
                ]
              v3 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 age = 2; bool active = 3; }"
                ]
          assertCompatible (checkCompatAll BackwardTransitive v3 [v2, v1])

      , testCase "transitive fails if any version is incompatible" $ do
          let v1 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 value = 2; }"
                ]
              v2 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; int32 value = 2; bool active = 3; }"
                ]
              v3 = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; string value = 2; bool active = 3; }"
                ]
          assertIncompatible (checkCompatAll BackwardTransitive v3 [v2, v1])
      ]

  , testGroup "NONE level"
      [ testCase "NONE always passes" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { double name = 1; }"
                ]
          assertCompatible (checkCompat None new old)
      ]

  , testGroup "Field name changes"
      [ testCase "renaming a field warns" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string name = 1; }"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message Msg { string full_name = 1; }"
                ]
          assertHasRule "FIELD_NAME_CHANGED" (checkBackward new old)
      ]

  , testGroup "Complex schema evolution"
      [ testCase "safe evolution: add fields, reserve removed" $ do
          let old = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message User {"
                , "  string name = 1;"
                , "  string email = 2;"
                , "  int32 age = 3;"
                , "}"
                ]
              new = parseOrDie $ T.unlines
                [ "syntax = \"proto3\";"
                , "message User {"
                , "  string name = 1;"
                , "  string email = 2;"
                , "  reserved 3;"
                , "  string phone = 4;"
                , "  bool active = 5;"
                , "}"
                ]
          assertCompatible (checkFull new old)
      ]
  ]

-- Helpers

simpleSchema :: Text
simpleSchema = T.unlines
  [ "syntax = \"proto3\";"
  , "message Msg {"
  , "  string name = 1;"
  , "  int32 value = 2;"
  , "}"
  ]

parseOrDie :: Text -> ProtoFile
parseOrDie src = case parseProtoFile "<test>" src of
  Left err -> error ("Parse failed: " <> show err)
  Right pf -> pf

assertCompatible :: CompatResult -> IO ()
assertCompatible result =
  assertBool ("Expected compatible, got errors: " <> show (compatErrors result))
    (isCompatible result)

assertIncompatible :: CompatResult -> IO ()
assertIncompatible result =
  assertBool "Expected incompatible, but was compatible"
    (not (isCompatible result))

assertHasRule :: Text -> CompatResult -> IO ()
assertHasRule rule result =
  assertBool ("Expected rule '" <> T.unpack rule <> "' in errors: " <> show (compatErrors result))
    (any (\e -> ceRule e == rule) (compatErrors result))
