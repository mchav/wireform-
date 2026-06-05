{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for reading @buf.validate@ rules from a parsed @.proto@ and for the
-- compile-once, typed (non-dynamic) validation path.
module Test.Protovalidate.Schema (tests) where

import Data.List (sort)
import Data.Text (Text)
import Data.Word (Word32)
import GHC.Generics (Generic)
import Test.Syd

import Protovalidate

-- A generated-style message record. 'ToCel' is derived generically, so
-- validating it never builds a dynamic message.
data User = User
  { id :: Text
  , age :: Word32
  , email :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToCel)

userProto :: Text
userProto =
  "syntax = \"proto3\";\n\
  \package test.v1;\n\
  \message User {\n\
  \  string id = 1 [(buf.validate.field).string.min_len = 2];\n\
  \  uint32 age = 2 [(buf.validate.field).uint32.lte = 150];\n\
  \  string email = 3 [(buf.validate.field).string.email = true];\n\
  \  option (buf.validate.message).cel = {\n\
  \    id: \"id_required_with_age\"\n\
  \    message: \"id must be set when age is set\"\n\
  \    expression: \"this.age == 0u || this.id != ''\"\n\
  \  };\n\
  \}\n"

userRules :: MessageRules
userRules =
  case parseProtoRules userProto of
    Left err -> error ("parse failed: " <> show err)
    Right rs -> case lookup "User" rs of
      Just mr -> mr
      Nothing -> error "no rules extracted for User"

ids :: [Violation] -> [Text]
ids = sort . map violationConstraintId

tests :: Spec
tests =
  describe
    "schema + typed validation" $ sequence_
    [ it "rules are extracted from .proto annotations" $
        (not (null (mrFields userRules))) `shouldBe` True
    , it "valid typed message passes (no dynamic conversion)" $
        validateValue validator (User "ab" 30 "alice@example.com") `shouldBe` []
    , it "invalid typed message reports each rule + message CEL" $
        ids (validateValue validator (User "" 200 "bad"))
          `shouldBe` sort ["string.min_len", "uint32.lte", "string.email", "id_required_with_age"]
    , it "compiled validator is reusable across messages" $
        map (null . validateValue validator)
          [ User "ab" 1 "a@b.co"
          , User "" 1 "a@b.co"
          ]
          `shouldBe` [True, False]
    ]
  where
    validator = compileValidator userRules
