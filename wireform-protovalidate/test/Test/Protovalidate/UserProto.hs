{-# LANGUAGE OverloadedStrings #-}

-- | The @.proto@ source used by "Test.Protovalidate.TH". It lives in its own
-- module so it can be referenced from a Template Haskell splice (a top-level
-- binding cannot be spliced in the module that defines it).
module Test.Protovalidate.UserProto (userProto, eventProto) where

import Data.Text (Text)

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

-- | A proto exercising message-literal time bounds (@timestamp@/@duration@) and
-- enum @defined_only@ through the compile-time path.
eventProto :: Text
eventProto =
  "syntax = \"proto3\";\n\
  \package test.v1;\n\
  \enum Kind { K_UNSPECIFIED = 0; K_A = 1; K_B = 2; }\n\
  \message Event {\n\
  \  google.protobuf.Timestamp at = 1 [(buf.validate.field).timestamp.gt = { seconds: 1000 }];\n\
  \  google.protobuf.Duration ttl = 2 [(buf.validate.field).duration.lte = { seconds: 60 }];\n\
  \  Kind kind = 3 [(buf.validate.field).enum.defined_only = true];\n\
  \}\n"
