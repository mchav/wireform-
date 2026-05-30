{-# LANGUAGE OverloadedStrings #-}

-- | The @.proto@ source used by "Test.Protovalidate.TH". It lives in its own
-- module so it can be referenced from a Template Haskell splice (a top-level
-- binding cannot be spliced in the module that defines it).
module Test.Protovalidate.UserProto (userProto) where

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
