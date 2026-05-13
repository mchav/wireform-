{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.Uany@ well-known type.
--
-- Generated from @google/protobuf/any.proto@ at compile time.
module Proto.Google.Protobuf.Uany where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/any.proto")
