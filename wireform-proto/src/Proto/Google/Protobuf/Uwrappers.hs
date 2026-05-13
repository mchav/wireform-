{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.Uwrappers@ well-known type.
--
-- Generated from @google/protobuf/wrappers.proto@ at compile time.
module Proto.Google.Protobuf.Uwrappers where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/wrappers.proto")
