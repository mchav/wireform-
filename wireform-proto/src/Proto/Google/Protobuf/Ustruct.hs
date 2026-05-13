{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.Ustruct@ well-known type.
--
-- Generated from @google/protobuf/struct.proto@ at compile time.
module Proto.Google.Protobuf.Ustruct where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/struct.proto")
