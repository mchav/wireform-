{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.Uduration@ well-known type.
--
-- Generated from @google/protobuf/duration.proto@ at compile time.
module Proto.Google.Protobuf.Uduration where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/duration.proto")
