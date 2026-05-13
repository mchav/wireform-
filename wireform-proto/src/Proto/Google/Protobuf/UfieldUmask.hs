{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.UfieldUmask@ well-known type.
--
-- Generated from @google/protobuf/field_mask.proto@ at compile time.
module Proto.Google.Protobuf.UfieldUmask where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/field_mask.proto")
