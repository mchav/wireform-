{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.UsourceUcontext@ well-known type.
--
-- Generated from @google/protobuf/source_context.proto@ at compile time.
module Proto.Google.Protobuf.UsourceUcontext where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/source_context.proto")
