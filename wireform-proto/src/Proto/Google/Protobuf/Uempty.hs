{-# LANGUAGE TemplateHaskell #-}

-- | @google.protobuf.Uempty@ well-known type.
--
-- Generated from @google/protobuf/empty.proto@ at compile time.
module Proto.Google.Protobuf.Uempty where

import Proto.TH (loadProtoWith, defaultLoadOpts, LoadOpts(..))

$(loadProtoWith defaultLoadOpts { loIncludeDirs = ["data/", "data/proto/", "."] }
    "data/proto/google/protobuf/empty.proto")
