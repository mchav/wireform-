{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-unused-matches #-}

-- | Wire-compatible @google.rpc.Status@ generated via wireform-proto.
module Proto.Status where

import Proto.TH (loadProto)
import Proto.Google.Protobuf.Any qualified

$(loadProto "proto/official/status.proto")
