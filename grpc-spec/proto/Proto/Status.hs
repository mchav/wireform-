{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-unused-matches #-}

-- | Wire-compatible @google.rpc.Status@ generated via wireform-proto.
module Proto.Status where

import Proto.Google.Protobuf.Any qualified
import Proto.TH (loadProto)


$(loadProto "proto/official/status.proto")
