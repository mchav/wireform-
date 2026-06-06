{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans -Wno-unused-matches #-}

-- | Wire-compatible @xds.data.orca.v3.OrcaLoadReport@ generated via wireform-proto.
module Proto.OrcaLoadReport where

import Proto.TH (loadProto)


$(loadProto "proto/official/orca_load_report.proto")
