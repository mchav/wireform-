{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.CapnProto.Derive.Instances () where

import CapnProto.Derive
import Test.CapnProto.Derive.Types


deriveCapnProto ''Position
deriveCapnProto ''Blob
deriveCapnProto ''Tag
deriveCapnProto ''User
deriveCapnProto ''Color
deriveCapnProto ''Profile
