{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Annotation-only instances for the auto-detection fixtures.
-- The only annotation the user needs to write is @tag N@; the
-- deriver figures out 'FKRepeated' / 'FKMap' / 'FKMaybe' / 'FKOneof'
-- from the field's Haskell type.
module Test.Proto.Derive.AutoInstances () where

import Proto.Derive (deriveProto)

import Test.Proto.Derive.AutoTypes
  ( AutoCard
  , AutoEnvelope
  , AutoPackedNums
  , AutoTagged
  )

deriveProto ''AutoCard
deriveProto ''AutoTagged
deriveProto ''AutoEnvelope
deriveProto ''AutoPackedNums
