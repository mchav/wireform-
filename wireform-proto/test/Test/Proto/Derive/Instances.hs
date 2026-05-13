{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Splice site for the proto deriver. Splitting types from
splices works around TH stage restrictions: 'deriveProto' must
inspect the names defined in 'Test.Proto.Derive.Types', which
requires the types to be in a *previously compiled* module.
-}
module Test.Proto.Derive.Instances () where

import Proto.TH.Derive (deriveProto)
import Test.Proto.Derive.Types (Address, User)


deriveProto ''Address
deriveProto ''User
