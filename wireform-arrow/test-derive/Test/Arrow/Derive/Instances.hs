{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Arrow.Derive.Instances
  ( outcomeIsRejected
  ) where

import Language.Haskell.TH (recover)

import Arrow.Derive

import Test.Arrow.Derive.Types

deriveArrow ''Tag
deriveArrow ''Profile
deriveArrow ''WithTag
deriveArrow ''Event
deriveArrow ''Result

-- | Witness that 'deriveArrow' rejects sum types at splice time.
--
-- The splice is wrapped in 'recover': if @deriveArrow ''Outcome@
-- aborts (the expected outcome for a sum type), the recovery
-- branch returns 'True'; otherwise 'False'. The runtime test in
-- "Test.Arrow.Derive" then asserts the value is 'True'.
outcomeIsRejected :: Bool
outcomeIsRejected =
  $(recover [| True |]
      (do _ <- deriveArrow ''Outcome
          [| False |]))
