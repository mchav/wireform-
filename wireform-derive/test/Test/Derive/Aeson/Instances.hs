{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | TH splice site for the Aeson deriver. Lives in its own module so
that the @ANN@ pragmas in "Test.Derive.Aeson.Types" are visible to
'reifyAnnotations' (TH stage restriction).
-}
module Test.Derive.Aeson.Instances () where

import Test.Derive.Aeson.Types
import Wireform.Derive.Aeson


deriveJSON ''Address
deriveJSON ''UserId
deriveJSON ''Color
deriveJSON ''Shape
