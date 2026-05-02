{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | TH splice site for the Aeson deriver. Lives in its own module so
-- that the @ANN@ pragmas in "Test.Derive.Aeson.Types" are visible to
-- 'reifyAnnotations' (TH stage restriction).
module Test.Derive.Aeson.Instances () where

import Wireform.Derive.Aeson

import Test.Derive.Aeson.Types

deriveJSON ''Address
deriveJSON ''UserId
deriveJSON ''Color
deriveJSON ''Shape
