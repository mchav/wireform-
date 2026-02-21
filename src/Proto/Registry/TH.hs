{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Template Haskell support for building 'MessageRegistry' values.
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- import Proto.Registry.TH
-- import Proto.Google.Protobuf.Timestamp (Timestamp)
-- import Proto.Google.Protobuf.Duration (Duration)
-- import Proto.Google.Protobuf.Empty (Empty)
--
-- myRegistry :: MessageRegistry
-- myRegistry = $(buildRegistry
--   [ [t| Timestamp |]
--   , [t| Duration |]
--   , [t| Empty |]
--   ])
-- @
module Proto.Registry.TH
  ( buildRegistry
  , registryFromList
  ) where

import Data.Proxy (Proxy(..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Proto.Registry (MessageRegistry, emptyRegistry, registerType)
import Proto.Message (IsMessage)

-- | Build a 'MessageRegistry' from a list of types, specified via
-- Template Haskell type quotations.
--
-- @
-- myReg :: MessageRegistry
-- myReg = $(buildRegistry
--   [ [t| Timestamp |]
--   , [t| Duration |]
--   ])
-- @
buildRegistry :: [Q Type] -> Q Exp
buildRegistry qtypes = do
  types <- sequence qtypes
  let regExpr = foldl addType [| id |] types
  [| $(regExpr) emptyRegistry |]
  where
    addType accQ ty =
      [| registerType (Proxy :: $(pure (AppT (ConT ''Proxy) ty))) . $(accQ) |]

-- | Build a 'MessageRegistry' at runtime from a list of registration
-- functions. Each generated module exports a @registerModuleTypes@
-- function that can be composed here.
--
-- @
-- import Proto.Google.Protobuf.Timestamp (registerModuleTypes)
-- import Proto.Google.Protobuf.Duration (registerModuleTypes)
--
-- myRegistry :: MessageRegistry
-- myRegistry = registryFromList [Timestamp.registerModuleTypes, Duration.registerModuleTypes]
-- @
registryFromList :: [MessageRegistry -> MessageRegistry] -> MessageRegistry
registryFromList fs = foldl (\acc f -> f acc) emptyRegistry fs
