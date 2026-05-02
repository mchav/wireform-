{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Bare records mirroring "Test.Proto.Derive.RegressionInstances"\'s
-- @loadProto@-generated types. The byte-equivalence regression
-- encodes the same logical data through both code paths and asserts
-- identical wire output.
module Test.Proto.Derive.RegressionTypes
  ( BridgeRegItem (..)
  , BridgeRegInventory (..)
  ) where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Vector as V
import GHC.Generics (Generic)

data BridgeRegItem = BridgeRegItem
  { brName  :: !Text
  , brCount :: !Int32
  } deriving stock (Show, Eq, Generic)

data BridgeRegInventory = BridgeRegInventory
  { briName  :: !Text
  , briItems :: !(V.Vector BridgeRegItem)
  } deriving stock (Show, Eq, Generic)
