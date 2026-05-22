{-# LANGUAGE CPP #-}

module Wireform.Transport.Capabilities
  ( SystemCapabilities (..)
  , IOUringFeatures (..)
  , NumaNodeInfo (..)
  , CoreTopology (..)
  , Placement (..)
  , detectCapabilities
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Foreign.C.Types (CLong (..))
import System.IO.Unsafe (unsafePerformIO)

data SystemCapabilities = SystemCapabilities
  { capPageSize        :: !Int
  , capHugePageSizes   :: ![Int]
  , capHasIOUring      :: !Bool
  , capIOUringFeatures :: !IOUringFeatures
  , capNumaNodes       :: ![NumaNodeInfo]
  , capIsolatedCores   :: ![Int]
  , capCoreCount       :: !Int
  , capCoreTopology    :: !CoreTopology
  } deriving stock (Show)

data IOUringFeatures = IOUringFeatures
  { ioUringFeatureProvidedBuffers :: !Bool
  , ioUringFeatureMultishotRecv   :: !Bool
  , ioUringFeatureSQPoll          :: !Bool
  } deriving stock (Show)

data NumaNodeInfo = NumaNodeInfo
  { numaNodeId       :: !Int
  , numaNodeCores    :: ![Int]
  , numaNodeMemoryMB :: !Int
  } deriving stock (Show)

data CoreTopology = CoreTopology
  { topoCoreToPackage :: !(IntMap Int)
  , topoCoreToL3      :: !(IntMap Int)
  , topoSiblings      :: !(IntMap [Int])
  } deriving stock (Show)

data Placement = Placement
  { placementNumaNode :: !(Maybe Int)
  , placementCore     :: !(Maybe Int)
  } deriving stock (Show)

{-# NOINLINE cachedCapabilities #-}
cachedCapabilities :: IORef (Maybe SystemCapabilities)
cachedCapabilities = unsafePerformIO (newIORef Nothing)

detectCapabilities :: IO SystemCapabilities
detectCapabilities = do
  cached <- readIORef cachedCapabilities
  case cached of
    Just c  -> pure c
    Nothing -> do
      c <- detectCapabilitiesUncached
      writeIORef cachedCapabilities (Just c)
      pure c

detectCapabilitiesUncached :: IO SystemCapabilities
detectCapabilitiesUncached = do
  ps <- getPageSize
  pure SystemCapabilities
    { capPageSize        = ps
    , capHugePageSizes   = []
    , capHasIOUring      = False
    , capIOUringFeatures = IOUringFeatures False False False
    , capNumaNodes       = []
    , capIsolatedCores   = []
    , capCoreCount       = 1
    , capCoreTopology    = CoreTopology IntMap.empty IntMap.empty IntMap.empty
    }

getPageSize :: IO Int
getPageSize = do
  ps <- c_page_size
  pure (fromIntegral ps)

foreign import ccall unsafe "hs_page_size"
  c_page_size :: IO CLong
