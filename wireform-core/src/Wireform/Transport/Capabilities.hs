{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}

module Wireform.Transport.Capabilities
  ( SystemCapabilities (..)
  , IOUringFeatures (..)
  , NumaNodeInfo (..)
  , CoreTopology (..)
  , Placement (..)
  , detectCapabilities
  , recommendPlacement
  ) where

import Control.Exception (try, SomeException)
import Data.Char (isDigit, isSpace)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe, fromMaybe)
import Foreign.C.Types (CLong (..))
import System.IO (hGetContents, IOMode (..), withFile)
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
  hugeSizes <- getHugePageSizes
  cores <- getCoreCount
  isolated <- getIsolatedCores
  hasUring <- getHasIOUring
  uringFeats <- getIOUringFeatures
  numa <- getNumaNodes
  pure SystemCapabilities
    { capPageSize        = ps
    , capHugePageSizes   = hugeSizes
    , capHasIOUring      = hasUring
    , capIOUringFeatures = uringFeats
    , capNumaNodes       = numa
    , capIsolatedCores   = isolated
    , capCoreCount       = cores
    , capCoreTopology    = CoreTopology IntMap.empty IntMap.empty IntMap.empty
    }

-- | Recommend a placement for a given fd (NIC NUMA node detection).
recommendPlacement :: Maybe Int -> IO Placement
recommendPlacement _fdNumaNode = pure (Placement Nothing Nothing)

------------------------------------------------------------------------
-- Platform-specific detection
------------------------------------------------------------------------

foreign import ccall unsafe "hs_page_size"
  c_page_size :: IO CLong

getPageSize :: IO Int
getPageSize = fromIntegral <$> c_page_size

getCoreCount :: IO Int
getCoreCount = do
#if defined(linux_HOST_OS)
  r <- tryReadFile "/proc/cpuinfo"
  case r of
    Nothing -> pure 1
    Just s  -> pure $ max 1 (length (filter ("processor" `isPrefixOf`) (lines s)))
#else
  pure 1
#endif

getHugePageSizes :: IO [Int]
getHugePageSizes = do
#if defined(linux_HOST_OS)
  r <- tryReadFile "/sys/kernel/mm/hugepages"
  case r of
    Nothing -> pure []
    Just _  -> do
      r2 <- tryReadFile "/proc/meminfo"
      case r2 of
        Nothing -> pure []
        Just s  -> pure $ mapMaybe parseHugeLine (lines s)
#else
  pure []
#endif
  where
    parseHugeLine l
      | "Hugepagesize:" `isPrefixOf` l =
          let ws = words l
          in case ws of
               [_, n, "kB"] -> Just (read n * 1024)
               _            -> Nothing
      | otherwise = Nothing

getIsolatedCores :: IO [Int]
getIsolatedCores = do
#if defined(linux_HOST_OS)
  r <- tryReadFile "/sys/devices/system/cpu/isolated"
  case r of
    Nothing -> pure []
    Just s  -> pure (parseCpuList (filter (not . isSpace) s))
#else
  pure []
#endif

getHasIOUring :: IO Bool
getHasIOUring = do
#if defined(linux_HOST_OS)
  r <- tryReadFile "/proc/version"
  case r of
    Nothing -> pure False
    Just s  -> pure (detectKernelVersion s >= (5, 1))
#else
  pure False
#endif

getIOUringFeatures :: IO IOUringFeatures
getIOUringFeatures = do
#if defined(linux_HOST_OS)
  hasUring <- getHasIOUring
  if not hasUring
    then pure (IOUringFeatures False False False)
    else do
      ver <- kernelVersion
      pure IOUringFeatures
        { ioUringFeatureProvidedBuffers = ver >= (5, 19)
        , ioUringFeatureMultishotRecv   = ver >= (6, 0)
        , ioUringFeatureSQPoll          = ver >= (5, 1)
        }
#else
  pure (IOUringFeatures False False False)
#endif

getNumaNodes :: IO [NumaNodeInfo]
getNumaNodes = do
#if defined(linux_HOST_OS)
  r <- tryReadFile "/sys/devices/system/node/online"
  case r of
    Nothing -> pure []
    Just s  -> do
      let nodeIds = parseCpuList (filter (not . isSpace) s)
      mapM getNodeInfo nodeIds
#else
  pure []
#endif
  where
    getNodeInfo nid = do
      cpuList <- tryReadFile ("/sys/devices/system/node/node" <> show nid <> "/cpulist")
      memInfo <- tryReadFile ("/sys/devices/system/node/node" <> show nid <> "/meminfo")
      let cores = maybe [] (parseCpuList . filter (not . isSpace)) cpuList
          memMB = maybe 0 parseNodeMemMB memInfo
      pure (NumaNodeInfo nid cores memMB)
    parseNodeMemMB s =
      let ls = filter (\l -> "MemTotal:" `isPrefixOf` dropWhile (== ' ') l) (lines s)
      in case ls of
           (l:_) -> case filter (all isDigit) (words l) of
                       (n:_) -> read n `div` 1024
                       _     -> 0
           _ -> 0

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

tryReadFile :: FilePath -> IO (Maybe String)
tryReadFile path = do
  r <- try (withFile path ReadMode hGetContents)
  case r of
    Left (_ :: SomeException) -> pure Nothing
    Right s                   -> pure (Just s)

parseCpuList :: String -> [Int]
parseCpuList "" = []
parseCpuList s = concatMap parseRange (splitOn ',' s)
  where
    parseRange r = case break (== '-') r of
      (a, "")    -> if all isDigit a && not (null a) then [read a] else []
      (a, '-':b) -> if all isDigit a && all isDigit b && not (null a) && not (null b)
                     then [read a .. read b]
                     else []
      _          -> []
    splitOn _ "" = []
    splitOn c xs = case break (== c) xs of
      (a, "")     -> [a]
      (a, _:rest) -> a : splitOn c rest

detectKernelVersion :: String -> (Int, Int)
detectKernelVersion s =
  let ws = words s
      ver = fromMaybe "0.0" $ do
              idx <- findIndex (== "version") (fmap (fmap toLower) ws)
              ws `safeIndex` (idx + 1)
  in parseVersion ver
  where
    findIndex _ [] = Nothing
    findIndex p (x:xs)
      | p x       = Just 0
      | otherwise  = (+ 1) <$> findIndex p xs
    safeIndex [] _ = Nothing
    safeIndex (x:_) 0 = Just x
    safeIndex (_:xs) n = safeIndex xs (n - 1)
    toLower c
      | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
      | otherwise = c

parseVersion :: String -> (Int, Int)
parseVersion s = case break (== '.') s of
  (major, '.':rest) -> case break (== '.') rest of
    (minor, _) -> (readDef 0 major, readDef 0 minor)
  _ -> (0, 0)
  where
    readDef d xs = if all isDigit xs && not (null xs) then read xs else d

kernelVersion :: IO (Int, Int)
kernelVersion = do
  r <- tryReadFile "/proc/version"
  pure $ maybe (0, 0) detectKernelVersion r
