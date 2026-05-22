{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Wireform.Transport.Capabilities
  ( SystemCapabilities (..)
  , IOUringFeatures (..)
  , NumaNodeInfo (..)
  , CoreTopology (..)
  , Placement (..)
  , detectCapabilities
  , recommendPlacement
  ) where

import qualified Control.Exception as CE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Data.Word (Word8)
import Foreign.C.Types (CLong (..))
import System.IO.Unsafe (unsafePerformIO)

import Wireform.Parser
import Wireform.Parser.Driver (parseByteString)

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

recommendPlacement :: Maybe Int -> IO Placement
recommendPlacement _fdNumaNode = pure (Placement Nothing Nothing)

------------------------------------------------------------------------
-- Parsers for /proc and /sys files (using our own parser)
------------------------------------------------------------------------

type P = Parser ()

-- | Parse a CPU list like "0-3,5,7-9" into [0,1,2,3,5,7,8,9].
pCpuList :: P [Int]
pCpuList = do
  first <- pRange
  rest  <- many (word8 0x2C *> pRange)  -- ','
  pure (concat (first : rest))
  where
    pRange :: P [Int]
    pRange = do
      lo <- anyAsciiDecimalInt
      withOption (word8 0x2D *> anyAsciiDecimalInt) -- '-'
        (\hi -> pure [lo..hi])
        (pure [lo])

-- | Parse a decimal integer, skipping leading whitespace.
pDecimal :: P Int
pDecimal = skipWs *> anyAsciiDecimalInt

-- | Skip ASCII whitespace.
skipWs :: P ()
skipWs = skipMany (satisfyAscii (\c -> c == ' ' || c == '\t' || c == '\n' || c == '\r'))

-- | Skip to after a keyword, then parse what follows.
pAfterKeyword :: ByteString -> P a -> P a
pAfterKeyword kw p = go
  where
    go = (byteString kw *> p) <|> (anyWord8 *> go)

-- | Count lines starting with a given prefix.
pCountPrefix :: ByteString -> P Int
pCountPrefix pfx = go 0
  where
    go !n = (byteString pfx *> pSkipLine *> go (n + 1))
        <|> (pSkipLine *> go n)  -- pSkipLine fails at EOF, terminating the loop
        <|> pure n

-- | Skip bytes until newline (inclusive). Fails at EOF.
pSkipLine :: P ()
pSkipLine = go
  where
    go = withAnyWord8 \w ->
      if w == 0x0A then pure () else go

-- | Parse "Linux version X.Y.Z ..." from /proc/version.
pKernelVersion :: P (Int, Int)
pKernelVersion = pAfterKeyword "version " $ do
  major <- anyAsciiDecimalInt
  word8 0x2E  -- '.'
  minor <- anyAsciiDecimalInt
  pure (major, minor)

-- | Parse "Hugepagesize:    2048 kB" lines from /proc/meminfo.
pHugePageSize :: P [Int]
pHugePageSize = go []
  where
    go !acc =
      (do byteString "Hugepagesize:"
          skipWs
          n <- anyAsciiDecimalInt
          skipWs
          byteString "kB"
          pSkipLine
          go (n * 1024 : acc))
      <|> (pSkipLine *> go acc)
      <|> pure (reverse acc)

-- | Parse "MemTotal:       12345 kB" from a NUMA node's meminfo.
pNodeMemMB :: P Int
pNodeMemMB = go
  where
    go = (do byteString "MemTotal:"
             skipWs
             n <- anyAsciiDecimalInt
             pure (n `div` 1024))
     <|> (pSkipLine *> go)
     <|> pure 0

------------------------------------------------------------------------
-- File reading + parsing
------------------------------------------------------------------------

tryReadFileBS :: FilePath -> IO (Maybe ByteString)
tryReadFileBS path = do
  r <- CE.try @CE.SomeException (BS.readFile path)
  case r of
    Left _  -> pure Nothing
    Right s -> pure (Just s)

runP :: P a -> ByteString -> Maybe a
runP p bs = case parseByteString p bs of
  Right a -> Just a
  Left _  -> Nothing

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
  r <- tryReadFileBS "/proc/cpuinfo"
  case r of
    Nothing -> pure 1
    Just bs -> pure $ max 1 (maybe 1 id (runP (pCountPrefix "processor") bs))
#else
  pure 1
#endif

getHugePageSizes :: IO [Int]
getHugePageSizes = do
#if defined(linux_HOST_OS)
  r <- tryReadFileBS "/proc/meminfo"
  case r of
    Nothing -> pure []
    Just bs -> pure $ maybe [] id (runP pHugePageSize bs)
#else
  pure []
#endif

getIsolatedCores :: IO [Int]
getIsolatedCores = do
#if defined(linux_HOST_OS)
  r <- tryReadFileBS "/sys/devices/system/cpu/isolated"
  case r of
    Nothing -> pure []
    Just bs -> pure $ maybe [] id (runP (skipWs *> pCpuList) (BS.filter (/= 0x0A) bs))
#else
  pure []
#endif

getHasIOUring :: IO Bool
getHasIOUring = do
#if defined(linux_HOST_OS)
  ver <- kernelVersion
  pure (ver >= (5, 1))
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
  r <- tryReadFileBS "/sys/devices/system/node/online"
  case r of
    Nothing -> pure []
    Just bs -> do
      let nodeIds = maybe [] id (runP (skipWs *> pCpuList) (BS.filter (/= 0x0A) bs))
      mapM getNodeInfo nodeIds
#else
  pure []
#endif
  where
    getNodeInfo nid = do
      cpuList <- tryReadFileBS ("/sys/devices/system/node/node" <> bsShow nid <> "/cpulist")
      memInfo <- tryReadFileBS ("/sys/devices/system/node/node" <> bsShow nid <> "/meminfo")
      let cores = case cpuList of
            Nothing -> []
            Just bs -> maybe [] id (runP pCpuList (BS.filter (/= 0x0A) bs))
          memMB = case memInfo of
            Nothing -> 0
            Just bs -> maybe 0 id (runP pNodeMemMB bs)
      pure (NumaNodeInfo nid cores memMB)

    bsShow :: Int -> FilePath
    bsShow = show

kernelVersion :: IO (Int, Int)
kernelVersion = do
#if defined(linux_HOST_OS)
  r <- tryReadFileBS "/proc/version"
  pure $ case r of
    Nothing -> (0, 0)
    Just bs -> maybe (0, 0) id (runP pKernelVersion bs)
#else
  pure (0, 0)
#endif
