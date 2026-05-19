module Network.HTTP2.HPACK.Table
  ( DynamicTable
  , newDynamicTable
  , insertEntry
  , lookupEntry
  , lookupName
  , lookupNameValue
  , tableSize
  , tableMaxSize
  , setMaxSize
  , staticTableSize
  , staticTable
  , staticTableEntry
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type Header = (ByteString, ByteString)

staticTable :: V.Vector Header
staticTable = V.fromList
  [ (":authority", "")
  , (":method", "GET")
  , (":method", "POST")
  , (":path", "/")
  , (":path", "/index.html")
  , (":scheme", "http")
  , (":scheme", "https")
  , (":status", "200")
  , (":status", "204")
  , (":status", "206")
  , (":status", "304")
  , (":status", "400")
  , (":status", "404")
  , (":status", "500")
  , ("accept-charset", "")
  , ("accept-encoding", "gzip, deflate")
  , ("accept-language", "")
  , ("accept-ranges", "")
  , ("accept", "")
  , ("access-control-allow-origin", "")
  , ("age", "")
  , ("allow", "")
  , ("authorization", "")
  , ("cache-control", "")
  , ("content-disposition", "")
  , ("content-encoding", "")
  , ("content-language", "")
  , ("content-length", "")
  , ("content-location", "")
  , ("content-range", "")
  , ("content-type", "")
  , ("cookie", "")
  , ("date", "")
  , ("etag", "")
  , ("expect", "")
  , ("expires", "")
  , ("from", "")
  , ("host", "")
  , ("if-match", "")
  , ("if-modified-since", "")
  , ("if-none-match", "")
  , ("if-range", "")
  , ("if-unmodified-since", "")
  , ("last-modified", "")
  , ("link", "")
  , ("location", "")
  , ("max-forwards", "")
  , ("proxy-authenticate", "")
  , ("proxy-authorization", "")
  , ("range", "")
  , ("referer", "")
  , ("refresh", "")
  , ("retry-after", "")
  , ("server", "")
  , ("set-cookie", "")
  , ("strict-transport-security", "")
  , ("transfer-encoding", "")
  , ("user-agent", "")
  , ("vary", "")
  , ("via", "")
  , ("www-authenticate", "")
  ]

staticTableSize :: Int
staticTableSize = V.length staticTable

staticTableEntry :: Int -> Maybe Header
staticTableEntry idx
  | idx >= 1 && idx <= staticTableSize = Just (V.unsafeIndex staticTable (idx - 1))
  | otherwise = Nothing

data DynamicTable = DynamicTable
  { dtEntries :: !(IORef (V.Vector Header))
  , dtSize :: !(IORef Int)
  , dtMaxSize :: !(IORef Int)
  , dtAbsoluteMaxSize :: !(IORef Int)
  }

entrySize :: Header -> Int
entrySize (name, value) = BS.length name + BS.length value + 32

newDynamicTable :: Int -> IO DynamicTable
newDynamicTable maxSz = do
  entries <- newIORef V.empty
  size <- newIORef 0
  maxSize <- newIORef maxSz
  absMax <- newIORef maxSz
  pure DynamicTable
    { dtEntries = entries
    , dtSize = size
    , dtMaxSize = maxSize
    , dtAbsoluteMaxSize = absMax
    }

insertEntry :: DynamicTable -> Header -> IO ()
insertEntry dt entry = do
  let sz = entrySize entry
  maxSz <- readIORef (dtMaxSize dt)
  if sz > maxSz
    then do
      writeIORef (dtEntries dt) V.empty
      writeIORef (dtSize dt) 0
    else do
      evict dt (maxSz - sz)
      entries <- readIORef (dtEntries dt)
      writeIORef (dtEntries dt) (V.cons entry entries)
      modifyIORef' (dtSize dt) (+ sz)

evict :: DynamicTable -> Int -> IO ()
evict dt targetSize = do
  entries <- readIORef (dtEntries dt)
  curSize <- readIORef (dtSize dt)
  go entries curSize
  where
    go entries curSize
      | curSize <= targetSize || V.null entries = do
          writeIORef (dtEntries dt) entries
          writeIORef (dtSize dt) curSize
      | otherwise =
          let lastEntry = V.unsafeLast entries
              entries' = V.unsafeInit entries
              curSize' = curSize - entrySize lastEntry
          in go entries' curSize'

lookupEntry :: DynamicTable -> Int -> IO (Maybe Header)
lookupEntry dt idx = do
  if idx >= 1 && idx <= staticTableSize
    then pure (staticTableEntry idx)
    else do
      let dynIdx = idx - staticTableSize - 1
      entries <- readIORef (dtEntries dt)
      if dynIdx >= 0 && dynIdx < V.length entries
        then pure (Just (V.unsafeIndex entries dynIdx))
        else pure Nothing

lookupName :: DynamicTable -> ByteString -> IO (Maybe Int)
lookupName dt name = do
  case findStaticName name of
    Just idx -> pure (Just idx)
    Nothing -> do
      entries <- readIORef (dtEntries dt)
      let go i
            | i >= V.length entries = pure Nothing
            | fst (V.unsafeIndex entries i) == name =
                pure (Just (staticTableSize + i + 1))
            | otherwise = go (i + 1)
      go 0

lookupNameValue :: DynamicTable -> Header -> IO (Maybe (Int, Bool))
lookupNameValue dt (name, value) = do
  case findStaticNameValue (name, value) of
    Just idx -> pure (Just (idx, True))
    Nothing -> do
      case findStaticName name of
        nameHit -> do
          entries <- readIORef (dtEntries dt)
          let go i nameIdx
                | i >= V.length entries =
                    case nameIdx of
                      Just ni -> pure (Just (ni, False))
                      Nothing -> pure Nothing
                | V.unsafeIndex entries i == (name, value) =
                    pure (Just (staticTableSize + i + 1, True))
                | fst (V.unsafeIndex entries i) == name =
                    go (i + 1) (nameIdx <|> Just (staticTableSize + i + 1))
                | otherwise = go (i + 1) nameIdx
          go 0 nameHit
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) (Just x) _ = Just x
    (<|>) Nothing y = y

findStaticName :: ByteString -> Maybe Int
findStaticName name = go 0
  where
    go i
      | i >= staticTableSize = Nothing
      | fst (V.unsafeIndex staticTable i) == name = Just (i + 1)
      | otherwise = go (i + 1)

findStaticNameValue :: Header -> Maybe Int
findStaticNameValue (name, value) = go 0
  where
    go i
      | i >= staticTableSize = Nothing
      | V.unsafeIndex staticTable i == (name, value) = Just (i + 1)
      | otherwise = go (i + 1)

tableSize :: DynamicTable -> IO Int
tableSize dt = readIORef (dtSize dt)

tableMaxSize :: DynamicTable -> IO Int
tableMaxSize dt = readIORef (dtMaxSize dt)

setMaxSize :: DynamicTable -> Int -> IO ()
setMaxSize dt newMax = do
  writeIORef (dtMaxSize dt) newMax
  evict dt newMax
