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
  , internName
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import qualified Data.Vector as V

type Header = (ByteString, ByteString)

-- Pre-built index for O(log n) static table lookups.
{-# NOINLINE staticNameValueIndex #-}
staticNameValueIndex :: Map (ByteString, ByteString) Int
staticNameValueIndex = Map.fromList
  [(V.unsafeIndex staticTable i, i + 1) | i <- [0 .. V.length staticTable - 1]]

{-# NOINLINE staticNameIndex #-}
staticNameIndex :: Map ByteString Int
staticNameIndex = Map.fromList
  [(fst (V.unsafeIndex staticTable i), i + 1) | i <- [0 .. V.length staticTable - 1]]

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

-- | Dynamic table implemented as a ring buffer for O(1) insert/evict.
-- HPACK inserts at the front (index 0) and evicts from the back.
data DynamicTable = DynamicTable
  { dtBuffer :: !(IORef (V.Vector Header))  -- ring storage
  , dtHead :: !(IORef Int)                  -- index of newest entry
  , dtCount :: !(IORef Int)                 -- number of active entries
  , dtCapacity :: !(IORef Int)              -- buffer capacity (slots)
  , dtSize :: !(IORef Int)                  -- current HPACK size (bytes)
  , dtMaxSize :: !(IORef Int)               -- max HPACK size
  }

ringInitialCapacity :: Int
ringInitialCapacity = 128

entrySize :: Header -> Int
entrySize (name, value) = BS.length name + BS.length value + 32

newDynamicTable :: Int -> IO DynamicTable
newDynamicTable maxSz = do
  buf <- newIORef (V.replicate ringInitialCapacity ("", ""))
  hd <- newIORef 0
  cnt <- newIORef 0
  cap <- newIORef ringInitialCapacity
  size <- newIORef 0
  maxSize <- newIORef maxSz
  pure DynamicTable
    { dtBuffer = buf
    , dtHead = hd
    , dtCount = cnt
    , dtCapacity = cap
    , dtSize = size
    , dtMaxSize = maxSize
    }

insertEntry :: DynamicTable -> Header -> IO ()
insertEntry dt entry = do
  let sz = entrySize entry
  maxSz <- readIORef (dtMaxSize dt)
  if sz > maxSz
    then do
      writeIORef (dtCount dt) 0
      writeIORef (dtSize dt) 0
    else do
      evict dt (maxSz - sz)
      capacity <- readIORef (dtCapacity dt)
      count <- readIORef (dtCount dt)
      -- Grow buffer if full
      if count >= capacity
        then growBuffer dt
        else pure ()
      capacity' <- readIORef (dtCapacity dt)
      hd <- readIORef (dtHead dt)
      -- Insert at position before current head (ring wraps)
      let newHead = (hd - 1 + capacity') `mod` capacity'
      buf <- readIORef (dtBuffer dt)
      let buf' = buf V.// [(newHead, entry)]
      writeIORef (dtBuffer dt) buf'
      writeIORef (dtHead dt) newHead
      modifyIORef' (dtCount dt) (+ 1)
      modifyIORef' (dtSize dt) (+ sz)

growBuffer :: DynamicTable -> IO ()
growBuffer dt = do
  buf <- readIORef (dtBuffer dt)
  hd <- readIORef (dtHead dt)
  cnt <- readIORef (dtCount dt)
  cap <- readIORef (dtCapacity dt)
  let newCap = cap * 2
      -- Linearize existing entries into new buffer
      newBuf = V.generate newCap $ \i ->
        if i < cnt
          then V.unsafeIndex buf ((hd + i) `mod` cap)
          else ("", "")
  writeIORef (dtBuffer dt) newBuf
  writeIORef (dtHead dt) 0
  writeIORef (dtCapacity dt) newCap

evict :: DynamicTable -> Int -> IO ()
evict dt targetSize = do
  curSize <- readIORef (dtSize dt)
  if curSize <= targetSize
    then pure ()
    else do
      count <- readIORef (dtCount dt)
      if count == 0
        then pure ()
        else do
          -- Evict from the tail (oldest entry)
          buf <- readIORef (dtBuffer dt)
          hd <- readIORef (dtHead dt)
          capacity <- readIORef (dtCapacity dt)
          let tailIdx = (hd + count - 1) `mod` capacity
              lastEntry = V.unsafeIndex buf tailIdx
          modifyIORef' (dtCount dt) (subtract 1)
          modifyIORef' (dtSize dt) (subtract (entrySize lastEntry))
          evict dt targetSize

{-# INLINE ringIndex #-}
ringIndex :: DynamicTable -> Int -> IO Header
ringIndex dt i = do
  buf <- readIORef (dtBuffer dt)
  hd <- readIORef (dtHead dt)
  capacity <- readIORef (dtCapacity dt)
  pure (V.unsafeIndex buf ((hd + i) `mod` capacity))

lookupEntry :: DynamicTable -> Int -> IO (Maybe Header)
lookupEntry dt idx
  | idx >= 1 && idx <= staticTableSize = pure (staticTableEntry idx)
  | otherwise = do
      let dynIdx = idx - staticTableSize - 1
      count <- readIORef (dtCount dt)
      if dynIdx >= 0 && dynIdx < count
        then Just <$> ringIndex dt dynIdx
        else pure Nothing

lookupName :: DynamicTable -> ByteString -> IO (Maybe Int)
lookupName dt name =
  case findStaticName name of
    Just idx -> pure (Just idx)
    Nothing -> do
      count <- readIORef (dtCount dt)
      let go i
            | i >= count = pure Nothing
            | otherwise = do
                entry <- ringIndex dt i
                if fst entry == name
                  then pure (Just (staticTableSize + i + 1))
                  else go (i + 1)
      go 0

lookupNameValue :: DynamicTable -> Header -> IO (Maybe (Int, Bool))
lookupNameValue dt (name, value) =
  case findStaticNameValue (name, value) of
    Just idx -> pure (Just (idx, True))
    Nothing -> do
      let nameHit = findStaticName name
      count <- readIORef (dtCount dt)
      let go i nameIdx
            | i >= count =
                case nameIdx of
                  Just ni -> pure (Just (ni, False))
                  Nothing -> pure Nothing
            | otherwise = do
                entry <- ringIndex dt i
                if entry == (name, value)
                  then pure (Just (staticTableSize + i + 1, True))
                  else if fst entry == name
                    then go (i + 1) (firstJust nameIdx (Just (staticTableSize + i + 1)))
                    else go (i + 1) nameIdx
      go 0 nameHit
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y = y

{-# INLINE findStaticName #-}
findStaticName :: ByteString -> Maybe Int
findStaticName name = Map.lookup name staticNameIndex

{-# INLINE findStaticNameValue #-}
findStaticNameValue :: Header -> Maybe Int
findStaticNameValue hdr = Map.lookup hdr staticNameValueIndex

tableSize :: DynamicTable -> IO Int
tableSize dt = readIORef (dtSize dt)

tableMaxSize :: DynamicTable -> IO Int
tableMaxSize dt = readIORef (dtMaxSize dt)

setMaxSize :: DynamicTable -> Int -> IO ()
setMaxSize dt newMax = do
  writeIORef (dtMaxSize dt) newMax
  evict dt newMax

-- | Intern a header name: if it matches a static table entry's name,
-- return the static table's ByteString (shared, not a recv buffer slice).
-- This allows the recv buffer memory to be reused/GC'd sooner.
{-# INLINE internName #-}
internName :: ByteString -> ByteString
internName name = case Map.lookup name staticNameIndex of
  Just idx -> fst (V.unsafeIndex staticTable (idx - 1))
  Nothing -> name
