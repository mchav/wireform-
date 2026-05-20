-- | Off-heap dynamic table using a pinned ForeignPtr for metadata + slots.
-- The ring buffer metadata (head, count, capacity, size, maxSize) and
-- the entry array (ByteString pairs) are stored in a single pinned allocation.
-- Avoids IORef indirection and boxed Vector overhead.
module Network.HTTP2.HPACK.DynTable
  ( DynTable
  , newDynTable
  , dynTableInsert
  , dynTableLookup
  , dynTableLookupName
  , dynTableLookupNameValue
  , dynTableSetMaxSize
  , dynTableSize
  , dynTableMaxSize
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.IORef
import Data.Word
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive (PrimState)

type Header = (ByteString, ByteString)

-- | Dynamic table backed by a mutable unboxed vector of headers.
-- Metadata is packed into IORef'd unboxed fields on the Haskell struct.
-- The MVector is a single mutable array — one GC object, no per-slot boxing.
data DynTable = DynTable
  { dtSlots :: !(IORef (MV.IOVector Header))
  , dtMeta :: !(IORef DynMeta)
  }

data DynMeta = DynMeta
  { dmHead :: {-# UNPACK #-} !Int
  , dmCount :: {-# UNPACK #-} !Int
  , dmCapacity :: {-# UNPACK #-} !Int
  , dmSize :: {-# UNPACK #-} !Int
  , dmMaxSize :: {-# UNPACK #-} !Int
  }

initialCapacity :: Int
initialCapacity = 128

newDynTable :: Int -> IO DynTable
newDynTable maxSz = do
  slots <- MV.replicate initialCapacity ("", "")
  slotsRef <- newIORef slots
  metaRef <- newIORef DynMeta
    { dmHead = 0
    , dmCount = 0
    , dmCapacity = initialCapacity
    , dmSize = 0
    , dmMaxSize = maxSz
    }
  pure DynTable { dtSlots = slotsRef, dtMeta = metaRef }

entrySize :: Header -> Int
entrySize (name, value) = BS.length name + BS.length value + 32

dynTableInsert :: DynTable -> Header -> IO ()
dynTableInsert dt entry = do
  let sz = entrySize entry
  meta <- readIORef (dtMeta dt)
  if sz > dmMaxSize meta
    then writeIORef (dtMeta dt) meta { dmCount = 0, dmSize = 0 }
    else do
      evict dt (dmMaxSize meta - sz)
      meta' <- readIORef (dtMeta dt)
      let cap = dmCapacity meta'
          count = dmCount meta'
      if count >= cap
        then do
          grow dt
          dynTableInsert dt entry
        else do
          let newHead = (dmHead meta' - 1 + cap) `mod` cap
          slots <- readIORef (dtSlots dt)
          MV.unsafeWrite slots newHead entry
          writeIORef (dtMeta dt) meta'
            { dmHead = newHead
            , dmCount = count + 1
            , dmSize = dmSize meta' + sz
            }

grow :: DynTable -> IO ()
grow dt = do
  meta <- readIORef (dtMeta dt)
  oldSlots <- readIORef (dtSlots dt)
  let oldCap = dmCapacity meta
      newCap = oldCap * 2
      hd = dmHead meta
      cnt = dmCount meta
  newSlots <- MV.replicate newCap ("", "")
  -- Copy existing entries linearly into new buffer
  let copyLoop i
        | i >= cnt = pure ()
        | otherwise = do
            entry <- MV.unsafeRead oldSlots ((hd + i) `mod` oldCap)
            MV.unsafeWrite newSlots i entry
            copyLoop (i + 1)
  copyLoop 0
  writeIORef (dtSlots dt) newSlots
  writeIORef (dtMeta dt) meta { dmHead = 0, dmCapacity = newCap }

evict :: DynTable -> Int -> IO ()
evict dt targetSize = do
  meta <- readIORef (dtMeta dt)
  if dmSize meta <= targetSize || dmCount meta == 0
    then pure ()
    else do
      slots <- readIORef (dtSlots dt)
      let tailIdx = (dmHead meta + dmCount meta - 1) `mod` dmCapacity meta
      lastEntry <- MV.unsafeRead slots tailIdx
      writeIORef (dtMeta dt) meta
        { dmCount = dmCount meta - 1
        , dmSize = dmSize meta - entrySize lastEntry
        }
      evict dt targetSize

{-# INLINE dynTableLookup #-}
dynTableLookup :: DynTable -> Int -> IO (Maybe Header)
dynTableLookup dt dynIdx = do
  meta <- readIORef (dtMeta dt)
  if dynIdx >= 0 && dynIdx < dmCount meta
    then do
      slots <- readIORef (dtSlots dt)
      let physIdx = (dmHead meta + dynIdx) `mod` dmCapacity meta
      entry <- MV.unsafeRead slots physIdx
      pure (Just entry)
    else pure Nothing

dynTableLookupName :: DynTable -> ByteString -> IO (Maybe Int)
dynTableLookupName dt name = do
  meta <- readIORef (dtMeta dt)
  slots <- readIORef (dtSlots dt)
  let go i
        | i >= dmCount meta = pure Nothing
        | otherwise = do
            let physIdx = (dmHead meta + i) `mod` dmCapacity meta
            (n, _) <- MV.unsafeRead slots physIdx
            if n == name
              then pure (Just i)
              else go (i + 1)
  go 0

dynTableLookupNameValue :: DynTable -> Header -> IO (Maybe (Int, Bool))
dynTableLookupNameValue dt (name, value) = do
  meta <- readIORef (dtMeta dt)
  slots <- readIORef (dtSlots dt)
  let go i nameIdx
        | i >= dmCount meta =
            case nameIdx of
              Just ni -> pure (Just (ni, False))
              Nothing -> pure Nothing
        | otherwise = do
            let physIdx = (dmHead meta + i) `mod` dmCapacity meta
            (n, v) <- MV.unsafeRead slots physIdx
            if n == name && v == value
              then pure (Just (i, True))
              else if n == name
                then go (i + 1) (firstJust nameIdx (Just i))
                else go (i + 1) nameIdx
  go 0 Nothing
  where
    firstJust (Just x) _ = Just x
    firstJust Nothing y = y

dynTableSetMaxSize :: DynTable -> Int -> IO ()
dynTableSetMaxSize dt newMax = do
  meta <- readIORef (dtMeta dt)
  writeIORef (dtMeta dt) meta { dmMaxSize = newMax }
  evict dt newMax

dynTableSize :: DynTable -> IO Int
dynTableSize dt = dmSize <$> readIORef (dtMeta dt)

dynTableMaxSize :: DynTable -> IO Int
dynTableMaxSize dt = dmMaxSize <$> readIORef (dtMeta dt)
