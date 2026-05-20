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
import qualified Data.ByteString.Unsafe as BSU
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector as V

import Network.HTTP2.HPACK.DynTable

type Header = (ByteString, ByteString)

-- C-side static table lookup via FNV-1a hash + open addressing.
foreign import ccall unsafe "wireform_hpack_static_find_name"
  c_find_name :: Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "wireform_hpack_static_find_name_value"
  c_find_name_value :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO CInt

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

-- | Dynamic table backed by mutable IOVector (single GC object, O(1) ops).
type DynamicTable = DynTable

newDynamicTable :: Int -> IO DynamicTable
newDynamicTable = newDynTable

insertEntry :: DynamicTable -> Header -> IO ()
insertEntry = dynTableInsert

lookupEntry :: DynamicTable -> Int -> IO (Maybe Header)
lookupEntry dt idx
  | idx >= 1 && idx <= staticTableSize = pure (staticTableEntry idx)
  | otherwise = dynTableLookup dt (idx - staticTableSize - 1)

lookupName :: DynamicTable -> ByteString -> IO (Maybe Int)
lookupName dt name =
  case findStaticName name of
    Just idx -> pure (Just idx)
    Nothing -> do
      result <- dynTableLookupName dt name
      pure (fmap (\i -> staticTableSize + i + 1) result)

lookupNameValue :: DynamicTable -> Header -> IO (Maybe (Int, Bool))
lookupNameValue dt (name, value) =
  case findStaticNameValue (name, value) of
    Just idx -> pure (Just (idx, True))
    Nothing -> do
      dynResult <- dynTableLookupNameValue dt (name, value)
      case dynResult of
        Just (i, True) -> pure (Just (staticTableSize + i + 1, True))
        Just (i, False) ->
          case findStaticName name of
            Just ni -> pure (Just (ni, False))
            Nothing -> pure (Just (staticTableSize + i + 1, False))
        Nothing ->
          case findStaticName name of
            Just ni -> pure (Just (ni, False))
            Nothing -> pure Nothing

tableSize :: DynamicTable -> IO Int
tableSize = dynTableSize

tableMaxSize :: DynamicTable -> IO Int
tableMaxSize = dynTableMaxSize

setMaxSize :: DynamicTable -> Int -> IO ()
setMaxSize = dynTableSetMaxSize

{-# INLINE findStaticName #-}
findStaticName :: ByteString -> Maybe Int
findStaticName name = unsafePerformIO $
  BSU.unsafeUseAsCStringLen name $ \(ptr, len) -> do
    result <- c_find_name (castPtr ptr) (fromIntegral len)
    pure (if result == 0 then Nothing else Just (fromIntegral result))

{-# INLINE findStaticNameValue #-}
findStaticNameValue :: Header -> Maybe Int
findStaticNameValue (name, value) = unsafePerformIO $
  BSU.unsafeUseAsCStringLen name $ \(nPtr, nLen) ->
    BSU.unsafeUseAsCStringLen value $ \(vPtr, vLen) -> do
      result <- c_find_name_value
        (castPtr nPtr) (fromIntegral nLen)
        (castPtr vPtr) (fromIntegral vLen)
      pure (if result == 0 then Nothing else Just (fromIntegral result))

{-# INLINE internName #-}
internName :: ByteString -> ByteString
internName name = case findStaticName name of
  Just idx -> fst (V.unsafeIndex staticTable (idx - 1))
  Nothing -> name
