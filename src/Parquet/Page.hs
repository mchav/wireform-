{-# LANGUAGE BangPatterns #-}
-- | Parquet page headers (Thrift compact protocol).
--
-- See @parquet.thrift@ (@PageHeader@, @DataPageHeader@, @DictionaryPageHeader@,
-- @DataPageHeaderV2@).
module Parquet.Page
  ( PageHeader (..)
  , DataPageHeader (..)
  , DictionaryPageHeader (..)
  , DataPageHeaderV2 (..)
  , readPageHeaderAt
  , pageTypeDataPage
  , pageTypeDictionaryPage
  , pageTypeDataPageV2
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32)
import qualified Data.Vector as V

import qualified Thrift.Value as TV
import Thrift.Decode (decodeCompactFrom)

-- | Thrift @PageType@: @DATA_PAGE@.
pageTypeDataPage :: Int32
pageTypeDataPage = 0

-- | Thrift @PageType@: @DICTIONARY_PAGE@.
pageTypeDictionaryPage :: Int32
pageTypeDictionaryPage = 2

-- | Thrift @PageType@: @DATA_PAGE_V2@.
pageTypeDataPageV2 :: Int32
pageTypeDataPageV2 = 3

data PageHeader = PageHeader
  { phType                  :: !Int32
  , phUncompressedPageSize  :: !(Maybe Int32)
  , phCompressedPageSize    :: !(Maybe Int32)
  , phDataPage              :: !(Maybe DataPageHeader)
  , phDictionaryPage        :: !(Maybe DictionaryPageHeader)
  , phDataPageV2            :: !(Maybe DataPageHeaderV2)
  } deriving stock (Show, Eq)

-- | Nested @DataPageHeader@ (field 5 of @PageHeader@).
data DataPageHeader = DataPageHeader
  { dphNumValues :: !Int32
  , dphEncoding  :: !Int32
  } deriving stock (Show, Eq)

-- | Nested @DictionaryPageHeader@ (field 7 of @PageHeader@).
data DictionaryPageHeader = DictionaryPageHeader
  { dictNumValues :: !Int32
  , dictEncoding  :: !Int32
  } deriving stock (Show, Eq)

-- | Nested @DataPageHeaderV2@ (field 8 of @PageHeader@).
-- Parquet Thrift field IDs 1–7 of the @DataPageHeaderV2@ struct.
data DataPageHeaderV2 = DataPageHeaderV2
  { dph2NumValues    :: !Int32
  , dph2NumNulls     :: !Int32
  , dph2NumRows      :: !Int32
  , dph2Encoding     :: !Int32
  , dph2DefLevelsLen :: !Int32
  , dph2RepLevelsLen :: !Int32
  , dph2IsCompressed :: !Bool
  } deriving stock (Show, Eq)

readPageHeaderAt :: ByteString -> Int -> Either String (PageHeader, Int)
readPageHeaderAt bs off = do
  (v, endOff) <- decodeCompactFrom bs off
  ph <- pageHeaderFromThrift v
  pure (ph, endOff)

pageHeaderFromThrift :: TV.Value -> Either String PageHeader
pageHeaderFromThrift (TV.Struct fields) = do
  let fm = assocList fields
  ty <- getI32 fm 1 "PageHeader.type"
  let unc = getOptionalI32 fm 2
      comp = getOptionalI32 fm 3
  dph <- case lookupField fm 5 of
    Just v -> Just <$> dataPageHeaderFromThrift v
    Nothing -> Right Nothing
  dct <- case lookupField fm 7 of
    Just v -> Just <$> dictionaryPageHeaderFromThrift v
    Nothing -> Right Nothing
  v2 <- case lookupField fm 8 of
    Just v -> Just <$> dataPageHeaderV2FromThrift v
    Nothing -> Right Nothing
  pure
    PageHeader
      { phType = ty
      , phUncompressedPageSize = unc
      , phCompressedPageSize = comp
      , phDataPage = dph
      , phDictionaryPage = dct
      , phDataPageV2 = v2
      }
pageHeaderFromThrift _ = Left "Parquet.Page: expected PageHeader struct"

dataPageHeaderFromThrift :: TV.Value -> Either String DataPageHeader
dataPageHeaderFromThrift (TV.Struct fields) = do
  let fm = assocList fields
  n <- getI32 fm 1 "DataPageHeader.num_values"
  enc <- case lookupField fm 2 of
    Just (TV.I32 e) -> Right e
    Nothing -> Right 0
    _ -> Left "Parquet.Page: DataPageHeader.encoding invalid"
  pure DataPageHeader {dphNumValues = n, dphEncoding = enc}
dataPageHeaderFromThrift _ = Left "Parquet.Page: expected DataPageHeader struct"

dictionaryPageHeaderFromThrift :: TV.Value -> Either String DictionaryPageHeader
dictionaryPageHeaderFromThrift (TV.Struct fields) = do
  let fm = assocList fields
  n <- getI32 fm 1 "DictionaryPageHeader.num_values"
  enc <- case lookupField fm 2 of
    Just (TV.I32 e) -> Right e
    Nothing -> Right 0
    _ -> Left "Parquet.Page: DictionaryPageHeader.encoding invalid"
  pure DictionaryPageHeader {dictNumValues = n, dictEncoding = enc}
dictionaryPageHeaderFromThrift _ = Left "Parquet.Page: expected DictionaryPageHeader struct"

dataPageHeaderV2FromThrift :: TV.Value -> Either String DataPageHeaderV2
dataPageHeaderV2FromThrift (TV.Struct fields) = do
  let fm = assocList fields
  nv <- getI32 fm 1 "DataPageHeaderV2.num_values"
  nn <- getI32 fm 2 "DataPageHeaderV2.num_nulls"
  nr <- getI32 fm 3 "DataPageHeaderV2.num_rows"
  enc <- getI32 fm 4 "DataPageHeaderV2.encoding"
  dl <- getI32 fm 5 "DataPageHeaderV2.definition_levels_byte_length"
  rl <- getI32 fm 6 "DataPageHeaderV2.repetition_levels_byte_length"
  let isComp = case lookupField fm 7 of
        Just (TV.Bool b) -> b
        _ -> True
  pure DataPageHeaderV2
    { dph2NumValues    = nv
    , dph2NumNulls     = nn
    , dph2NumRows      = nr
    , dph2Encoding     = enc
    , dph2DefLevelsLen = dl
    , dph2RepLevelsLen = rl
    , dph2IsCompressed = isComp
    }
dataPageHeaderV2FromThrift _ = Left "Parquet.Page: expected DataPageHeaderV2 struct"

assocList :: V.Vector (Int16, TV.Value) -> [(Int16, TV.Value)]
assocList = V.toList

lookupField :: [(Int16, TV.Value)] -> Int16 -> Maybe TV.Value
lookupField fm fid = lookup fid fm

getI32 :: [(Int16, TV.Value)] -> Int16 -> String -> Either String Int32
getI32 fm fid name = case lookupField fm fid of
  Just (TV.I32 v) -> Right v
  _ -> Left $ "Parquet.Page: missing or invalid field " ++ name

getOptionalI32 :: [(Int16, TV.Value)] -> Int16 -> Maybe Int32
getOptionalI32 fm fid = case lookupField fm fid of
  Just (TV.I32 v) -> Just v
  _ -> Nothing
