{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
-- | Parquet page headers (Thrift compact protocol).
--
-- See @parquet.thrift@ (@PageHeader@, @DataPageHeader@, @DictionaryPageHeader@,
-- @DataPageHeaderV2@).
module Parquet.Page
  ( PageHeader (..)
  , PageType (..)
  , DataPageHeader (..)
  , DictionaryPageHeader (..)
  , DataPageHeaderV2 (..)
  , readPageHeaderAt
  , pageTypeTag
  , pageTypeIsDataPageV1
  , pageTypeIsDataPageV2
  , pageTypeIsDictionary
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int32)
import qualified Data.Vector as V

import qualified Thrift.Value as TV
import Thrift.Decode (decodeCompactFrom)

-- | Discriminated union over Parquet page types. Mirrors the
-- @PageType@ enum in @parquet.thrift@ (DATA_PAGE=0, INDEX_PAGE=1,
-- DICTIONARY_PAGE=2, DATA_PAGE_V2=3) but each constructor carries the
-- nested struct that the spec couples with that variant — there is no
-- meaningful "DATA_PAGE without DataPageHeader" value on the wire, so
-- we don't represent one.
data PageType
  = PtDataPage      !DataPageHeader
  | PtIndexPage
  | PtDictionaryPage !DictionaryPageHeader
  | PtDataPageV2    !DataPageHeaderV2
  deriving stock (Show, Eq)

-- | The on-the-wire @PageType@ tag (Thrift @PageHeader.type@, field 1).
pageTypeTag :: PageType -> Int32
pageTypeTag = \case
  PtDataPage{}       -> 0
  PtIndexPage        -> 1
  PtDictionaryPage{} -> 2
  PtDataPageV2{}     -> 3

pageTypeIsDataPageV1 :: PageType -> Bool
pageTypeIsDataPageV1 PtDataPage{} = True
pageTypeIsDataPageV1 _            = False

pageTypeIsDataPageV2 :: PageType -> Bool
pageTypeIsDataPageV2 PtDataPageV2{} = True
pageTypeIsDataPageV2 _              = False

pageTypeIsDictionary :: PageType -> Bool
pageTypeIsDictionary PtDictionaryPage{} = True
pageTypeIsDictionary _                  = False

data PageHeader = PageHeader
  { phType                 :: !PageType
  , phUncompressedPageSize :: !(Maybe Int32)
  , phCompressedPageSize   :: !(Maybe Int32)
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
  pageType <- case ty of
    0 -> case lookupField fm 5 of
      Just v  -> PtDataPage <$> dataPageHeaderFromThrift v
      Nothing -> Left "Parquet.Page: DATA_PAGE without DataPageHeader (field 5)"
    1 -> Right PtIndexPage
    2 -> case lookupField fm 7 of
      Just v  -> PtDictionaryPage <$> dictionaryPageHeaderFromThrift v
      Nothing -> Left "Parquet.Page: DICTIONARY_PAGE without DictionaryPageHeader (field 7)"
    3 -> case lookupField fm 8 of
      Just v  -> PtDataPageV2 <$> dataPageHeaderV2FromThrift v
      Nothing -> Left "Parquet.Page: DATA_PAGE_V2 without DataPageHeaderV2 (field 8)"
    _ -> Left ("Parquet.Page: unknown PageType tag " ++ show ty)
  pure PageHeader
    { phType = pageType
    , phUncompressedPageSize = unc
    , phCompressedPageSize = comp
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
