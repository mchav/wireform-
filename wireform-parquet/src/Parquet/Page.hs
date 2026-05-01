{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Parquet page headers (Thrift compact protocol).
--
-- See @parquet.thrift@ (@PageHeader@, @DataPageHeader@, @DictionaryPageHeader@,
-- @DataPageHeaderV2@). Thrift field placement is mediated by the
-- pattern synonyms in "Parquet.Thrift.Schema".
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

import Parquet.Thrift.Schema
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
  let fm = V.toList fields
  ty <- requireI32 fm "PageHeader.type" $ \case
    PageHeader_Type v -> Just v
    _                 -> Nothing
  let unc = findField fm $ \case
        PageHeader_UncompressedSize v -> Just v
        _                             -> Nothing
      comp = findField fm $ \case
        PageHeader_CompressedSize v -> Just v
        _                           -> Nothing
  pageType <- case ty of
    0 -> case findField fm (\case
           PageHeader_DataPageHeader fs -> Just fs
           _                            -> Nothing) of
      Just fs -> PtDataPage <$> dataPageHeaderFromThrift (TV.Struct fs)
      Nothing -> Left
        "Parquet.Page: DATA_PAGE without DataPageHeader (field 5)"
    1 -> Right PtIndexPage
    2 -> case findField fm (\case
           PageHeader_DictionaryPageHeader fs -> Just fs
           _                                  -> Nothing) of
      Just fs -> PtDictionaryPage <$> dictionaryPageHeaderFromThrift (TV.Struct fs)
      Nothing -> Left
        "Parquet.Page: DICTIONARY_PAGE without DictionaryPageHeader (field 7)"
    3 -> case findField fm (\case
           PageHeader_DataPageHeaderV2 fs -> Just fs
           _                              -> Nothing) of
      Just fs -> PtDataPageV2 <$> dataPageHeaderV2FromThrift (TV.Struct fs)
      Nothing -> Left
        "Parquet.Page: DATA_PAGE_V2 without DataPageHeaderV2 (field 8)"
    _ -> Left ("Parquet.Page: unknown PageType tag " ++ show ty)
  pure PageHeader
    { phType = pageType
    , phUncompressedPageSize = unc
    , phCompressedPageSize = comp
    }
pageHeaderFromThrift _ = Left "Parquet.Page: expected PageHeader struct"

dataPageHeaderFromThrift :: TV.Value -> Either String DataPageHeader
dataPageHeaderFromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  n <- requireI32 fm "DataPageHeader.num_values" $ \case
    DataPageHeader_NumValues v -> Just v
    _                          -> Nothing
  -- encoding defaults to 0 (PLAIN) when absent, per historic writers.
  let enc = maybe 0 id $ findField fm $ \case
        DataPageHeader_Encoding v -> Just v
        _                         -> Nothing
  pure DataPageHeader {dphNumValues = n, dphEncoding = enc}
dataPageHeaderFromThrift _ = Left "Parquet.Page: expected DataPageHeader struct"

dictionaryPageHeaderFromThrift :: TV.Value -> Either String DictionaryPageHeader
dictionaryPageHeaderFromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  n <- requireI32 fm "DictionaryPageHeader.num_values" $ \case
    DictionaryPageHeader_NumValues v -> Just v
    _                                -> Nothing
  let enc = maybe 0 id $ findField fm $ \case
        DictionaryPageHeader_Encoding v -> Just v
        _                               -> Nothing
  pure DictionaryPageHeader {dictNumValues = n, dictEncoding = enc}
dictionaryPageHeaderFromThrift _ = Left "Parquet.Page: expected DictionaryPageHeader struct"

dataPageHeaderV2FromThrift :: TV.Value -> Either String DataPageHeaderV2
dataPageHeaderV2FromThrift (TV.Struct fields) = do
  let fm = V.toList fields
  nv <- requireI32 fm "DataPageHeaderV2.num_values" $ \case
    DataPageHeaderV2_NumValues v -> Just v
    _                            -> Nothing
  nn <- requireI32 fm "DataPageHeaderV2.num_nulls" $ \case
    DataPageHeaderV2_NumNulls v -> Just v
    _                           -> Nothing
  nr <- requireI32 fm "DataPageHeaderV2.num_rows" $ \case
    DataPageHeaderV2_NumRows v -> Just v
    _                          -> Nothing
  enc <- requireI32 fm "DataPageHeaderV2.encoding" $ \case
    DataPageHeaderV2_Encoding v -> Just v
    _                           -> Nothing
  dl <- requireI32 fm "DataPageHeaderV2.definition_levels_byte_length" $ \case
    DataPageHeaderV2_DefinitionLevelsByteLength v -> Just v
    _                                             -> Nothing
  rl <- requireI32 fm "DataPageHeaderV2.repetition_levels_byte_length" $ \case
    DataPageHeaderV2_RepetitionLevelsByteLength v -> Just v
    _                                             -> Nothing
  -- is_compressed is optional; spec default is true.
  let isComp = maybe True id $ findField fm $ \case
        DataPageHeaderV2_IsCompressed b -> Just b
        _                               -> Nothing
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

-- | Specialised @require@ for @Int32@ fields, producing a
-- @Parquet.Page@-flavoured error message.
requireI32
  :: [(Int16, TV.Value)] -> String
  -> ((Int16, TV.Value) -> Maybe Int32) -> Either String Int32
requireI32 fm name probe = case findField fm probe of
  Just v  -> Right v
  Nothing -> Left $ "Parquet.Page: missing or invalid field " ++ name
