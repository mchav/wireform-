{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | High-level IO entry points for Apache Lance datasets.

A Lance "dataset" on disk has the layout

@
/<root>.lance/
  data/<fragment-uuid>.lance        -- one per fragment
  _versions/<inv-version>.manifest  -- one per committed version
  _transactions/<id>.txn            -- one per uncommitted txn
@

The @_versions/@ filenames use a /reversed/ version
convention: the on-disk number is @2^64 − 1 − v@, so
directory listings sort the newest manifest first. The
decoders here surface the real version number to callers.

This module exposes:

  * 'openLanceFile' — read a single @.lance@ data file (an IO
    wrapper around 'Lance.Format.readLanceFile').
  * 'findManifestVersions' — list every @_versions/*.manifest@
    under a dataset root, decoded back to @(version, path)@.
  * 'latestManifestVersion' — the active manifest = highest
    version number.
  * 'openLanceDataset' — high-level opener that returns a
    'LanceDataset' summary.
-}
module Lance.IO (
  -- * Single file
  openLanceFile,

  -- * Dataset discovery
  findManifestVersions,
  latestManifestVersion,

  -- * Dataset opener
  LanceDataset (..),
  openLanceDataset,
  openLanceDatasetAt,

  -- * Manifest file
  openLanceManifest,

  -- * Filename conventions
  decodeManifestFileName,
  encodeManifestFileName,

  -- * Re-exports
  module Lance.Format,
) where

import Data.ByteString qualified as BS
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import Data.Word (Word64)
import Lance.Format
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Read (readMaybe)


-- ============================================================
-- Single file
-- ============================================================

{- | IO wrapper around 'readLanceFile': read the bytes from disk
and run the envelope check + footer parse.
-}
openLanceFile :: FilePath -> IO (Either String LanceFile)
openLanceFile fp = do
  bs <- BS.readFile fp
  pure (readLanceFile bs)


-- ============================================================
-- Dataset discovery
-- ============================================================

{- | Decode the @_versions/<n>.manifest@ filename convention back
into a Lance version. The on-disk number @n@ is
@2^64 − 1 − version@, so the smallest @n@ is the newest
version.

> decodeManifestFileName \"18446744073709551614.manifest\" == Just 1
> decodeManifestFileName \"18446744073709551613.manifest\" == Just 2
> decodeManifestFileName \"foo.manifest\" == Nothing
-}
decodeManifestFileName :: FilePath -> Maybe Word64
decodeManifestFileName name
  | takeExtension name /= ".manifest" = Nothing
  | otherwise = case readMaybe (takeBaseName name) :: Maybe Integer of
      -- Use Integer for the parse so we can detect ranges that
      -- would overflow Word64 (e.g. random user data).
      Just n
        | n >= 0 && n <= maxW64
        , let inv = (maxW64 :: Integer) - n
        , inv >= 0 ->
            Just (fromIntegral inv)
      _ -> Nothing
  where
    maxW64 = fromIntegral (maxBound :: Word64)


{- | Inverse of 'decodeManifestFileName' — produce the on-disk
name for a given version.
-}
encodeManifestFileName :: Word64 -> FilePath
encodeManifestFileName v =
  let inv = (maxBound :: Word64) - v
  in show inv ++ ".manifest"


{- | List every @_versions/*.manifest@ under a Lance dataset
root, returning @(version, absolute path)@ pairs sorted
newest-first.
-}
findManifestVersions :: FilePath -> IO [(Word64, FilePath)]
findManifestVersions root = do
  let dir = root </> "_versions"
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      entries <- listDirectory dir
      let candidates = mapMaybe (toEntry dir) entries
      pure (sortBy (comparing (Down . fst)) candidates)
  where
    toEntry dir name = do
      v <- decodeManifestFileName name
      Just (v, dir </> name)


-- | Pick the active (highest-numbered) manifest version, if any.
latestManifestVersion :: FilePath -> IO (Maybe (Word64, FilePath))
latestManifestVersion root = do
  vs <- findManifestVersions root
  pure $ case vs of
    [] -> Nothing
    (x : _) -> Just x


-- ============================================================
-- Dataset opener
-- ============================================================

{- | A Lance dataset opened from disk. Carries everything we can
produce without decoding the manifest's protobuf body —
enumerated versions, the active manifest's typed footer (if
it could be opened), and the data fragments under @data/@.
-}
data LanceDataset = LanceDataset
  { ldRoot :: !FilePath
  , ldVersions :: ![(Word64, FilePath)]
  {- ^ All committed versions, newest-first. Each entry is
  @(version, path-to-manifest)@.
  -}
  , ldLatestVersion :: !(Maybe Word64)
  , ldLatestManifestFooter :: !(Maybe LanceManifestFooter)
  {- ^ The 16-byte manifest footer of the active manifest file
  (the @_versions/@ entry whose decoded version is
  'ldLatestVersion'). The protobuf @Manifest@ body it points
  at lives downstream (out-of-tree); this module commits
  only to the byte-range surface a future protobuf decoder
  would consume.
  -}
  , ldDataFiles :: ![FilePath]
  {- ^ Absolute paths to every @data/*.lance@ fragment file in
  the dataset directory, sorted lexicographically.
  -}
  }
  deriving (Show, Eq)


{- | Read a manifest file's bytes off disk and decode the
16-byte footer. Returns the footer and the raw bytes (so the
caller can splice the protobuf body out via
@BS.take (lmfManifestPosition footer) bs ...@).
-}
openLanceManifest :: FilePath -> IO (Either String (LanceManifestFooter, BS.ByteString))
openLanceManifest fp = do
  bs <- BS.readFile fp
  pure $ case parseManifestFooter bs of
    Left err -> Left err
    Right f -> Right (f, bs)


{- | Open a Lance dataset from a directory.

/Caveats:/

  * The dataset's /active/ data fragments are tracked inside
    the manifest's protobuf body, which we don't decode here.
    'ldDataFiles' therefore enumerates /every/ @data/*.lance@
    file on disk, including stale ones that older versions
    no longer reference. A real query planner needs the
    protobuf decoder to filter this list down.
  * 'ldLatestManifest' is 'Nothing' when @_versions/@ is
    empty (a freshly-initialised dataset) /or/ when the
    latest manifest's footer is malformed (we propagate the
    error to the @Left@ branch).
-}
openLanceDataset :: FilePath -> IO (Either String LanceDataset)
openLanceDataset root = do
  ok <- doesDirectoryExist root
  if not ok
    then pure (Left ("Lance.IO: not a directory: " ++ root))
    else do
      versions <- findManifestVersions root
      let latestV = fst <$> headMaybe versions
      latestFooter <- case versions of
        [] -> pure (Right Nothing)
        ((_, p) : _) -> do
          present <- doesFileExist p
          if not present
            then pure (Right Nothing)
            else do
              r <- openLanceManifest p
              pure (fmap (Just . fst) r)
      dataFiles <- listDataFiles root
      case latestFooter of
        Left err -> pure (Left ("Lance.IO: bad latest manifest: " ++ err))
        Right lf ->
          pure $
            Right
              LanceDataset
                { ldRoot = root
                , ldVersions = versions
                , ldLatestVersion = latestV
                , ldLatestManifestFooter = lf
                , ldDataFiles = dataFiles
                }
  where
    headMaybe [] = Nothing
    headMaybe (x : _) = Just x


listDataFiles :: FilePath -> IO [FilePath]
listDataFiles root = do
  let dir = root </> "data"
  ok <- doesDirectoryExist dir
  if not ok
    then pure []
    else do
      entries <- listDirectory dir
      pure $ sortBy compare $ mapMaybe (keepDot dir) entries
  where
    keepDot dir e
      | takeExtension e == ".lance" = Just (dir </> e)
      | otherwise = Nothing


-- ============================================================
-- Per-version opener (time travel)
-- ============================================================

{- | Open a Lance dataset /at a specific committed version/.
The footer-decoded manifest is the one for @atVersion@; the
on-disk @data/@ enumeration is unchanged (Lance keeps
superseded fragments around until a vacuum). Use
'datasetActiveDataFilePaths' on the returned dataset to get
just the version's actually-active fragments.

Returns 'Left' if @atVersion@ isn't on disk (the manifest
is gone — typically because it was vacuumed or never
existed).
-}
openLanceDatasetAt
  :: FilePath
  -> Word64
  -> IO (Either String LanceDataset)
openLanceDatasetAt root atVersion = do
  ok <- doesDirectoryExist root
  if not ok
    then pure (Left ("Lance.IO: not a directory: " ++ root))
    else do
      versions <- findManifestVersions root
      let manifest = lookup atVersion versions
      case manifest of
        Nothing ->
          pure
            ( Left
                ( "Lance.IO: version "
                    ++ show atVersion
                    ++ " not found"
                )
            )
        Just p -> do
          r <- openLanceManifest p
          dataFiles <- listDataFiles root
          case r of
            Left err -> pure (Left err)
            Right (footer, _) ->
              pure $
                Right
                  LanceDataset
                    { ldRoot = root
                    , ldVersions = versions
                    , ldLatestVersion = Just atVersion
                    , ldLatestManifestFooter = Just footer
                    , ldDataFiles = dataFiles
                    }
