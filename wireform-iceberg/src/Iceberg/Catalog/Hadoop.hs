{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Iceberg "Hadoop" file-based catalog.

Stores all catalog state in a filesystem layout (local FS, HDFS, or
S3-like blob stores - this module is filesystem-agnostic via a small
'FileSystem' record):

@
<warehouse>/<ns_part1>/<ns_part2>/.../<table>/
  data/                          -- data files (managed by writer)
  metadata/
    v1.metadata.json             -- table metadata, snapshot 1
    v2.metadata.json             -- table metadata, snapshot 2
    ...
    version-hint.text            -- "<N>"   (current version)
    <uuid>-<N>.metadata.json     -- optional UUID-prefixed alias
@

A successful commit writes the new @v\<N+1\>.metadata.json@ first,
then atomically replaces @version-hint.text@ with @"\<N+1\>"@. The
spec requires that 'fsAtomicReplace' be implemented as a true atomic
replace (e.g. POSIX 'rename'); on object stores that don't have one,
the catalog falls back to a conditional PUT and retries with backoff
on contention. This module captures the protocol, not the
conditional-PUT machinery.
-}
module Iceberg.Catalog.Hadoop (
  -- * Filesystem abstraction
  FileSystem (..),
  localFileSystem,

  -- * Catalog
  HadoopCatalog (..),
  mkHadoopCatalog,

  -- * Operations
  currentVersion,
  currentMetadata,
  commitMetadata,
  metadataPath,
  versionHintPath,
  tableDir,
) where

import Control.Exception (IOException, try)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Iceberg.JSON (metadataFromJSON, metadataToJSON)
import Iceberg.Types (TableMetadata)
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.IO qualified as IO


-- ============================================================
-- Filesystem abstraction
-- ============================================================

{- | A minimal file-system contract. The catalog requires:

* read-file (return 'Nothing' if absent), write-file, list-directory,
* an /atomic/ replace primitive used to advance @version-hint.text@.

'localFileSystem' wires these up to 'System.Directory' /
'System.IO'. Object-store implementations supply their own
'fsAtomicReplace' (e.g. S3 PutObject with @If-Match@ ETag, or HDFS
@rename@).
-}
data FileSystem = FileSystem
  { fsReadFile :: FilePath -> IO (Maybe ByteString)
  , fsWriteFile :: FilePath -> ByteString -> IO ()
  , fsAtomicReplace :: FilePath -> ByteString -> IO ()
  -- ^ Replace the file atomically (writes a temp file then renames).
  , fsListDirectory :: FilePath -> IO [FilePath]
  , fsCreateDirectory :: FilePath -> IO ()
  -- ^ @createDirectoryIfMissing True@ semantics.
  }


-- | Wire 'FileSystem' up to local POSIX-style I/O.
localFileSystem :: FileSystem
localFileSystem =
  FileSystem
    { fsReadFile = \p -> do
        ex <- Dir.doesFileExist p
        if ex
          then Just <$> BS.readFile p
          else pure Nothing
    , fsWriteFile = BS.writeFile
    , fsAtomicReplace = \p bytes -> do
        let tmp = p ++ ".tmp"
        BS.writeFile tmp bytes
        Dir.renamePath tmp p
    , fsListDirectory = \p -> do
        ex <- Dir.doesDirectoryExist p
        if ex then Dir.listDirectory p else pure []
    , fsCreateDirectory = Dir.createDirectoryIfMissing True
    }


-- ============================================================
-- Catalog
-- ============================================================

{- | A Hadoop catalog handle. Carries the warehouse root and a
'FileSystem' implementation; the catalog itself is stateless.
-}
data HadoopCatalog = HadoopCatalog
  { hcWarehouse :: !FilePath
  , hcFs :: !FileSystem
  }


mkHadoopCatalog :: FilePath -> FileSystem -> HadoopCatalog
mkHadoopCatalog = HadoopCatalog


-- | Resolve the table directory inside the warehouse.
tableDir :: HadoopCatalog -> V.Vector Text -> Text -> FilePath
tableDir hc namespace tableName =
  foldl
    (</>)
    (hcWarehouse hc)
    (map T.unpack (V.toList namespace ++ [tableName]))


-- | @<table-dir>/metadata/v\<N\>.metadata.json@
metadataPath :: HadoopCatalog -> V.Vector Text -> Text -> Int -> FilePath
metadataPath hc ns name v =
  tableDir hc ns name </> "metadata" </> ("v" ++ show v ++ ".metadata.json")


-- | @<table-dir>/metadata/version-hint.text@
versionHintPath :: HadoopCatalog -> V.Vector Text -> Text -> FilePath
versionHintPath hc ns name =
  tableDir hc ns name </> "metadata" </> "version-hint.text"


-- ============================================================
-- Operations
-- ============================================================

{- | Read the integer in @version-hint.text@. Returns 'Nothing' when
the table directory doesn't exist or the hint file is absent.
-}
currentVersion :: HadoopCatalog -> V.Vector Text -> Text -> IO (Maybe Int)
currentVersion hc ns name = do
  mb <- fsReadFile (hcFs hc) (versionHintPath hc ns name)
  case mb of
    Nothing -> pure Nothing
    Just bs -> case reads (T.unpack (T.strip (decodeUtf8' bs))) of
      [(n, "")] -> pure (Just n)
      _ -> pure Nothing
  where
    decodeUtf8' = T.pack . map (toEnum . fromIntegral) . BS.unpack


{- | Read and parse the latest @v\<N\>.metadata.json@. Returns
@Left@ for I/O / decode errors and @Right Nothing@ when the table
doesn't exist yet.
-}
currentMetadata
  :: HadoopCatalog
  -> V.Vector Text
  -> Text
  -> IO (Either String (Maybe (Int, TableMetadata)))
currentMetadata hc ns name = do
  mv <- currentVersion hc ns name
  case mv of
    Nothing -> pure (Right Nothing)
    Just v -> do
      mb <- fsReadFile (hcFs hc) (metadataPath hc ns name v)
      case mb of
        Nothing -> pure (Left ("Hadoop catalog: missing v" ++ show v ++ ".metadata.json"))
        Just bs -> case Aeson.eitherDecodeStrict bs of
          Left e -> pure (Left ("metadata JSON parse: " ++ e))
          Right j -> case metadataFromJSON j of
            Left e -> pure (Left ("metadata decode: " ++ e))
            Right tm -> pure (Right (Just (v, tm)))


{- | Commit a new metadata snapshot. Increments the version, writes
@v\<N+1\>.metadata.json@, then atomically replaces
@version-hint.text@. Returns the new version number.

If @assertedCurrentVersion@ does not match the on-disk version, the
commit is aborted with @Left@. Callers should retry with the
updated metadata after a conflict (Iceberg's "optimistic
concurrency" pattern).
-}
commitMetadata
  :: HadoopCatalog
  -> V.Vector Text
  -- ^ namespace
  -> Text
  -- ^ table name
  -> Maybe Int
  -- ^ asserted current version, or 'Nothing' for create
  -> TableMetadata
  -- ^ the new metadata
  -> IO (Either String Int)
commitMetadata hc ns name asserted tm = do
  observed <- currentVersion hc ns name
  if observed /= asserted
    then
      pure
        ( Left
            ( "Hadoop catalog: version conflict, expected "
                ++ show asserted
                ++ " but found "
                ++ show observed
            )
        )
    else do
      let !newVersion = maybe 1 (+ 1) observed
          !mp = metadataPath hc ns name newVersion
          !vp = versionHintPath hc ns name
          !mdJson = BL.toStrict (Aeson.encode (metadataToJSON tm))
      eRes <- try $ do
        fsCreateDirectory (hcFs hc) (tableDir hc ns name </> "metadata")
        fsWriteFile (hcFs hc) mp mdJson
        fsAtomicReplace
          (hcFs hc)
          vp
          (BS.pack (map (fromIntegral . fromEnum) (show newVersion)))
      case eRes :: Either IOException () of
        Left e -> pure (Left ("Hadoop catalog: I/O error: " ++ show e))
        Right _ -> pure (Right newVersion)


-- Suppress -Widentities for the helpers above.
_unusedIO :: IO.Handle -> IO ()
_unusedIO _ = pure ()
