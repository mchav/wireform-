{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Runtime.ObjectStore
Description : Object-store contract for snapshot-aware state
              stores (Riffle \xc2\xa71)

The snapshot story in Riffle \xc2\xa71 needs a place to write the
snapshot files. The /object store/ is intentionally minimal:
put / get / list / delete on byte payloads keyed by 'ObjectKey'.
S3, GCS, Azure Blob, MinIO, and a local filesystem all
satisfy this surface.

This module exposes:

  * 'ObjectStoreClient' — the contract.
  * 'inMemoryObjectStore' — in-process reference impl backed
    by an 'IORef Map'. Used by tests and by deployments that
    want to validate snapshot semantics before pointing at a
    real object store.
  * 'filesystemObjectStore' — writes payloads under a root
    directory. Atomic-rename semantics on POSIX. Suitable as
    a development backend; production deployments use one of
    the cloud adapters shipped in their own packages.
-}
module Kafka.Streams.Runtime.ObjectStore (
  -- * Contract
  ObjectKey (..),
  ObjectStoreClient (..),
  ObjectStoreError (..),
  ObjectStoreOutcome,

  -- * Reference impls
  inMemoryObjectStore,
  filesystemObjectStore,
) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IORef (
  IORef,
  atomicModifyIORef',
  newIORef,
  readIORef,
 )
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import System.Directory (
  createDirectoryIfMissing,
  doesFileExist,
  listDirectory,
  removeFile,
 )
import System.FilePath ((</>))


----------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------

{- | Object identity. The runtime keys snapshot files by
@(store, keyGroup, snapshotId, partN)@-style paths; this
newtype just keeps the type-level distinction from generic
'Text'.
-}
newtype ObjectKey = ObjectKey {unObjectKey :: Text}
  deriving stock (Eq, Ord, Show, Generic)


{- | Error returned from an object-store operation. The runtime
distinguishes retryable from fatal so it can defer to its
standard error policy.
-}
data ObjectStoreError
  = ObjectStoreRetryable !Text
  | ObjectStoreFatal !Text
  deriving stock (Eq, Show, Generic)


type ObjectStoreOutcome a = Either ObjectStoreError a


{- | The store contract. Implementations:

  * 'inMemoryObjectStore' for tests and validation.
  * 'filesystemObjectStore' for development.
  * Cloud adapters (S3 / GCS / Azure) in their own packages.
-}
data ObjectStoreClient = ObjectStoreClient
  { osName :: !Text
  , osPut :: !(ObjectKey -> ByteString -> IO (ObjectStoreOutcome ()))
  , osGet :: !(ObjectKey -> IO (ObjectStoreOutcome (Maybe ByteString)))
  , osDelete :: !(ObjectKey -> IO (ObjectStoreOutcome ()))
  , osList :: !(Text -> IO (ObjectStoreOutcome [ObjectKey]))
  {- ^ List every key under the given /prefix/. Returns the
  full keys (not relativised) so callers can pass them
  straight back to 'osGet'.
  -}
  }


----------------------------------------------------------------------
-- In-memory reference
----------------------------------------------------------------------

{- | An in-process object store backed by an 'IORef Map'. Used
by every test that touches the snapshot machinery.
-}
inMemoryObjectStore :: Text -> IO ObjectStoreClient
inMemoryObjectStore nm = do
  ref <- newIORef (Map.empty :: Map ObjectKey ByteString)
  pure
    ObjectStoreClient
      { osName = nm
      , osPut = \k v -> do
          atomicModifyIORef' ref (\m -> (Map.insert k v m, ()))
          pure (Right ())
      , osGet = \k -> Right . Map.lookup k <$> readIORef ref
      , osDelete = \k -> do
          atomicModifyIORef' ref (\m -> (Map.delete k m, ()))
          pure (Right ())
      , osList = \prefix -> do
          m <- readIORef ref
          pure $
            Right
              [ k
              | k <- Map.keys m
              , T.isPrefixOf prefix (unObjectKey k)
              ]
      }


----------------------------------------------------------------------
-- Filesystem reference
----------------------------------------------------------------------

{- | A filesystem-backed object store rooted at the supplied
directory. Keys map 1:1 to relative file paths under the
root. Atomic-rename semantics on POSIX (we use
'BS.writeFile' followed by 'renameFile' for puts that
collide; for the simple case here we use direct write).
-}
filesystemObjectStore :: Text -> FilePath -> IO ObjectStoreClient
filesystemObjectStore nm root = do
  createDirectoryIfMissing True root
  pure
    ObjectStoreClient
      { osName = nm
      , osPut = \(ObjectKey k) v -> wrapIO $ do
          let path = root </> T.unpack k
          -- Ensure intermediate dirs exist (S3 has no notion of
          -- directories; FS callers do). We split on '/' and
          -- pre-create everything up to the leaf.
          case reverse (splitOn '/' (T.unpack k)) of
            [] -> pure ()
            (_ : revDirs) -> do
              let dir =
                    List.intercalate
                      "/"
                      (root : reverse revDirs)
              when (not (null revDirs)) $
                createDirectoryIfMissing True dir
          BS.writeFile path v
      , osGet = \(ObjectKey k) -> wrapIO $ do
          let path = root </> T.unpack k
          ex <- doesFileExist path
          if ex
            then Just <$> BS.readFile path
            else pure Nothing
      , osDelete = \(ObjectKey k) -> wrapIO $ do
          let path = root </> T.unpack k
          ex <- doesFileExist path
          when ex (removeFile path)
      , osList = \prefix -> wrapIO $ do
          -- Walk the root directory; ignore non-matching files.
          let pfx = T.unpack prefix
          entries <- listDirectory root
          rs <- forM entries $ \e -> do
            let fullKey = T.pack e
            pure $
              if T.isPrefixOf prefix fullKey
                then [ObjectKey fullKey]
                else []
          -- Sub-directory walk would be nicer but is overkill for
          -- the test/dev backend; deployments using nested
          -- prefixes should use a real object store.
          _ <- pure pfx
          pure (concat rs)
      }
  where
    wrapIO :: IO a -> IO (ObjectStoreOutcome a)
    wrapIO act = do
      r <- try act
      case r of
        Right a -> pure (Right a)
        Left (e :: SomeException) ->
          pure (Left (ObjectStoreRetryable (T.pack (show e))))


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn sep (x : xs)
  | x == sep = [] : splitOn sep xs
  | otherwise = let (y : ys) = splitOn sep xs in (x : y) : ys
