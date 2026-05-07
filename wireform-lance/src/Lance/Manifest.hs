{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Decoder for the Lance @lance.table.Manifest@ protobuf message.
--
-- A Lance @_versions/<n>.manifest@ file is a serialised
-- @lance.table.Manifest@ protobuf message followed by a 16-byte
-- fixed footer (decoded by 'Lance.Format.parseManifestFooter').
-- The protobuf body lives at @manifest_position .. (file_size - 16)@.
--
-- This module is a thin IO facade over the auto-generated typed
-- decoders in "Lance.Pb.Lance.Table" / "Lance.Pb.Lance.File".
-- The generated modules are produced by @cabal run gen-lance-pb@
-- from @proto/lance/{file,table}.proto@; do not edit them by hand.
module Lance.Manifest
  ( -- * Top-level
    decodeManifest
  , readDatasetManifest
    -- * Active data files
  , datasetActiveDataFiles
    -- * Re-exports of the generated types
  , Pb.Manifest (..)
  , Pb.Manifest'WriterVersion (..)
  , Pb.Manifest'DataStorageFormat (..)
  , Pb.DataFragment (..)
  , Pb.DataFile (..)
  , Pb.DeletionFile (..)
  , Pb.DeletionFile'DeletionFileType (..)
  , Pb.UUID (..)
  ) where

import Control.Exception (try, SomeException)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Text (Text)

import qualified Proto.Decode as ProtoDecode

import Lance.Format (LanceManifestFooter (..), manifestFooterSize)
import Lance.IO (openLanceManifest, LanceDataset (..))
import qualified Lance.Pb.Lance.Table as Pb

-- | Decode the bytes of a serialised @lance.table.Manifest@
-- message into the typed record. The bytes are the slice of a
-- manifest file from @manifest_position@ to the start of the
-- trailing 16-byte footer.
decodeManifest :: ByteString -> Either String Pb.Manifest
decodeManifest bs = case ProtoDecode.decodeMessage bs of
  Left e  -> Left ("Lance.Manifest: " ++ show e)
  Right m -> Right m

-- | Read a manifest file off disk: parse the 16-byte footer to
-- find @manifest_position@, slice out the protobuf body, and
-- decode it. Returns @(footer, manifest)@ on success.
--
-- The manifest body on disk is u32-length-prefixed: the first
-- 4 bytes at @manifest_position@ are the little-endian length
-- of the serialised @Manifest@ message that follows. We strip
-- that prefix before handing the bytes to the protobuf decoder.
readDatasetManifest
  :: FilePath
  -> IO (Either String (LanceManifestFooter, Pb.Manifest))
readDatasetManifest fp = do
  res <- openLanceManifest fp
  case res of
    Left err           -> pure (Left err)
    Right (footer, bs) -> do
      let total      = BS.length bs
          startPos   = fromIntegral (lmfManifestPosition footer) :: Int
          bodyEnd    = total - manifestFooterSize
          rawLen     = bodyEnd - startPos
      if startPos < 0 || rawLen < 4 || startPos + rawLen > total
        then pure (Left "Lance.Manifest: manifest body out of range")
        else
          let raw      = BS.take rawLen (BS.drop startPos bs)
              prefixed = decodeU32LE (BS.take 4 raw)
              body     = BS.take prefixed (BS.drop 4 raw)
           in if prefixed > rawLen - 4
                then pure (Left "Lance.Manifest: u32 length prefix exceeds body")
                else case decodeManifest body of
                  Left e  -> pure (Left e)
                  Right m -> pure (Right (footer, m))

-- Decode a 4-byte little-endian u32 from the head of a
-- 'ByteString' as an 'Int'. Caller must ensure the slice is
-- exactly 4 bytes.
decodeU32LE :: BS.ByteString -> Int
decodeU32LE bs =
  let b0 = fromIntegral (BS.index bs 0) :: Int
      b1 = fromIntegral (BS.index bs 1) :: Int
      b2 = fromIntegral (BS.index bs 2) :: Int
      b3 = fromIntegral (BS.index bs 3) :: Int
   in b0 + b1 * 0x100 + b2 * 0x10000 + b3 * 0x1000000

-- | Convenience: for a 'LanceDataset' that's already been
-- opened via 'Lance.IO.openLanceDataset', read the active
-- manifest's protobuf body and return the relative @path@s of
-- every active 'DataFile' in fragment order.
--
-- Returns an empty list for an uninitialised dataset (no
-- @_versions/@) and an error for a dataset whose latest
-- manifest can't be decoded.
datasetActiveDataFiles :: LanceDataset -> IO (Either String [Text])
datasetActiveDataFiles ds = case ldVersions ds of
  []         -> pure (Right [])
  ((_, p):_) -> do
    res <- try (readDatasetManifest p)
            :: IO (Either SomeException (Either String (LanceManifestFooter, Pb.Manifest)))
    case res of
      Left e         -> pure (Left ("Lance.Manifest: " ++ show e))
      Right (Left e) -> pure (Left e)
      Right (Right (_, m)) ->
        pure (Right (do
          frag <- V.toList (Pb.manifestFragments m)
          file <- V.toList (Pb.dataFragmentFiles frag)
          pure (Pb.dataFilePath file)))
