{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
-- | @iceberg@ CLI - inspect Iceberg metadata files, manifests, and
-- (optionally) talk to a REST catalog.
--
-- Usage:
--
-- @
-- iceberg metadata-show          PATH                    # decode + pretty-print TableMetadata JSON
-- iceberg metadata-validate      PATH                    # run Iceberg.Validate against TableMetadata
-- iceberg manifest-show          PATH                    # decode an Avro manifest entry file
-- iceberg manifest-list-show     PATH                    # decode an Avro manifest-list file
-- iceberg view-show              PATH                    # decode + pretty-print ViewMetadata JSON
-- iceberg expire                 PATH NOW_MS [--max-age MS] [--min N]   # plan snapshot expiration
-- iceberg orphans                PATH PATH_LISTING_FILE  # plan orphan-file deletion
-- iceberg rest list-namespaces   URL [--token T]
-- iceberg rest load-table        URL NS NAME [--token T]
-- @
--
-- The CLI is a thin shell around the library APIs. It reads files /
-- stdin, calls the library, and prints JSON to stdout. Real-world
-- writers should embed the library directly; the CLI exists for
-- exploration and debugging.
module Main where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

#ifdef HAVE_REST_CLIENT
import qualified Iceberg.Catalog.REST as REST
import qualified Iceberg.Catalog.REST.Client as Client
#endif
import qualified Iceberg.JSON as IJ
import qualified Iceberg.Maintenance as M
import qualified Iceberg.Read as IR
import qualified Iceberg.Types as I
import qualified Iceberg.Validate as IV

main :: IO ()
main = do
  args <- getArgs
  case args of
    "metadata-show" : path : _      -> cmdMetadataShow path
    "metadata-validate" : path : _  -> cmdMetadataValidate path
    "manifest-show" : path : _      -> cmdManifestShow path
    "manifest-list-show" : path : _ -> cmdManifestListShow path
    "view-show" : path : _          -> cmdViewShow path
    "expire" : path : nowStr : rest -> cmdExpire path nowStr rest
    "orphans" : path : listing : _  -> cmdOrphans path listing
#ifdef HAVE_REST_CLIENT
    "rest" : "list-namespaces" : url : rest -> cmdRestListNs url rest
    "rest" : "load-table" : url : ns : name : rest -> cmdRestLoadTable url ns name rest
#else
    "rest" : _ -> die "iceberg: REST client not compiled (rebuild with -frest-client)"
#endif
    _ -> usage

usage :: IO a
usage = do
  hPutStrLn stderr usageText
  exitFailure

usageText :: String
usageText = unlines
  [ "usage: iceberg <command> [args...]"
  , ""
  , "  metadata-show          PATH"
  , "  metadata-validate      PATH"
  , "  manifest-show          PATH"
  , "  manifest-list-show     PATH"
  , "  view-show              PATH"
  , "  expire                 PATH NOW_MS [--max-age MS] [--min N]"
  , "  orphans                PATH FILE_LISTING"
  , "  rest list-namespaces   URL [--token TOKEN]"
  , "  rest load-table        URL NAMESPACE NAME [--token TOKEN]"
  ]

-- ============================================================
-- Metadata commands
-- ============================================================

cmdMetadataShow :: FilePath -> IO ()
cmdMetadataShow path = do
  bs <- BS.readFile path
  case Aeson.eitherDecodeStrict bs >>= IJ.metadataFromJSON of
    Right tm -> do
      BL.putStr (AesonP.encodePretty (IJ.metadataToJSON tm))
      putStrLn ""
    Left e   -> die ("metadata-show: " ++ e)

cmdMetadataValidate :: FilePath -> IO ()
cmdMetadataValidate path = do
  bs <- BS.readFile path
  tm <- case Aeson.eitherDecodeStrict bs >>= IJ.metadataFromJSON of
    Right t -> pure t
    Left e  -> die ("metadata-validate: " ++ e)
  case IV.validateMetadata tm of
    IV.ValidationOk -> putStrLn "OK: TableMetadata is valid"
    IV.ValidationErrors errs -> do
      mapM_ T.putStrLn errs
      exitFailure

cmdManifestShow :: FilePath -> IO ()
cmdManifestShow path = do
  bs <- BS.readFile path
  case IR.readManifestEntries bs of
    Right (_, entries) -> do
      putStrLn $ "Manifest with " ++ show (V.length entries) ++ " entries"
      V.forM_ entries $ \me -> do
        putStrLn $ "  " ++ show (I.meStatus me)
                 ++ " " ++ T.unpack (I.meFilePath me)
                 ++ " (records=" ++ show (I.meRecordCount me)
                 ++ ", size=" ++ show (I.meFileSizeBytes me)
                 ++ ")"
    Left e -> die ("manifest-show: " ++ e)

cmdManifestListShow :: FilePath -> IO ()
cmdManifestListShow path = do
  bs <- BS.readFile path
  case IR.readManifestList bs of
    Right (_, mfs) -> do
      putStrLn $ "Manifest list with " ++ show (V.length mfs) ++ " entries"
      V.forM_ mfs $ \mf -> do
        putStrLn $ "  " ++ T.unpack (I.mfPath mf)
                 ++ " content=" ++ show (I.mfContent mf)
                 ++ " seq=" ++ show (I.mfSequenceNumber mf)
                 ++ " added=" ++ show (I.mfAddedSnapshotId mf)
    Left e -> die ("manifest-list-show: " ++ e)

cmdViewShow :: FilePath -> IO ()
cmdViewShow path = do
  bs <- BS.readFile path
  case Aeson.eitherDecodeStrict bs >>= IJ.viewMetadataFromJSON of
    Right vm -> do
      BL.putStr (AesonP.encodePretty (IJ.viewMetadataToJSON vm))
      putStrLn ""
    Left e -> die ("view-show: " ++ e)

-- ============================================================
-- Maintenance commands
-- ============================================================

cmdExpire :: FilePath -> String -> [String] -> IO ()
cmdExpire path nowStr rest = do
  bs <- BS.readFile path
  tm <- case Aeson.eitherDecodeStrict bs >>= IJ.metadataFromJSON of
    Right t -> pure t
    Left e  -> die ("expire: " ++ e)
  let now = read nowStr
      policy = parseExpiryArgs rest
      result = M.expireSnapshots now policy tm
  putStrLn $ "Would drop " ++ show (length (M.exExpiredSnapshots result))
          ++ " snapshots:"
  mapM_ (\s -> putStrLn $ "  snapshot " ++ show (I.snapId s)
                       ++ " ts=" ++ show (I.snapTimestampMs s)
                       ++ " manifest_list=" ++ T.unpack (I.snapManifestList s))
        (M.exExpiredSnapshots result)

parseExpiryArgs :: [String] -> M.ExpiryPolicy
parseExpiryArgs = go M.defaultExpiryPolicy
  where
    go p ("--max-age" : v : rest) =
      go p { M.epMaxAgeMs = Just (read v) } rest
    go p ("--min" : v : rest) =
      go p { M.epMinSnapshots = read v } rest
    go p [] = p
    go p (_ : rest) = go p rest

cmdOrphans :: FilePath -> FilePath -> IO ()
cmdOrphans metaPath listingPath = do
  bs <- BS.readFile metaPath
  tm <- case Aeson.eitherDecodeStrict bs >>= IJ.metadataFromJSON of
    Right t -> pure t
    Left e  -> die ("orphans: " ++ e)
  listing <- BSC.lines <$> BS.readFile listingPath
  let allPaths = Set.fromList (map (T.pack . BSC.unpack) listing)
      orphans = M.orphanFileCandidates tm allPaths
  mapM_ T.putStrLn (Set.toList orphans)

-- ============================================================
-- REST commands (only when -frest-client is on)
-- ============================================================

#ifdef HAVE_REST_CLIENT
cmdRestListNs :: String -> [String] -> IO ()
cmdRestListNs url rest = do
  cc <- Client.mkClient (BSC.pack url) Nothing (parseAuth rest)
  ns <- Client.listNamespaces cc
  V.forM_ ns $ \nsv -> T.putStrLn (T.intercalate "." (V.toList nsv))

cmdRestLoadTable :: String -> String -> String -> [String] -> IO ()
cmdRestLoadTable url ns name rest = do
  cc <- Client.mkClient (BSC.pack url) Nothing (parseAuth rest)
  let ti = REST.TableIdentifier
            (V.fromList (map T.pack (split '.' ns)))
            (T.pack name)
  result <- Client.loadTable cc ti
  BL.putStr (AesonP.encodePretty
               (IJ.metadataToJSON (REST.ltrMetadata result)))
  putStrLn ""

parseAuth :: [String] -> Client.AuthHeader
parseAuth ("--token" : t : _) = Client.BearerToken (BSC.pack t)
parseAuth (_ : rest) = parseAuth rest
parseAuth [] = Client.NoAuth

split :: Char -> String -> [String]
split c s = case break (== c) s of
  (chunk, "")         -> [chunk]
  (chunk, _ : rest_)  -> chunk : split c rest_
#endif
