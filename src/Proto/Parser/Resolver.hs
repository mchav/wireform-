-- | Import resolution for .proto files with include directory support.
--
-- When a proto file contains @import "google/protobuf/timestamp.proto"@,
-- the resolver searches the include directories in order to find the
-- file, then parses it and transitively resolves its imports.
module Proto.Parser.Resolver
  ( -- * Configuration
    ResolveConfig (..)
  , defaultResolveConfig

    -- * Resolution
  , resolveProtoFile
  , resolveProtoImports
  , ResolvedProto (..)
  , ResolveError (..)

    -- * Bundled well-known types
  , bundledIncludeDir
  ) where

import Control.Monad (foldM)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeDirectory)

import Proto.AST
import Proto.Parser (parseProtoFile)

-- | Configuration for proto file resolution.
data ResolveConfig = ResolveConfig
  { rcIncludeDirs :: ![FilePath]
    -- ^ Directories to search for imported .proto files, in order.
    -- The directory containing the importing file is always searched first.
  , rcBundledDir  :: !(Maybe FilePath)
    -- ^ Path to bundled well-known proto files (google/protobuf/*.proto).
    -- If Nothing, uses the built-in bundled protos from the package.
  } deriving stock (Show, Eq)

defaultResolveConfig :: ResolveConfig
defaultResolveConfig = ResolveConfig
  { rcIncludeDirs = []
  , rcBundledDir  = Nothing
  }

-- | A fully resolved proto file with all its transitive imports.
data ResolvedProto = ResolvedProto
  { rpFile    :: !ProtoFile
  , rpPath    :: !FilePath
  , rpImports :: !(Map Text ResolvedProto)
  } deriving stock (Show)

data ResolveError
  = ParseError !FilePath !String
  | FileNotFound !FilePath !Text ![FilePath]
  | CircularImport ![Text]
  deriving stock (Show, Eq)

-- | The path to bundled well-known protobuf definitions shipped with this package.
-- This should be set to the data-dir path at install time.
-- For development, it defaults to the proto/ directory in the repo root.
bundledIncludeDir :: FilePath
bundledIncludeDir = "proto"

-- | Resolve a proto file and all its transitive imports.
resolveProtoFile
  :: ResolveConfig
  -> FilePath
  -> IO (Either ResolveError ResolvedProto)
resolveProtoFile cfg path = do
  let cfg' = cfg { rcIncludeDirs = takeDirectory path : rcIncludeDirs cfg <> bundledDirs cfg }
  resolve cfg' Map.empty [] path

bundledDirs :: ResolveConfig -> [FilePath]
bundledDirs cfg = case rcBundledDir cfg of
  Just d  -> [d]
  Nothing -> [bundledIncludeDir]

resolve
  :: ResolveConfig
  -> Map Text ResolvedProto
  -> [Text]
  -> FilePath
  -> IO (Either ResolveError ResolvedProto)
resolve cfg cache visiting path = do
  let pathT = T.pack path
  if pathT `elem` visiting
    then pure (Left (CircularImport (reverse (pathT : visiting))))
    else case Map.lookup pathT cache of
      Just resolved -> pure (Right resolved)
      Nothing -> do
        contents <- TIO.readFile path
        case parseProtoFile path contents of
          Left e -> pure (Left (ParseError path (show e)))
          Right pf -> do
            let imports = protoImports pf
            result <- foldM (resolveImport cfg (pathT : visiting)) (Right Map.empty) imports
            case result of
              Left e -> pure (Left e)
              Right importMap -> do
                let resolved = ResolvedProto
                      { rpFile    = pf
                      , rpPath    = path
                      , rpImports = importMap
                      }
                pure (Right resolved)

resolveImport
  :: ResolveConfig
  -> [Text]
  -> Either ResolveError (Map Text ResolvedProto)
  -> ImportDef
  -> IO (Either ResolveError (Map Text ResolvedProto))
resolveImport _ _ (Left e) _ = pure (Left e)
resolveImport cfg visiting (Right acc) imp = do
  let importPath = T.unpack (Proto.AST.importPath imp)
  found <- findFile (rcIncludeDirs cfg) importPath
  case found of
    Nothing -> pure (Left (FileNotFound importPath (Proto.AST.importPath imp) (rcIncludeDirs cfg)))
    Just fullPath -> do
      result <- resolve cfg acc visiting fullPath
      case result of
        Left e -> pure (Left e)
        Right resolved -> pure (Right (Map.insert (Proto.AST.importPath imp) resolved acc))

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = pure Nothing
findFile (dir:dirs) rel = do
  let full = dir </> rel
  exists <- doesFileExist full
  if exists
    then pure (Just full)
    else findFile dirs rel

-- | Resolve all imports from a proto file, returning the full import graph.
resolveProtoImports
  :: [FilePath]  -- ^ Include directories
  -> FilePath    -- ^ Proto file path
  -> IO (Either ResolveError ResolvedProto)
resolveProtoImports includeDirs path =
  resolveProtoFile (defaultResolveConfig { rcIncludeDirs = includeDirs }) path
