-- | Cabal Setup.hs hook for automatic protobuf code generation.
--
-- == Basic usage
--
-- @
-- -- Setup.hs
-- import Distribution.Simple
-- import Proto.Setup
--
-- main :: IO ()
-- main = defaultMainWithHooks simpleUserHooks
--   { preBuild = \\args flags -> do
--       protoGenPreBuildHook defaultProtoGenConfig
--       preBuild simpleUserHooks args flags
--   }
-- @
--
-- Then in your @.cabal@ file:
--
-- @
-- build-type: Custom
--
-- custom-setup
--   setup-depends: base, hs-proto, Cabal, directory, filepath, text
--
-- library
--   hs-source-dirs: src, gen
-- @
--
-- == With codegen hooks
--
-- Register hooks via 'pgcHooks' to produce extra code based on proto attributes:
--
-- @
-- import Proto.Setup
-- import Proto.CodeGen.Hooks
--
-- main :: IO ()
-- main = defaultMainWithHooks simpleUserHooks
--   { preBuild = \\args flags -> do
--       protoGenPreBuildHook defaultProtoGenConfig
--         { pgcHooks = onMessageAttribute "audited" $ \\val ctx ->
--             case val of
--               CBool True -> ["-- audited: " \<> mhcHsTypeName ctx]
--               _          -> []
--         }
--       preBuild simpleUserHooks args flags
--   }
-- @
module Proto.Setup
  ( ProtoGenConfig (..)
  , defaultProtoGenConfig
  , protoGenPreBuildHook
  , generateProtos
  , generateProtoFile
  ) where

import Control.Exception (catch, IOException)
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist,
  getModificationTime, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), (<.>), takeDirectory, takeExtension)

import Proto.AST (ProtoFile(..))
import Proto.Parser (parseProtoFile)
import Proto.CodeGen (generateModuleText, defaultGenerateOpts, GenerateOpts(..),
                      TypeRegistry, hsModuleName, moduleNameForProto)
import Proto.CodeGen.Hooks (CodeGenHooks, defaultCodeGenHooks)
import qualified Data.Map.Strict as Map

-- | Configuration for automatic protobuf code generation.
--
-- Use 'pgcHooks' to register codegen hooks that fire based on proto attributes:
--
-- @
-- import Proto.Setup
-- import Proto.CodeGen.Hooks
--
-- myConfig :: 'ProtoGenConfig'
-- myConfig = 'defaultProtoGenConfig'
--   { 'pgcHooks' = myHooks
--   }
-- @
data ProtoGenConfig = ProtoGenConfig
  { pgcProtoDir     :: FilePath
  , pgcIncludeDirs  :: [FilePath]
  , pgcOutputDir    :: FilePath
  , pgcModulePrefix :: T.Text
  , pgcLazySub      :: Bool
  , pgcHooks        :: CodeGenHooks
  }

defaultProtoGenConfig :: ProtoGenConfig
defaultProtoGenConfig = ProtoGenConfig
  { pgcProtoDir    = "proto"
  , pgcIncludeDirs = []
  , pgcOutputDir   = "gen"
  , pgcModulePrefix = "Proto.Gen"
  , pgcLazySub     = False
  , pgcHooks       = defaultCodeGenHooks
  }

-- | Pre-build hook: generate Haskell from all .proto files in pgcProtoDir.
-- Only regenerates when source is newer than output.
protoGenPreBuildHook :: ProtoGenConfig -> IO ()
protoGenPreBuildHook = generateProtos

-- | Find and generate all .proto files.
generateProtos :: ProtoGenConfig -> IO ()
generateProtos cfg = do
  let protoDir = pgcProtoDir cfg
  exists <- doesDirectoryExist protoDir
  if not exists
    then putStrLn $ "[hs-proto] Proto directory not found: " <> protoDir
    else do
      protos <- findProtoFiles protoDir
      unless (null protos) $
        putStrLn $ "[hs-proto] Found " <> show (length protos) <> " .proto file(s) in " <> protoDir
      forM_ protos $ \relPath ->
        generateProtoFile cfg (protoDir </> relPath)

-- | Generate Haskell for one .proto file. Skips if output is up-to-date.
generateProtoFile :: ProtoGenConfig -> FilePath -> IO ()
generateProtoFile cfg protoPath = do
  contents <- TIO.readFile protoPath
  case parseProtoFile protoPath contents of
    Left err ->
      putStrLn $ "[hs-proto] Parse error in " <> protoPath <> ": " <> show err
    Right pf -> do
      let opts = defaultGenerateOpts
            { genModulePrefix    = pgcModulePrefix cfg
            , genLazySubmessages = pgcLazySub cfg
            , genHooks           = pgcHooks cfg
            }
          emptyReg = Map.empty :: TypeRegistry
          code = generateModuleText opts emptyReg protoPath pf
          outPath = pgcOutputDir cfg </> moduleToPath opts protoPath pf <.> "hs"
      needsRegen <- checkStale protoPath outPath
      when needsRegen $ do
        createDirectoryIfMissing True (takeDirectory outPath)
        TIO.writeFile outPath code
        putStrLn $ "[hs-proto] Generated " <> outPath

findProtoFiles :: FilePath -> IO [FilePath]
findProtoFiles root = go ""
  where
    go prefix = do
      let dir = root </> prefix
      entries <- listDirectory dir `catch` (\(_ :: IOException) -> pure [])
      fmap concat $ forM entries $ \entry -> do
        let rel = if null prefix then entry else prefix </> entry
            full = root </> rel
        isDir <- doesDirectoryExist full
        if isDir
          then go rel
          else pure [rel | takeExtension entry == ".proto"]

checkStale :: FilePath -> FilePath -> IO Bool
checkStale src out = do
  outExists <- doesFileExist out
  if not outExists
    then pure True
    else do
      srcT <- getModificationTime src
      outT <- getModificationTime out
      pure (srcT > outT)

moduleToPath :: GenerateOpts -> FilePath -> ProtoFile -> FilePath
moduleToPath opts fp pf =
  let modName = T.unpack (moduleNameForProto opts fp pf)
  in fmap (\c -> if c == '.' then '/' else c) modName
