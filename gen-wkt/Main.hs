-- | Regenerate the well-known type modules from the bundled .proto files.
--
-- Run from the workspace root:
--
-- @
-- cabal run gen-wkt
-- @
--
-- This overwrites @wireform-proto\/src\/Proto\/Google\/Protobuf\/*.hs@
-- with freshly-generated code, ensuring the checked-in modules always
-- match the codegen output.
module Main (main) where

import Control.Monad (forM, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)

import Proto.Parser.Resolver
import Proto.CodeGen

protoDir :: FilePath
protoDir = "wireform-proto" </> "data" </> "proto"

outputDir :: FilePath
outputDir = "wireform-proto" </> "src"

protoFiles :: [FilePath]
protoFiles =
  [ "google/protobuf/any.proto"
  , "google/protobuf/duration.proto"
  , "google/protobuf/empty.proto"
  , "google/protobuf/field_mask.proto"
  , "google/protobuf/source_context.proto"
  , "google/protobuf/struct.proto"
  , "google/protobuf/timestamp.proto"
  , "google/protobuf/wrappers.proto"
  ]

opts :: GenerateOpts
opts = defaultGenerateOpts
  { genModulePrefix = "Proto"
  }

main :: IO ()
main = do
  resolved <- forM protoFiles $ \rel -> do
    let full = protoDir </> rel
    result <- resolveProtoImports [protoDir] full
    case result of
      Left err -> error ("Failed to resolve " <> full <> ": " <> show err)
      Right rp -> pure (rel, rp)

  let registry = buildTypeRegistry opts resolved

  forM_ resolved $ \(rel, rp) -> do
    let code = generateModuleText opts registry rel (rpFile rp)
        modName = T.unpack (moduleNameForProto opts rel (rpFile rp))
        modPath = fmap (\c -> if c == '.' then '/' else c) modName
        outFile = outputDir </> modPath <.> "hs"
    createDirectoryIfMissing True (takeDirectory outFile)
    TIO.writeFile outFile code
    hPutStrLn stderr ("Wrote " <> outFile)
