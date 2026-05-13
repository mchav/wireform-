{- | Regenerate the Lance protobuf modules from the bundled
@proto/lance/{file,table}.proto@ files.

Run: @cabal run wireform-lance:gen-lance-pb@

This overwrites @src/Lance/Pb/Lance/{File,Table}.hs@ with
freshly-generated code, ensuring the checked-in modules
always match the codegen output. Same pattern as
@cabal run gen-wkt@ for the well-known types.
-}
module Main (main) where

import Control.Monad (forM, forM_)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Proto.CodeGen
import Proto.IDL.Parser.Resolver
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO (hPutStrLn, stderr)


protoDir :: FilePath
protoDir = "wireform-lance/proto"


outputDir :: FilePath
outputDir = "wireform-lance/src"


protoFiles :: [FilePath]
protoFiles =
  [ "lance/file.proto"
  , "lance/file2.proto"
  , "lance/table.proto"
  ]


opts :: GenerateOpts
opts =
  defaultGenerateOpts
    { genModulePrefix = "Lance.Pb"
    }


main :: IO ()
main = do
  resolved <- forM protoFiles $ \rel -> do
    let full = protoDir </> rel
    -- Look up imports under both the lance proto dir and the
    -- top-level wireform proto dir, so that 'file.proto' resolves
    -- locally and 'google/protobuf/{any,timestamp}.proto' resolves
    -- to the wireform-bundled WKTs.
    result <- resolveProtoImports [protoDir, protoDir </> "lance", "proto"] full
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
