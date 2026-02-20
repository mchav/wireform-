-- | hs-proto code generator CLI.
--
-- Usage: hs-proto-gen [OPTIONS] INPUT.proto
--
-- Options:
--   -I DIR, --include DIR    Add import search directory (repeatable)
--   --module-prefix PREFIX   Module prefix (default: Proto.Gen)
--   --lazy-submessages       Enable lazy submessage decoding
--   --help                   Show this help
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Proto.Parser.Resolver
import Proto.CodeGen (generateModuleText, defaultGenerateOpts, GenerateOpts(..))

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitFailure
    Right (opts, includeDirs, inputFile) -> do
      result <- resolveProtoImports includeDirs inputFile
      case result of
        Left (ParseError path msg) -> do
          hPutStrLn stderr ("Parse error in " <> path <> ": " <> msg)
          exitFailure
        Left (FileNotFound _ importPath searched) -> do
          hPutStrLn stderr ("Import not found: " <> T.unpack importPath)
          hPutStrLn stderr ("Searched directories:")
          mapM_ (\d -> hPutStrLn stderr ("  " <> d)) searched
          exitFailure
        Left (CircularImport chain) -> do
          hPutStrLn stderr ("Circular import detected: " <> show chain)
          exitFailure
        Right resolved -> do
          let code = generateModuleText opts (rpFile resolved)
          TIO.putStr code

usage :: String
usage = unlines
  [ "Usage: hs-proto-gen [OPTIONS] INPUT.proto"
  , ""
  , "Options:"
  , "  -I DIR, --include DIR    Add import search directory (repeatable)"
  , "  --module-prefix PREFIX   Module prefix (default: Proto.Gen)"
  , "  --lazy-submessages       Enable lazy submessage decoding"
  , "  --help                   Show this help"
  , ""
  , "The proto/ directory with google/protobuf well-known types is"
  , "automatically included in the search path."
  ]

parseArgs :: [String] -> Either String (GenerateOpts, [FilePath], FilePath)
parseArgs = go defaultGenerateOpts []
  where
    go _opts _incs [] = Left "No input file specified"
    go _opts _incs ["--help"] = Left ""
    go opts incs [f] = Right (opts, reverse incs, f)
    go opts incs ("--module-prefix" : prefix : rest) =
      go (opts { genModulePrefix = T.pack prefix }) incs rest
    go opts incs ("--lazy-submessages" : rest) =
      go (opts { genLazySubmessages = True }) incs rest
    go opts incs ("-I" : dir : rest) =
      go opts (dir : incs) rest
    go opts incs ("--include" : dir : rest) =
      go opts (dir : incs) rest
    go opts incs (arg : rest)
      | take 2 arg == "-I" =
          go opts (drop 2 arg : incs) rest
    go _ _ (unknown : _) = Left ("Unknown option: " <> unknown)
