-- | hs-proto code generator CLI.
--
-- Usage: hs-proto-gen [--module-prefix PREFIX] [--lazy-submessages] INPUT.proto
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Proto.Parser (parseProtoFile)
import Proto.CodeGen (generateModuleText, defaultGenerateOpts, GenerateOpts(..))

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> do
      hPutStrLn stderr err
      hPutStrLn stderr usage
      exitFailure
    Right (opts, inputFile) -> do
      contents <- TIO.readFile inputFile
      case parseProtoFile inputFile contents of
        Left err -> do
          hPutStrLn stderr ("Parse error: " <> show err)
          exitFailure
        Right protoFile -> do
          let code = generateModuleText opts protoFile
          TIO.putStr code

usage :: String
usage = unlines
  [ "Usage: hs-proto-gen [OPTIONS] INPUT.proto"
  , ""
  , "Options:"
  , "  --module-prefix PREFIX   Module prefix (default: Proto.Gen)"
  , "  --lazy-submessages       Enable lazy submessage decoding"
  , "  --help                   Show this help"
  ]

parseArgs :: [String] -> Either String (GenerateOpts, FilePath)
parseArgs = go defaultGenerateOpts
  where
    go _opts [] = Left "No input file specified"
    go _opts ["--help"] = Left ""
    go opts [f] = Right (opts, f)
    go opts ("--module-prefix" : prefix : rest) =
      go (opts { genModulePrefix = T.pack prefix }) rest
    go opts ("--lazy-submessages" : rest) =
      go (opts { genLazySubmessages = True }) rest
    go _ (unknown : _) = Left ("Unknown option: " <> unknown)
