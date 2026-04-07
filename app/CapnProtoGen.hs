module Main where

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import CapnProto.CodeGen (generateCapnProtoTypes)
import CapnProto.Parser (parseCapnProto)

data Opts = Opts
  { optInput  :: FilePath
  , optOutput :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> header "wireform-capnp-gen — Cap'n Proto code generator for Haskell"
   <> progDesc "Generate Haskell types from .capnp files"
    )
  src <- TIO.readFile (optInput opts)
  case parseCapnProto src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> do
      let code = generateCapnProtoTypes schema
      case optOutput opts of
        Nothing -> TIO.putStr code
        Just dir -> do
          let outFile = dir </> "Generated" <.> "hs"
          createDirectoryIfMissing True (takeDirectory outFile)
          TIO.writeFile outFile code
          hPutStrLn stderr ("Wrote " <> outFile)

optsParser :: Parser Opts
optsParser = Opts
  <$> strOption
    ( long "input" <> short 'i' <> metavar "FILE"
   <> help "Input .capnp file"
    )
  <*> optional (strOption
    ( long "output" <> short 'o' <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
