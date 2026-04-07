module Main where

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Bond.CodeGen (generateBondTypes)
import Bond.Parser (parseBond)

data Opts = Opts
  { optInput  :: FilePath
  , optOutput :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> header "wireform-bond-gen — Bond code generator for Haskell"
   <> progDesc "Generate Haskell types from .bond files"
    )
  src <- TIO.readFile (optInput opts)
  case parseBond src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> do
      let code = generateBondTypes schema
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
   <> help "Input .bond file"
    )
  <*> optional (strOption
    ( long "output" <> short 'o' <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
