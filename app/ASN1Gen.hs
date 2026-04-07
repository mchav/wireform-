module Main where

import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import ASN1.CodeGen (generateASN1Types)
import ASN1.Parser (parseASN1Module)

data Opts = Opts
  { optInput  :: FilePath
  , optOutput :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> header "wireform-asn1-gen — ASN.1 code generator for Haskell"
   <> progDesc "Generate Haskell types from ASN.1 module definition files"
    )
  src <- TIO.readFile (optInput opts)
  case parseASN1Module src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right modl -> do
      let code = generateASN1Types modl
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
   <> help "Input ASN.1 module definition file"
    )
  <*> optional (strOption
    ( long "output" <> short 'o' <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
