module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, takeExtension, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Avro.CodeGen (generateAvroTypes)
import Avro.IDL (parseAvroIDL, AvroIDL(..))
import Avro.IDLConvert (idlToType)
import Avro.Schema.Parse (parseAvroSchemaFile)
import qualified Data.Vector as V

data Opts = Opts
  { optInput  :: FilePath
  , optOutput :: Maybe FilePath
  }

main :: IO ()
main = do
  opts <- execParser $ info (optsParser <**> helper)
    ( fullDesc
   <> header "wireform-avro-gen — Avro code generator for Haskell"
   <> progDesc "Generate Haskell types from .avdl or .avsc files"
    )
  let ext = takeExtension (optInput opts)
  code <- case ext of
    ".avdl" -> do
      src <- TIO.readFile (optInput opts)
      case parseAvroIDL src of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right idl ->
          let types = map idlToType (V.toList (aidlDeclarations idl))
          in pure (T.intercalate "\n\n" (map generateAvroTypes types))
    _ -> do
      result <- parseAvroSchemaFile (optInput opts)
      case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right schema -> pure (generateAvroTypes schema)
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
   <> help "Input .avdl or .avsc file"
    )
  <*> optional (strOption
    ( long "output" <> short 'o' <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
