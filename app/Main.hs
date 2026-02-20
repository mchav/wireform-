{-# LANGUAGE ApplicativeDo #-}
-- | hs-proto code generator CLI.
--
-- Usage:
--
-- @
-- hs-proto-gen [OPTIONS] INPUT.proto [INPUT2.proto ...]
--
--   -I, --include DIR         Add import search directory (repeatable)
--   -o, --out DIR             Output directory for generated files (default: .)
--       --module-prefix PFX   Haskell module prefix (default: Proto.Gen)
--       --lazy-submessages    Enable lazy submessage decoding
--       --print               Print the proto file back (exact print)
--       --summary             Print a structural summary
-- @
module Main where

import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Proto.Parser.Resolver
import Proto.CodeGen (generateModuleText, defaultGenerateOpts, GenerateOpts(..))
import Proto.AST (ProtoFile(..))
import Proto.CodeGen.Types (hsModuleName)
import Proto.Print (printProtoFile)
import Proto.Inspect (summarize, prettyPrintSummary)

data Cmd
  = CmdGenerate GenOpts
  | CmdPrint    PrintOpts
  | CmdSummary  SummaryOpts

data GenOpts = GenOpts
  { goIncludeDirs     :: [FilePath]
  , goOutputDir       :: FilePath
  , goModulePrefix    :: String
  , goLazySubmessages :: Bool
  , goInputFiles      :: [FilePath]
  }

data PrintOpts = PrintOpts
  { poIncludeDirs :: [FilePath]
  , poInputFile   :: FilePath
  }

data SummaryOpts = SummaryOpts
  { soIncludeDirs :: [FilePath]
  , soInputFile   :: FilePath
  }

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdGenerate go -> runGenerate go
    CmdPrint po    -> runPrint po
    CmdSummary so  -> runSummary so
  where
    opts = info (cmdParser <**> helper)
      ( fullDesc
     <> header "hs-proto-gen — Protocol Buffers code generator for Haskell"
     <> progDesc "Generate Haskell types and instances from .proto files"
      )

cmdParser :: Parser Cmd
cmdParser = subparser
  ( command "generate" (info (CmdGenerate <$> genOptsParser)
      (progDesc "Generate Haskell code from .proto files"))
  <> command "print" (info (CmdPrint <$> printOptsParser)
      (progDesc "Parse and exact-print a .proto file"))
  <> command "summary" (info (CmdSummary <$> summaryOptsParser)
      (progDesc "Print a structural summary of a .proto file"))
  )
  <|> (CmdGenerate <$> genOptsParser)  -- default: generate

genOptsParser :: Parser GenOpts
genOptsParser = do
  includeDirs <- many $ strOption
    ( short 'I'
   <> long "include"
   <> metavar "DIR"
   <> help "Add import search directory (repeatable)"
    )
  outputDir <- strOption
    ( short 'o'
   <> long "out"
   <> metavar "DIR"
   <> value "."
   <> showDefault
   <> help "Output directory for generated Haskell files"
    )
  modulePrefix <- strOption
    ( long "module-prefix"
   <> metavar "PREFIX"
   <> value "Proto.Gen"
   <> showDefault
   <> help "Haskell module name prefix"
    )
  lazySub <- switch
    ( long "lazy-submessages"
   <> help "Enable lazy submessage decoding"
    )
  inputFiles <- some (argument str (metavar "FILES..." <> help ".proto input files"))
  pure GenOpts
    { goIncludeDirs     = includeDirs
    , goOutputDir       = outputDir
    , goModulePrefix    = modulePrefix
    , goLazySubmessages = lazySub
    , goInputFiles      = inputFiles
    }

printOptsParser :: Parser PrintOpts
printOptsParser = PrintOpts
  <$> many (strOption (short 'I' <> long "include" <> metavar "DIR"))
  <*> argument str (metavar "FILE" <> help ".proto file to print")

summaryOptsParser :: Parser SummaryOpts
summaryOptsParser = SummaryOpts
  <$> many (strOption (short 'I' <> long "include" <> metavar "DIR"))
  <*> argument str (metavar "FILE" <> help ".proto file to summarize")

runGenerate :: GenOpts -> IO ()
runGenerate go = forM_ (goInputFiles go) $ \inputFile -> do
  result <- resolveProtoImports (goIncludeDirs go) inputFile
  case result of
    Left err -> do
      hPutStrLn stderr (showResolveError err)
      exitFailure
    Right resolved -> do
      let opts = defaultGenerateOpts
            { genModulePrefix    = T.pack (goModulePrefix go)
            , genLazySubmessages = goLazySubmessages go
            }
      let code = generateModuleText opts (rpFile resolved)
      if goOutputDir go == "-"
        then TIO.putStr code
        else do
          let modPath = modulePathFromProto opts (rpFile resolved)
              outFile = goOutputDir go </> modPath <.> "hs"
          createDirectoryIfMissing True (takeDirectory outFile)
          TIO.writeFile outFile code
          hPutStrLn stderr ("Wrote " <> outFile)

runPrint :: PrintOpts -> IO ()
runPrint po = do
  result <- resolveProtoImports (poIncludeDirs po) (poInputFile po)
  case result of
    Left err -> hPutStrLn stderr (showResolveError err) >> exitFailure
    Right resolved -> TIO.putStr (printProtoFile (rpFile resolved))

runSummary :: SummaryOpts -> IO ()
runSummary so = do
  result <- resolveProtoImports (soIncludeDirs so) (soInputFile so)
  case result of
    Left err -> hPutStrLn stderr (showResolveError err) >> exitFailure
    Right resolved -> TIO.putStr (prettyPrintSummary (summarize (rpFile resolved)))

showResolveError :: ResolveError -> String
showResolveError (ParseError path msg) = "Parse error in " <> path <> ": " <> msg
showResolveError (FileNotFound _ importPath searched) =
  "Import not found: " <> T.unpack importPath <> "\nSearched: " <> show searched
showResolveError (CircularImport chain) = "Circular import: " <> show chain

modulePathFromProto :: GenerateOpts -> ProtoFile -> FilePath
modulePathFromProto opts pf =
  let prefix = T.unpack (genModulePrefix opts)
      pkg = maybe "Generated" (T.unpack . hsModuleName) (protoPackage pf)
      modName = prefix <> "." <> pkg
  in fmap dotToSlash modName
  where
    dotToSlash '.' = '/'
    dotToSlash c   = c

