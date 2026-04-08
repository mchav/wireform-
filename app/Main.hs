{-# LANGUAGE ApplicativeDo #-}
module Main where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory, takeExtension, (<.>))
import System.IO (hPutStrLn, stderr)

import Options.Applicative

import Proto.Parser.Resolver
import Proto.CodeGen (generateModuleText, defaultGenerateOpts, GenerateOpts(..),
                      buildTypeRegistry, moduleNameForProto)
import Proto.AST (ProtoFile(..))
import Proto.Print (printProtoFile)
import Proto.Inspect (summarize, prettyPrintSummary)

import Avro.CodeGen (generateAvroTypes)
import Avro.IDL (parseAvroIDL, AvroIDL(..))
import Avro.IDLConvert (idlToType)
import Avro.Schema.Parse (parseAvroSchemaFile)

import Thrift.CodeGen (generateThriftTypes)
import Thrift.Parser (parseThrift)

import Bond.CodeGen (generateBondTypes)
import Bond.Parser (parseBond)

import CapnProto.CodeGen (generateCapnProtoTypes)
import CapnProto.Parser (parseCapnProto)

import FlatBuffers.CodeGen (generateFlatBuffersTypes)
import FlatBuffers.Parser (parseFlatBuffers)

import ASN1.CodeGen (generateASN1Types)
import ASN1.Parser (parseASN1Module)

-- Top-level command dispatch
data Command
  = CmdProto ProtoCmd
  | CmdAvro AvroOpts
  | CmdThrift GenOpts
  | CmdBond GenOpts
  | CmdCapnProto GenOpts
  | CmdFlatBuffers GenOpts
  | CmdASN1 GenOpts

-- Proto has sub-subcommands to preserve generate/print/summary
data ProtoCmd
  = ProtoGenerate ProtoOpts
  | ProtoPrint ProtoPrintOpts
  | ProtoSummary ProtoPrintOpts

data ProtoOpts = ProtoOpts
  { poInput    :: !FilePath
  , poOutput   :: !(Maybe FilePath)
  , poModule   :: !(Maybe T.Text)
  , poIncludes :: ![FilePath]
  }

data ProtoPrintOpts = ProtoPrintOpts
  { ppoInput    :: !FilePath
  , ppoIncludes :: ![FilePath]
  }

data AvroOpts = AvroOpts
  { aoInput  :: !FilePath
  , aoOutput :: !(Maybe FilePath)
  , aoModule :: !(Maybe T.Text)
  , aoFormat :: !(Maybe T.Text)
  }

data GenOpts = GenOpts
  { goInput  :: !FilePath
  , goOutput :: !(Maybe FilePath)
  , goModule :: !(Maybe T.Text)
  }

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    CmdProto pc       -> runProtoCmd pc
    CmdAvro ao        -> runAvro ao
    CmdThrift go      -> runThrift go
    CmdBond go        -> runBond go
    CmdCapnProto go   -> runCapnProto go
    CmdFlatBuffers go -> runFlatBuffers go
    CmdASN1 go        -> runASN1 go
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> header "wireform-gen — code generator for multiple serialization formats"
     <> progDesc "Generate Haskell types and instances from schema files"
      )

commandParser :: Parser Command
commandParser = subparser
  ( command "proto" (info (CmdProto <$> protoCmdParser <**> helper)
      (progDesc "Generate Haskell from .proto files"))
  <> command "avro" (info (CmdAvro <$> avroOptsParser <**> helper)
      (progDesc "Generate Haskell from .avsc or .avdl files"))
  <> command "thrift" (info (CmdThrift <$> genOptsParser <**> helper)
      (progDesc "Generate Haskell from .thrift files"))
  <> command "bond" (info (CmdBond <$> genOptsParser <**> helper)
      (progDesc "Generate Haskell from .bond files"))
  <> command "capnp" (info (CmdCapnProto <$> genOptsParser <**> helper)
      (progDesc "Generate Haskell from .capnp files"))
  <> command "fbs" (info (CmdFlatBuffers <$> genOptsParser <**> helper)
      (progDesc "Generate Haskell from .fbs files"))
  <> command "asn1" (info (CmdASN1 <$> genOptsParser <**> helper)
      (progDesc "Generate Haskell from ASN.1 module definitions"))
  )

-- Shared options for simple formats (thrift, bond, capnp, fbs, asn1)
genOptsParser :: Parser GenOpts
genOptsParser = GenOpts
  <$> strOption
    ( short 'i' <> long "input" <> metavar "FILE"
   <> help "Input schema file"
    )
  <*> optional (strOption
    ( short 'o' <> long "output" <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
  <*> optional (strOption
    ( short 'm' <> long "module" <> metavar "PREFIX"
   <> help "Module name prefix"
    ))

-- Proto: sub-subcommands (generate is the default)
protoCmdParser :: Parser ProtoCmd
protoCmdParser = subparser
  ( command "generate" (info (ProtoGenerate <$> protoOptsParser)
      (progDesc "Generate Haskell code from .proto files"))
  <> command "print" (info (ProtoPrint <$> protoPrintOptsParser)
      (progDesc "Parse and exact-print a .proto file"))
  <> command "summary" (info (ProtoSummary <$> protoPrintOptsParser)
      (progDesc "Print a structural summary of a .proto file"))
  )
  <|> (ProtoGenerate <$> protoOptsParser)

protoOptsParser :: Parser ProtoOpts
protoOptsParser = do
  includes <- many $ strOption
    ( short 'I' <> long "include" <> metavar "DIR"
   <> help "Proto include path (can repeat)"
    )
  input <- strOption
    ( short 'i' <> long "input" <> metavar "FILE"
   <> help "Input .proto file"
    )
  output <- optional $ strOption
    ( short 'o' <> long "output" <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    )
  modPrefix <- optional $ strOption
    ( short 'm' <> long "module" <> metavar "PREFIX"
   <> help "Module name prefix (default: Proto.Gen)"
    )
  pure ProtoOpts
    { poInput    = input
    , poOutput   = output
    , poModule   = modPrefix
    , poIncludes = includes
    }

protoPrintOptsParser :: Parser ProtoPrintOpts
protoPrintOptsParser = ProtoPrintOpts
  <$> strOption
    ( short 'i' <> long "input" <> metavar "FILE"
   <> help "Input .proto file"
    )
  <*> many (strOption
    ( short 'I' <> long "include" <> metavar "DIR"
   <> help "Proto include path (can repeat)"
    ))

avroOptsParser :: Parser AvroOpts
avroOptsParser = AvroOpts
  <$> strOption
    ( short 'i' <> long "input" <> metavar "FILE"
   <> help "Input .avsc or .avdl file"
    )
  <*> optional (strOption
    ( short 'o' <> long "output" <> metavar "DIR"
   <> help "Output directory (default: stdout)"
    ))
  <*> optional (strOption
    ( short 'm' <> long "module" <> metavar "PREFIX"
   <> help "Module name prefix"
    ))
  <*> optional (strOption
    ( long "format" <> metavar "avsc|avdl"
   <> help "Input format (default: auto-detect by extension)"
    ))

------------------------------------------------------------------------
-- Runners
------------------------------------------------------------------------

runProtoCmd :: ProtoCmd -> IO ()
runProtoCmd (ProtoGenerate po) = runProtoGenerate po
runProtoCmd (ProtoPrint ppo)   = runProtoPrint ppo
runProtoCmd (ProtoSummary ppo) = runProtoSummary ppo

runProtoGenerate :: ProtoOpts -> IO ()
runProtoGenerate po = do
  let prefix = maybe "Proto.Gen" T.unpack (poModule po)
      codegenOpts = defaultGenerateOpts
        { genModulePrefix = T.pack prefix }
  result <- resolveProtoImports (poIncludes po) (poInput po)
  case result of
    Left err -> do
      hPutStrLn stderr (showResolveError err)
      exitFailure
    Right resolved -> do
      let protoRelPath = stripIncludeDirs (poIncludes po) (poInput po)
          allResolved = (protoRelPath, resolved)
                      : collectTransitiveImports (poIncludes po) resolved
          registry = buildTypeRegistry codegenOpts allResolved
          code = generateModuleText codegenOpts registry protoRelPath (rpFile resolved)
      case poOutput po of
        Nothing  -> TIO.putStr code
        Just dir -> do
          let modPath = modulePathFromProto codegenOpts protoRelPath (rpFile resolved)
              outFile = dir </> modPath <.> "hs"
          createDirectoryIfMissing True (takeDirectory outFile)
          TIO.writeFile outFile code
          hPutStrLn stderr ("Wrote " <> outFile)

runProtoPrint :: ProtoPrintOpts -> IO ()
runProtoPrint ppo = do
  result <- resolveProtoImports (ppoIncludes ppo) (ppoInput ppo)
  case result of
    Left err -> hPutStrLn stderr (showResolveError err) >> exitFailure
    Right resolved -> TIO.putStr (printProtoFile (rpFile resolved))

runProtoSummary :: ProtoPrintOpts -> IO ()
runProtoSummary ppo = do
  result <- resolveProtoImports (ppoIncludes ppo) (ppoInput ppo)
  case result of
    Left err -> hPutStrLn stderr (showResolveError err) >> exitFailure
    Right resolved -> TIO.putStr (prettyPrintSummary (summarize (rpFile resolved)))

runAvro :: AvroOpts -> IO ()
runAvro ao = do
  let fmt = case aoFormat ao of
              Just f  -> T.unpack f
              Nothing -> case takeExtension (aoInput ao) of
                           ".avdl" -> "avdl"
                           _       -> "avsc"
  code <- case fmt of
    "avdl" -> do
      src <- TIO.readFile (aoInput ao)
      case parseAvroIDL src of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right idl ->
          let types = map idlToType (V.toList (aidlDeclarations idl))
          in pure (T.intercalate "\n\n" (map generateAvroTypes types))
    _ -> do
      result <- parseAvroSchemaFile (aoInput ao)
      case result of
        Left err -> hPutStrLn stderr err >> exitFailure
        Right schema -> pure (generateAvroTypes schema)
  writeOutput (aoOutput ao) code

runThrift :: GenOpts -> IO ()
runThrift go = do
  src <- TIO.readFile (goInput go)
  case parseThrift src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> writeOutput (goOutput go) (generateThriftTypes schema)

runBond :: GenOpts -> IO ()
runBond go = do
  src <- TIO.readFile (goInput go)
  case parseBond src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> writeOutput (goOutput go) (generateBondTypes schema)

runCapnProto :: GenOpts -> IO ()
runCapnProto go = do
  src <- TIO.readFile (goInput go)
  case parseCapnProto src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> writeOutput (goOutput go) (generateCapnProtoTypes schema)

runFlatBuffers :: GenOpts -> IO ()
runFlatBuffers go = do
  src <- TIO.readFile (goInput go)
  case parseFlatBuffers src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right schema -> writeOutput (goOutput go) (generateFlatBuffersTypes schema)

runASN1 :: GenOpts -> IO ()
runASN1 go = do
  src <- TIO.readFile (goInput go)
  case parseASN1Module src of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right modl -> writeOutput (goOutput go) (generateASN1Types modl)

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

writeOutput :: Maybe FilePath -> T.Text -> IO ()
writeOutput Nothing code = TIO.putStr code
writeOutput (Just dir) code = do
  let outFile = dir </> "Generated" <.> "hs"
  createDirectoryIfMissing True (takeDirectory outFile)
  TIO.writeFile outFile code
  hPutStrLn stderr ("Wrote " <> outFile)

collectTransitiveImports :: [FilePath] -> ResolvedProto -> [(FilePath, ResolvedProto)]
collectTransitiveImports dirs rp =
  concatMap (\(_, imp) -> (stripIncludeDirs dirs (rpPath imp), imp)
                        : collectTransitiveImports dirs imp)
            (Map.toList (rpImports rp))

showResolveError :: ResolveError -> String
showResolveError (ParseError _path msg) = msg
showResolveError (FileNotFound _ importPath' searched) =
  "error: import not found: " <> T.unpack importPath'
  <> "\n  searched directories:\n"
  <> concatMap (\d -> "    - " <> d <> "\n") searched
showResolveError (CircularImport chain) =
  "error: circular import detected\n"
  <> "  import chain: " <> T.unpack (T.intercalate " -> " chain)

modulePathFromProto :: GenerateOpts -> FilePath -> ProtoFile -> FilePath
modulePathFromProto opts_ filePath pf =
  let modName = T.unpack (moduleNameForProto opts_ filePath pf)
  in fmap dotToSlash modName
  where
    dotToSlash '.' = '/'
    dotToSlash c   = c

stripIncludeDirs :: [FilePath] -> FilePath -> FilePath
stripIncludeDirs dirs fp =
  let t = T.pack fp
      attempts = fmap (\d -> T.stripPrefix (T.pack (d <> "/")) t) dirs
  in case [rest | Just rest <- attempts] of
    (r:_) -> T.unpack r
    []    -> fp
