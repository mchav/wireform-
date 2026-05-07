{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Kafka protocol code generator executable
Copyright   : (c) 2025
License     : BSD-3-Clause

This executable reads Kafka protocol JSON definitions and generates
Haskell source files for each message type.

Usage:

> kafka-codegen <protocol-dir> <output-dir>

Where:
  - protocol-dir: Directory containing Kafka protocol JSON files
  - output-dir: Directory where generated Haskell files will be written
-}
module Main (main) where

import Control.Exception (catch, SomeException)
import Control.Monad (forM_, when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Kafka.Protocol.Codegen.Generator
import Kafka.Protocol.Codegen.Parser
import Kafka.Protocol.Codegen.Types
import Prettyprinter
import Prettyprinter.Render.Text
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [protocolDir, outputDir] -> runCodegen protocolDir outputDir
    _ -> do
      hPutStrLn stderr "Usage: kafka-codegen <protocol-dir> <output-dir>"
      hPutStrLn stderr ""
      hPutStrLn stderr "Example:"
      hPutStrLn stderr "  kafka-codegen kafka/clients/src/main/resources/common/message src/Kafka/Protocol/Generated"
      exitFailure

runCodegen :: FilePath -> FilePath -> IO ()
runCodegen protocolDir outputDir = do
  putStrLn $ "Reading protocol definitions from: " ++ protocolDir
  putStrLn $ "Generating code to: " ++ outputDir
  putStrLn ""
  
  -- Parse all protocol files
  (schemas, errors) <- parseProtocolDirectory protocolDir
  
  -- Report parsing errors
  when (not $ null errors) $ do
    putStrLn "=== Parsing Errors ==="
    forM_ errors $ \(file, err) -> do
      putStrLn $ "Error in " ++ file ++ ":"
      putStrLn $ "  " ++ err
      putStrLn ""
  
  -- Generate code for each schema
  putStrLn $ "Successfully parsed " ++ show (length schemas) ++ " protocol definitions"
  putStrLn ""
  
  -- Create output directory and clean old generated files
  createDirectoryIfMissing True outputDir
  cleanGeneratedFiles outputDir
  
  forM_ schemas $ \schema -> do
    let moduleName = toHaskellModuleName (schemaName schema)
        fileName = T.unpack (schemaName schema) ++ ".hs"
        filePath = outputDir </> fileName
        code = generateMessageModule schema
        rendered = renderStrict (layoutPretty defaultLayoutOptions code)
    
    putStrLn $ "Generating " ++ fileName
    T.writeFile filePath rendered
  
  -- Generate message inventory JSON
  putStrLn ""
  putStrLn "Generating message inventory..."
  let inventoryJson = generateMessageInventory schemas
      inventoryPath = "message-inventory.json"
  T.writeFile inventoryPath inventoryJson
  putStrLn $ "Generated " ++ inventoryPath ++ " with " ++ show (length schemas) ++ " message types"
  
  putStrLn ""
  putStrLn $ "Code generation complete! Generated " ++ show (length schemas) ++ " modules."
  putStrLn ""
  putStrLn "Note: The generated code contains TODOs for full implementation."
  putStrLn "      Serialization instances need version-aware logic."

-- | Clean old generated Haskell files from the output directory.
-- Only removes .hs files to avoid accidentally deleting other content.
cleanGeneratedFiles :: FilePath -> IO ()
cleanGeneratedFiles outputDir = do
  exists <- doesDirectoryExist outputDir
  when exists $ do
    putStrLn "Cleaning old generated files..."
    entries <- listDirectory outputDir
    let hsFiles = filter (\f -> takeExtension f == ".hs") entries
    forM_ hsFiles $ \file -> do
      let fullPath = outputDir </> file
      removeFile fullPath `catch` \(e :: SomeException) -> 
        hPutStrLn stderr $ "Warning: Could not remove " ++ file ++ ": " ++ show e
    when (not $ null hsFiles) $ 
      putStrLn $ "Removed " ++ show (length hsFiles) ++ " old generated files."
    putStrLn ""

