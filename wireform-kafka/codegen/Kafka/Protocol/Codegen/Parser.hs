{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Kafka.Protocol.Codegen.Parser
Description : Parse Kafka protocol JSON definitions
Copyright   : (c) 2025
License     : BSD-3-Clause

This module provides utilities to parse Kafka's protocol definition JSON files
into Haskell data structures suitable for code generation.

The Kafka protocol definitions are stored as JSON files with C++-style comments,
so we need to strip comments before parsing.
-}
module Kafka.Protocol.Codegen.Parser
  ( -- * Parsing Functions
    parseProtocolFile
  , parseProtocolDirectory
  , stripComments
    -- * File Discovery
  , findProtocolFiles
  , isProtocolFile
  ) where

import Control.Monad (filterM)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Kafka.Protocol.Codegen.Types
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

-- | Parse a single Kafka protocol JSON file.
-- The file may contain C++-style comments (//) which will be stripped.
parseProtocolFile :: FilePath -> IO (Either String ProtocolSchema)
parseProtocolFile path = do
  content <- BL.readFile path
  let stripped = stripComments content
  return $ eitherDecode stripped

-- | Parse all protocol files in a directory.
-- Returns a list of successfully parsed schemas and a list of errors.
parseProtocolDirectory
  :: FilePath
  -> IO ([ProtocolSchema], [(FilePath, String)])
parseProtocolDirectory dir = do
  files <- findProtocolFiles dir
  results <- mapM parseWithPath files
  let (errors, schemas) = partitionResults results
  return (schemas, errors)
  where
    parseWithPath :: FilePath -> IO (Either (FilePath, String) ProtocolSchema)
    parseWithPath path = do
      result <- parseProtocolFile path
      return $ case result of
        Left err -> Left (path, err)
        Right schema -> Right schema
    
    partitionResults :: [Either a b] -> ([a], [b])
    partitionResults = foldr partition ([], [])
      where
        partition (Left x) (ls, rs) = (x:ls, rs)
        partition (Right x) (ls, rs) = (ls, x:rs)

-- | Find all protocol JSON files in a directory (non-recursive).
findProtocolFiles :: FilePath -> IO [FilePath]
findProtocolFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      entries <- listDirectory dir
      let paths = map (dir </>) entries
      filterM (\p -> (&&) <$> doesFileExist p <*> pure (isProtocolFile p)) paths

-- | Check if a file is a protocol JSON file (ends with .json).
isProtocolFile :: FilePath -> Bool
isProtocolFile path = takeExtension path == ".json"

-- | Strip C++-style line comments from JSON content.
-- This is necessary because Kafka's protocol JSON files contain // comments.
--
-- This implementation is simple and doesn't handle comments in strings,
-- but that's okay since the protocol files don't use that pattern.
stripComments :: ByteString -> ByteString
stripComments = TL.encodeUtf8 . TL.unlines . map stripLine . TL.lines . TL.decodeUtf8
  where
    stripLine :: TL.Text -> TL.Text
    stripLine line =
      case TL.breakOn "//" line of
        (before, _comment) ->
          -- Check if // is actually in a string by counting quotes before it
          if countQuotes before `mod` 2 == 0
            then TL.stripEnd before
            else line
    
    countQuotes :: TL.Text -> Int
    countQuotes = TL.foldl' (\acc c -> if c == '"' then acc + 1 else acc) 0

