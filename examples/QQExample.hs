{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example: QuasiQuoter for inline proto definitions.
--
-- Run with: cabal run example-qq
module Main where

import qualified Data.ByteString as BS

import Proto.Encode
import Proto.Decode
import Proto.QQ

-- Define protobuf messages inline using the quasiquoter.
-- This parses the proto IDL at compile time and generates
-- Haskell data types and instances.
[proto|
  syntax = "proto3";

  message SearchRequest {
    string query = 1;
    int32 page_number = 2;
    int32 result_per_page = 3;
  }

  message SearchResponse {
    repeated string results = 1;
    int32 total_count = 2;
  }

  enum SearchKind {
    SEARCH_KIND_ALL = 0;
    SEARCH_KIND_IMAGES = 1;
    SEARCH_KIND_NEWS = 2;
  }
|]

main :: IO ()
main = do
  putStrLn "=== QuasiQuoter Example ===\n"

  let req = defaultSearchRequest
        { searchRequestQuery         = "haskell protobuf"
        , searchRequestPageNumber    = 1
        , searchRequestResultPerPage = 20
        }
  putStrLn $ "SearchRequest: " <> show req

  let encoded = encodeMessage req
  putStrLn $ "Encoded: " <> show (BS.length encoded) <> " bytes"

  case decodeMessage encoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: SearchRequest) -> do
      putStrLn $ "Decoded: " <> show decoded
      putStrLn $ "Match: " <> show (decoded == req)

  putStrLn "\nDone."
