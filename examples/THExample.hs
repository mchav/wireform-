{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Example: Template Haskell splice to generate types from a .proto file.
--
-- Run with: cabal run example-th
module Main where

import qualified Data.ByteString as BS

import Proto.Encode
import Proto.Decode
import Proto.TH

-- Generate types from the proto file at compile time.
-- This creates: GetPersonRequest, ListPeopleRequest, AddPersonResponse
$(loadProto "examples/proto/simple.proto")

main :: IO ()
main = do
  putStrLn "=== Template Haskell Example ===\n"

  -- The types are generated at compile time from person.proto.
  -- GetPersonRequest has an id field.
  let req = defaultGetPersonRequest { personId = 42 }
  putStrLn $ "GetPersonRequest: " <> show req

  let encoded = encodeMessage req
  putStrLn $ "Encoded: " <> show (BS.length encoded) <> " bytes"

  case decodeMessage encoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: GetPersonRequest) -> do
      putStrLn $ "Decoded: " <> show decoded
      putStrLn $ "Match: " <> show (decoded == req)

  -- ListPeopleRequest
  let listReq = defaultListPeopleRequest
        { pageSize = 10
        , pageToken = "token123"
        }
  putStrLn $ "\nListPeopleRequest: " <> show listReq

  let listEncoded = encodeMessage listReq
  putStrLn $ "Encoded: " <> show (BS.length listEncoded) <> " bytes"

  case decodeMessage listEncoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: ListPeopleRequest) -> do
      putStrLn $ "Decoded: " <> show decoded
      putStrLn $ "Match: " <> show (decoded == listReq)

  -- AddPersonResponse
  let resp = defaultAddPersonResponse
        { success = True
        , errorMessage = ""
        }
  putStrLn $ "\nAddPersonResponse: " <> show resp

  let respEncoded = encodeMessage resp
  case decodeMessage respEncoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: AddPersonResponse) ->
      putStrLn $ "Roundtrip match: " <> show (decoded == resp)

  putStrLn "\nDone."
