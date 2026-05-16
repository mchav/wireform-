{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Example: Template Haskell splice to generate types from a .proto file.

Run with: cabal run example-th
-}
module Main where

import Data.ByteString qualified as BS
import Data.Reflection (Given (..))
import Proto.Decode
import Proto.Encode
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)
import Proto.TH


-- The generated JSON instances carry a 'Given ExtensionRegistry' constraint
-- for proto2 extensions; satisfy it with the empty registry.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry


-- Generate types from the proto file at compile time.
-- This creates: GetPersonRequest, ListPeopleRequest, AddPersonResponse
$(loadProto "examples/proto/simple.proto")


main :: IO ()
main = do
  putStrLn "=== Template Haskell Example ===\n"

  -- 'loadProto' scopes each generated record field by lowerCamelCasing
  -- the owning message's type name, mirroring 'Proto.CodeGen.scopedFieldName'.
  -- So @int32 person_id = 1@ inside @message GetPersonRequest@ becomes
  -- @getPersonRequestPersonId :: Int32@, not @personId@. Two messages
  -- declaring the same field name in the same file would otherwise
  -- collide on the same record selector at the module level.
  let req = defaultGetPersonRequest {getPersonRequestPersonId = 42}
  putStrLn $ "GetPersonRequest: " <> show req

  let encoded = encodeMessage req
  putStrLn $ "Encoded: " <> show (BS.length encoded) <> " bytes"

  case decodeMessage encoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: GetPersonRequest) -> do
      putStrLn $ "Decoded: " <> show decoded
      putStrLn $ "Match: " <> show (decoded == req)

  -- ListPeopleRequest
  let listReq =
        defaultListPeopleRequest
          { listPeopleRequestPageSize = 10
          , listPeopleRequestPageToken = "token123"
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
  let resp =
        defaultAddPersonResponse
          { addPersonResponseSuccess = True
          , addPersonResponseErrorMessage = ""
          }
  putStrLn $ "\nAddPersonResponse: " <> show resp

  let respEncoded = encodeMessage resp
  case decodeMessage respEncoded of
    Left err -> putStrLn $ "Decode error: " <> show err
    Right (decoded :: AddPersonResponse) ->
      putStrLn $ "Roundtrip match: " <> show (decoded == resp)

  putStrLn "\nDone."
