{-# LANGUAGE OverloadedStrings #-}
-- | Smoke tests for the REST catalog HTTP client. These focus on URL
-- construction and JSON payload shapes; full network integration tests
-- belong outside this suite to keep it offline-friendly.
module Test.Iceberg.RESTClient (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Catalog.REST
import qualified Iceberg.Catalog.REST.Client as Client

tests :: TestTree
tests = testGroup "Iceberg.Catalog.REST.Client"
  [ testCase "AuthHeader values are constructible" $ do
      case Client.NoAuth of _ -> pure ()
      case Client.BearerToken "tok" of _ -> pure ()
      case Client.RawHeader "X-Foo" "bar" of _ -> pure ()

  , testCase "CommitTableRequest round-trips through Aeson" $ do
      let req = CommitTableRequest
            { ctReqIdentifier = TableIdentifier (V.singleton "db") "orders"
            , ctReqRequirements = V.singleton (AssertTableUUID "uuid-x")
            , ctReqUpdates = V.singleton (SetCurrentSchema 2)
            }
      case Aeson.eitherDecode (Aeson.encode req) of
        Right (req' :: CommitTableRequest) -> req' @?= req
        Left e -> assertFailure e
  ]
