module Test.Iceberg.RESTCatalog (tests) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import Iceberg.Catalog.REST
import Iceberg.Types

tests :: TestTree
tests = testGroup "Iceberg.Catalog.REST"
  [ testCase "CatalogConfig JSON round-trip" $ do
      let cc = CatalogConfig
            { ccDefaults  = Map.fromList [("warehouse", "s3://b")]
            , ccOverrides = Map.empty
            }
      case Aeson.eitherDecode (Aeson.encode cc) of
        Right cc' -> cc' @?= cc
        Left e    -> assertFailure e

  , testCase "TableIdentifier JSON round-trip" $ do
      let ti = TableIdentifier (V.fromList ["sales", "fact"]) "orders"
      case Aeson.eitherDecode (Aeson.encode ti) of
        Right ti' -> ti' @?= ti
        Left e    -> assertFailure e

  , testCase "ListNamespacesResponse JSON round-trip" $ do
      let r = ListNamespacesResponse $ V.fromList
            [ V.fromList ["a"], V.fromList ["a", "b"] ]
      let bs = Aeson.encode r
      case Aeson.eitherDecode bs of
        Right r' -> r' @?= r
        Left e   -> assertFailure e

  , testCase "TableUpdate SetCurrentSchema JSON round-trip" $ do
      let upd = SetCurrentSchema 7
          bs = Aeson.encode upd
      case Aeson.eitherDecode bs of
        Right (upd' :: TableUpdate) -> upd' @?= upd
        Left e -> assertFailure e

  , testCase "TableRequirement AssertCreate encodes" $ do
      BL.length (Aeson.encode AssertCreate) > 0 @?= True

  , testCase "CatalogError matches Iceberg REST shape" $ do
      let err = CatalogError "not found" "NoSuchTableException" 404
      case Aeson.eitherDecode (Aeson.encode err) of
        Right e' -> e' @?= err
        Left e   -> assertFailure e

  , testCase "RenameTableRequest JSON round-trip" $ do
      let req = RenameTableRequest
            { rtSource      = TableIdentifier (V.singleton "ns") "old"
            , rtDestination = TableIdentifier (V.singleton "ns") "new"
            }
      case Aeson.eitherDecode (Aeson.encode req) of
        Right (req' :: RenameTableRequest) -> req' @?= req
        Left e -> assertFailure e

  , testCase "RegisterTableRequest JSON round-trip" $ do
      let req = RegisterTableRequest
            { rgrName             = "orders"
            , rgrMetadataLocation = "s3://b/m/v3.metadata.json"
            , rgrOverwrite        = False
            }
      case Aeson.eitherDecode (Aeson.encode req) of
        Right (req' :: RegisterTableRequest) -> req' @?= req
        Left e -> assertFailure e

  , testCase "UpdateNamespacePropertiesRequest JSON round-trip" $ do
      let req = UpdateNamespacePropertiesRequest
            { unprRemovals = V.fromList ["k1"]
            , unprUpdates  = Map.fromList [("k2", "v2")]
            }
      case Aeson.eitherDecode (Aeson.encode req) of
        Right (req' :: UpdateNamespacePropertiesRequest) -> req' @?= req
        Left e -> assertFailure e

  , testCase "UpdateNamespacePropertiesResponse JSON round-trip" $ do
      let resp = UpdateNamespacePropertiesResponse
            { unprspUpdated = V.fromList ["k1"]
            , unprspRemoved = V.fromList ["k2"]
            , unprspMissing = V.fromList ["k3"]
            }
      case Aeson.eitherDecode (Aeson.encode resp) of
        Right (resp' :: UpdateNamespacePropertiesResponse) -> resp' @?= resp
        Left e -> assertFailure e
  ]
