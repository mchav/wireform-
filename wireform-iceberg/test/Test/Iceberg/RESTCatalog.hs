module Test.Iceberg.RESTCatalog (tests) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Iceberg.Catalog.REST
import Iceberg.Types
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.Catalog.REST" $
    sequence_
      [ it "CatalogConfig JSON round-trip" $ do
          let cc =
                CatalogConfig
                  { ccDefaults = Map.fromList [("warehouse", "s3://b")]
                  , ccOverrides = Map.empty
                  }
          case Aeson.eitherDecode (Aeson.encode cc) of
            Right cc' -> cc' `shouldBe` cc
            Left e -> expectationFailure e
      , it "TableIdentifier JSON round-trip" $ do
          let ti = TableIdentifier (V.fromList ["sales", "fact"]) "orders"
          case Aeson.eitherDecode (Aeson.encode ti) of
            Right ti' -> ti' `shouldBe` ti
            Left e -> expectationFailure e
      , it "ListNamespacesResponse JSON round-trip" $ do
          let r =
                ListNamespacesResponse $
                  V.fromList
                    [V.fromList ["a"], V.fromList ["a", "b"]]
          let bs = Aeson.encode r
          case Aeson.eitherDecode bs of
            Right r' -> r' `shouldBe` r
            Left e -> expectationFailure e
      , it "TableUpdate SetCurrentSchema JSON round-trip" $ do
          let upd = SetCurrentSchema 7
              bs = Aeson.encode upd
          case Aeson.eitherDecode bs of
            Right (upd' :: TableUpdate) -> upd' `shouldBe` upd
            Left e -> expectationFailure e
      , it "TableRequirement AssertCreate encodes" $ do
          BL.length (Aeson.encode AssertCreate) > 0 `shouldBe` True
      , it "CatalogError matches Iceberg REST shape" $ do
          let err = CatalogError "not found" "NoSuchTableException" 404
          case Aeson.eitherDecode (Aeson.encode err) of
            Right e' -> e' `shouldBe` err
            Left e -> expectationFailure e
      , it "RenameTableRequest JSON round-trip" $ do
          let req =
                RenameTableRequest
                  { rtSource = TableIdentifier (V.singleton "ns") "old"
                  , rtDestination = TableIdentifier (V.singleton "ns") "new"
                  }
          case Aeson.eitherDecode (Aeson.encode req) of
            Right (req' :: RenameTableRequest) -> req' `shouldBe` req
            Left e -> expectationFailure e
      , it "RegisterTableRequest JSON round-trip" $ do
          let req =
                RegisterTableRequest
                  { rgrName = "orders"
                  , rgrMetadataLocation = "s3://b/m/v3.metadata.json"
                  , rgrOverwrite = False
                  }
          case Aeson.eitherDecode (Aeson.encode req) of
            Right (req' :: RegisterTableRequest) -> req' `shouldBe` req
            Left e -> expectationFailure e
      , it "UpdateNamespacePropertiesRequest JSON round-trip" $ do
          let req =
                UpdateNamespacePropertiesRequest
                  { unprRemovals = V.fromList ["k1"]
                  , unprUpdates = Map.fromList [("k2", "v2")]
                  }
          case Aeson.eitherDecode (Aeson.encode req) of
            Right (req' :: UpdateNamespacePropertiesRequest) -> req' `shouldBe` req
            Left e -> expectationFailure e
      , it "UpdateNamespacePropertiesResponse JSON round-trip" $ do
          let resp =
                UpdateNamespacePropertiesResponse
                  { unprspUpdated = V.fromList ["k1"]
                  , unprspRemoved = V.fromList ["k2"]
                  , unprspMissing = V.fromList ["k3"]
                  }
          case Aeson.eitherDecode (Aeson.encode resp) of
            Right (resp' :: UpdateNamespacePropertiesResponse) -> resp' `shouldBe` resp
            Left e -> expectationFailure e
      ]
