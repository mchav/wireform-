module Test.Iceberg.View (tests) where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Vector qualified as V
import Iceberg.JSON (viewMetadataFromJSON, viewMetadataToJSON)
import Iceberg.Types
import Iceberg.View
import Iceberg.Write (encodeViewMetadata)
import Test.Syd


tests :: Spec
tests =
  describe "Iceberg.View" $
    sequence_
      [ it "newViewMetadata + addViewVersion + JSON round-trip" $ do
          let schema = Schema 0 V.empty V.empty
              v1 =
                ViewVersion
                  { vvVersionId = 1
                  , vvTimestampMs = 1700000000000
                  , vvSchemaId = 0
                  , vvSummary = Map.fromList [("operation", "create")]
                  , vvRepresentations =
                      V.singleton
                        ( SqlViewRepresentation
                            "SELECT 1 AS x"
                            "spark"
                        )
                  , vvDefaultCatalog = Just "spark"
                  , vvDefaultNamespace = V.singleton "db"
                  }
              vm = addViewVersion v1 True 1700000000000 (newViewMetadata "uuid" "s3://b/v" schema)
          case viewMetadataFromJSON (viewMetadataToJSON vm) of
            Right vm' -> vm' `shouldBe` vm
            Left e -> expectationFailure e
      , it "encodeViewMetadata produces valid JSON" $ do
          let schema = Schema 0 V.empty V.empty
              vm = newViewMetadata "uuid" "s3://b/v" schema
              bs = encodeViewMetadata vm
          case Aeson.eitherDecodeStrict bs of
            Right (Aeson.Object _) -> pure ()
            _ -> expectationFailure "expected a JSON object"
      ]
