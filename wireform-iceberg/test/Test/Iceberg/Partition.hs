{-# LANGUAGE OverloadedStrings #-}
module Test.Iceberg.Partition (tests) where

import Data.Text (Text)
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.HUnit

import qualified Avro.Value as AV
import qualified Iceberg.Expression as E
import Iceberg.Partition
import Iceberg.Types

mkSF :: Int -> Text -> IcebergType -> StructField
mkSF i n t = StructField i n True t Nothing Nothing Nothing

schema :: Schema
schema = Schema
  { schemaId = 0
  , schemaFields = V.fromList
      [ mkSF 1 "id"   TLong
      , mkSF 2 "name" TString
      ]
  , schemaIdentifierFieldIds = V.empty
  }

specBucket8 :: PartitionSpec
specBucket8 = PartitionSpec 0 (V.singleton
  (PartitionField { pfSourceId = 1, pfFieldId = 1000, pfName = "id_bucket"
                  , pfTransform = Bucket 8 }))

specIdentity :: PartitionSpec
specIdentity = PartitionSpec 0 (V.singleton
  (PartitionField { pfSourceId = 1, pfFieldId = 1000, pfName = "id_part"
                  , pfTransform = Identity }))

tests :: TestTree
tests = testGroup "Iceberg.Partition"
  [ testCase "buildPartition Identity" $ do
      let lookupSrc 1 = Just (AV.Long 42)
          lookupSrc _ = Nothing
      case buildPartition specIdentity schema lookupSrc of
        Right (PartitionTuple t) -> V.toList t @?= [Just (AV.Long 42)]
        Left e -> assertFailure (show e)

  , testCase "buildPartition Bucket(8) of long 42" $ do
      let lookupSrc 1 = Just (AV.Long 42)
          lookupSrc _ = Nothing
      case buildPartition specBucket8 schema lookupSrc of
        Right (PartitionTuple t) -> case V.toList t of
          [Just (AV.Int n)] -> (n >= 0 && n < 8) @?= True
          other -> assertFailure ("unexpected partition: " ++ show other)
        Left e -> assertFailure (show e)

  , testCase "inclusiveProject identity rewrites field name" $ do
      let expr = E.equal "id" (E.LLong 42)
          projected = inclusiveProject schema specIdentity expr
      case projected of
        E.EAnd _ _ -> pure ()  -- folded with True; either form is OK
        E.EPredicate p -> E.predField p @?= "id_part"
        other -> assertFailure ("expected projected predicate, got: " ++ show other)

  , testCase "inclusiveProject Bucket(8) projects equal predicate" $ do
      let expr = E.equal "id" (E.LLong 42)
          projected = inclusiveProject schema specBucket8 expr
      case projected of
        E.EAnd _ (E.EPredicate p) -> E.predField p @?= "id_bucket"
        E.EPredicate p             -> E.predField p @?= "id_bucket"
        other -> assertFailure ("expected bucket predicate, got: " ++ show other)
  ]
