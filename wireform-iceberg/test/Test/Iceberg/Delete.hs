-- | Tests for 'Iceberg.Delete' position + equality delete writers.
module Test.Iceberg.Delete (tests) where

import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import Test.Tasty
import Test.Tasty.HUnit

import qualified Iceberg.Delete as ID
import Iceberg.Types (DeleteFile (..), DeleteFileContent (..), FileFormat (..))

import qualified Parquet.Read as PR
import qualified Parquet.Types as P
import qualified Parquet.Write as PW

tests :: TestTree
tests = testGroup "Iceberg.Delete"
  [ testCase "position delete file: header counts and reserved field-ids" $ do
      let rows = V.fromList
            [ ID.PositionDeleteRow "s3://bucket/data/00.parquet" 0
            , ID.PositionDeleteRow "s3://bucket/data/00.parquet" 7
            , ID.PositionDeleteRow "s3://bucket/data/01.parquet" 12
            ]
          (bytes, df) = ID.writePositionDeleteFile
                          "s3://bucket/deletes/pos.parquet"
                          rows
      -- DeleteFile manifest fields must reflect what we wrote.
      dfRecordCount df       @?= 3
      dfContent df           @?= PositionDeletes
      dfFileFormat df        @?= ParquetFormat
      dfFileSizeInBytes df   @?= fromIntegral (BS.length bytes)
      V.toList (dfEqualityFieldIds df) @?= []

      -- The Parquet file must round-trip and carry the reserved Iceberg
      -- field-ids on its leaf columns; without those an Iceberg reader
      -- can't identify the columns.
      case PR.loadParquetFile bytes of
        Left e   -> assertFailure e
        Right pf -> do
          let leaves = V.filter (\se -> case P.seType se of
                                          Just _  -> True
                                          Nothing -> False)
                                (P.fmSchema (PR.pfFooter pf))
          V.length leaves @?= 2
          P.seFieldId (V.unsafeIndex leaves 0)
            @?= Just ID.positionDeleteFilePathFieldId
          P.seFieldId (V.unsafeIndex leaves 1)
            @?= Just ID.positionDeletePosFieldId

  , testCase "equality delete file: equality_field_ids set + per-column field-ids" $ do
      let cols = V.fromList
            [ PW.ColInt64     (VP.fromList [1, 2, 3])
            , PW.ColByteArray (V.fromList ["alpha", "beta", "gamma"])
            ]
          schemaCols =
            [ ID.EqualityDeleteSchema 17 "id"   P.PTInt64
            , ID.EqualityDeleteSchema 23 "name" P.PTByteArray
            ]
      case ID.writeEqualityDeleteFile
             "s3://bucket/deletes/eq.parquet" schemaCols cols of
        Left e -> assertFailure e
        Right (bytes, df) -> do
          dfRecordCount df @?= 3
          dfContent df     @?= EqualityDeletes
          V.toList (dfEqualityFieldIds df) @?= [17, 23]
          case PR.loadParquetFile bytes of
            Left e   -> assertFailure e
            Right pf -> do
              let leaves = V.filter (\se -> case P.seType se of
                                              Just _  -> True
                                              Nothing -> False)
                                    (P.fmSchema (PR.pfFooter pf))
              V.length leaves @?= 2
              P.seFieldId (V.unsafeIndex leaves 0) @?= Just 17
              P.seFieldId (V.unsafeIndex leaves 1) @?= Just 23

  , testCase "equality delete: rejects column count mismatch" $ do
      let cols = V.singleton (PW.ColInt64 (VP.fromList [1, 2]))
          schemaCols =
            [ ID.EqualityDeleteSchema 1 "a" P.PTInt64
            , ID.EqualityDeleteSchema 2 "b" P.PTInt64
            ]
      case ID.writeEqualityDeleteFile "x" schemaCols cols of
        Left _ -> pure ()
        Right _ -> assertFailure "expected column count mismatch error"

  , testCase "equality delete: rejects empty schema" $ do
      case ID.writeEqualityDeleteFile "x" [] V.empty of
        Left _ -> pure ()
        Right _ -> assertFailure "expected empty schema error"
  ]
