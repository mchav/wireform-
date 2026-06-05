-- | Tests for 'Iceberg.Delete' position + equality delete writers.
module Test.Iceberg.Delete (tests) where

import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import System.Exit (ExitCode (..))
import qualified System.Process as Proc
import Test.Syd

import qualified Iceberg.Delete as ID
import qualified Iceberg.Read
import qualified Iceberg.Types
import Iceberg.Types (DeleteFile (..), DeleteFileContent (..), FileFormat (..))

import qualified Parquet.Read as PR
import qualified Parquet.Types as P
import qualified Parquet.Write as PW

tests :: Spec
tests = describe "Iceberg.Delete" $ sequence_
  [ it "position delete file: header counts and reserved field-ids" $ do
      let rows = V.fromList
            [ ID.PositionDeleteRow "s3://bucket/data/00.parquet" 0
            , ID.PositionDeleteRow "s3://bucket/data/00.parquet" 7
            , ID.PositionDeleteRow "s3://bucket/data/01.parquet" 12
            ]
          (bytes, df) = ID.writePositionDeleteFile
                          "s3://bucket/deletes/pos.parquet"
                          rows
      -- DeleteFile manifest fields must reflect what we wrote.
      dfRecordCount df       `shouldBe` 3
      dfContent df           `shouldBe` PositionDeletes
      dfFileFormat df        `shouldBe` ParquetFormat
      dfFileSizeInBytes df   `shouldBe` fromIntegral (BS.length bytes)
      V.toList (dfEqualityFieldIds df) `shouldBe` []

      -- The Parquet file must round-trip and carry the reserved Iceberg
      -- field-ids on its leaf columns; without those an Iceberg reader
      -- can't identify the columns.
      case PR.loadParquetFile bytes of
        Left e   -> expectationFailure e
        Right pf -> do
          let leaves = V.filter (\se -> case P.seType se of
                                          Just _  -> True
                                          Nothing -> False)
                                (P.fmSchema (PR.pfFooter pf))
          V.length leaves `shouldBe` 2
          P.seFieldId (V.unsafeIndex leaves 0)
            `shouldBe` Just ID.positionDeleteFilePathFieldId
          P.seFieldId (V.unsafeIndex leaves 1)
            `shouldBe` Just ID.positionDeletePosFieldId

  , it "equality delete file: equality_field_ids set + per-column field-ids" $ do
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
        Left e -> expectationFailure e
        Right (bytes, df) -> do
          dfRecordCount df `shouldBe` 3
          dfContent df     `shouldBe` EqualityDeletes
          V.toList (dfEqualityFieldIds df) `shouldBe` [17, 23]
          case PR.loadParquetFile bytes of
            Left e   -> expectationFailure e
            Right pf -> do
              let leaves = V.filter (\se -> case P.seType se of
                                              Just _  -> True
                                              Nothing -> False)
                                    (P.fmSchema (PR.pfFooter pf))
              V.length leaves `shouldBe` 2
              P.seFieldId (V.unsafeIndex leaves 0) `shouldBe` Just 17
              P.seFieldId (V.unsafeIndex leaves 1) `shouldBe` Just 23

  , it "equality delete: rejects column count mismatch" $ do
      let cols = V.singleton (PW.ColInt64 (VP.fromList [1, 2]))
          schemaCols =
            [ ID.EqualityDeleteSchema 1 "a" P.PTInt64
            , ID.EqualityDeleteSchema 2 "b" P.PTInt64
            ]
      case ID.writeEqualityDeleteFile "x" schemaCols cols of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected column count mismatch error"

  , it "equality delete: rejects empty schema" $ do
      case ID.writeEqualityDeleteFile "x" [] V.empty of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected empty schema error"

  , it "writePositionDeleteFile -> readPositionDeleteFile round-trip" $ do
      let rows = V.fromList
            [ ID.PositionDeleteRow "s3://b/data/00.parquet" 0
            , ID.PositionDeleteRow "s3://b/data/00.parquet" 7
            , ID.PositionDeleteRow "s3://b/data/00.parquet" 15
            , ID.PositionDeleteRow "s3://b/data/01.parquet" 3
            ]
          (bytes, _) = ID.writePositionDeleteFile "out" rows
      case ID.readPositionDeleteFile bytes of
        Left e -> expectationFailure ("read: " ++ e)
        Right rows' -> rows' `shouldBe` rows

  , it "end-to-end: write deletes -> read deletes -> apply to data rows" $ do
      -- 10-row data file. Position deletes drop rows 1, 4, 8 from
      -- the target path (rows 1, 4 from another path are
      -- ignored).
      let dataFilePath = "s3://b/data/users.parquet"
          deletes = V.fromList
            [ ID.PositionDeleteRow dataFilePath 1
            , ID.PositionDeleteRow dataFilePath 4
            , ID.PositionDeleteRow dataFilePath 8
            , ID.PositionDeleteRow "s3://b/data/orders.parquet" 0
            ]
          (deleteBytes, _) = ID.writePositionDeleteFile "deletes/0.parquet" deletes
      case ID.readPositionDeleteFile deleteBytes of
        Left e -> expectationFailure ("read: " ++ e)
        Right deletes' -> do
          deletes' `shouldBe` deletes
          -- Apply to the data rows. We use the existing
          -- Iceberg.Read.applyPositionDeletes which expects a
          -- different PositionDelete record shape (Iceberg.Types
          -- carries 'pdFilePath' / 'pdPosition'), so we adapt.
          let dataRows  = V.fromList ([0..9] :: [Int])
              icebergDs = V.map
                (\(ID.PositionDeleteRow p pos) ->
                    Iceberg.Types.PositionDelete p pos) deletes'
              kept = Iceberg.Read.applyPositionDeletes
                       icebergDs dataFilePath dataRows
          V.toList kept `shouldBe` [0, 2, 3, 5, 6, 7, 9]

  , it "equality delete file: pyarrow sees Iceberg field_ids" $ do
      -- Regression for the bug where seFieldId was encoded into
      -- parquet.thrift SchemaElement field 8 (which is @precision@)
      -- instead of field 9 (@field_id@). Other Parquet readers
      -- (pyarrow/Java/etc.) therefore silently dropped the field_id,
      -- which for an Iceberg equality-delete file means the delete
      -- row's columns can't be matched back to a data column.
      pyOk <- pyarrowAvailable
      if not pyOk
        then pure ()  -- pyarrow absent on the build agent; skip.
        else do
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
            Left e -> expectationFailure e
            Right (bytes, _) -> do
              let path = "/tmp/wireform-equality-delete-field-ids.parquet"
              BS.writeFile path bytes
              pyarrowAssert "pyarrow recovers field_id=17 for 'id'"
                [ "pf = pq.ParquetFile('" ++ path ++ "')"
                , "txt = str(pf.schema)"
                , "assert 'field_id=17' in txt and 'id' in txt, txt"
                , "assert 'field_id=23' in txt and 'name' in txt, txt"
                ]
  ]

pyarrowAvailable :: IO Bool
pyarrowAvailable = do
  (code, _, _) <- Proc.readProcessWithExitCode
                    "python3" ["-c", "import pyarrow.parquet"] ""
  pure (code == ExitSuccess)

pyarrowAssert :: String -> [String] -> IO ()
pyarrowAssert label snippet = do
  (code, out, err) <- Proc.readProcessWithExitCode "python3"
    [ "-c"
    , unlines
        ( "import pyarrow.parquet as pq"
        : snippet
       ++ ["print('PYARROW_OK')"]
        )
    ]
    ""
  case code of
    ExitSuccess
      | "PYARROW_OK" `isInfixOf` out -> pure ()
      | otherwise -> expectationFailure (label ++ ": pyarrow output: " ++ out)
    _ -> expectationFailure (label ++ ":\nstdout=" ++ out ++ "\nstderr=" ++ err)
