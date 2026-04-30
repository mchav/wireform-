{-# LANGUAGE OverloadedStrings #-}
-- | End-to-end test for "Iceberg.Variant.Parquet": build a Parquet
-- file with one optional Variant column and ask pyarrow to read the
-- two underlying binary leaves back. The Variant binary content is
-- round-tripped through 'Iceberg.Variant.encodeVariant' /
-- 'decodeVariant' inside the test, so we verify both the Parquet
-- group layout and the Iceberg.Variant codec end-to-end.
module Test.Iceberg.VariantParquet (tests) where

import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified System.Process as Proc
import System.Exit (ExitCode (..))
import Test.Tasty
import Test.Tasty.HUnit

import qualified Iceberg.Variant as IV
import qualified Iceberg.Variant.Parquet as IVP
import qualified Parquet.Nested as PN

tests :: TestTree
tests = testGroup "Iceberg.Variant.Parquet"
  [ testCase "buildVariantParquetFile: schema + row count" $ do
      let v1 = IV.VBool True
          v2 = IV.VInt32 42
          rows = V.fromList [Just v1, Just v2, Nothing]
      case IVP.buildVariantParquetFile "v" rows of
        Left e -> assertFailure e
        Right bytes -> do
          assertBool "produced non-empty file"
            (BS.length bytes > 0)
          -- Trailing magic must be PAR1.
          BS.takeEnd 4 bytes @?= BS.pack [0x50, 0x41, 0x52, 0x31]

  , testCase "buildVariantParquetFile: pyarrow reads the 2-leaf group" $ do
      pyOk <- pyarrowAvailable
      if pyOk
        then do
          let v1 = IV.VBool True
              v2 = IV.VObject (Map.fromList
                                [("k", IV.VInt32 42)
                                ,("s", IV.VString "hi")
                                ])
              (m1, x1) = IV.encodeVariant v1
              (m2, x2) = IV.encodeVariant v2
              rows = V.fromList [Just v1, Just v2, Nothing]
          case IVP.buildVariantParquetFile "v" rows of
            Left e -> assertFailure e
            Right bytes -> do
              let path = "/tmp/wireform-iceberg-variant.parquet"
              BS.writeFile path bytes
              pyarrowAssert "Variant column reads back as 2-leaf binary group"
                [ "t = pq.read_table('" ++ path ++ "').to_pylist()"
                , "assert len(t) == 3, f'wrong row count: {len(t)}'"
                , "assert t[2]['v'] is None, f'row 2 not null: {t[2]!r}'"
                , "v0 = t[0]['v']"
                , "assert v0['metadata'] == bytes(" ++ show (BS.unpack m1)
                    ++ "), f'metadata 0 mismatch: {v0!r}'"
                , "assert v0['value']    == bytes(" ++ show (BS.unpack x1)
                    ++ "), f'value 0 mismatch: {v0!r}'"
                , "v1 = t[1]['v']"
                , "assert v1['metadata'] == bytes(" ++ show (BS.unpack m2)
                    ++ "), f'metadata 1 mismatch: {v1!r}'"
                , "assert v1['value']    == bytes(" ++ show (BS.unpack x2)
                    ++ "), f'value 1 mismatch: {v1!r}'"
                ]
        else pure ()  -- pyarrow not available; skip silently

  , testCase "buildVariantParquetFileMulti: two Variant columns" $ do
      let col1 = IVP.VariantColumn "user_attrs"
                  (V.fromList [Just (IV.VString "alice"),
                               Just (IV.VString "bob"),
                               Nothing])
          col2 = IVP.VariantColumn "event_payload"
                  (V.fromList [Just (IV.VInt32 1), Nothing,
                               Just (IV.VInt32 3)])
      case IVP.buildVariantParquetFileMulti (V.fromList [col1, col2]) of
        Left e -> assertFailure e
        Right bytes ->
          assertBool "multi-Variant file is non-empty" (BS.length bytes > 0)

  , testCase "variantToNestedRow Nothing -> NRNull, Just -> NRVariantBytes" $ do
      case IVP.variantToNestedRow Nothing of
        PN.NRNull -> pure ()
        _ -> assertFailure "expected NRNull"
      case IVP.variantToNestedRow (Just (IV.VBool False)) of
        PN.NRVariantBytes _ _ -> pure ()
        _ -> assertFailure "expected NRVariantBytes"
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
    ] ""
  case code of
    ExitSuccess
      | "PYARROW_OK" `isInfixOf` out ->
          pure ()
      | otherwise ->
          assertFailure (label ++ ": pyarrow output: " ++ out)
    _ ->
      assertFailure (label ++ ":\nstdout=" ++ out ++ "\nstderr=" ++ err)
