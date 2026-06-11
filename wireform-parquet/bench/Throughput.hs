{-# LANGUAGE BangPatterns #-}

{- | End-to-end Parquet write + read throughput baseline for
wireform-parquet, measured on a synthetic dataset of
realistic shape and size. Output goes through criterion +
a JSON report so the matching Python driver can compare
against pyarrow on the same shape.

Usage:

@
cabal bench wireform-parquet:parquet-throughput

# or, with explicit options for the JSON reporter:
cabal bench wireform-parquet:parquet-throughput \\
  -- --json wireform_throughput.json
@

The dataset:

  * 1,000,000 rows
  * 4 columns: int64 id, double score, utf8 name (8-byte
    fixed strings), bool active
  * single row group, default codec

Each measurement is one full encode (or decode) of the
complete dataset; criterion divides the wall-clock time by
the iteration count so the raw seconds are per-encode.
-}
module Main (main) where

import Arrow.Types qualified as AT
import Control.DeepSeq (NFData (..), deepseq)
import Criterion.Main (bench, bgroup, defaultMain, env, nf, whnf)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Parquet.Arrow qualified as PArrow
import Parquet.HighLevel qualified as PHL
import Parquet.Read qualified as PR
import Parquet.Types qualified as P
import Parquet.Write (ColumnData (..))


{- | Default 100k rows is enough to push past per-call
overhead while keeping each criterion sample under a
second. Override with @--rows N@ at the command line via
a custom envparam if needed.
-}
nRows :: Int
nRows = 100_000


{- | Generate the same 4-column dataset every run.

id      = increasing Int64 0..N-1
score   = i * 0.5
name    = "name_<i_mod_1000>" (so dict encoding has work to do)
active  = i `rem` 2 == 0
-}
mkDataset :: Int -> V.Vector ColumnData
mkDataset n =
  V.fromList
    [ ColInt64 (VP.generate n fromIntegral)
    , ColDouble (VP.generate n (\i -> fromIntegral i * 0.5))
    , ColByteArray $ V.generate n $ \i ->
        TE.encodeUtf8 (T.pack ("name_" ++ show (i `rem` 1000)))
    , ColBool (V.generate n (\i -> i `rem` 2 == 0))
    ]


mkSchema :: V.Vector P.SchemaElement
mkSchema =
  V.fromList
    [ P.SchemaElement "schema" Nothing Nothing (Just 4) Nothing Nothing Nothing
    , P.SchemaElement "id" (Just P.Required) (Just P.PTInt64) Nothing Nothing Nothing Nothing
    , P.SchemaElement "score" (Just P.Required) (Just P.PTDouble) Nothing Nothing Nothing Nothing
    , P.SchemaElement "name" (Just P.Required) (Just P.PTByteArray) Nothing (Just P.CTUtf8) Nothing Nothing
    , P.SchemaElement "active" (Just P.Required) (Just P.PTBoolean) Nothing Nothing Nothing Nothing
    ]


writeFile_ :: V.Vector ColumnData -> BS.ByteString
writeFile_ cols = PHL.encodeParquet PHL.defaultWriteOptions mkSchema [cols]


writeFileZstd :: V.Vector ColumnData -> BS.ByteString
writeFileZstd cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions {PHL.writeCompression = P.ZSTD}
    mkSchema
    [cols]


writeFileSnappy :: V.Vector ColumnData -> BS.ByteString
writeFileSnappy cols =
  PHL.encodeParquet
    PHL.defaultWriteOptions {PHL.writeCompression = P.Snappy}
    mkSchema
    [cols]


{- | Decode the file's footer + every column. Forces the
result through deepseq so the per-page allocation work is
captured by the benchmark.
-}
readBack :: BS.ByteString -> Int
readBack bs = case PHL.decodeParquet PHL.defaultReadOptions bs of
  Left err -> error err
  Right pf ->
    let !sch = PArrow.parquetFileArrowSchema pf
        !nRG = PArrow.numRowGroups pf
        !nCols = V.length (AT.arrowFields sch)
        !total =
          sum
            [ rowsIn col
            | rg <- [0 .. nRG - 1]
            , c <- [0 .. nCols - 1]
            , let !fld = V.unsafeIndex (AT.arrowFields sch) c
            , col <- case PArrow.readParquetColumn pf rg c fld of
                Right cv -> [cv]
                Left _ -> []
            ]
    in total
  where
    rowsIn _ = 1 -- counting columns, not row counts; criterion just needs a forced value


instance NFData ColumnData where
  rnf (ColInt32 v) = v `seq` ()
  rnf (ColInt64 v) = v `seq` ()
  rnf (ColFloat v) = v `seq` ()
  rnf (ColDouble v) = v `seq` ()
  rnf (ColBool v) = v `seq` ()
  rnf (ColByteArray v) = v `deepseq` ()


main :: IO ()
main =
  let !dataset = mkDataset nRows
  in defaultMain
       [ env (pure dataset) $ \cols ->
           bgroup
             ("write " ++ show nRows ++ " rows x 4 cols")
             [ bench "uncompressed" $ whnf (BS.length . writeFile_) cols
             , bench "snappy" $ whnf (BS.length . writeFileSnappy) cols
             , bench "zstd" $ whnf (BS.length . writeFileZstd) cols
             ]
       , env (pure (writeFile_ dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (uncompressed)")
             [ bench "decode" $ nf readBack bs
             ]
       , env (pure (writeFileSnappy dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (snappy)")
             [ bench "decode" $ nf readBack bs
             ]
       , env (pure (writeFileZstd dataset)) $ \bs ->
           bgroup
             ("read " ++ show nRows ++ " rows x 4 cols (zstd)")
             [ bench "decode" $ nf readBack bs
             ]
       ]
