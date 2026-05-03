{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
-- | Cross-library example: write a Parquet file with @wireform-parquet@,
-- then load it back through the @dataframe@ library
-- (<https://hackage.haskell.org/package/dataframe>) and run a few
-- aggregations on it via the typed expression DSL.
--
-- This exists to demonstrate that wireform's Parquet writer produces
-- bytes that other Haskell tooling can consume — there's no API
-- coupling between the two libraries, just the on-disk Parquet
-- format. The example doubles as an interop smoke test: every value
-- the dataframe pipeline reports is also computed in pure Haskell
-- from the source vectors, and the two are compared.
--
-- The dataframe dependency is heavy (cassava, attoparsec, regex-tdfa,
-- snappy-hs, zstd, zlib, granite, vector-algorithms, …) so the
-- example is hidden behind the @+dataframe-bridge@ Cabal flag and
-- excluded from a default @cabal build all@. To build and run it:
--
-- > cabal run example-dataframe-bridge -fdataframe-bridge
module Main (main) where

import qualified Data.ByteString as BS
import           Data.Foldable (for_)
import           Data.Int (Int64)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import           System.Directory
                   ( createDirectoryIfMissing
                   , doesFileExist
                   , removeFile
                   )
import           System.FilePath ((</>))
import           System.IO.Temp (withSystemTempDirectory)

import qualified Parquet.Types as P
import qualified Parquet.Write as PW

-- The dataframe library re-exports its core surface from the
-- @DataFrame@ module; the conventional aliases are @D@ for the
-- table operations and @F@ for the expression DSL.
import qualified DataFrame as D
import qualified DataFrame.Functions as F
import           DataFrame.Operators ((|>), (.>.))

-- | The synthetic dataset: a tiny e-commerce order log. Three
-- columns line up with the three Parquet primitive shapes the
-- writer emits below ('PTInt64', 'PTByteArray', 'PTDouble').
data Order = Order
  { orderId :: !Int64
  , region  :: !Text
  , amount  :: !Double
  } deriving (Show, Eq)

orders :: [Order]
orders =
  [ Order  1 "us-west" 12.50
  , Order  2 "us-west" 17.00
  , Order  3 "us-east"  5.25
  , Order  4 "eu"      31.80
  , Order  5 "us-east"  9.75
  , Order  6 "eu"      22.40
  , Order  7 "us-west" 14.10
  , Order  8 "apac"    44.99
  , Order  9 "us-east"  7.30
  , Order 10 "apac"    19.20
  , Order 11 "eu"      28.65
  , Order 12 "us-west"  3.45
  ]

main :: IO ()
main = withSystemTempDirectory "wireform-dataframe-bridge" $ \dir -> do
  createDirectoryIfMissing True dir
  let parquetPath = dir </> "orders.parquet"

  -- 1. Write the data with wireform-parquet.
  putStrLn "=== wireform-parquet writer ==="
  let bytes = encodeOrders orders
  BS.writeFile parquetPath bytes
  putStrLn $ "wrote " ++ show (BS.length bytes) ++ " bytes to " ++ parquetPath

  -- 2. Hand the file off to the dataframe library and explore it.
  putStrLn "\n=== dataframe reader ==="
  df <- D.readParquet parquetPath
  putStrLn $ "dimensions: " ++ show (D.dimensions df)
  print (D.describeColumns df)

  -- 3. Run a typed aggregation: total + order count + mean per
  --    region, sorted by total descending.
  --
  --    Aggregations that need a typed column reference go through
  --    @F.col \@<type>@; the @|>@ pipe operator and @\`F.as\`@
  --    column-naming helper come from DataFrame.Operators /
  --    DataFrame.Functions respectively.
  putStrLn "=== regional totals (dataframe pipeline) ==="
  let perRegion =
        df
          |> D.groupBy ["region"]
          |> D.aggregate
               [ F.sum   (F.col @Double "amount")   `D.as` "total"
               , F.count (F.col @Int64  "order_id") `D.as` "orders"
               , F.mean  (F.col @Double "amount")   `D.as` "avg"
               ]
          |> D.sortBy [D.Desc (F.col @Double "total")]
  print perRegion

  -- 4. Cross-check against the source-of-truth Haskell computation
  --    so the example doubles as an interop test.
  putStrLn "=== ground-truth check ==="
  let groundTruth = expectedTotals orders
  for_ groundTruth $ \(reg, expectedTot, expectedCount) -> do
    -- Pull dataframe's reported total back into Haskell to compare.
    let actualTot   = lookupDouble perRegion "region" reg "total"
        actualCount = lookupInt    perRegion "region" reg "orders"
    putStrLn $ T.unpack reg
            ++ ": dataframe total="    ++ showD actualTot
            ++ "  expected="           ++ showD expectedTot
            ++ "  dataframe count="    ++ show  actualCount
            ++ "  expected="           ++ show  expectedCount
    assertClose ("total for " ++ T.unpack reg) expectedTot actualTot
    assertEq    ("count for " ++ T.unpack reg) expectedCount actualCount

  -- 5. A richer pipeline: filter to large orders, derive a
  --    synthetic "discounted" column, then take the top three rows.
  putStrLn "\n=== filter + derive + take (dataframe pipeline) ==="
  let topDiscounted =
        df
          |> D.filterWhere (F.col @Double "amount" .>. F.lit 15.0)
          |> D.derive "discounted_amount"
               (F.col @Double "amount" * F.lit 0.9)
          |> D.sortBy [D.Desc (F.col @Double "discounted_amount")]
          |> D.take 3
  print topDiscounted

  -- Tidy up the temp file (withSystemTempDirectory removes the dir,
  -- but doing this explicitly keeps the example readable).
  exists <- doesFileExist parquetPath
  if exists then removeFile parquetPath else pure ()

  putStrLn "\nOK — wireform → dataframe round-trip passed."

-- ---------------------------------------------------------------------------
-- wireform-parquet writer side
-- ---------------------------------------------------------------------------

-- | Build a single-row-group Parquet file from the order list.
--
-- Three columns:
--
--   order_id : INT64                   / required
--   region   : BYTE_ARRAY (UTF-8)      / required
--   amount   : DOUBLE                  / required
encodeOrders :: [Order] -> BS.ByteString
encodeOrders rows =
  let schema = V.fromList
        [ P.SchemaElement
            { P.seName          = "schema"
            , P.seRepetition    = Nothing
            , P.seType          = Nothing
            , P.seNumChildren   = Just 3
            , P.seConvertedType = Nothing
            , P.seLogicalType   = Nothing
            , P.seFieldId       = Nothing
            }
        , P.SchemaElement "order_id"
            (Just P.Required) (Just P.PTInt64)
            Nothing Nothing                Nothing Nothing
        , P.SchemaElement "region"
            (Just P.Required) (Just P.PTByteArray)
            Nothing (Just P.CTUtf8)        Nothing Nothing
        , P.SchemaElement "amount"
            (Just P.Required) (Just P.PTDouble)
            Nothing Nothing                Nothing Nothing
        ]

      idCol     = VP.fromList (map orderId rows)
      regionCol = V.fromList  (map (TE.encodeUtf8 . region) rows)
      amountCol = VP.fromList (map amount rows)

      cols = V.fromList
        [ PW.ColInt64     idCol
        , PW.ColByteArray regionCol
        , PW.ColDouble    amountCol
        ]
  in  PW.buildParquetFile schema (V.singleton cols)

-- ---------------------------------------------------------------------------
-- Ground-truth (pure-Haskell) reference aggregates
-- ---------------------------------------------------------------------------

expectedTotals :: [Order] -> [(Text, Double, Int)]
expectedTotals rows = map summarise (uniq (map region rows))
  where
    summarise reg =
      let matching = filter ((== reg) . region) rows
      in  (reg, sum (map amount matching), length matching)

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

-- ---------------------------------------------------------------------------
-- Pulling typed values back out of a dataframe row
-- ---------------------------------------------------------------------------

-- | Pull the value of @resultCol :: Double@ from the row whose
-- @keyCol@ equals @keyVal@. Used here just to verify dataframe's
-- aggregate output matches our Haskell-side computation; in real
-- code you'd more typically keep the result inside dataframe and
-- use 'D.filterWhere' / 'D.select' to build the comparison.
lookupDouble :: D.DataFrame -> Text -> Text -> Text -> Double
lookupDouble df keyCol keyVal resultCol =
  case findRow keyCol keyVal df of
    Just r  -> rowDouble resultCol r
    Nothing -> error $ "lookupDouble: no row with "
                     <> T.unpack keyCol <> " = " <> T.unpack keyVal

lookupInt :: D.DataFrame -> Text -> Text -> Text -> Int
lookupInt df keyCol keyVal resultCol =
  case findRow keyCol keyVal df of
    Just r  -> rowInt resultCol r
    Nothing -> error $ "lookupInt: no row with "
                     <> T.unpack keyCol <> " = " <> T.unpack keyVal

-- | First row whose @keyCol@ matches @keyVal@, or 'Nothing'.
findRow :: Text -> Text -> D.DataFrame -> Maybe [(Text, D.Any)]
findRow keyCol keyVal df =
  let matches = filter (\r -> rowText keyCol r == keyVal) (D.toRowList df)
  in  case matches of
        (r:_) -> Just r
        []    -> Nothing

rowText :: Text -> [(Text, D.Any)] -> Text
rowText col r = case lookup col r of
  Just a -> case D.fromAny a :: Maybe Text of
    Just t  -> t
    Nothing -> error $ "rowText: column " <> T.unpack col <> " is not Text"
  Nothing -> error $ "rowText: missing column " <> T.unpack col

rowDouble :: Text -> [(Text, D.Any)] -> Double
rowDouble col r = case lookup col r of
  Just a -> case D.fromAny a :: Maybe Double of
    Just d  -> d
    Nothing -> error $ "rowDouble: column " <> T.unpack col <> " is not Double"
  Nothing -> error $ "rowDouble: missing column " <> T.unpack col

rowInt :: Text -> [(Text, D.Any)] -> Int
rowInt col r = case lookup col r of
  Just a -> case D.fromAny a :: Maybe Int of
    Just i  -> i
    Nothing -> error $ "rowInt: column " <> T.unpack col <> " is not Int"
  Nothing -> error $ "rowInt: missing column " <> T.unpack col

-- ---------------------------------------------------------------------------
-- Tiny assertion helpers
-- ---------------------------------------------------------------------------

assertClose :: String -> Double -> Double -> IO ()
assertClose msg expected actual
  | abs (expected - actual) < 1e-6 = pure ()
  | otherwise = error $ "assertion failed: " <> msg
              <> " expected " <> showD expected
              <> " got "      <> showD actual

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq msg expected actual
  | expected == actual = pure ()
  | otherwise = error $ "assertion failed: " <> msg
              <> " expected " <> show expected
              <> " got "      <> show actual

showD :: Double -> String
showD d =
  let !rounded = fromIntegral (round (d * 100) :: Int) / 100 :: Double
  in  show rounded
