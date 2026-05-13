-- | Benchmark data types + I/O.
--
-- Two layers:
--
-- * 'CriterionReport' parses the @--json@ output criterion writes for
--   each @cabal bench@ run. It is wider than we want and contains
--   per-iteration regression information we don't surface in the
--   README.
--
-- * 'BenchSummary' is the distilled shape we commit to the per-package
--   @bench-results/summary/@ directory. One file per benchmark
--   comparison (e.g. @cbor-vs-cborg-encode.json@), shared between the
--   table renderer ('Wireform.Stats.Table') and the SVG renderer
--   ('Wireform.Stats.SVG').
--
-- The split exists so a slow benchmark run can produce a stable
-- summary that the regen tool consumes without having to re-parse the
-- (much larger) criterion JSON every time.
module Wireform.Stats.Bench
  ( -- * Criterion JSON
    CriterionReport (..)
  , CriterionMeasurement (..)
  , parseCriterionJson
    -- * Summary
  , BenchSummary (..)
  , BenchSeries (..)
  , Unit (..)
  , unitText
  , distillSummary
  , readSummary
  , writeSummary
    -- * Conversion to render-time inputs
  , summaryToBarChart
  , summaryToTable
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , (.:)
  , (.:?)
  , (.=)
  , eitherDecodeStrict
  , encode
  , object
  , withObject
  )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)

import Wireform.Stats.SVG qualified as SVG
import Wireform.Stats.Table qualified as Tbl

-- ---------------------------------------------------------------------------
-- Criterion JSON
-- ---------------------------------------------------------------------------

-- | The subset of criterion's JSON output we care about. Full schema:
-- <https://hackage.haskell.org/package/criterion-1.6.4.0/docs/Criterion-Internal.html>.
data CriterionReport = CriterionReport
  { crBenchName    :: !Text
  , crMeanSeconds  :: !Double
  , crStddevSeconds :: !Double
  , crMeasurements :: ![CriterionMeasurement]
  } deriving stock (Eq, Show, Generic)

data CriterionMeasurement = CriterionMeasurement
  { cmIters   :: !Double
  , cmTimeSec :: !Double
  } deriving stock (Eq, Show, Generic)

instance FromJSON CriterionReport where
  parseJSON = withObject "CriterionReport" $ \o -> do
    name        <- o .: "reportName"
    analysis    <- o .: "reportAnalysis"
    mean        <- analysis .: "anMean"
    stdDev      <- analysis .: "anStdDev"
    meanSec     <- mean   .: "estPoint"
    stdDevSec   <- stdDev .: "estPoint"
    rawMeas     <- o .:? "reportMeasured"
    let measurements = case rawMeas of
          Nothing -> []
          Just xs -> map fromMeasured xs
    pure CriterionReport
      { crBenchName     = name
      , crMeanSeconds   = meanSec
      , crStddevSeconds = stdDevSec
      , crMeasurements  = measurements
      }

-- | A single criterion 'Measurement' as it appears in the JSON
-- output. Only the two fields we need are extracted.
fromMeasured :: Measured -> CriterionMeasurement
fromMeasured (Measured i t) = CriterionMeasurement i t

data Measured = Measured !Double !Double

instance FromJSON Measured where
  parseJSON = withObject "Measured" $ \o ->
    Measured <$> o .: "measIters" <*> o .: "measTime"

-- | Parse criterion's @--json@ output (a JSON array of reports).
parseCriterionJson :: ByteString -> Either String [CriterionReport]
parseCriterionJson = eitherDecodeStrict

-- ---------------------------------------------------------------------------
-- Summary
-- ---------------------------------------------------------------------------

-- | The distilled per-comparison shape. One 'BenchSummary' = one
-- markdown table + one chart.
data BenchSummary = BenchSummary
  { bsId            :: !Text
    -- ^ Stable identifier; matches the @bench:\<id\>@ marker key in
    -- the README and the filename under @bench-results/summary/@.
  , bsTitle         :: !Text
    -- ^ Human-readable title shown above the chart and in the table
    -- header sentence.
  , bsUnit          :: !Unit
  , bsHigherIsBetter :: !Bool
  , bsGroups        :: ![Text]
    -- ^ Group labels (e.g. @["encode", "decode"]@).
  , bsSeries        :: ![BenchSeries]
    -- ^ One series per legend entry; values aligned with 'bsGroups'.
  , bsBaseline      :: !(Maybe Text)
    -- ^ Optional name of the series used as the @1.00x@ ratio
    -- baseline in the table.
  , bsCapturedAt    :: !UTCTime
  , bsToolchain     :: !Text
    -- ^ Free-form description of the GHC + OS + CPU the numbers
    -- were captured on.
  } deriving stock (Eq, Show, Generic)

data BenchSeries = BenchSeries
  { bsName   :: !Text
  , bsValues :: ![Double]
  } deriving stock (Eq, Show, Generic)

data Unit = Nanos | Micros | Millis | Seconds | BytesPerSec | OpsPerSec
  deriving stock (Eq, Show, Generic)

unitText :: Unit -> Text
unitText Nanos       = "ns"
unitText Micros      = "µs"
unitText Millis      = "ms"
unitText Seconds     = "s"
unitText BytesPerSec = "MB/s"
unitText OpsPerSec   = "ops/s"

instance ToJSON Unit where
  toJSON = toJSON . show
instance FromJSON Unit where
  parseJSON v = do
    (s :: String) <- parseJSON v
    case s of
      "Nanos"       -> pure Nanos
      "Micros"      -> pure Micros
      "Millis"      -> pure Millis
      "Seconds"     -> pure Seconds
      "BytesPerSec" -> pure BytesPerSec
      "OpsPerSec"   -> pure OpsPerSec
      other         -> fail ("Unit: unknown " <> other)

instance ToJSON BenchSeries where
  toJSON s = object
    [ "name"   .= bsName s
    , "values" .= bsValues s
    ]
instance FromJSON BenchSeries where
  parseJSON = withObject "BenchSeries" $ \o ->
    BenchSeries <$> o .: "name" <*> o .: "values"

instance ToJSON BenchSummary where
  toJSON s = object
    [ "id"             .= bsId s
    , "title"          .= bsTitle s
    , "unit"           .= bsUnit s
    , "higherIsBetter" .= bsHigherIsBetter s
    , "groups"         .= bsGroups s
    , "series"         .= bsSeries s
    , "baseline"       .= bsBaseline s
    , "capturedAt"     .= bsCapturedAt s
    , "toolchain"      .= bsToolchain s
    ]
instance FromJSON BenchSummary where
  parseJSON = withObject "BenchSummary" $ \o ->
    BenchSummary
      <$> o .:  "id"
      <*> o .:  "title"
      <*> o .:  "unit"
      <*> o .:  "higherIsBetter"
      <*> o .:  "groups"
      <*> o .:  "series"
      <*> o .:? "baseline"
      <*> o .:  "capturedAt"
      <*> o .:  "toolchain"

readSummary :: FilePath -> IO (Either String BenchSummary)
readSummary p = eitherDecodeStrict <$> BS.readFile p

writeSummary :: FilePath -> BenchSummary -> IO ()
writeSummary p s = BSL.writeFile p (encode s <> BSL.singleton 10)

-- | Build a 'BenchSummary' from a list of @(seriesName, [(group, seconds)])@
-- triples. The current time is used as @capturedAt@; the toolchain
-- string is the free-form caller-provided description.
distillSummary
  :: Text                                   -- ^ id
  -> Text                                   -- ^ title
  -> Unit                                   -- ^ unit
  -> Bool                                   -- ^ higher is better
  -> Maybe Text                             -- ^ baseline series name
  -> Text                                   -- ^ toolchain
  -> [Text]                                 -- ^ groups
  -> [(Text, [Double])]                     -- ^ series (in chosen unit)
  -> IO BenchSummary
distillSummary i t u hib baseline toolchain groups series = do
  now <- getCurrentTime
  pure BenchSummary
    { bsId             = i
    , bsTitle          = t
    , bsUnit           = u
    , bsHigherIsBetter = hib
    , bsGroups         = groups
    , bsSeries         = [BenchSeries n vs | (n, vs) <- series]
    , bsBaseline       = baseline
    , bsCapturedAt     = now
    , bsToolchain      = toolchain
    }

-- ---------------------------------------------------------------------------
-- Conversions for downstream renderers
-- ---------------------------------------------------------------------------

summaryToBarChart :: BenchSummary -> SVG.BarChart
summaryToBarChart s = SVG.BarChart
  { SVG.chartTitle    = bsTitle s
  , SVG.chartSubtitle = Just (bsToolchain s)
  , SVG.chartUnit     = unitText (bsUnit s)
  , SVG.chartGroups   = bsGroups s
  , SVG.chartSeries   = [SVG.Series (bsName ss) (bsValues ss) | ss <- bsSeries s]
  , SVG.chartHigherIsBetter = bsHigherIsBetter s
  }

summaryToTable :: BenchSummary -> Tbl.Table
summaryToTable s =
  let header = "Operation" : map bsName (bsSeries s) ++ ["ratio"]
      align  = Tbl.AlignLeft : replicate (length (bsSeries s)) Tbl.AlignRight ++ [Tbl.AlignRight]
      baselineSeries =
        case bsBaseline s of
          Just n -> lookup n [(bsName ss, bsValues ss) | ss <- bsSeries s]
          Nothing -> case bsSeries s of
            (ss : _) -> Just (bsValues ss)
            []       -> Nothing
      mkRow gi g =
        let cell ss = formatValue (bsUnit s) (nthOr 0 gi (bsValues ss))
            ratio = case (baselineSeries, bsSeries s) of
              (Just bv, _ : nextSeries : _) ->
                let base = nthOr 0 gi bv
                    -- For "higher is better" we report each series
                    -- relative to the baseline; for "lower is better"
                    -- we invert so >1.00 always means "the comparison
                    -- series is better."
                    cmp  = nthOr 0 gi (bsValues nextSeries)
                    raw  = if base == 0 then 0 else cmp / base
                    adj  = if bsHigherIsBetter s then raw else (if raw == 0 then 0 else 1 / raw)
                in formatRatio adj
              _ -> "-"
        in g : map cell (bsSeries s) ++ [ratio]
  in Tbl.Table
       { Tbl.tableHeader = header
       , Tbl.tableAlign  = align
       , Tbl.tableRows   = zipWith mkRow [0 ..] (bsGroups s)
       }

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

nthOr :: a -> Int -> [a] -> a
nthOr d _ [] = d
nthOr _ 0 (x : _) = x
nthOr d n (_ : xs) = nthOr d (n - 1) xs

formatValue :: Unit -> Double -> Text
formatValue u v = T.pack (showWithPrecision v) <> " " <> unitText u
  where
    showWithPrecision x
      | abs x >= 1000 = show (round x :: Int)
      | abs x >= 100  = show (round x :: Int)
      | abs x >= 10   = case (properFraction x :: (Int, Double)) of
          (i, f) -> show i <> "." <> show (round (f * 10) :: Int)
      | otherwise     = case (properFraction x :: (Int, Double)) of
          (i, f) -> show i <> "." <> pad2 (show (round (abs f * 100) :: Int))

    pad2 s = if length s < 2 then '0' : s else s

formatRatio :: Double -> Text
formatRatio r = T.pack (showWith r) <> "x"
  where
    showWith x = case (properFraction x :: (Int, Double)) of
      (i, f) -> show i <> "." <> pad2 (show (round (abs f * 100) :: Int))
    pad2 s = if length s < 2 then '0' : s else s
