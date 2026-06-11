{- | shields.io \"endpoint\" badge JSON emitter.

shields.io's \"endpoint\" badge type fetches a JSON document from a
caller-controlled URL and renders a SVG badge from it. Schema:
<https://shields.io/badges/endpoint-badge>.

We commit one such JSON file per (package, badge-kind) pair under
@badges/@ at the repo root. README files reference them as:

@
![tests](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/iand675/wireform-/main/badges/wireform-cbor-tests.json)
@

so the badges live-update on every push to @main@ without anyone
rewriting the README itself.
-}
module Wireform.Stats.Shields (
  -- * Badge spec
  Badge (..),
  Color (..),
  colorText,
  writeBadge,

  -- * Constructors for the badges we use
  testsBadge,
  coverageBadge,
) where

import Data.Aeson (ToJSON (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Wireform.Stats.Coverage (CoverageSummary (..))
import Wireform.Stats.Test (TestSummary (..))


-- ---------------------------------------------------------------------------
-- Badge
-- ---------------------------------------------------------------------------

{- | The subset of the shields.io endpoint schema we use. Other
fields (style, namedLogo, namedLogoColor, etc.) can be added when
we have a use for them.
-}
data Badge = Badge
  { badgeLabel :: !Text
  -- ^ Left side of the badge. Conventionally lowercase.
  , badgeMessage :: !Text
  -- ^ Right side of the badge.
  , badgeColor :: !Color
  }
  deriving stock (Eq, Show)


instance ToJSON Badge where
  toJSON b =
    object
      [ "schemaVersion" .= (1 :: Int)
      , "label" .= badgeLabel b
      , "message" .= badgeMessage b
      , "color" .= colorText (badgeColor b)
      ]


{- | Standard shields.io named colors plus an escape hatch for
explicit hex.
-}
data Color
  = ColorBrightgreen
  | ColorGreen
  | ColorYellow
  | ColorYellowgreen
  | ColorOrange
  | ColorRed
  | ColorBlue
  | ColorLightgrey
  | ColorHex !Text
  deriving stock (Eq, Show)


colorText :: Color -> Text
colorText ColorBrightgreen = "brightgreen"
colorText ColorGreen = "green"
colorText ColorYellow = "yellow"
colorText ColorYellowgreen = "yellowgreen"
colorText ColorOrange = "orange"
colorText ColorRed = "red"
colorText ColorBlue = "blue"
colorText ColorLightgrey = "lightgrey"
colorText (ColorHex t) = t


writeBadge :: FilePath -> Badge -> IO ()
writeBadge p b = BSL.writeFile p (encode b <> BSL.singleton 10)


-- ---------------------------------------------------------------------------
-- Constructors
-- ---------------------------------------------------------------------------

{- | Build a tests badge.

* @passing N@ in green when no failures or errors.
* @N \/ M@ in red otherwise.
-}
testsBadge :: TestSummary -> Badge
testsBadge ts
  | tsTotal ts == 0 =
      Badge "tests" "no data" ColorLightgrey
  | tsFailures ts == 0 && tsErrors ts == 0 =
      Badge
        "tests"
        (T.pack (show (tsTotal ts)) <> " passing")
        ColorBrightgreen
  | otherwise =
      Badge
        "tests"
        (T.pack (show (tsPassed ts)) <> " / " <> T.pack (show (tsTotal ts)))
        ColorRed


{- | Build a coverage badge keyed off the top-line expressions percent.

Color buckets follow the convention codecov.io uses by default.
-}
coverageBadge :: CoverageSummary -> Badge
coverageBadge cs =
  let pct = covExpressions cs
      msg = T.pack (showFixed1 pct) <> "%"
      color
        | pct >= 90 = ColorBrightgreen
        | pct >= 80 = ColorGreen
        | pct >= 70 = ColorYellowgreen
        | pct >= 60 = ColorYellow
        | pct >= 50 = ColorOrange
        | otherwise = ColorRed
  in Badge "coverage" msg color


showFixed1 :: Double -> String
showFixed1 x =
  let n = round (x * 10) :: Int
  in show (n `div` 10) <> "." <> show (n `mod` 10)
