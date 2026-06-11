{- | Test-result data layer.

Parses the JUnit XML that @tasty --xml=foo.xml@ writes. This is
the standard format the rest of the testing world uses, and
supported by tasty out of the box, so we don't have to add a
per-test-suite dep on a custom ingredient.

We dogfood [wireform-xml](../../wireform-xml/) for the parse: the
JUnit document is loaded with 'XML.Decode.decode' and walked via
'XML.Path' helpers.
-}
module Wireform.Stats.Test (
  -- * Summary
  TestSummary (..),
  SuiteSummary (..),
  parseJUnit,
  readJUnit,

  -- * Render
  summaryToTestLine,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Data.Vector qualified as V
import XML.Decode qualified as XD
import XML.Value (
  Attribute (..),
  Document (..),
  Name (..),
  Node (..),
 )


-- ---------------------------------------------------------------------------
-- Summary
-- ---------------------------------------------------------------------------

-- | Per-package test summary, distilled from a tasty JUnit XML file.
data TestSummary = TestSummary
  { tsTotal :: !Int
  , tsPassed :: !Int
  , tsFailures :: !Int
  , tsErrors :: !Int
  , tsSkipped :: !Int
  , tsTimeSec :: !Double
  , tsSuites :: ![SuiteSummary]
  }
  deriving stock (Eq, Show)


-- | Per-test-suite breakdown.
data SuiteSummary = SuiteSummary
  { ssName :: !Text
  , ssTotal :: !Int
  , ssFailures :: !Int
  , ssErrors :: !Int
  , ssSkipped :: !Int
  , ssTimeSec :: !Double
  }
  deriving stock (Eq, Show)


-- | Parse a JUnit XML byte string into a 'TestSummary'.
parseJUnit :: ByteString -> Either String TestSummary
parseJUnit bytes = do
  -- tasty emits a single \<testsuites\> root with one \<testsuite\>
  -- per group.
  doc <- XD.decode bytes
  let suites = childElementsNamed "testsuite" (docRoot doc)
      ss = map readSuite suites
  pure
    TestSummary
      { tsTotal = sum (map ssTotal ss)
      , tsPassed = sum (map (\s -> ssTotal s - ssFailures s - ssErrors s - ssSkipped s) ss)
      , tsFailures = sum (map ssFailures ss)
      , tsErrors = sum (map ssErrors ss)
      , tsSkipped = sum (map ssSkipped ss)
      , tsTimeSec = sum (map ssTimeSec ss)
      , tsSuites = ss
      }


-- | Direct children of an element with the given local name.
childElementsNamed :: Text -> Node -> [Node]
childElementsNamed name (Element _ _ kids) =
  [ k
  | k <- V.toList kids
  , case k of
      Element (Name local _ _) _ _ -> local == name
      _ -> False
  ]
childElementsNamed _ _ = []


-- | Convenience: read + parse a JUnit XML file.
readJUnit :: FilePath -> IO (Either String TestSummary)
readJUnit p = parseJUnit <$> BS.readFile p


readSuite :: Node -> SuiteSummary
readSuite n =
  SuiteSummary
    { ssName = attr "name" n & fromMaybe "?"
    , ssTotal = attr "tests" n & maybe 0 readInt
    , ssFailures = attr "failures" n & maybe 0 readInt
    , ssErrors = attr "errors" n & maybe 0 readInt
    , ssSkipped = attr "skipped" n & maybe 0 readInt
    , ssTimeSec = attr "time" n & maybe 0 readDouble
    }


-- ---------------------------------------------------------------------------
-- Render
-- ---------------------------------------------------------------------------

{- | A single human-readable line summarising the suite, e.g.
@347 tests passing across 4 categories. 0 failures, 0 errors,
0 skipped. 1.34 s.@
-}
summaryToTestLine :: TestSummary -> Text
summaryToTestLine s =
  let plural n one many = if n == 1 then one else many
      tests =
        T.pack (show (tsTotal s))
          <> " "
          <> plural (tsTotal s) "test" "tests"
          <> " "
          <> if tsFailures s == 0 && tsErrors s == 0
            then "passing"
            else "(" <> T.pack (show (tsPassed s)) <> " passing)"
      groups =
        T.pack (show (length (tsSuites s)))
          <> " "
          <> plural (length (tsSuites s)) "category" "categories"
      failures = T.pack (show (tsFailures s)) <> " " <> plural (tsFailures s) "failure" "failures"
      errors = T.pack (show (tsErrors s)) <> " " <> plural (tsErrors s) "error" "errors"
      skipped = T.pack (show (tsSkipped s)) <> " skipped"
      time = T.pack (formatTime (tsTimeSec s))
  in tests <> " across " <> groups <> ". " <> failures <> ", " <> errors <> ", " <> skipped <> ". " <> time <> "."


formatTime :: Double -> String
formatTime t
  | t < 1 = let ms = round (t * 1000) :: Int in show ms <> " ms"
  | t < 60 =
      let s = round (t * 100) :: Int
      in show (s `div` 100) <> "." <> pad2 (show (s `mod` 100)) <> " s"
  | otherwise = let m = round (t / 60) :: Int in show m <> " min"
  where
    pad2 s = if length s < 2 then '0' : s else s


-- ---------------------------------------------------------------------------
-- Internals
-- ---------------------------------------------------------------------------

attr :: Text -> Node -> Maybe Text
attr k (Element _ as _) =
  let go i =
        if i >= V.length as
          then Nothing
          else case V.unsafeIndex as i of
            Attribute (Name local _ _) v
              | local == k -> Just v
              | otherwise -> go (i + 1)
  in go 0
attr _ _ = Nothing


readInt :: Text -> Int
readInt t = case TR.decimal t of
  Right (n, _) -> n
  Left _ -> 0


readDouble :: Text -> Double
readDouble t = case TR.double t of
  Right (n, _) -> n
  Left _ -> 0


(&) :: a -> (a -> b) -> b
x & f = f x


infixl 1 &
