{-# LANGUAGE OverloadedStrings #-}
module Test.Hermes.Range (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Hedgehog ((===), Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import qualified Network.HTTP.Headers.Range as R
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (assertEqual, testCase)

parseOk :: ByteString -> Either String R.Range
parseOk bs = case runParser R.rangeParser bs of
  OK r leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right r
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail    -> Left "parse failed"
  Err err -> Left err

render :: R.Range -> ByteString
render = M.toStrictByteString . R.renderRange

unit_closed_range :: TestTree
unit_closed_range = testCase "closed byte range" $
  case parseOk "bytes=0-499" of
    Right (R.ByteRanges (R.ByteRangeInt 0 (Just 499) :| [])) -> pure ()
    other -> error (show other)

unit_open_range :: TestTree
unit_open_range = testCase "open-ended range" $
  case parseOk "bytes=500-" of
    Right (R.ByteRanges (R.ByteRangeInt 500 Nothing :| [])) -> pure ()
    other -> error (show other)

unit_suffix :: TestTree
unit_suffix = testCase "suffix range" $
  case parseOk "bytes=-1024" of
    Right (R.ByteRanges (R.ByteRangeSuffix 1024 :| [])) -> pure ()
    other -> error (show other)

unit_multi :: TestTree
unit_multi = testCase "multiple byte ranges" $
  case parseOk "bytes=0-499, 1000-, -500" of
    Right (R.ByteRanges ne) ->
      assertEqual "ranges"
        [ R.ByteRangeInt 0 (Just 499)
        , R.ByteRangeInt 1000 Nothing
        , R.ByteRangeSuffix 500
        ]
        (NE.toList ne)
    other -> error (show other)

unit_render :: TestTree
unit_render = testCase "render closed + open + suffix" $
  let v = R.ByteRanges (R.ByteRangeInt 0 (Just 99) :|
                          [ R.ByteRangeInt 200 Nothing
                          , R.ByteRangeSuffix 50
                          ])
  in assertEqual "rendered" "bytes=0-99,200-,-50" (render v)

-- ---------------------------------------------------------------------------
-- Property: bytes ranges round-trip
-- ---------------------------------------------------------------------------

brGen :: Gen R.ByteRange
brGen = Gen.choice
  [ do a <- Gen.word64 (Range.linear 0 1_000_000)
       b <- Gen.word64 (Range.linear a (a + 1_000_000))
       pure (R.ByteRangeInt a (Just b))
  , R.ByteRangeInt   <$> Gen.word64 (Range.linear 0 1_000_000) <*> pure Nothing
  , R.ByteRangeSuffix <$> Gen.word64 (Range.linear 1 1_000_000)
  ]

prop_roundtrip :: Property
prop_roundtrip = property $ do
  rs <- forAll (Gen.list (Range.linear 1 5) brGen)
  let r = R.ByteRanges (NE.fromList rs)
      bs = render r
  case parseOk bs of
    Right r' -> r === r'
    Left err -> error (err <> " on " <> show bs)

tests :: TestTree
tests = testGroup "Range"
  [ unit_closed_range
  , unit_open_range
  , unit_suffix
  , unit_multi
  , unit_render
  , testProperty "round-trip" prop_roundtrip
  ]
