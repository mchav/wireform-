{-# LANGUAGE OverloadedStrings #-}
module Test.Hermes.ContentRange (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST

import Hedgehog ((===), Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.Headers.ContentRange as CR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (assertEqual, testCase)

parseOk :: ByteString -> Either String CR.ContentRange
parseOk bs = case runParser CR.contentRangeParser bs of
  OK c leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right c
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail    -> Left "parse failed"
  Err err -> Left err

render :: CR.ContentRange -> ByteString
render = M.toStrictByteString . CR.renderContentRange

unit_satisfied :: TestTree
unit_satisfied = testCase "satisfied bytes/total" $
  case parseOk "bytes 0-499/1234" of
    Right (CR.ContentRange unit (CR.RangeRespSatisfied 0 499 (Just 1234))) ->
      assertEqual "unit" (ST.fromString "bytes") unit
    other -> error (show other)

unit_satisfied_star :: TestTree
unit_satisfied_star = testCase "satisfied with unknown total" $
  case parseOk "bytes 0-9/*" of
    Right (CR.ContentRange _ (CR.RangeRespSatisfied 0 9 Nothing)) -> pure ()
    other -> error (show other)

unit_unsatisfied :: TestTree
unit_unsatisfied = testCase "unsatisfied range" $
  case parseOk "bytes */4096" of
    Right (CR.ContentRange _ (CR.RangeRespUnsatisfied (Just 4096))) -> pure ()
    other -> error (show other)

unit_render_satisfied :: TestTree
unit_render_satisfied = testCase "render satisfied" $
  let v = CR.ContentRange (ST.fromString "bytes")
                          (CR.RangeRespSatisfied 0 99 (Just 200))
  in assertEqual "rendered" "bytes 0-99/200" (render v)

unit_render_unsatisfied :: TestTree
unit_render_unsatisfied = testCase "render unsatisfied" $
  let v = CR.ContentRange (ST.fromString "bytes")
                          (CR.RangeRespUnsatisfied (Just 4096))
  in assertEqual "rendered" "bytes */4096" (render v)

-- Property: satisfied form round-trips for arbitrary positions.
respGen :: Gen CR.RangeResp
respGen = Gen.choice
  [ do a <- Gen.word64 (Range.linear 0 1_000_000)
       b <- Gen.word64 (Range.linear a (a + 1_000_000))
       t <- Gen.maybe (Gen.word64 (Range.linear (b + 1) 2_000_000))
       pure (CR.RangeRespSatisfied a b t)
  , CR.RangeRespUnsatisfied <$>
      Gen.maybe (Gen.word64 (Range.linear 0 1_000_000))
  ]

prop_roundtrip :: Property
prop_roundtrip = property $ do
  resp <- forAll respGen
  let v  = CR.ContentRange (ST.fromString "bytes") resp
      bs = render v
  case parseOk bs of
    Right v' -> v === v'
    Left err -> error (err <> " on " <> show bs)

tests :: TestTree
tests = testGroup "ContentRange"
  [ unit_satisfied
  , unit_satisfied_star
  , unit_unsatisfied
  , unit_render_satisfied
  , unit_render_unsatisfied
  , testProperty "round-trip" prop_roundtrip
  ]
