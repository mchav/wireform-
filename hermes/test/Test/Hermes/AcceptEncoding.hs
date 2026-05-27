{-# LANGUAGE OverloadedStrings #-}
module Test.Hermes.AcceptEncoding (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Hedgehog ((===), Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.HTTP.ContentCoding as CC
import qualified Network.HTTP.Headers.AcceptEncoding as AE
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (assertEqual, testCase)

parseOk :: ByteString -> Either String AE.AcceptEncoding
parseOk bs = case runParser AE.acceptEncodingParser bs of
  OK ae leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right ae
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail    -> Left "parse failed"
  Err err -> Left err

render :: AE.AcceptEncoding -> ByteString
render = M.toStrictByteString . AE.renderAcceptEncoding

unit_single :: TestTree
unit_single = testCase "single coding" $
  case parseOk "gzip" of
    Right (AE.AcceptEncoding [AE.WeightedEncoding (AE.NamedEncoding CC.GZip) 1]) -> pure ()
    other -> error ("unexpected parse: " <> show other)

-- Regression for the wireform-core switch bug fixed alongside
-- this change: a switch-literal that is both a terminal /and/ a
-- prefix of a longer literal used to consume one byte past the
-- terminal before falling back to the terminal action. That
-- broke every quality-weighted Accept-* parser whose first
-- entry's @q=1.0@ ended at a separator (the parser would eat
-- the comma too, then refuse to continue).
unit_q1_followed_by_more :: TestTree
unit_q1_followed_by_more = testCase "q=1.0 followed by another entry" $
  case parseOk "br;q=1.0, gzip;q=0.5, *;q=0" of
    Right (AE.AcceptEncoding xs) ->
      assertEqual "entry count" 3 (length xs)
    other -> error (show other)

unit_multi_with_q :: TestTree
unit_multi_with_q = testCase "multi-coding with q values" $
  case parseOk "br;q=1.0, gzip;q=0.5, *;q=0" of
    Right (AE.AcceptEncoding [b, g, w]) -> do
      assertEqual "br tag"     (AE.NamedEncoding CC.Brotli) (AE.encodingTag b)
      assertEqual "br weight"  1                            (AE.encodingWeight b)
      assertEqual "gzip tag"   (AE.NamedEncoding CC.GZip)   (AE.encodingTag g)
      assertEqual "gzip q"     0.5                          (AE.encodingWeight g)
      assertEqual "* tag"      AE.AnyEncoding               (AE.encodingTag w)
      assertEqual "* q"        0                            (AE.encodingWeight w)
    other -> error ("unexpected parse: " <> show other)

unit_render_omits_q1 :: TestTree
unit_render_omits_q1 = testCase "renderer omits q=1.0" $
  let v = AE.AcceptEncoding
        [ AE.WeightedEncoding (AE.NamedEncoding CC.GZip) 1
        , AE.WeightedEncoding (AE.NamedEncoding CC.Brotli) 0.8
        ]
  in assertEqual "rendered" "gzip, br;q=0.8" (render v)

-- ---------------------------------------------------------------------------
-- Property: render → parse round-trip
-- ---------------------------------------------------------------------------

codingGen :: Gen CC.ContentCoding
codingGen = Gen.element [CC.GZip, CC.Brotli, CC.Deflate, CC.ZStd, CC.Identity]

tagGen :: Gen AE.EncodingTag
tagGen = Gen.choice
  [ pure AE.AnyEncoding
  , AE.NamedEncoding <$> codingGen
  ]

-- Stick to the discrete weights the parser actually distinguishes
-- (0, 0.1, 0.5, 0.999, 1) so the round-trip doesn't get tripped
-- up by floating-point representation drift.
weightGen :: Gen Double
weightGen = Gen.element [0, 0.1, 0.5, 0.999, 1]

wencGen :: Gen AE.WeightedEncoding
wencGen = AE.WeightedEncoding <$> tagGen <*> weightGen

prop_roundtrip :: Property
prop_roundtrip = property $ do
  ws <- forAll (Gen.list (Range.linear 1 5) wencGen)
  let bs = render (AE.AcceptEncoding ws)
  case parseOk bs of
    Right (AE.AcceptEncoding ws') -> ws === ws'
    Left err -> error ("round-trip failed: " <> err <> " on " <> show bs)

tests :: TestTree
tests = testGroup "AcceptEncoding"
  [ unit_single
  , unit_q1_followed_by_more
  , unit_multi_with_q
  , unit_render_omits_q1
  , testProperty "round-trip" prop_roundtrip
  ]
