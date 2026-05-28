{-# LANGUAGE OverloadedStrings #-}
module Test.Hermes.AcceptRanges (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Short as ST

import qualified Network.HTTP.Headers.AcceptRanges as AR
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util (Result (..), runParser)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parseOk :: ByteString -> Either String AR.AcceptRanges
parseOk bs = case runParser AR.acceptRangesParser bs of
  OK ar leftover
    | BS.null (BS.dropWhile (\w -> w == 0x20 || w == 0x09) leftover) ->
        Right ar
    | otherwise -> Left ("unconsumed: " <> show leftover)
  Fail    -> Left "parse failed"
  Err err -> Left err

render :: AR.AcceptRanges -> ByteString
render = M.toStrictByteString . AR.renderAcceptRanges

unit_none :: TestTree
unit_none = testCase "literal none disables ranges" $ do
  case parseOk "none" of
    Right AR.AcceptRangesNone -> pure ()
    other                     -> error (show other)
  assertEqual "render none" "none" (render AR.AcceptRangesNone)

unit_units :: TestTree
unit_units = testCase "unit list" $
  case parseOk "bytes, custom-unit" of
    Right (AR.AcceptRangesUnits (a :| [b])) -> do
      assertEqual "first"  (ST.fromString "bytes")       a
      assertEqual "second" (ST.fromString "custom-unit") b
    other -> error (show other)

unit_render_units :: TestTree
unit_render_units = testCase "render unit list" $
  let v = AR.AcceptRangesUnits (ST.fromString "bytes" :| [ST.fromString "rows"])
  in assertEqual "rendered" "bytes, rows" (render v)

tests :: TestTree
tests = testGroup "AcceptRanges"
  [ unit_none
  , unit_units
  , unit_render_units
  ]
