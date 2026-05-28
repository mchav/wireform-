{-# LANGUAGE OverloadedStrings #-}
{- |
Test the @rfc8941String@ escape that was previously disabled.

A round-trip through 'rfc8941String' must produce a quoted-string
on the wire that escapes any embedded DQUOTE or backslash, and
the @rfc8941String@ parser ('Network.HTTP.Headers.Parsing.Util.rfc8941String')
must accept it back as the same value.
-}
module Test.Hermes.RenderingUtil (tests) where

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Text.Short as ST

import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
  (Result (..), RFC8941String, mkRFC8941String, runParser, rfc8941String, unsafeToRFC8941String)
import qualified Network.HTTP.Headers.Rendering.Util as R
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

-- Build an RFC8941String value from a 'ByteString' for tests; the
-- input is restricted to bytes the parser will accept later.
mkVal :: ByteString -> RFC8941String
mkVal bs = case mkRFC8941String (st bs) of
  Just s  -> s
  Nothing -> error ("mkVal: input not RFC8941-stringable: " <> show bs)
  where
    st x = case ST.fromByteString x of
      Just s  -> s
      Nothing -> error "non-UTF-8 in test input"

renderToBytes :: RFC8941String -> ByteString
renderToBytes = M.toStrictByteString . R.rfc8941String

parseValue :: ByteString -> Either String RFC8941String
parseValue bs = case runParser rfc8941String bs of
  OK v leftover
    | BS.null leftover -> Right v
    | otherwise        -> Left ("unconsumed: " <> show leftover)
  Fail    -> Left "parse failed"
  Err err -> Left err

unit_simple :: TestTree
unit_simple = testCase "no escaping for plain ASCII" $
  let v   = mkVal "hello world"
      out = renderToBytes v
  in assertEqual "rendered" "\"hello world\"" out

unit_escape_quote :: TestTree
unit_escape_quote = testCase "embedded DQUOTE is escaped" $
  let v   = mkVal "a\"b"
      out = renderToBytes v
  in do
    assertEqual "rendered" "\"a\\\"b\"" out
    -- Parser recovers the original payload.
    case parseValue out of
      Right v' -> assertEqual "decoded"
                    (ST.toByteString (unsafeToRFC8941String v))
                    (ST.toByteString (unsafeToRFC8941String v'))
      Left err -> error err

unit_escape_backslash :: TestTree
unit_escape_backslash = testCase "embedded backslash is escaped" $
  let v   = mkVal "a\\b"
      out = renderToBytes v
  in do
    assertEqual "rendered" "\"a\\\\b\"" out
    case parseValue out of
      Right v' -> assertEqual "decoded"
                    (ST.toByteString (unsafeToRFC8941String v))
                    (ST.toByteString (unsafeToRFC8941String v'))
      Left err -> error err

unit_double_escape :: TestTree
unit_double_escape = testCase "both DQUOTE and backslash" $
  let v   = mkVal "a\"b\\c"
      out = renderToBytes v
  in do
    assertBool ("\\\" in " <> show out) ("\\\"" `BS.isInfixOf` out)
    assertBool ("\\\\ in " <> show out) ("\\\\" `BS.isInfixOf` out)

tests :: TestTree
tests = testGroup "RenderingUtil"
  [ unit_simple
  , unit_escape_quote
  , unit_escape_backslash
  , unit_double_escape
  ]
