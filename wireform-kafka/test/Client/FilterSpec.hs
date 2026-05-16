{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the KIP-906 client-side record filter.
module Client.FilterSpec (tests) where

import qualified Data.HashSet as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Client.Filter as F

tests :: TestTree
tests = testGroup "ConsumerRecordFilter (KIP-906)"
  [ testCase "identityFilter keeps everything"
      identity_keeps_all
  , testCase "byKeyEquals matches exactly"
      key_equals
  , testCase "byHeaderEquals matches a specific header value"
      header_equals
  , testCase "byTopicIn keeps only listed topics"
      topic_in
  , testCase "<&&> intersects two filters"
      and_filter
  , testCase "<||> unions two filters"
      or_filter
  , testCase "negateFilter inverts"
      negate_filter
  ]

mkRec :: T.Text -> T.Text -> Maybe T.Text -> [(T.Text, T.Text)] -> C.ConsumerRecord
mkRec t k v hdrs = C.ConsumerRecord
  { topic     = t
  , partition = 0
  , offset    = 0
  , timestamp = 0
  , key       = fmap TE.encodeUtf8 (Just k)
  , value     = maybe "" TE.encodeUtf8 v
  , headers   = [(hk, TE.encodeUtf8 hv) | (hk, hv) <- hdrs]
  }

sample :: [C.ConsumerRecord]
sample =
  [ mkRec "a" "k1" (Just "v1") []
  , mkRec "a" "k2" (Just "v2") [("trace", "1")]
  , mkRec "b" "k3" (Just "v3") [("trace", "2")]
  ]

identity_keeps_all :: IO ()
identity_keeps_all = F.applyFilter F.identityFilter sample @?= sample

key_equals :: IO ()
key_equals =
  map (.key) (F.applyFilter (F.byKeyEquals "k2") sample)
    @?= [Just (TE.encodeUtf8 "k2")]

header_equals :: IO ()
header_equals =
  map (.key) (F.applyFilter (F.byHeaderEquals "trace" "1") sample)
    @?= [Just (TE.encodeUtf8 "k2")]

topic_in :: IO ()
topic_in =
  map (.topic) (F.applyFilter (F.byTopicIn (Set.singleton "b")) sample)
    @?= ["b"]

and_filter :: IO ()
and_filter =
  let f = F.byTopicIn (Set.singleton "a") F.<&&> F.byKeyEquals "k1"
  in map (.key) (F.applyFilter f sample) @?= [Just (TE.encodeUtf8 "k1")]

or_filter :: IO ()
or_filter =
  let f = F.byKeyEquals "k1" F.<||> F.byKeyEquals "k3"
  in map (.key) (F.applyFilter f sample)
    @?= [Just (TE.encodeUtf8 "k1"), Just (TE.encodeUtf8 "k3")]

negate_filter :: IO ()
negate_filter = do
  let f = F.negateFilter (F.byTopicIn (Set.singleton "a"))
  map (.topic) (F.applyFilter f sample) @?= ["b"]
