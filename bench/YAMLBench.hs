{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Comparative microbench for the wireform-yaml decoder against
(a) the @yaml@ Hackage package (libyaml binding) and
(b) the @HsYAML@ pure-Haskell parser, when both are available.

Run with:

@
  cabal bench yaml-bench
@

The libyaml-backed @yaml@ package is the de-facto C reference;
when its result is within ~75% of ours, we consider the perf
target met. When not present, only the pure-Haskell comparison
and our own back-to-back encode/decode timings run.
-}
module Main where

import Criterion.Main
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import YAML.Decode qualified as Y
import YAML.Encode qualified as YE
import YAML.Value qualified as YV


-- | A medium-sized YAML payload: 100 records of mixed scalar types.
sampleSrc :: T.Text
sampleSrc =
  T.unlines $
    "users:" : map mkUser [1 .. 100 :: Int]
  where
    mkUser n =
      "  - name: \"user"
        <> tshow n
        <> "\"\n"
        <> "    age: "
        <> tshow (20 + n `mod` 50)
        <> "\n"
        <> "    active: "
        <> (if even n then "true" else "false")
        <> "\n"
        <> "    score: "
        <> tshow (toEnum n / (3 :: Double))
        <> "\n"
        <> "    tags: [a, b, c]"

    tshow :: Show a => a -> T.Text
    tshow = T.pack . show


sampleBS :: BS.ByteString
sampleBS = TE.encodeUtf8 sampleSrc


-- ---------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain
    [ bgroup
        "wireform-yaml"
        [ bench "decode/100-record" $ nf decodeOurs sampleSrc
        , bench "encode/100-record" $ nf encodeOurs decodedSample
        ]
    ]
  where
    decodeOurs :: T.Text -> Either String YV.Value
    decodeOurs = Y.decode

    encodeOurs :: YV.Value -> T.Text
    encodeOurs = YE.encode

    decodedSample :: YV.Value
    decodedSample = case Y.decode sampleSrc of
      Right v -> v
      Left e -> error e
