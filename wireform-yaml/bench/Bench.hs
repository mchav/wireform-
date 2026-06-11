{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Data.YAML qualified as HsYAML
import Data.Yaml qualified as Libyaml
import YAML.Decode qualified (preprocess)
import YAML.Decode qualified as Y
import YAML.Encode qualified as YE
import YAML.Value qualified as YV


-- | A tiny mapping (~20 byte input).
tiny :: BS.ByteString
tiny = "key: value"


-- | A mid-size nested document (a few hundred bytes).
small :: BS.ByteString
small =
  BS8.unlines
    [ "name: example"
    , "version: 1.2.3"
    , "tags:"
    , "  - haskell"
    , "  - yaml"
    , "  - parser"
    , "owner:"
    , "  name: Alice"
    , "  email: alice@example.com"
    , "config:"
    , "  threads: 8"
    , "  timeout: 30"
    , "  retry: true"
    , "  endpoints:"
    , "    - https://example.com"
    , "    - https://example.org"
    ]


-- | A flow-heavy mid-size document.
flowMid :: BS.ByteString
flowMid =
  BS8.unlines
    [ "matrix: [[1,2,3], [4,5,6], [7,8,9]]"
    , "tags: { a: 1, b: 2, c: 3, d: 4 }"
    , "items: [{x: 1, y: 2}, {x: 3, y: 4}, {x: 5, y: 6}]"
    ]


-- | A scalar-heavy document (many quoted strings).
quoted :: BS.ByteString
quoted =
  BS8.unlines $
    "items:"
      : [ "  - \"item " <> BS8.pack (show n) <> " with some quoted text\""
        | n <- [(1 :: Int) .. 50]
        ]


-- | A block scalar / literal text body.
literalBody :: BS.ByteString
literalBody =
  BS8.unlines $
    "code: |"
      : [ "  line " <> BS8.pack (show n) <> " of body text"
        | n <- [(1 :: Int) .. 50]
        ]


-- | A larger doc (~2 KB) of mixed content.
big :: BS.ByteString
big = BS.concat (replicate 4 small)


decodeBytes :: BS.ByteString -> Either String YV.Stream
decodeBytes = Y.decodeStreamBS


decodeHsYAML :: BS.ByteString -> Either String Int
decodeHsYAML bs = case HsYAML.decodeNode (BSL.fromStrict bs) of
  Right ns -> Right (length ns)
  Left (_, e) -> Left e


decodeLibyaml :: BS.ByteString -> Either String Aeson.Value
decodeLibyaml bs = case Libyaml.decodeEither' bs of
  Right v -> Right v
  Left e -> Left (show e)


main :: IO ()
main =
  defaultMain
    [ bgroup
        "decode"
        [ bench "tiny" $ nf decodeBytes tiny
        , bench "small" $ nf decodeBytes small
        , bench "flowMid" $ nf decodeBytes flowMid
        , bench "quoted50" $ nf decodeBytes quoted
        , bench "literal50" $ nf decodeBytes literalBody
        , bench "big" $ nf decodeBytes big
        ]
    , bgroup
        "decode-hsyaml"
        [ bench "tiny" $ nf decodeHsYAML tiny
        , bench "small" $ nf decodeHsYAML small
        , bench "flowMid" $ nf decodeHsYAML flowMid
        , bench "quoted50" $ nf decodeHsYAML quoted
        , bench "literal50" $ nf decodeHsYAML literalBody
        , bench "big" $ nf decodeHsYAML big
        ]
    , bgroup
        "decode-libyaml"
        [ bench "tiny" $ nf decodeLibyaml tiny
        , bench "small" $ nf decodeLibyaml small
        , bench "flowMid" $ nf decodeLibyaml flowMid
        , bench "quoted50" $ nf decodeLibyaml quoted
        , bench "literal50" $ nf decodeLibyaml literalBody
        , bench "big" $ nf decodeLibyaml big
        ]
    , bgroup
        "preprocess"
        [ bench "small" $ nf (length . Y.preprocess) (decodeText small)
        , bench "big" $ nf (length . Y.preprocess) (decodeText big)
        ]
    , bgroup
        "encode"
        [ env (pure (decodeBody small)) $ \v ->
            bench "small" $
              nf YE.encode v
        , env (pure (decodeBody flowMid)) $ \v ->
            bench "flowMid" $
              nf YE.encode v
        ]
    ]
  where
    decodeBody bs = case decodeBytes bs of
      Right s -> YV.docBody (V.head (YV.unStream s))
      Left e -> error e

    decodeText = TE.decodeUtf8
