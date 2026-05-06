{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Criterion.Main
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified YAML.Decode              as Y
import qualified YAML.Encode              as YE
import qualified YAML.Value               as YV
import qualified Data.Vector              as V

-- | A tiny mapping (~20 byte input).
tiny :: BS.ByteString
tiny = "key: value"

-- | A mid-size nested document (a few hundred bytes).
small :: BS.ByteString
small = BS8.unlines
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
flowMid = BS8.unlines
  [ "matrix: [[1,2,3], [4,5,6], [7,8,9]]"
  , "tags: { a: 1, b: 2, c: 3, d: 4 }"
  , "items: [{x: 1, y: 2}, {x: 3, y: 4}, {x: 5, y: 6}]"
  ]

-- | A scalar-heavy document (many quoted strings).
quoted :: BS.ByteString
quoted = BS8.unlines $
  "items:" : [ "  - \"item " <> BS8.pack (show n) <> " with some quoted text\""
             | n <- [(1 :: Int) .. 50] ]

-- | A block scalar / literal text body.
literalBody :: BS.ByteString
literalBody = BS8.unlines $
  "code: |" :
  [ "  line " <> BS8.pack (show n) <> " of body text"
  | n <- [(1 :: Int) .. 50] ]

-- | A larger doc (~2 KB) of mixed content.
big :: BS.ByteString
big = BS.concat (replicate 4 small)

decodeBytes :: BS.ByteString -> Either String YV.Stream
decodeBytes = Y.decodeStreamBS

main :: IO ()
main = defaultMain
  [ bgroup "decode"
      [ bench "tiny"       $ nf decodeBytes tiny
      , bench "small"      $ nf decodeBytes small
      , bench "flowMid"    $ nf decodeBytes flowMid
      , bench "quoted50"   $ nf decodeBytes quoted
      , bench "literal50"  $ nf decodeBytes literalBody
      , bench "big"        $ nf decodeBytes big
      ]
  , bgroup "encode"
      [ env (pure (decodeBody small)) $ \v -> bench "small" $
          nf YE.encode v
      , env (pure (decodeBody flowMid)) $ \v -> bench "flowMid" $
          nf YE.encode v
      ]
  ]
  where
    decodeBody bs = case decodeBytes bs of
      Right s -> YV.docBody (V.head (YV.unStream s))
      Left e  -> error e
