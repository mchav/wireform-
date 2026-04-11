{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Control.Exception (evaluate)
import GHC.Stats
import System.Mem (performGC)
import HTML.Parse (parseHTML, tokenizeOnlyIO, treeBuildOnlyIO)
import HTML.Value (HTMLDocument)

mediumHTML :: BS.ByteString
mediumHTML =
  let header = "<html><body><div class=\"catalog\">\n"
      footer = "</div></body></html>\n"
      mkItem :: Int -> String
      mkItem i = concat
        [ "  <div class=\"item\" id=\"i", show i, "\">\n"
        , "    <span class=\"name\">Product ", show i, "</span>\n"
        , "    <span class=\"price\">", show (fromIntegral i * 9.99 :: Double), "</span>\n"
        , "    <p class=\"description\">This is the description for product number "
        , show i, " in our catalog</p>\n"
        , "    <span class=\"category\">Category ", show (i `mod` 10), "</span>\n"
        , "    <span class=\"inStock\">", if even i then "true" else "false", "</span>\n"
        , "  </div>\n"
        ]
      items = concatMap mkItem [1..100 :: Int]
  in BS8.pack (header ++ items ++ footer)

parseHTMLIO :: BS.ByteString -> IO HTMLDocument
parseHTMLIO !bs = evaluate $! parseHTML bs
{-# NOINLINE parseHTMLIO #-}

tokenizeIO :: BS.ByteString -> IO Int
tokenizeIO !bs = tokenizeOnlyIO bs
{-# NOINLINE tokenizeIO #-}

treeBuildIO :: BS.ByteString -> IO ()
treeBuildIO !bs = treeBuildOnlyIO bs
{-# NOINLINE treeBuildIO #-}

bench :: String -> Int -> IO a -> IO (Int, Double)
bench label n act = do
  performGC
  s0 <- getRTSStats
  let !alloc0 = allocated_bytes s0
      !mut0   = mutator_elapsed_ns s0
  go n
  performGC
  s1 <- getRTSStats
  let !alloc1 = allocated_bytes s1
      !mut1   = mutator_elapsed_ns s1
      !totalAlloc = alloc1 - alloc0
      !perIter = totalAlloc `div` fromIntegral n
      !mutNs = mut1 - mut0
      !mutSec = fromIntegral mutNs / 1e9 :: Double
      !totalBytes = fromIntegral (BS.length mediumHTML) * fromIntegral n :: Double
      !mbps = totalBytes / (mutSec * 1e6)
  putStrLn $ label ++ ":"
  putStrLn $ "  alloc/iter: " ++ show perIter ++ " bytes"
  putStrLn $ "  MUT: " ++ show mutSec ++ "s"
  putStrLn $ "  throughput: " ++ show (round mbps :: Int) ++ " MB/s"
  pure (fromIntegral perIter, mbps)
  where
    go 0 = pure ()
    go !i = act >> go (i - 1)

main :: IO ()
main = do
  rtsEnabled <- getRTSStatsEnabled
  if not rtsEnabled
    then putStrLn "Run with +RTS -T to get allocation stats"
    else do
      putStrLn $ "Input: " ++ show (BS.length mediumHTML) ++ " bytes"
      let n = 5000
      (tokAlloc, _tokMbps)    <- bench "tokenize-only" n (tokenizeIO mediumHTML)
      (tbAlloc, _tbMbps)      <- bench "tree-build-only" n (treeBuildIO mediumHTML)
      (parseAlloc, _parseMbps) <- bench "full-parse" n (parseHTMLIO mediumHTML)
      putStrLn ""
      putStrLn "--- breakdown ---"
      putStrLn $ "tree-build overhead (vs tokenize): " ++ show (tbAlloc - tokAlloc) ++ " bytes/parse"
      putStrLn $ "doc-build overhead (vs tree-build): " ++ show (parseAlloc - tbAlloc) ++ " bytes/parse"
