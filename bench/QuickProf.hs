{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString as BS
import Control.Exception (evaluate)
import GHC.Stats
import System.Mem (performGC)
import Data.IORef
import HTML.Parse (parseHTML)
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

main :: IO ()
main = do
  rtsEnabled <- getRTSStatsEnabled
  if not rtsEnabled
    then putStrLn "Run with +RTS -T to get allocation stats"
    else do
      putStrLn $ "Input: " ++ show (BS.length mediumHTML) ++ " bytes"
      let n = 10000
      sink <- newIORef (0 :: Int)
      performGC
      s0 <- getRTSStats
      let !alloc0 = allocated_bytes s0
          !mut0 = mutator_elapsed_ns s0
      go sink n
      performGC
      s1 <- getRTSStats
      let !alloc1 = allocated_bytes s1
          !mut1 = mutator_elapsed_ns s1
          !totalAlloc = alloc1 - alloc0
          !perParse = totalAlloc `div` fromIntegral n
          !mutNs = mut1 - mut0
          !mutSec = fromIntegral mutNs / 1e9 :: Double
          !totalBytes = fromIntegral (BS.length mediumHTML) * fromIntegral n :: Double
          !mbps = totalBytes / (mutSec * 1e6)
      sinkVal <- readIORef sink
      putStrLn $ "Sink: " ++ show sinkVal
      putStrLn $ "Iterations: " ++ show n
      putStrLn $ "Total allocated: " ++ show totalAlloc ++ " bytes"
      putStrLn $ "Per parse: " ++ show perParse ++ " bytes"
      putStrLn $ "Per input byte: " ++ show (fromIntegral perParse / fromIntegral (BS.length mediumHTML) :: Double) ++ "x"
      putStrLn $ "MUT time: " ++ show mutSec ++ "s"
      putStrLn $ "Throughput: " ++ show (round mbps :: Int) ++ " MB/s"
  where
    go :: IORef Int -> Int -> IO ()
    go _ 0 = pure ()
    go !sink !i = do
      !_ <- parseHTMLIO mediumHTML
      modifyIORef' sink (+ 1)
      go sink (i - 1)
