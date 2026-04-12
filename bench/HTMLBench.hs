{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Stats
import HTML.DOM
import HTML.Parse (parseHTML, tokenizeCallbackIO, tokenizeOnlyIO, treeBuildOnlyIO)
import HTML.Rewriter
import HTML.Selector
import HTML.Value (HTMLAttribute (..), HTMLDocument, HTMLNode (..))
import System.Mem (performGC)


-- ---------------------------------------------------------------------------
-- Test input: ~29 KB HTML document
-- ---------------------------------------------------------------------------

mediumHTML :: BS.ByteString
mediumHTML =
  let header = "<html><body><div class=\"catalog\">\n"
      footer = "</div></body></html>\n"
      mkItem :: Int -> String
      mkItem i =
        concat
          [ "  <div class=\"item\" id=\"i"
          , show i
          , "\">\n"
          , "    <span class=\"name\">Product "
          , show i
          , "</span>\n"
          , "    <span class=\"price\">"
          , show (fromIntegral i * 9.99 :: Double)
          , "</span>\n"
          , "    <p class=\"description\">This is the description for product number "
          , show i
          , " in our catalog</p>\n"
          , "    <span class=\"category\">Category "
          , show (i `mod` 10)
          , "</span>\n"
          , "    <span class=\"inStock\">"
          , if even i then "true" else "false"
          , "</span>\n"
          , "  </div>\n"
          ]
      items = concatMap mkItem [1 .. 100 :: Int]
  in BS8.pack (header ++ items ++ footer)


-- ---------------------------------------------------------------------------
-- Bench harness
-- ---------------------------------------------------------------------------

data BenchResult = BenchResult
  { brLabel :: !String
  , brAllocIter :: !Int
  , brMbps :: !Double
  , brMutSec :: !Double
  }


bench :: String -> Int -> Int -> IO a -> IO BenchResult
bench label iters inputSize act = do
  performGC
  s0 <- getRTSStats
  let !alloc0 = allocated_bytes s0
      !mut0 = mutator_elapsed_ns s0
  go iters
  performGC
  s1 <- getRTSStats
  let !alloc1 = allocated_bytes s1
      !mut1 = mutator_elapsed_ns s1
      !totalAlloc = alloc1 - alloc0
      !perIter = fromIntegral totalAlloc `div` iters
      !mutNs = mut1 - mut0
      !mutSec = fromIntegral mutNs / 1e9 :: Double
      !totalBytes = fromIntegral inputSize * fromIntegral iters :: Double
      !mbps = totalBytes / (mutSec * 1e6)
  pure (BenchResult label perIter mbps mutSec)
  where
    go 0 = pure ()
    go !i = act >> go (i - 1)


benchSmall :: String -> Int -> IO a -> IO (Int, Double)
benchSmall label iters act = do
  performGC
  s0 <- getRTSStats
  let !alloc0 = allocated_bytes s0
      !mut0 = mutator_elapsed_ns s0
  go iters
  performGC
  s1 <- getRTSStats
  let !alloc1 = allocated_bytes s1
      !mut1 = mutator_elapsed_ns s1
      !totalAlloc = alloc1 - alloc0
      !perIter = fromIntegral totalAlloc `div` iters
      !mutNs = mut1 - mut0
      !nsPerIter = fromIntegral mutNs / fromIntegral iters :: Double
  pure (perIter, nsPerIter)
  where
    go 0 = pure ()
    go !i = act >> go (i - 1)


-- ---------------------------------------------------------------------------
-- Formatters
-- ---------------------------------------------------------------------------

printResult :: BenchResult -> String -> Int -> IO Bool
printResult br targetLabel targetAlloc = do
  let pass = brAllocIter br <= targetAlloc
      tag = if pass then "  OK  " else " MISS "
  putStrLn $ "[" ++ tag ++ "] " ++ brLabel br
  putStrLn $ "          throughput: " ++ show (round (brMbps br) :: Int) ++ " MB/s   (target: " ++ targetLabel ++ ")"
  putStrLn $ "          alloc/iter: " ++ show (brAllocIter br) ++ " bytes   (target: ≤" ++ show targetAlloc ++ ")"
  putStrLn $ "          MUT time:   " ++ showMs (brMutSec br)
  pure pass


printSmallResult :: String -> Int -> Double -> Int -> Double -> IO Bool
printSmallResult label allocIter nsIter allocTarget nsTarget = do
  let allocOk = allocIter <= allocTarget
      timeOk = nsIter <= nsTarget
      pass = allocOk && timeOk
      tag = if pass then "  OK  " else " MISS "
  putStrLn $ "[" ++ tag ++ "] " ++ label
  putStrLn $ "          time/iter:  " ++ showNs nsIter ++ "   (target: <" ++ showNs nsTarget ++ ")"
  putStrLn $ "          alloc/iter: " ++ show allocIter ++ " bytes   (target: ≤" ++ show allocTarget ++ ")"
  pure pass


showMs :: Double -> String
showMs s = show (round (s * 1000) :: Int) ++ " ms"


showNs :: Double -> String
showNs ns
  | ns < 1000 = show (round ns :: Int) ++ " ns"
  | ns < 1e6 = show (round (ns / 1000) :: Int) ++ " µs"
  | otherwise = show (round (ns / 1e6) :: Int) ++ " ms"


-- ---------------------------------------------------------------------------
-- Benchmark implementations
-- ---------------------------------------------------------------------------

parseHTMLIO :: BS.ByteString -> IO HTMLDocument
parseHTMLIO !bs = evaluate $! parseHTML bs
{-# NOINLINE parseHTMLIO #-}


tokenizeIO :: BS.ByteString -> IO Int
tokenizeIO !bs = tokenizeOnlyIO bs
{-# NOINLINE tokenizeIO #-}


treeBuildIO :: BS.ByteString -> IO ()
treeBuildIO !bs = treeBuildOnlyIO bs
{-# NOINLINE treeBuildIO #-}


-- Tokenize incremental: feed 4KB chunks through tokenizeCallbackIO
-- with tag-boundary splitting (same approach as the rewriter).
tokenizeIncrementalIO :: BS.ByteString -> IO Int
tokenizeIncrementalIO !bs = do
  countRef <- newIORef (0 :: Int)
  leftoverRef <- newIORef BS.empty
  let chunks = splitChunks 4096 bs
      processChunk !chunk = do
        prev <- readIORef leftoverRef
        let !combined = if BS.null prev then chunk else prev <> chunk
            !splitPt = findSafeBreak combined
            !toProcess = BS.take splitPt combined
            !remainder = BS.drop splitPt combined
        writeIORef leftoverRef remainder
        when (not (BS.null toProcess)) $
          tokenizeCallbackIO toProcess $ \_ _ _ ->
            modifyIORef' countRef (+ 1)
  mapM_ processChunk chunks
  lo <- readIORef leftoverRef
  when (not (BS.null lo)) $
    tokenizeCallbackIO lo $ \_ _ _ ->
      modifyIORef' countRef (+ 1)
  readIORef countRef
{-# NOINLINE tokenizeIncrementalIO #-}


findSafeBreak :: BS.ByteString -> Int
findSafeBreak !bs = go (BS.length bs - 1)
  where
    go !i
      | i < 0 = BS.length bs
      | otherwise = case BS.index bs i of
          0x3C -> i
          0x3E -> BS.length bs
          _ -> go (i - 1)


-- Tree build incremental: use HTML.DOM incremental parser
treeBuildIncrementalIO :: BS.ByteString -> IO ()
treeBuildIncrementalIO !bs = do
  p <- newParser
  let chunks = splitChunks 4096 bs
  mapM_ (feedParser p) chunks
  doc <- finishParser p
  _ <- evaluate $! documentElement doc
  pure ()
{-# NOINLINE treeBuildIncrementalIO #-}


-- Rewriter passthrough: no handlers
rewriterPassthroughIO :: Rewriter -> BS.ByteString -> IO ()
rewriterPassthroughIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterPassthroughIO #-}


-- Rewriter with selector matching: 5 selectors, callbacks read but don't mutate
rewriterSelectorIO :: Rewriter -> BS.ByteString -> IO ()
rewriterSelectorIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterSelectorIO #-}


-- Rewriter with mutations: attribute changes
rewriterMutateIO :: Rewriter -> BS.ByteString -> IO ()
rewriterMutateIO !rw !bs = do
  !result <- rewrite rw bs
  _ <- evaluate $! BS.length result
  pure ()
{-# NOINLINE rewriterMutateIO #-}


-- CSS selector parse
selectorParseIO :: Text -> IO ()
selectorParseIO !sel = do
  case parseSelector sel of
    Right !_ -> pure ()
    Left e -> error (show e)
{-# NOINLINE selectorParseIO #-}


-- CSS selector match: matchCompound against a single element
selectorMatchSingleIO :: Selector -> IO ()
selectorMatchSingleIO !(Selector (ComplexSelector (CompoundSelector hd subs) _ : _)) = do
  let !attrs = smallArrayFromList [HTMLAttribute "class" "item", HTMLAttribute "id" "i42"]
      !_ = matchCompound (CompoundSelector hd subs) "div" attrs
  pure ()
selectorMatchSingleIO _ = pure ()
{-# NOINLINE selectorMatchSingleIO #-}


splitChunks :: Int -> BS.ByteString -> [BS.ByteString]
splitChunks chunkSize bs = go 0
  where
    !len = BS.length bs
    go !off
      | off >= len = []
      | otherwise =
          let !end = min (off + chunkSize) len
          in BS.take (end - off) (BS.drop off bs) : go end


-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rtsEnabled <- getRTSStatsEnabled
  if not rtsEnabled
    then putStrLn "Run with +RTS -T to get allocation stats"
    else runBenchmarks


runBenchmarks :: IO ()
runBenchmarks = do
  let !inputSize = BS.length mediumHTML
  putStrLn $ "Input size: " ++ show inputSize ++ " bytes"
  putStrLn $ replicate 72 '='

  -- Build rewriter configs
  let Right rwPassthrough = buildRewriter (pure ())

  let selDiv = mustParse "div.item"
      selSpan = mustParse "span.name"
      selPrice = mustParse "span.price"
      selDesc = mustParse "p.description"
      selCat = mustParse "span.category"

  let Right rwSelector = buildRewriter $ do
        onElement selDiv $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selSpan $ \er -> do
          _ <- getTagName er
          pure ()
        onElement selPrice $ \er -> do
          _ <- getTagName er
          pure ()
        onText selDesc $ \tr -> do
          _ <- getTextContent tr
          pure ()
        onElement selCat $ \er -> do
          _ <- getElemAttr er "class"
          pure ()

  let Right rwMutate = buildRewriter $ do
        onElement selDiv $ \er -> do
          setElemAttr er "data-processed" "true"
        onElement selSpan $ \er -> do
          setTagName er "strong"
        onText selDesc $ \tr -> do
          content <- getTextContent tr
          replaceTextChunk tr (T.toUpper content) AsText

  let doc = parseDocument mediumHTML

  let n = 5000
  passRef <- newIORef (0 :: Int)
  totalRef <- newIORef (0 :: Int)

  let checkPass ok = do
        modifyIORef' totalRef (+ 1)
        if ok then modifyIORef' passRef (+ 1) else pure ()

  putStrLn ""
  putStrLn "--- Tokenizer ---"
  r1 <- bench "tokenize (one-shot)" n inputSize (tokenizeIO mediumHTML)
  printResult r1 "≥1200 MB/s" 70000 >>= checkPass

  r2 <- bench "tokenize (incremental, 4KB)" n inputSize (tokenizeIncrementalIO mediumHTML)
  printResult r2 "≥1000 MB/s" 90000 >>= checkPass

  putStrLn ""
  putStrLn "--- Tree Builder ---"
  r3 <- bench "tree-build (one-shot)" n inputSize (treeBuildIO mediumHTML)
  printResult r3 "≥400 MB/s" 430000 >>= checkPass

  r4 <- bench "tree-build (incremental)" n inputSize (treeBuildIncrementalIO mediumHTML)
  printResult r4 "≥350 MB/s" 500000 >>= checkPass

  putStrLn ""
  putStrLn "--- Rewriter ---"
  r5 <- bench "rewriter (passthrough)" n inputSize (rewriterPassthroughIO rwPassthrough mediumHTML)
  printResult r5 "≥800 MB/s" 100000 >>= checkPass

  r6 <- bench "rewriter (selector matching)" n inputSize (rewriterSelectorIO rwSelector mediumHTML)
  printResult r6 "≥600 MB/s" 120000 >>= checkPass

  r7 <- bench "rewriter (with mutations)" n inputSize (rewriterMutateIO rwMutate mediumHTML)
  printResult r7 "≥400 MB/s" 200000 >>= checkPass

  putStrLn ""
  putStrLn "--- CSS Selectors ---"
  let selectorN = 200000
  (selAlloc, selNs) <- benchSmall "selector-parse" selectorN (selectorParseIO "div.item > span.name[href^=\"https\"]")
  printSmallResult "selector parse" selAlloc selNs 1000 1000.0 >>= checkPass

  let matchN = 200000
  (matchAlloc, matchNs) <- benchSmall "selector-match" matchN $ do
    let root = documentElement doc
        _ = querySelectorAll root "div.item"
    evaluate $! length (querySelectorAll root "div.item")
  printSmallResult "selector match (DOM, all nodes)" matchAlloc matchNs 50000 500000.0 >>= checkPass

  putStrLn ""
  putStrLn $ replicate 72 '='
  passed <- readIORef passRef
  total <- readIORef totalRef
  putStrLn $ show passed ++ "/" ++ show total ++ " benchmarks within target"


mustParse :: Text -> Selector
mustParse t = case parseSelector t of
  Right s -> s
  Left e -> error ("parseSelector: " ++ show e)
