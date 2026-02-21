{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import Data.IORef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import System.CPUTime
import Data.Int (Int32)

import Proto.VectorBuilder (GrowList, emptyGrowList, snocGrowList, growListToVector)

main :: IO ()
main = do
  putStrLn "GrowList strategy benchmark (times in us/op, alloc in bytes/op)"
  putStrLn (replicate 72 '=')
  forM_ [10, 50, 200, 1000, 5000] $ \n -> do
    putStrLn ("\n--- n = " <> show n <> " ---")
    let iters = max 100 (5000000 `div` n)
    benchStrategy "cons+reverse     " iters n consReverseBuild
    benchStrategy "endo-cons        " iters n endoConsBuild
    benchStrategy "chunked-growlist " iters n chunkedGrowListBuild
    benchStrategy "ST-mutable-known " iters n stMutableKnownBuild
    benchStrategy "ST-mutable-grow  " iters n stMutableGrowBuild
    benchStrategy "dlist-fromListN  " iters n dlistFromListNBuild
    if n <= 1000
      then benchStrategy "V.snoc (naive)   " iters n vsnocBuild
      else putStrLn "  V.snoc (naive)   :  (skipped, O(n^2))"
    benchStrategy "V.unfoldrExactN  " iters n unfoldrBuild

benchStrategy :: String -> Int -> Int -> (Int -> V.Vector Int32) -> IO ()
benchStrategy name iters n build = do
  t1 <- getCPUTime
  !total <- go iters 0
  t2 <- getCPUTime
  let !elapsed = t2 - t1
      usPerOp :: Double
      usPerOp = fromIntegral elapsed / (fromIntegral iters * 1e6)
  putStrLn ("  " <> name <> ":  " <> showF usPerOp <> " us/op  (check=" <> show total <> ")")
  where
    go :: Int -> Int -> IO Int
    go 0 !acc = pure acc
    go !i !acc = do
      let !v = build n
      go (i - 1) (acc + V.length v)

showF :: Double -> String
showF d =
  let whole = floor d :: Int
      frac  = round ((d - fromIntegral whole) * 100) :: Int
  in show whole <> "." <> (if frac < 10 then "0" else "") <> show frac

-- 1: cons list + reverse + V.fromList
consReverseBuild :: Int -> V.Vector Int32
consReverseBuild n = V.fromList (go n [])
  where
    go 0 !acc = reverse acc
    go !i !acc = go (i - 1) (fromIntegral i : acc)
{-# NOINLINE consReverseBuild #-}

-- 2: Endo-style (difference list via function composition with cons)
endoConsBuild :: Int -> V.Vector Int32
endoConsBuild n = V.fromListN n (dl [])
  where
    dl = go n id
    go :: Int -> ([Int32] -> [Int32]) -> ([Int32] -> [Int32])
    go 0 !f = f
    go !i !f = go (i - 1) (f . (fromIntegral i :))
{-# NOINLINE endoConsBuild #-}

-- 3: Our chunked GrowList
chunkedGrowListBuild :: Int -> V.Vector Int32
chunkedGrowListBuild n = growListToVector (go n emptyGrowList)
  where
    go 0 !acc = acc
    go !i !acc = go (i - 1) (snocGrowList acc (fromIntegral i))
{-# NOINLINE chunkedGrowListBuild #-}

-- 4: ST mutable vector, size known upfront
stMutableKnownBuild :: Int -> V.Vector Int32
stMutableKnownBuild n = runST $ do
  mv <- MV.new n
  let go !idx
        | idx >= n  = V.unsafeFreeze mv
        | otherwise = do
            MV.unsafeWrite mv idx (fromIntegral (idx + 1))
            go (idx + 1)
  go 0
{-# NOINLINE stMutableKnownBuild #-}

-- 5: ST mutable vector with doubling (size unknown)
stMutableGrowBuild :: Int -> V.Vector Int32
stMutableGrowBuild n = runST $ do
  mv0 <- MV.new 8
  let go !mv !idx !i
        | i > n = V.freeze (MV.take idx mv)
        | idx >= MV.length mv = do
            mv' <- MV.grow mv (MV.length mv)
            go mv' idx i
        | otherwise = do
            MV.unsafeWrite mv idx (fromIntegral i)
            go mv (idx + 1) (i + 1)
  go mv0 0 1
{-# NOINLINE stMutableGrowBuild #-}

-- 6: DList via fromListN (knows size, builds forward)
dlistFromListNBuild :: Int -> V.Vector Int32
dlistFromListNBuild n = V.fromListN n (dl [])
  where
    dl = go n id
    go :: Int -> ([Int32] -> [Int32]) -> ([Int32] -> [Int32])
    go 0 !f = f
    go !i !f = go (i - 1) (\rest -> f (fromIntegral i : rest))
{-# NOINLINE dlistFromListNBuild #-}

-- 7: V.snoc (O(n^2), baseline for small n)
vsnocBuild :: Int -> V.Vector Int32
vsnocBuild n = go 1 V.empty
  where
    go !i !acc
      | i > n     = acc
      | otherwise = go (i + 1) (V.snoc acc (fromIntegral i))
{-# NOINLINE vsnocBuild #-}

-- 8: V.unfoldrExactN (generates forward, known length)
unfoldrBuild :: Int -> V.Vector Int32
unfoldrBuild n = V.unfoldrExactN n (\i -> (fromIntegral i, i + 1)) (1 :: Int)
{-# NOINLINE unfoldrBuild #-}
