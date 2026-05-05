{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Property tests for 'Columnar.Stream' and
-- 'Columnar.Predicate'.
--
-- The Iter combinators are tiny but they're the iteration
-- backbone of the columnar formats; an off-by-one in
-- 'iterTake' / 'iterDrop' / 'iterRowSlice' would silently corrupt
-- every Parquet / Arrow / ORC stream the facade decodes. Drive
-- them through Hedgehog with random list inputs.
module Main (main) where

import Hedgehog ( (===), forAll, property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Columnar.IO as CIO
import qualified Columnar.Predicate as Pred
import qualified Columnar.Stream as IS

import qualified Data.ByteString as BS
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)

main :: IO ()
main = defaultMain $ testGroup "wireform-columnar"
  [ iterProps
  , iterCombinatorProps
  , predicateProps
  , predicateUnits
  , columnarIOUnits
  ]

-- ============================================================
-- iterChunk / iterScan / iterMergeBy / iterPrefetch / iterParallelMap
-- ============================================================

iterCombinatorProps :: TestTree
iterCombinatorProps = testGroup "Columnar.Stream new combinators"
  [ testProperty "iterChunk n preserves concat" $ property $ do
      n  <- forAll (Gen.int (Range.linear 1 10))
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      case IS.iterToList (IS.iterChunk n (IS.iterFromList xs)) of
        Right chunks -> do
          concat chunks === xs
          -- All chunks except possibly the last have length n
          all (\c -> length c == n) (init1 chunks) === True
          -- Last chunk has length in [1..n] when xs nonempty
          case chunks of
            [] -> null xs === True
            _  -> let !lastLen = length (last chunks)
                  in (lastLen >= 1 && lastLen <= n) === True
        Left e -> H.footnote e >> H.failure

  , testProperty "iterChunk n=0 yields empty" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      IS.iterToList (IS.iterChunk 0 (IS.iterFromList xs)) === Right []

  , testProperty "iterScan matches Data.List.scanl'" $ property $ do
      xs   <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      seed <- forAll (Gen.int (Range.linear 0 100))
      IS.iterToList (IS.iterScan (+) seed (IS.iterFromList xs))
        === Right (scanl' (+) seed xs)

  , testProperty "iterMergeBy on sorted inputs == merge-sort union" $ property $ do
      xs <- sortedList
      ys <- sortedList
      zs <- sortedList
      let merged = IS.iterMergeBy compare
            [IS.iterFromList xs, IS.iterFromList ys, IS.iterFromList zs]
      case IS.iterToList merged of
        Right got -> got === sortedMerge3 xs ys zs
        Left e    -> H.footnote e >> H.failure

  , testProperty "iterMergeBy [] is empty" $ property $
      IS.iterToList (IS.iterMergeBy compare ([] :: [IS.Iter Int])) === Right []

  , testProperty "iterMergeBy [single] is identity" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      IS.iterToList (IS.iterMergeBy compare [IS.iterFromList xs])
        === Right xs

  , testProperty "iterIOPrefetch preserves order and contents" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      depth <- forAll (Gen.int (Range.linear 1 8))
      got <- H.evalIO $ do
        prefetched <- IS.iterIOPrefetch depth (IS.iterIOFromIter (IS.iterFromList xs))
        IS.iterIOToList prefetched
      got === Right xs

  , testProperty "iterParallelMap preserves order and applies the function" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      depth <- forAll (Gen.int (Range.linear 1 4))
      got <- H.evalIO $ do
        mapped <- IS.iterParallelMap depth (\x -> pure (x * 2))
                                     (IS.iterIOFromIter (IS.iterFromList xs))
        IS.iterIOToList mapped
      got === Right (map (* 2) xs)
  ]
  where
    sortedList = forAll
      ((\xs -> sortAsc xs) <$>
        Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100)))
    sortAsc = foldr insertAsc []
    insertAsc x [] = [x]
    insertAsc x (y:ys) | x <= y = x : y : ys
                      | otherwise = y : insertAsc x ys

    sortedMerge3 xs ys zs = sortAscMerge xs (sortAscMerge ys zs)
      where
        sortAscMerge as [] = as
        sortAscMerge [] bs = bs
        sortAscMerge (a:as) (b:bs)
          | a <= b    = a : sortAscMerge as (b:bs)
          | otherwise = b : sortAscMerge (a:as) bs

    init1 [] = []
    init1 [_] = []
    init1 (x:xs) = x : init1 xs

    scanl' f z = go z
      where
        go !acc []     = [acc]
        go !acc (x:xs) = acc : go (f acc x) xs

-- ============================================================
-- Columnar.IO unit tests
-- ============================================================

columnarIOUnits :: TestTree
columnarIOUnits = testGroup "Columnar.IO"
  [ testCase "loadFileEager + loadFileMmap return equal bytes" $
      withSystemTempFile "wfio.bin" $ \path h -> do
        let !payload = BS.replicate 200_000 0xAB
        BS.hPut h payload
        hClose h
        eager <- CIO.loadFileEager path
        mmaped <- CIO.loadFileMmap path
        eager  @?= payload
        mmaped @?= payload
  , testCase "loadFile picks mmap above MmapAbove threshold" $
      withSystemTempFile "wfio-big.bin" $ \path h -> do
        let !payload = BS.replicate 200_000 0xCD
        BS.hPut h payload
        hClose h
        bs <- CIO.loadFile path
        BS.length bs @?= 200_000
  , testCase "loadFile uses eager path under threshold" $
      withSystemTempFile "wfio-small.bin" $ \path h -> do
        let !payload = BS.replicate 1024 0xEF
        BS.hPut h payload
        hClose h
        bs <- CIO.loadFile path
        BS.length bs @?= 1024
  ]

-- ============================================================
-- Iter properties
-- ============================================================

iterProps :: TestTree
iterProps = testGroup "Columnar.Stream.Iter"
  [ testProperty "iterToList . iterFromList = id" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      IS.iterToList (IS.iterFromList xs) === Right xs

  , testProperty "iterMap = map" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      let f = (+ 1)
      IS.iterToList (IS.iterMap f (IS.iterFromList xs))
        === Right (map f xs)

  , testProperty "iterFilter = filter" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      let p = even
      IS.iterToList (IS.iterFilter p (IS.iterFromList xs))
        === Right (filter p xs)

  , testProperty "iterTake n = take n" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      n  <- forAll (Gen.int (Range.linear 0 60))
      IS.iterToList (IS.iterTake n (IS.iterFromList xs))
        === Right (take n xs)

  , testProperty "iterDrop n = drop n" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      n  <- forAll (Gen.int (Range.linear 0 60))
      IS.iterToList (IS.iterDrop n (IS.iterFromList xs))
        === Right (drop n xs)

  , testProperty "iterAppend = (++)" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      ys <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
      IS.iterToList (IS.iterAppend (IS.iterFromList xs) (IS.iterFromList ys))
        === Right (xs ++ ys)

  , testProperty "iterConcat = concat" $ property $ do
      xss <- forAll
        (Gen.list (Range.linear 0 5)
          (Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 100))))
      IS.iterToList
        (IS.iterConcat (IS.iterFromList (map IS.iterFromList xss)))
        === Right (concat xss)

  , testProperty "iterFold = foldl'" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      IS.iterFold (+) 0 (IS.iterFromList xs)
        === Right (sum xs)

  , testProperty "iterLength = length" $ property $ do
      xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
      IS.iterLength (IS.iterFromList xs)
        === Right (length xs)

  , testProperty "iterFromIndexed n f matches map f [0..n-1]" $ property $ do
      n <- forAll (Gen.int (Range.linear 0 30))
      let f i = Right (i * 2)
      IS.iterToList (IS.iterFromIndexed n f)
        === Right [i * 2 | i <- [0 .. n - 1]]

  , testProperty "iterMapM threads errors" $ property $ do
      xs <- forAll (Gen.list (Range.linear 1 20) (Gen.int (Range.linear 0 100)))
      let f x = if x == 7 then Left "boom" else Right x
          expected = case break (== 7) xs of
            (pre, [])    -> Right pre
            (_,   _:_)   -> Left "boom"
      IS.iterToList (IS.iterMapM f (IS.iterFromList xs))
        === expected

  , testProperty "iterRowSlice respects offset+len" $ property $ do
      -- Each element is a list of ints (its 'row count' is the
      -- list length). Cross-element slicing should match plain
      -- list slicing of the flattened stream.
      xss <- forAll
        (Gen.list (Range.linear 0 6)
          (Gen.list (Range.linear 0 5) (Gen.int (Range.linear 0 100))))
      offset <- forAll (Gen.int (Range.linear 0 30))
      taken  <- forAll (Gen.int (Range.linear 0 30))
      let it = IS.iterFromList xss
          sliced = IS.iterRowSlice length (\s l xs -> take l (drop s xs))
                                 offset taken it
          expected = take taken (drop offset (concat xss))
      case IS.iterToList sliced of
        Left e -> H.footnote e >> H.failure
        Right got -> concat got === expected
  ]

-- ============================================================
-- Predicate properties
-- ============================================================

predicateProps :: TestTree
predicateProps = testGroup "Columnar.Predicate.evalRange"
  [ testProperty "PEq inside range -> MaybeKeep, outside -> Skip" $ property $ do
      mn <- forAll (Gen.int (Range.linear (-1000) 1000))
      mx <- forAll (Gen.int (Range.linear mn 1000))
      v  <- forAll (Gen.int (Range.linear (-2000) 2000))
      let result = Pred.evalRange (Pred.PVInt64 (fromIntegral mn))
                                  (Pred.PVInt64 (fromIntegral mx))
                                  (Pred.PEq (Pred.PVInt64 (fromIntegral v)))
      if v >= mn && v <= mx
        then result === Pred.PMaybeKeep
        else result === Pred.PSkip

  , testProperty "PLt v -> Skip iff v <= mn" $ property $ do
      mn <- forAll (Gen.int (Range.linear (-1000) 1000))
      mx <- forAll (Gen.int (Range.linear mn 1000))
      v  <- forAll (Gen.int (Range.linear (-2000) 2000))
      let result = Pred.evalRange (Pred.PVInt64 (fromIntegral mn))
                                  (Pred.PVInt64 (fromIntegral mx))
                                  (Pred.PLt (Pred.PVInt64 (fromIntegral v)))
      if v <= mn
        then result === Pred.PSkip
        else result === Pred.PMaybeKeep
  ]

predicateUnits :: TestTree
predicateUnits = testGroup "Columnar.Predicate units"
  [ testCase "combineDecisions PSkip _ = PSkip" $
      Pred.combineDecisions Pred.PSkip Pred.PMaybeKeep @?= Pred.PSkip
  , testCase "combineDecisions PMaybeKeep PMaybeKeep = PMaybeKeep" $
      Pred.combineDecisions Pred.PMaybeKeep Pred.PMaybeKeep @?= Pred.PMaybeKeep
  , testCase "pvLess on incomparable returns False" $
      Pred.pvLess (Pred.PVInt32 1) (Pred.PVText "x") @?= False
  ]
