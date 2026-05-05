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
import Data.Int (Int64)
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
  [ -- Soundness: PSkip is only returned when no value in
    -- [mn, mx] satisfies the predicate. Generate a triple
    -- (mn, mx, v) and a leaf predicate, ask evalRange, then
    -- *exhaustively* check whether any integer in [mn, mx]
    -- satisfies the predicate. If evalRange says PSkip but
    -- some integer satisfies, that's a false negative —
    -- the soundness violation we care about.
    testProperty "PSkip => no integer in [mn,mx] satisfies the predicate (Int64)" $
      property $ do
        mn <- forAll (Gen.int (Range.linear (-50) 50))
        mx <- forAll (Gen.int (Range.linear mn  60))
        op <- forAll
          (Gen.choice
            [ pure Pred.PEq, pure Pred.PNeq
            , pure Pred.PLt, pure Pred.PLtEq
            , pure Pred.PGt, pure Pred.PGtEq
            ] <*> (Pred.PVInt64 . fromIntegral <$> Gen.int (Range.linear (-100) 100)))
        let !decision = Pred.evalRange
              (Pred.PVInt64 (fromIntegral mn))
              (Pred.PVInt64 (fromIntegral mx))
              op
            range64 = map fromIntegral [mn .. mx] :: [Int64]
            !satisfies = case op of
              Pred.PEq    (Pred.PVInt64 v) -> any (== v) range64
              Pred.PNeq   (Pred.PVInt64 v) -> any (/= v) range64
              Pred.PLt    (Pred.PVInt64 v) -> any (<  v) range64
              Pred.PLtEq  (Pred.PVInt64 v) -> any (<= v) range64
              Pred.PGt    (Pred.PVInt64 v) -> any (>  v) range64
              Pred.PGtEq  (Pred.PVInt64 v) -> any (>= v) range64
              _ -> True
        case decision of
          Pred.PSkip      -> satisfies === False
          Pred.PMaybeKeep -> H.success  -- always sound

  , testProperty "PSkip soundness for Int32 ranges" $ property $ do
      mn <- forAll (Gen.int32 (Range.linear (-50) 50))
      mx <- forAll (Gen.int32 (Range.linear mn  60))
      v  <- forAll (Gen.int32 (Range.linear (-100) 100))
      let !decision = Pred.evalRange
            (Pred.PVInt32 mn) (Pred.PVInt32 mx) (Pred.PEq (Pred.PVInt32 v))
      case decision of
        Pred.PSkip -> (v >= mn && v <= mx) === False
        _          -> H.success

  , testProperty "PSkip soundness for Double ranges" $ property $ do
      mn <- forAll (Gen.double (Range.linearFrac (-50.0) 50.0))
      mx <- forAll (Gen.double (Range.linearFrac mn 60.0))
      v  <- forAll (Gen.double (Range.linearFrac (-100.0) 100.0))
      let !decision = Pred.evalRange
            (Pred.PVDouble mn) (Pred.PVDouble mx) (Pred.PEq (Pred.PVDouble v))
      case decision of
        Pred.PSkip -> (v >= mn && v <= mx) === False
        _          -> H.success

  , testProperty "PSkip soundness for Text ranges (UTF-8 byte order)" $
      property $ do
        let alpha = Gen.text (Range.linear 1 4) Gen.alpha
        mn <- forAll alpha
        mx <- forAll (Gen.filter (>= mn) alpha)
        v  <- forAll alpha
        let !decision = Pred.evalRange
              (Pred.PVText mn) (Pred.PVText mx) (Pred.PEq (Pred.PVText v))
        case decision of
          Pred.PSkip -> (v >= mn && v <= mx) === False
          _          -> H.success

  , testProperty "PIn rejects only when every member is outside the range" $
      property $ do
        mn <- forAll (Gen.int (Range.linear (-50) 50))
        mx <- forAll (Gen.int (Range.linear mn  60))
        ks <- forAll (Gen.list (Range.linear 1 5) (Gen.int (Range.linear (-100) 100)))
        let !decision = Pred.evalRange
              (Pred.PVInt64 (fromIntegral mn))
              (Pred.PVInt64 (fromIntegral mx))
              (Pred.PIn (map (Pred.PVInt64 . fromIntegral) ks))
            !anyInside = any (\k -> k >= mn && k <= mx) ks
        if anyInside
          then decision === Pred.PMaybeKeep
          else decision === Pred.PSkip

  , testProperty "PIsNull always returns PMaybeKeep (range-only stats)" $
      property $ do
        mn <- forAll (Gen.int (Range.linear (-100) 100))
        mx <- forAll (Gen.int (Range.linear mn 100))
        Pred.evalRange (Pred.PVInt64 (fromIntegral mn))
                       (Pred.PVInt64 (fromIntegral mx)) Pred.PIsNull
          === Pred.PMaybeKeep

  , testProperty "PIsNotNull always returns PMaybeKeep" $ property $ do
      mn <- forAll (Gen.int (Range.linear (-100) 100))
      mx <- forAll (Gen.int (Range.linear mn 100))
      Pred.evalRange (Pred.PVInt64 (fromIntegral mn))
                     (Pred.PVInt64 (fromIntegral mx)) Pred.PIsNotNull
        === Pred.PMaybeKeep

  , testProperty "PNeq always returns PMaybeKeep (can't prove from range alone)" $
      property $ do
        mn <- forAll (Gen.int (Range.linear (-100) 100))
        mx <- forAll (Gen.int (Range.linear mn 100))
        v  <- forAll (Gen.int (Range.linear (-200) 200))
        Pred.evalRange (Pred.PVInt64 (fromIntegral mn))
                       (Pred.PVInt64 (fromIntegral mx))
                       (Pred.PNeq (Pred.PVInt64 (fromIntegral v)))
          === Pred.PMaybeKeep

  , testProperty "Cross-type comparison degrades to PMaybeKeep" $ property $ do
      v   <- forAll (Gen.int (Range.linear (-100) 100))
      txt <- forAll (Gen.text (Range.linear 0 5) Gen.alpha)
      let !decision = Pred.evalRange (Pred.PVInt64 (fromIntegral v))
                                     (Pred.PVInt64 (fromIntegral v))
                                     (Pred.PEq (Pred.PVText txt))
      decision === Pred.PMaybeKeep
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
