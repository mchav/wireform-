{-# LANGUAGE OverloadedStrings #-}

{- | Property tests for 'Columnar.Stream' and
'Columnar.Predicate'.

The Iter combinators are tiny but they're the iteration
backbone of the columnar formats; an off-by-one in
'iterTake' / 'iterDrop' / 'iterRowSlice' would silently corrupt
every Parquet / Arrow / ORC stream the facade decodes. Drive
them through Hedgehog with random list inputs.
-}
module Main (main) where

import Columnar.IO qualified as CIO
import Columnar.LZ4 qualified as LZ4
import Columnar.Predicate qualified as Pred
import Columnar.Stream qualified as IS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.Int (Int64)
import Data.Word qualified as W
import Hedgehog (forAll, property, (===))
import Hedgehog qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Syd
import Test.Syd.Hedgehog ()


main :: IO ()
main =
  sydTest $
    describe
      "wireform-columnar" $ sequence_
      [ iterProps
      , iterCombinatorProps
      , predicateProps
      , predicateUnits
      , columnarIOUnits
      , lz4Tests
      ]


-- ============================================================
-- Columnar.LZ4 — pure-Haskell raw block codec
-- ============================================================

lz4Tests :: Spec
lz4Tests =
  describe
    "Columnar.LZ4" $ sequence_
    [ it "decompress empty -> empty" $
        LZ4.decompress 0 BS.empty `shouldBe` Right BS.empty
    , it "decompress: single all-literals sequence" $
        -- token = (5 << 4) | 0 = 0x50, then 5 literal bytes.
        let !block = BS.pack [0x50, 0x68, 0x65, 0x6c, 0x6c, 0x6f] -- "hello"
        in LZ4.decompress 16 block `shouldBe` Right (BSC.pack "hello")
    , it "decompress: literal extension" $
        -- 20 literals: nibble=15, ext=[5], then 20 bytes.
        let !lits = BS.replicate 20 0x41 -- 'A' x 20
            !block = BS.pack (0xF0 : 5 : BS.unpack lits)
        in LZ4.decompress 32 block `shouldBe` Right (BS.replicate 20 0x41)
    , -- Back-reference + overlapping-match wire-byte tests are
      -- exercised through the round-trip property below; we
      -- can't hand-write a "minimal" block here because liblz4
      -- (rightly) enforces the spec's "last 5 bytes literal +
      -- match ends >= 12 bytes from end" rule and our minimal
      -- sequences violated those constraints.
      it "compress . decompress = id (short text)" $
        let !payload = BSC.pack "hello world hello world hello world"
            !compressed = LZ4.compress payload
        in LZ4.decompress (BS.length payload) compressed `shouldBe` Right payload
    , it "compress . decompress = id (highly repetitive)" $
        let !payload = BS.replicate 1024 0xAB
            !compressed = LZ4.compress payload
        in do
            BS.length compressed < BS.length payload `shouldBe` True -- did compress
            LZ4.decompress (BS.length payload) compressed `shouldBe` Right payload
    , it "compress . decompress = id (random bytes)" $ property $ do
        bs <-
          forAll
            (BS.pack <$> Gen.list (Range.linear 0 4096) (Gen.word8 Range.linearBounded))
        let !c = LZ4.compress bs
        LZ4.decompress (BS.length bs) c === Right bs
    , it "decompress refuses output > maxOutput" $ property $ do
        let !payload = BS.replicate 64 0x21
            !c = LZ4.compress payload
        -- maxOutput one byte too small
        case LZ4.decompress (BS.length payload - 1) c of
          Left _ -> H.success
          Right _ -> H.failure
    , it "decompress detects truncated blocks" $ property $ do
        bs <-
          forAll
            (BS.pack <$> Gen.list (Range.linear 16 256) (Gen.word8 Range.linearBounded))
        let !c = LZ4.compress bs
        -- Drop the last byte: result might or might not parse,
        -- but if it parses it must NOT equal the original.
        cutBy <- forAll (Gen.int (Range.linear 1 (max 1 (BS.length c - 1))))
        let !truncated = BS.take (BS.length c - cutBy) c
        case LZ4.decompress (BS.length bs) truncated of
          Left _ -> H.success
          Right back -> H.diff back (/=) bs
    ]


-- ============================================================
-- iterChunk / iterScan / iterMergeBy / iterPrefetch / iterParallelMap
-- ============================================================

iterCombinatorProps :: Spec
iterCombinatorProps =
  describe
    "Columnar.Stream new combinators" $ sequence_
    [ it "iterChunk n preserves concat" $ property $ do
        n <- forAll (Gen.int (Range.linear 1 10))
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        case IS.iterToList (IS.iterChunk n (IS.iterFromList xs)) of
          Right chunks -> do
            concat chunks === xs
            -- All chunks except possibly the last have length n
            all (\c -> length c == n) (init1 chunks) === True
            -- Last chunk has length in [1..n] when xs nonempty
            case chunks of
              [] -> null xs === True
              _ ->
                let !lastLen = length (last chunks)
                in (lastLen >= 1 && lastLen <= n) === True
          Left e -> H.footnote e >> H.failure
    , it "iterChunk n=0 yields empty" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        IS.iterToList (IS.iterChunk 0 (IS.iterFromList xs)) === Right []
    , it "iterScan matches Data.List.scanl'" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        seed <- forAll (Gen.int (Range.linear 0 100))
        IS.iterToList (IS.iterScan (+) seed (IS.iterFromList xs))
          === Right (scanl' (+) seed xs)
    , it "iterMergeBy on sorted inputs == merge-sort union" $ property $ do
        xs <- sortedList
        ys <- sortedList
        zs <- sortedList
        let merged =
              IS.iterMergeBy
                compare
                [IS.iterFromList xs, IS.iterFromList ys, IS.iterFromList zs]
        case IS.iterToList merged of
          Right got -> got === sortedMerge3 xs ys zs
          Left e -> H.footnote e >> H.failure
    , it "iterMergeBy [] is empty" $
        property $
          IS.iterToList (IS.iterMergeBy compare ([] :: [IS.Iter Int])) === Right []
    , it "iterMergeBy [single] is identity" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        IS.iterToList (IS.iterMergeBy compare [IS.iterFromList xs])
          === Right xs
    , it "iterIOPrefetch preserves order and contents" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        depth <- forAll (Gen.int (Range.linear 1 8))
        got <- H.evalIO $ do
          prefetched <- IS.iterIOPrefetch depth (IS.iterIOFromIter (IS.iterFromList xs))
          IS.iterIOToList prefetched
        got === Right xs
    , it "iterParallelMap preserves order and applies the function" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        depth <- forAll (Gen.int (Range.linear 1 4))
        got <- H.evalIO $ do
          mapped <-
            IS.iterParallelMap
              depth
              (\x -> pure (x * 2))
              (IS.iterIOFromIter (IS.iterFromList xs))
          IS.iterIOToList mapped
        got === Right (map (* 2) xs)
    ]
  where
    sortedList =
      forAll
        ( sortAsc
            <$> Gen.list (Range.linear 0 20) (Gen.int (Range.linear 0 100))
        )
    sortAsc = foldr insertAsc []
    insertAsc x [] = [x]
    insertAsc x (y : ys)
      | x <= y = x : y : ys
      | otherwise = y : insertAsc x ys

    sortedMerge3 xs ys zs = sortAscMerge xs (sortAscMerge ys zs)
      where
        sortAscMerge as [] = as
        sortAscMerge [] bs = bs
        sortAscMerge (a : as) (b : bs)
          | a <= b = a : sortAscMerge as (b : bs)
          | otherwise = b : sortAscMerge (a : as) bs

    init1 [] = []
    init1 [_] = []
    init1 (x : xs) = x : init1 xs

    scanl' f = go
      where
        go !acc [] = [acc]
        go !acc (x : xs) = acc : go (f acc x) xs


-- ============================================================
-- Columnar.IO unit tests
-- ============================================================

columnarIOUnits :: Spec
columnarIOUnits =
  describe
    "Columnar.IO" $ sequence_
    [ it "loadFileEager + loadFileMmap return equal bytes" $
        withSystemTempFile "wfio.bin" $ \path h -> do
          let !payload = BS.replicate 200_000 0xAB
          BS.hPut h payload
          hClose h
          eager <- CIO.loadFileEager path
          mmaped <- CIO.loadFileMmap path
          eager `shouldBe` payload
          mmaped `shouldBe` payload
    , it "loadFile picks mmap above MmapAbove threshold" $
        withSystemTempFile "wfio-big.bin" $ \path h -> do
          let !payload = BS.replicate 200_000 0xCD
          BS.hPut h payload
          hClose h
          bs <- CIO.loadFile path
          BS.length bs `shouldBe` 200_000
    , it "loadFile uses eager path under threshold" $
        withSystemTempFile "wfio-small.bin" $ \path h -> do
          let !payload = BS.replicate 1024 0xEF
          BS.hPut h payload
          hClose h
          bs <- CIO.loadFile path
          BS.length bs `shouldBe` 1024
    ]


-- ============================================================
-- Iter properties
-- ============================================================

iterProps :: Spec
iterProps =
  describe
    "Columnar.Stream.Iter" $ sequence_
    [ it "iterToList . iterFromList = id" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        IS.iterToList (IS.iterFromList xs) === Right xs
    , it "iterMap = map" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        let f = (+ 1)
        IS.iterToList (IS.iterMap f (IS.iterFromList xs))
          === Right (map f xs)
    , it "iterFilter = filter" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        let p = even
        IS.iterToList (IS.iterFilter p (IS.iterFromList xs))
          === Right (filter p xs)
    , it "iterTake n = take n" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        n <- forAll (Gen.int (Range.linear 0 60))
        IS.iterToList (IS.iterTake n (IS.iterFromList xs))
          === Right (take n xs)
    , it "iterDrop n = drop n" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        n <- forAll (Gen.int (Range.linear 0 60))
        IS.iterToList (IS.iterDrop n (IS.iterFromList xs))
          === Right (drop n xs)
    , it "iterAppend = (++)" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        ys <- forAll (Gen.list (Range.linear 0 30) (Gen.int (Range.linear 0 100)))
        IS.iterToList (IS.iterAppend (IS.iterFromList xs) (IS.iterFromList ys))
          === Right (xs ++ ys)
    , it "iterConcat = concat" $ property $ do
        xss <-
          forAll
            ( Gen.list
                (Range.linear 0 5)
                (Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 100)))
            )
        IS.iterToList
          (IS.iterConcat (IS.iterFromList (map IS.iterFromList xss)))
          === Right (concat xss)
    , it "iterFold = foldl'" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        IS.iterFold (+) 0 (IS.iterFromList xs)
          === Right (sum xs)
    , it "iterLength = length" $ property $ do
        xs <- forAll (Gen.list (Range.linear 0 50) (Gen.int (Range.linear 0 100)))
        IS.iterLength (IS.iterFromList xs)
          === Right (length xs)
    , it "iterFromIndexed n f matches map f [0..n-1]" $ property $ do
        n <- forAll (Gen.int (Range.linear 0 30))
        let f i = Right (i * 2)
        IS.iterToList (IS.iterFromIndexed n f)
          === Right [i * 2 | i <- [0 .. n - 1]]
    , it "iterMapM threads errors" $ property $ do
        xs <- forAll (Gen.list (Range.linear 1 20) (Gen.int (Range.linear 0 100)))
        let f x = if x == 7 then Left "boom" else Right x
            expected = case break (== 7) xs of
              (pre, []) -> Right pre
              (_, _ : _) -> Left "boom"
        IS.iterToList (IS.iterMapM f (IS.iterFromList xs))
          === expected
    , it "iterRowSlice respects offset+len" $ property $ do
        -- Each element is a list of ints (its 'row count' is the
        -- list length). Cross-element slicing should match plain
        -- list slicing of the flattened stream.
        xss <-
          forAll
            ( Gen.list
                (Range.linear 0 6)
                (Gen.list (Range.linear 0 5) (Gen.int (Range.linear 0 100)))
            )
        offset <- forAll (Gen.int (Range.linear 0 30))
        taken <- forAll (Gen.int (Range.linear 0 30))
        let it = IS.iterFromList xss
            sliced =
              IS.iterRowSlice
                length
                (\s l xs -> take l (drop s xs))
                offset
                taken
                it
            expected = take taken (drop offset (concat xss))
        case IS.iterToList sliced of
          Left e -> H.footnote e >> H.failure
          Right got -> concat got === expected
    ]


-- ============================================================
-- Predicate properties
-- ============================================================

predicateProps :: Spec
predicateProps =
  describe
    "Columnar.Predicate.evalRange" $ sequence_
    [ -- Soundness: PSkip is only returned when no value in
      -- [mn, mx] satisfies the predicate. Generate a triple
      -- (mn, mx, v) and a leaf predicate, ask evalRange, then
      -- \*exhaustively* check whether any integer in [mn, mx]
      -- satisfies the predicate. If evalRange says PSkip but
      -- some integer satisfies, that's a false negative —
      -- the soundness violation we care about.
      it "PSkip => no integer in [mn,mx] satisfies the predicate (Int64)" $
        property $ do
          mn <- forAll (Gen.int (Range.linear (-50) 50))
          mx <- forAll (Gen.int (Range.linear mn 60))
          op <-
            forAll
              ( Gen.choice
                  [ pure Pred.PEq
                  , pure Pred.PNeq
                  , pure Pred.PLt
                  , pure Pred.PLtEq
                  , pure Pred.PGt
                  , pure Pred.PGtEq
                  ]
                  <*> (Pred.PVInt64 . fromIntegral <$> Gen.int (Range.linear (-100) 100))
              )
          let !decision =
                Pred.evalRange
                  (Pred.PVInt64 (fromIntegral mn))
                  (Pred.PVInt64 (fromIntegral mx))
                  op
              range64 = map fromIntegral [mn .. mx] :: [Int64]
              !satisfies = case op of
                Pred.PEq (Pred.PVInt64 v) -> v `elem` range64
                Pred.PNeq (Pred.PVInt64 v) -> any (/= v) range64
                Pred.PLt (Pred.PVInt64 v) -> any (< v) range64
                Pred.PLtEq (Pred.PVInt64 v) -> any (<= v) range64
                Pred.PGt (Pred.PVInt64 v) -> any (> v) range64
                Pred.PGtEq (Pred.PVInt64 v) -> any (>= v) range64
                _ -> True
          case decision of
            Pred.PSkip -> satisfies === False
            Pred.PMaybeKeep -> H.success -- always sound
    , it "PSkip soundness for Int32 ranges" $ property $ do
        mn <- forAll (Gen.int32 (Range.linear (-50) 50))
        mx <- forAll (Gen.int32 (Range.linear mn 60))
        v <- forAll (Gen.int32 (Range.linear (-100) 100))
        let !decision =
              Pred.evalRange
                (Pred.PVInt32 mn)
                (Pred.PVInt32 mx)
                (Pred.PEq (Pred.PVInt32 v))
        case decision of
          Pred.PSkip -> (v >= mn && v <= mx) === False
          _ -> H.success
    , it "PSkip soundness for Double ranges" $ property $ do
        mn <- forAll (Gen.double (Range.linearFrac (-50.0) 50.0))
        mx <- forAll (Gen.double (Range.linearFrac mn 60.0))
        v <- forAll (Gen.double (Range.linearFrac (-100.0) 100.0))
        let !decision =
              Pred.evalRange
                (Pred.PVDouble mn)
                (Pred.PVDouble mx)
                (Pred.PEq (Pred.PVDouble v))
        case decision of
          Pred.PSkip -> (v >= mn && v <= mx) === False
          _ -> H.success
    , it "PSkip soundness for Text ranges (UTF-8 byte order)" $
        property $ do
          let alpha = Gen.text (Range.linear 1 4) Gen.alpha
          mn <- forAll alpha
          mx <- forAll (Gen.filter (>= mn) alpha)
          v <- forAll alpha
          let !decision =
                Pred.evalRange
                  (Pred.PVText mn)
                  (Pred.PVText mx)
                  (Pred.PEq (Pred.PVText v))
          case decision of
            Pred.PSkip -> (v >= mn && v <= mx) === False
            _ -> H.success
    , it "PIn rejects only when every member is outside the range" $
        property $ do
          mn <- forAll (Gen.int (Range.linear (-50) 50))
          mx <- forAll (Gen.int (Range.linear mn 60))
          ks <- forAll (Gen.list (Range.linear 1 5) (Gen.int (Range.linear (-100) 100)))
          let !decision =
                Pred.evalRange
                  (Pred.PVInt64 (fromIntegral mn))
                  (Pred.PVInt64 (fromIntegral mx))
                  (Pred.PIn (map (Pred.PVInt64 . fromIntegral) ks))
              !anyInside = any (\k -> k >= mn && k <= mx) ks
          if anyInside
            then decision === Pred.PMaybeKeep
            else decision === Pred.PSkip
    , it "PIsNull always returns PMaybeKeep (range-only stats)" $
        property $ do
          mn <- forAll (Gen.int (Range.linear (-100) 100))
          mx <- forAll (Gen.int (Range.linear mn 100))
          Pred.evalRange
            (Pred.PVInt64 (fromIntegral mn))
            (Pred.PVInt64 (fromIntegral mx))
            Pred.PIsNull
            === Pred.PMaybeKeep
    , it "PIsNotNull always returns PMaybeKeep" $ property $ do
        mn <- forAll (Gen.int (Range.linear (-100) 100))
        mx <- forAll (Gen.int (Range.linear mn 100))
        Pred.evalRange
          (Pred.PVInt64 (fromIntegral mn))
          (Pred.PVInt64 (fromIntegral mx))
          Pred.PIsNotNull
          === Pred.PMaybeKeep
    , it "PNeq always returns PMaybeKeep (can't prove from range alone)" $
        property $ do
          mn <- forAll (Gen.int (Range.linear (-100) 100))
          mx <- forAll (Gen.int (Range.linear mn 100))
          v <- forAll (Gen.int (Range.linear (-200) 200))
          Pred.evalRange
            (Pred.PVInt64 (fromIntegral mn))
            (Pred.PVInt64 (fromIntegral mx))
            (Pred.PNeq (Pred.PVInt64 (fromIntegral v)))
            === Pred.PMaybeKeep
    , it "Cross-type comparison degrades to PMaybeKeep" $ property $ do
        v <- forAll (Gen.int (Range.linear (-100) 100))
        txt <- forAll (Gen.text (Range.linear 0 5) Gen.alpha)
        let !decision =
              Pred.evalRange
                (Pred.PVInt64 (fromIntegral v))
                (Pred.PVInt64 (fromIntegral v))
                (Pred.PEq (Pred.PVText txt))
        decision === Pred.PMaybeKeep
    ]


predicateUnits :: Spec
predicateUnits =
  describe
    "Columnar.Predicate units" $ sequence_
    [ it "combineDecisions PSkip _ = PSkip" $
        Pred.combineDecisions Pred.PSkip Pred.PMaybeKeep `shouldBe` Pred.PSkip
    , it "combineDecisions PMaybeKeep PMaybeKeep = PMaybeKeep" $
        Pred.combineDecisions Pred.PMaybeKeep Pred.PMaybeKeep `shouldBe` Pred.PMaybeKeep
    , it "pvLess on incomparable returns False" $
        Pred.pvLess (Pred.PVInt32 1) (Pred.PVText "x") `shouldBe` False
    ]
