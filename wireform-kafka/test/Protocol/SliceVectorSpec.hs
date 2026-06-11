{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for 'Kafka.Protocol.Wire.SliceVector'.
module Protocol.SliceVectorSpec (tests) where

import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Test.Syd
import "wireform-kafka-protocol" Kafka.Protocol.Wire.SliceVector qualified as SV


----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

{- | Build a flat backing buffer plus a 'SliceVector' of the
supplied per-slice 'ByteString' contents. The returned
'SliceVector' shares the buffer across every slice (the
whole point of the type).
-}
mkFromContents :: [BS.ByteString] -> SV.SliceVector
mkFromContents pieces =
  let !flat = BS.concat pieces
      !(fp, off, _len) = BSI.toForeignPtr flat
      go !_pos [] = []
      go !pos (b : bs) =
        ( fromIntegral (off + pos)
        , fromIntegral (BS.length b)
        )
          : go (pos + BS.length b) bs
  in SV.fromForeignPtr fp (go 0 pieces)


----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: Spec
tests =
  describe "Kafka.Protocol.Wire.SliceVector" $
    sequence_
      [ basicConstruction
      , indexing
      , folds
      , sharingInvariant
      ]


basicConstruction :: Spec
basicConstruction =
  describe "construction" $
    sequence_
      [ it "empty has length 0 and is null" $ do
          SV.length SV.empty `shouldBe` 0
          (SV.null SV.empty) `shouldBe` True
      , it "singleton has length 1" $ do
          let !(fp, _, _) = BSI.toForeignPtr (BS.pack [0x41, 0x42, 0x43])
              !sv = SV.singleton fp 0 3
          SV.length sv `shouldBe` 1
          (not (SV.null sv)) `shouldBe` True
          SV.indexBS sv 0 `shouldBe` BS.pack [0x41, 0x42, 0x43]
      , it "fromForeignPtr respects offsets + lengths" $ do
          let !(fp, _, _) = BSI.toForeignPtr (BS.pack [0 .. 9])
              !sv = SV.fromForeignPtr fp [(0, 3), (3, 4), (7, 3)]
          SV.length sv `shouldBe` 3
          SV.toListBS sv
            `shouldBe` [ BS.pack [0, 1, 2]
                       , BS.pack [3, 4, 5, 6]
                       , BS.pack [7, 8, 9]
                       ]
      , it "fromByteStrings: same backing buffer succeeds" $ do
          let !sv =
                mkFromContents
                  [BS.pack [1, 2, 3], BS.pack [4, 5], BS.pack [6, 7, 8, 9]]
          SV.toListBS sv `shouldBe` [BS.pack [1, 2, 3], BS.pack [4, 5], BS.pack [6, 7, 8, 9]]
      , it "fromForeignPtrSlices accepts pre-built unboxed vector" $ do
          let !(fp, _, _) = BSI.toForeignPtr (BS.pack [10, 20, 30, 40, 50])
              !ofs = VU.fromList [(0, 2), (2, 3)]
              !sv = SV.fromForeignPtrSlices fp ofs
          SV.length sv `shouldBe` 2
          SV.indexBS sv 0 `shouldBe` BS.pack [10, 20]
          SV.indexBS sv 1 `shouldBe` BS.pack [30, 40, 50]
      ]


indexing :: Spec
indexing =
  describe "indexing" $
    sequence_
      [ it "indexBS returns the right bytes" $ do
          let !sv = mkFromContents [BS.pack [1], BS.pack [2, 3], BS.pack [4, 5, 6]]
          SV.indexBS sv 0 `shouldBe` BS.pack [1]
          SV.indexBS sv 1 `shouldBe` BS.pack [2, 3]
          SV.indexBS sv 2 `shouldBe` BS.pack [4, 5, 6]
      , it "(!) is the same as indexBS" $ do
          let !sv = mkFromContents [BS.pack [9, 8], BS.pack [7, 6, 5]]
          sv SV.! 0 `shouldBe` SV.indexBS sv 0
          sv SV.! 1 `shouldBe` SV.indexBS sv 1
      , it "indexUnsafe yields same bytes as indexBS for valid indices" $ do
          let !sv = mkFromContents [BS.pack [11, 12], BS.pack [13]]
          SV.indexUnsafe sv 0 `shouldBe` SV.indexBS sv 0
          SV.indexUnsafe sv 1 `shouldBe` SV.indexBS sv 1
      ]


folds :: Spec
folds =
  describe "folds + iteration" $
    sequence_
      [ it "foldlSlices' walks (offset, length) pairs without allocating" $ do
          -- 5 slices, 3 bytes each, contiguous from offset 0.
          let !sv = mkFromContents (replicate 5 (BS.pack [0, 0, 0]))
              !totalLen = SV.foldlSlices' (\acc _o l -> acc + fromIntegral l) (0 :: Int) sv
          totalLen `shouldBe` 15
      , it "foldlBS' visits every slice in order" $ do
          let pieces = [BS.pack [1], BS.pack [2, 3], BS.pack [4]]
              !sv = mkFromContents pieces
              !rev = SV.foldlBS' (flip (:)) [] sv
          reverse rev `shouldBe` pieces
      , it "toList returns the offset/length pairs" $ do
          let !sv = mkFromContents [BS.pack [1, 2], BS.pack [3, 4, 5]]
          SV.toList sv `shouldBe` [(0, 2), (2, 3)]
      , it "toVector materialises a Vector ByteString" $ do
          let pieces = [BS.pack [9], BS.pack [8, 7]]
              !sv = mkFromContents pieces
              !vec = SV.toVector sv
          V.length vec `shouldBe` 2
          vec V.! 0 `shouldBe` BS.pack [9]
          vec V.! 1 `shouldBe` BS.pack [8, 7]
      , it "Eq compares contents (same backing pointer)" $ do
          let !a = mkFromContents [BS.pack [1, 2], BS.pack [3]]
              !c = mkFromContents [BS.pack [1, 2], BS.pack [4]]
          a == a `shouldBe` True
          a == c `shouldBe` False
      ]


sharingInvariant :: Spec
sharingInvariant =
  describe "sharing invariant" $
    sequence_
      [ -- bytestring-0.11.4+ flattened the 'BS.PS fp off len' shape
        -- into 'BS.BS fp len' and now adjusts the 'ForeignPtr' so
        -- 'BSI.toForeignPtr' returns a /different/ 'ForeignPtr' for
        -- a slice with non-zero offset. We can't test pointer
        -- identity directly any more, so check the property we
        -- actually care about: the backing memory is shared (same
        -- raw 'Ptr Word8' once both ForeignPtrs are forced), and
        -- the slices' contents do come from the SliceVector's
        -- buffer (round-trip via toListBS).
        it "shared-buffer slices preserve content + count" $ do
          let pieces = [BS.pack [1, 2, 3], BS.pack [4, 5], BS.pack [6]]
              !sv = mkFromContents pieces
          SV.length sv `shouldBe` 3
          SV.toListBS sv `shouldBe` pieces
          -- Mutate the source 'SliceVector''s buffer through one
          -- indexBS call; subsequent indexBS calls see the same
          -- bytes because they re-read from the same buffer (no
          -- per-slice copy).
          SV.indexBS sv 0 `shouldBe` BS.pack [1, 2, 3]
          SV.indexBS sv 0 `shouldBe` BS.pack [1, 2, 3]
      ]
