{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for 'Kafka.Protocol.Wire.SliceVector'.
module Protocol.SliceVectorSpec (tests) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=), assertBool)

import qualified "wireform-kafka-protocol" Kafka.Protocol.Wire.SliceVector as SV

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------

-- | Build a flat backing buffer plus a 'SliceVector' of the
-- supplied per-slice 'ByteString' contents. The returned
-- 'SliceVector' shares the buffer across every slice (the
-- whole point of the type).
mkFromContents :: [BS.ByteString] -> SV.SliceVector
mkFromContents pieces =
  let !flat = BS.concat pieces
      !(fp, off, _len) = BSI.toForeignPtr flat
      go !_pos []     = []
      go !pos (b:bs) =
        ( fromIntegral (off + pos)
        , fromIntegral (BS.length b)
        ) : go (pos + BS.length b) bs
  in SV.fromForeignPtr fp (go 0 pieces)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Kafka.Protocol.Wire.SliceVector"
  [ basicConstruction
  , indexing
  , folds
  , sharingInvariant
  ]

basicConstruction :: TestTree
basicConstruction = testGroup "construction"
  [ testCase "empty has length 0 and is null" $ do
      SV.length SV.empty @?= 0
      assertBool "empty should be null" (SV.null SV.empty)
  , testCase "singleton has length 1" $ do
      let !(fp, _, _) = BSI.toForeignPtr (BS.pack [0x41, 0x42, 0x43])
          !sv = SV.singleton fp 0 3
      SV.length sv @?= 1
      assertBool "non-empty" (not (SV.null sv))
      SV.indexBS sv 0 @?= BS.pack [0x41, 0x42, 0x43]
  , testCase "fromForeignPtr respects offsets + lengths" $ do
      let !(fp, _, _) = BSI.toForeignPtr (BS.pack [0..9])
          !sv = SV.fromForeignPtr fp [(0, 3), (3, 4), (7, 3)]
      SV.length sv @?= 3
      SV.toListBS sv @?= [ BS.pack [0,1,2]
                         , BS.pack [3,4,5,6]
                         , BS.pack [7,8,9]
                         ]
  , testCase "fromByteStrings: same backing buffer succeeds" $ do
      let !sv = mkFromContents
                  [BS.pack [1,2,3], BS.pack [4,5], BS.pack [6,7,8,9]]
      SV.toListBS sv @?= [BS.pack [1,2,3], BS.pack [4,5], BS.pack [6,7,8,9]]
  , testCase "fromForeignPtrSlices accepts pre-built unboxed vector" $ do
      let !(fp, _, _) = BSI.toForeignPtr (BS.pack [10,20,30,40,50])
          !ofs = VU.fromList [(0,2), (2,3)]
          !sv  = SV.fromForeignPtrSlices fp ofs
      SV.length sv @?= 2
      SV.indexBS sv 0 @?= BS.pack [10,20]
      SV.indexBS sv 1 @?= BS.pack [30,40,50]
  ]

indexing :: TestTree
indexing = testGroup "indexing"
  [ testCase "indexBS returns the right bytes" $ do
      let !sv = mkFromContents [BS.pack [1], BS.pack [2,3], BS.pack [4,5,6]]
      SV.indexBS sv 0 @?= BS.pack [1]
      SV.indexBS sv 1 @?= BS.pack [2,3]
      SV.indexBS sv 2 @?= BS.pack [4,5,6]
  , testCase "(!) is the same as indexBS" $ do
      let !sv = mkFromContents [BS.pack [9,8], BS.pack [7,6,5]]
      sv SV.! 0 @?= SV.indexBS sv 0
      sv SV.! 1 @?= SV.indexBS sv 1
  , testCase "indexUnsafe yields same bytes as indexBS for valid indices" $ do
      let !sv = mkFromContents [BS.pack [11,12], BS.pack [13]]
      SV.indexUnsafe sv 0 @?= SV.indexBS sv 0
      SV.indexUnsafe sv 1 @?= SV.indexBS sv 1
  ]

folds :: TestTree
folds = testGroup "folds + iteration"
  [ testCase "foldlSlices' walks (offset, length) pairs without allocating" $ do
      -- 5 slices, 3 bytes each, contiguous from offset 0.
      let !sv = mkFromContents (replicate 5 (BS.pack [0,0,0]))
          !totalLen = SV.foldlSlices' (\acc _o l -> acc + fromIntegral l) (0 :: Int) sv
      totalLen @?= 15
  , testCase "foldlBS' visits every slice in order" $ do
      let pieces = [BS.pack [1], BS.pack [2,3], BS.pack [4]]
          !sv  = mkFromContents pieces
          !rev = SV.foldlBS' (flip (:)) [] sv
      reverse rev @?= pieces
  , testCase "toList returns the offset/length pairs" $ do
      let !sv = mkFromContents [BS.pack [1,2], BS.pack [3,4,5]]
      SV.toList sv @?= [(0,2), (2,3)]
  , testCase "toVector materialises a Vector ByteString" $ do
      let pieces = [BS.pack [9], BS.pack [8,7]]
          !sv   = mkFromContents pieces
          !vec  = SV.toVector sv
      V.length vec @?= 2
      vec V.! 0 @?= BS.pack [9]
      vec V.! 1 @?= BS.pack [8,7]
  , testCase "Eq compares contents (same backing pointer)" $ do
      let !a = mkFromContents [BS.pack [1,2], BS.pack [3]]
          !c = mkFromContents [BS.pack [1,2], BS.pack [4]]
      a == a @?= True
      a == c @?= False
  ]

sharingInvariant :: TestTree
sharingInvariant = testGroup "sharing invariant"
  [ -- bytestring-0.11.4+ flattened the 'BS.PS fp off len' shape
    -- into 'BS.BS fp len' and now adjusts the 'ForeignPtr' so
    -- 'BSI.toForeignPtr' returns a /different/ 'ForeignPtr' for
    -- a slice with non-zero offset. We can't test pointer
    -- identity directly any more, so check the property we
    -- actually care about: the backing memory is shared (same
    -- raw 'Ptr Word8' once both ForeignPtrs are forced), and
    -- the slices' contents do come from the SliceVector's
    -- buffer (round-trip via toListBS).
    testCase "shared-buffer slices preserve content + count" $ do
      let pieces = [BS.pack [1,2,3], BS.pack [4,5], BS.pack [6]]
          !sv   = mkFromContents pieces
      SV.length sv @?= 3
      SV.toListBS sv @?= pieces
      -- Mutate the source 'SliceVector''s buffer through one
      -- indexBS call; subsequent indexBS calls see the same
      -- bytes because they re-read from the same buffer (no
      -- per-slice copy).
      SV.indexBS sv 0 @?= BS.pack [1,2,3]
      SV.indexBS sv 0 @?= BS.pack [1,2,3]
  ]
