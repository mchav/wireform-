module Test.Iceberg.Puffin (tests) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Test.Syd

import Iceberg.Puffin

tests :: Spec
tests = describe "Iceberg.Puffin" $ sequence_
  [ it "writePuffin -> readPuffin round-trips one blob" $ do
      let blob = PuffinBlob
            { pbType = "deletion-vector-v1"
            , pbFields = V.singleton 1
            , pbSnapshotId = 99
            , pbSequenceNumber = 1
            , pbProperties = Map.fromList [("k", "v")]
            , pbCompressionCodec = Nothing
            , pbData = BS.pack [1, 2, 3, 4, 5]
            }
          footer = PuffinFooter (V.singleton blob) Map.empty
          bs = writePuffin footer
      case readPuffin bs of
        Left e -> expectationFailure e
        Right pf -> do
          V.length (pfBlobs pf) `shouldBe` 1
          let b = V.unsafeIndex (pfBlobs pf) 0
          pbType b `shouldBe` "deletion-vector-v1"
          pbData b `shouldBe` BS.pack [1, 2, 3, 4, 5]
          pbProperties b `shouldBe` Map.fromList [("k", "v")]

  , it "writePuffin -> readPuffin round-trips multiple blobs" $ do
      let mkBlob i bs = PuffinBlob
            { pbType = "test"
            , pbFields = V.singleton i
            , pbSnapshotId = 1
            , pbSequenceNumber = 1
            , pbProperties = Map.empty
            , pbCompressionCodec = Nothing
            , pbData = bs
            }
          blobs = V.fromList
            [ mkBlob 1 (BS.pack [10, 20, 30])
            , mkBlob 2 (BS.pack [40, 50, 60, 70])
            , mkBlob 3 (BS.pack [80])
            ]
          bs = writePuffin (PuffinFooter blobs Map.empty)
      case readPuffin bs of
        Left e -> expectationFailure e
        Right pf -> do
          V.length (pfBlobs pf) `shouldBe` 3
          V.toList (V.map pbData (pfBlobs pf)) `shouldBe`
            [ BS.pack [10, 20, 30]
            , BS.pack [40, 50, 60, 70]
            , BS.pack [80]
            ]
  ]
