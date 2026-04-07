module Test.FlatBuffers (flatBuffersTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Word (Word32)
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)

import qualified FlatBuffers.Value as F
import FlatBuffers.Encode (encode)
import FlatBuffers.Decode (decode)

flatBuffersTests :: TestTree
flatBuffersTests = testGroup "FlatBuffers"
  [ unitTests
  , wireFormatTests
  , edgeCases
  ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "VBool true encodes" $ do
      let bs = encode (F.VBool True)
      BS.length bs @?= 5
      BS.index bs 4 @?= 0x01

  , testCase "VBool false encodes" $ do
      let bs = encode (F.VBool False)
      BS.length bs @?= 5
      BS.index bs 4 @?= 0x00

  , testCase "VInt8 encodes" $ do
      let bs = encode (F.VInt8 42)
      BS.length bs @?= 5
      BS.index bs 4 @?= 42

  , testCase "VInt16 encodes" $ do
      let bs = encode (F.VInt16 256)
      BS.length bs @?= 6

  , testCase "VInt32 encodes" $ do
      let bs = encode (F.VInt32 100000)
      BS.length bs @?= 8

  , testCase "VInt64 encodes" $ do
      let bs = encode (F.VInt64 maxBound)
      BS.length bs @?= 12

  , testCase "VWord8 encodes" $ do
      let bs = encode (F.VWord8 255)
      BS.length bs @?= 5

  , testCase "VWord16 encodes" $ do
      let bs = encode (F.VWord16 65535)
      BS.length bs @?= 6

  , testCase "VWord32 encodes" $ do
      let bs = encode (F.VWord32 maxBound)
      BS.length bs @?= 8

  , testCase "VWord64 encodes" $ do
      let bs = encode (F.VWord64 maxBound)
      BS.length bs @?= 12

  , testCase "VFloat encodes" $ do
      let bs = encode (F.VFloat 3.14)
      BS.length bs @?= 8

  , testCase "VDouble encodes" $ do
      let bs = encode (F.VDouble 2.718)
      BS.length bs @?= 12

  , testCase "VString encodes" $ do
      let bs = encode (F.VString (T.pack "hello"))
      BS.length bs > 4 @?= True

  , testCase "VVector empty encodes" $ do
      let bs = encode (F.VVector V.empty)
      BS.length bs @?= 8
  ]

wireFormatTests :: TestTree
wireFormatTests = testGroup "Wire format"
  [ testCase "Root offset is first 4 bytes LE" $ do
      let bs = encode (F.VInt32 42)
          off = readLE32 bs 0
      off @?= 4

  , testCase "VString length prefix" $ do
      let bs = encode (F.VString (T.pack "ABC"))
          strLen = readLE32 bs 4
      strLen @?= 3

  , testCase "VString NUL terminated" $ do
      let bs = encode (F.VString (T.pack "AB"))
          nulPos = 4 + 4 + 2
      BS.index bs nulPos @?= 0x00

  , testCase "VVector length prefix" $ do
      let bs = encode (F.VVector (V.fromList [F.VWord8 1, F.VWord8 2]))
          cnt = readLE32 bs 4
      cnt @?= 2

  , testCase "VInt32 LE bytes" $ do
      let bs = encode (F.VInt32 0x04030201)
      BS.index bs 4 @?= 0x01
      BS.index bs 5 @?= 0x02
      BS.index bs 6 @?= 0x03
      BS.index bs 7 @?= 0x04
  ]

edgeCases :: TestTree
edgeCases = testGroup "Edge cases"
  [ testCase "Empty string" $ do
      let bs = encode (F.VString T.empty)
      BS.length bs > 4 @?= True
      readLE32 bs 4 @?= 0

  , testCase "Empty vector" $ do
      let bs = encode (F.VVector V.empty)
      readLE32 bs 4 @?= 0

  , testCase "VStruct encodes" $ do
      let val = F.VStruct (V.fromList [F.VInt32 1, F.VInt32 2])
          bs = encode val
      BS.length bs @?= 12

  , testCase "Decode too short" $
      case decode (BS.pack [0, 0]) of
        Left _ -> pure ()
        Right _ -> assertFailure "expected error on short input"

  , testCase "Nested VVector" $ do
      let val = F.VVector (V.fromList [F.VVector (V.fromList [F.VWord8 1])])
          bs = encode val
      BS.length bs > 4 @?= True
  ]

readLE32 :: BS.ByteString -> Int -> Word32
readLE32 bs off =
  fromIntegral (BS.index bs off)
  + fromIntegral (BS.index bs (off+1)) * 256
  + fromIntegral (BS.index bs (off+2)) * 65536
  + fromIntegral (BS.index bs (off+3)) * 16777216
