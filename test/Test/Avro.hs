module Test.Avro (avroTests) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog

import Avro.Schema
import Avro.Value
import Avro.Encode (encodeAvro)
import Avro.Decode (decodeAvro)

avroTests :: TestTree
avroTests = testGroup "Avro Encode/Decode"
  [ testGroup "Primitive roundtrips (property)"
      [ testProperty "null roundtrip" $ property $ do
          let ty = AvroPrimitive AvroNull
          roundtrip ty AvNull

      , testProperty "bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          roundtrip (AvroPrimitive AvroBool) (AvBool b)

      , testProperty "int roundtrip" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          roundtrip (AvroPrimitive AvroInt) (AvInt n)

      , testProperty "long roundtrip" $ property $ do
          n <- forAll $ Gen.int64 Range.linearBounded
          roundtrip (AvroPrimitive AvroLong) (AvLong n)

      , testProperty "float roundtrip" $ property $ do
          f <- forAll $ Gen.float (Range.linearFrac (-1e6) 1e6)
          roundtrip (AvroPrimitive AvroFloat) (AvFloat f)

      , testProperty "double roundtrip" $ property $ do
          d <- forAll $ Gen.double (Range.linearFrac (-1e12) 1e12)
          roundtrip (AvroPrimitive AvroDouble) (AvDouble d)

      , testProperty "bytes roundtrip" $ property $ do
          bs <- forAll $ Gen.bytes (Range.linear 0 256)
          roundtrip (AvroPrimitive AvroBytes) (AvBytes bs)

      , testProperty "string roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 128) Gen.unicode
          roundtrip (AvroPrimitive AvroString) (AvString t)
      ]

  , testGroup "Edge cases (unit)"
      [ testCase "null encodes to empty" $ do
          let bs = encodeAvro (AvroPrimitive AvroNull) AvNull
          bs @?= BS.empty

      , testCase "empty string" $ do
          let ty = AvroPrimitive AvroString
              val = AvString ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty bytes" $ do
          let ty = AvroPrimitive AvroBytes
              val = AvBytes ""
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty array" $ do
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AvArray []
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "empty map" $ do
          let ty = AvroMap (AvroPrimitive AvroString)
              val = AvMap []
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 0 (null)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 0 AvNull
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "union index 1 (string)" $ do
          let ty = AvroUnion (V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString])
              val = AvUnion 1 (AvString "hello")
          decodeAvro ty (encodeAvro ty val) @?= Right val

      , testCase "bool true" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AvBool True)) @?= Right (AvBool True)

      , testCase "bool false" $ do
          let ty = AvroPrimitive AvroBool
          decodeAvro ty (encodeAvro ty (AvBool False)) @?= Right (AvBool False)

      , testCase "int 0" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt 0)) @?= Right (AvInt 0)

      , testCase "int min" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt minBound)) @?= Right (AvInt minBound)

      , testCase "int max" $ do
          let ty = AvroPrimitive AvroInt
          decodeAvro ty (encodeAvro ty (AvInt maxBound)) @?= Right (AvInt maxBound)

      , testCase "long min" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AvLong minBound)) @?= Right (AvLong minBound)

      , testCase "long max" $ do
          let ty = AvroPrimitive AvroLong
          decodeAvro ty (encodeAvro ty (AvLong maxBound)) @?= Right (AvLong maxBound)
      ]

  , testGroup "Record roundtrip"
      [ testProperty "mixed-field record" $ property $ do
          n <- forAll $ Gen.int32 Range.linearBounded
          t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
          b <- forAll Gen.bool
          d <- forAll $ Gen.double (Range.linearFrac (-1e6) 1e6)
          let ty = mkRecordType "TestRecord"
                     [ ("intField",    AvroPrimitive AvroInt)
                     , ("stringField", AvroPrimitive AvroString)
                     , ("boolField",   AvroPrimitive AvroBool)
                     , ("doubleField", AvroPrimitive AvroDouble)
                     ]
              val = AvRecord [AvInt n, AvString t, AvBool b, AvDouble d]
          roundtrip ty val
      ]

  , testGroup "Array roundtrip"
      [ testProperty "array of ints" $ property $ do
          ns <- forAll $ Gen.list (Range.linear 0 50) (Gen.int32 Range.linearBounded)
          let ty = AvroArray (AvroPrimitive AvroInt)
              val = AvArray (map AvInt ns)
          roundtrip ty val

      , testProperty "array of records" $ property $ do
          items <- forAll $ Gen.list (Range.linear 0 20) $ do
            i <- Gen.int32 Range.linearBounded
            s <- Gen.text (Range.linear 0 32) Gen.unicode
            pure (i, s)
          let recTy = mkRecordType "Item"
                        [ ("id",   AvroPrimitive AvroInt)
                        , ("name", AvroPrimitive AvroString)
                        ]
              ty = AvroArray recTy
              val = AvArray [AvRecord [AvInt i, AvString s] | (i, s) <- items]
          roundtrip ty val
      ]

  , testGroup "Map roundtrip"
      [ testProperty "map of longs" $ property $ do
          entries <- forAll $ Gen.list (Range.linear 0 30) $ do
            k <- Gen.text (Range.linear 1 32) Gen.alphaNum
            v <- Gen.int64 Range.linearBounded
            pure (k, v)
          let ty = AvroMap (AvroPrimitive AvroLong)
              val = AvMap [(k, AvLong v) | (k, v) <- entries]
          roundtrip ty val
      ]

  , testGroup "Union roundtrip"
      [ testProperty "null|string union" $ property $ do
          useNull <- forAll Gen.bool
          let branches = V.fromList [AvroPrimitive AvroNull, AvroPrimitive AvroString]
              ty = AvroUnion branches
          val <- if useNull
                 then pure (AvUnion 0 AvNull)
                 else do
                   t <- forAll $ Gen.text (Range.linear 0 64) Gen.unicode
                   pure (AvUnion 1 (AvString t))
          roundtrip ty val

      , testProperty "int|long|string union" $ property $ do
          branch <- forAll $ Gen.int (Range.linear 0 2)
          let branches = V.fromList
                [ AvroPrimitive AvroInt
                , AvroPrimitive AvroLong
                , AvroPrimitive AvroString
                ]
              ty = AvroUnion branches
          val <- case branch of
            0 -> AvUnion 0 . AvInt <$> forAll (Gen.int32 Range.linearBounded)
            1 -> AvUnion 1 . AvLong <$> forAll (Gen.int64 Range.linearBounded)
            _ -> AvUnion 2 . AvString <$> forAll (Gen.text (Range.linear 0 32) Gen.unicode)
          roundtrip ty val
      ]

  , testGroup "Fixed roundtrip"
      [ testProperty "fixed bytes" $ property $ do
          sz <- forAll $ Gen.int (Range.linear 0 64)
          bs <- forAll $ Gen.bytes (Range.singleton sz)
          let ty = AvroFixed "TestFixed" Nothing sz V.empty
              val = AvFixed bs
          roundtrip ty val

      , testCase "fixed empty" $ do
          let ty = AvroFixed "Empty" Nothing 0 V.empty
              val = AvFixed ""
          decodeAvro ty (encodeAvro ty val) @?= Right val
      ]

  , testGroup "Enum roundtrip"
      [ testProperty "enum ordinal" $ property $ do
          idx <- forAll $ Gen.int (Range.linear 0 9)
          let syms = V.fromList (map (T.pack . ("S" ++) . show) [0..9 :: Int])
              ty = AvroEnum "TestEnum" Nothing Nothing V.empty syms Nothing
              val = AvEnum idx
          roundtrip ty val
      ]
  ]

-- Helpers

roundtrip :: (MonadTest m) => AvroType -> AvroValue -> m ()
roundtrip ty val =
  decodeAvro ty (encodeAvro ty val) === Right val

mkRecordType :: Text -> [(Text, AvroType)] -> AvroType
mkRecordType name fields = AvroRecord
  { avroRecordName      = name
  , avroRecordNamespace = Nothing
  , avroRecordDoc       = Nothing
  , avroRecordAliases   = V.empty
  , avroRecordFields    = V.fromList
      [ AvroField
          { avroFieldName    = fname
          , avroFieldType    = ftype
          , avroFieldDefault = Nothing
          , avroFieldOrder   = Nothing
          , avroFieldAliases = V.empty
          , avroFieldDoc     = Nothing
          }
      | (fname, ftype) <- fields
      ]
  }
