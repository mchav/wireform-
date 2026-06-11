module Test.NDJSON (ndjsonTests) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NDJSON.Decode
import NDJSON.Encode
import Test.Syd
import Test.Syd.Hedgehog ()


ndjsonTests :: Spec
ndjsonTests =
  describe "NDJSON" $
    sequence_
      [ parseTests
      , emptyLineTests
      , streamTests
      , typedRecordTests
      , roundtripTests
      , largeFileTests
      ]


parseTests :: Spec
parseTests =
  describe "Basic parsing" $
    sequence_
      [ it "Parse multi-line NDJSON" $ do
          let input = "{\"a\":1}\n{\"b\":2}\n{\"c\":3}"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 3
          vals V.! 0 `shouldBe` Aeson.object [("a", Aeson.Number 1)]
          vals V.! 1 `shouldBe` Aeson.object [("b", Aeson.Number 2)]
          vals V.! 2 `shouldBe` Aeson.object [("c", Aeson.Number 3)]
      , it "Parse single line" $ do
          let input = "{\"x\":42}"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 1
      , it "Parse arrays" $ do
          let input = "[1,2,3]\n[4,5,6]"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 2
      , it "Parse mixed types" $ do
          let input = "\"hello\"\n42\ntrue\nnull"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 4
          vals V.! 0 `shouldBe` Aeson.String "hello"
          vals V.! 1 `shouldBe` Aeson.Number 42
          vals V.! 2 `shouldBe` Aeson.Bool True
          vals V.! 3 `shouldBe` Aeson.Null
      ]


emptyLineTests :: Spec
emptyLineTests =
  describe "Empty lines" $
    sequence_
      [ it "Empty lines are skipped" $ do
          let input = "{\"a\":1}\n\n{\"b\":2}\n\n"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 2
      , it "Only newlines yields empty" $ do
          let input = "\n\n\n"
              Right vals = decode (BSC.pack input)
          V.length vals `shouldBe` 0
      , it "Empty input" $ do
          let Right vals = decode BS.empty
          V.length vals `shouldBe` 0
      ]


streamTests :: Spec
streamTests =
  describe "Streaming" $
    sequence_
      [ it "Streaming decode processes all values" $ do
          let input = "{\"a\":1}\n{\"b\":2}\n{\"c\":3}"
          ref <- newIORef []
          result <- decodeStream (BSC.pack input) $ \val ->
            modifyIORef' ref (val :)
          result `shouldBe` Right ()
          vals <- reverse <$> readIORef ref
          length vals `shouldBe` 3
      , it "Streaming decode skips empty lines" $ do
          let input = "{\"a\":1}\n\n{\"b\":2}\n"
          ref <- newIORef []
          result <- decodeStream (BSC.pack input) $ \val ->
            modifyIORef' ref (val :)
          result `shouldBe` Right ()
          vals <- reverse <$> readIORef ref
          length vals `shouldBe` 2
      ]


data TestRecord = TestRecord
  { name :: !String
  , value :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)


typedRecordTests :: Spec
typedRecordTests =
  describe "Typed records" $
    sequence_
      [ it "Parse typed records" $ do
          let input = "{\"name\":\"foo\",\"value\":1}\n{\"name\":\"bar\",\"value\":2}"
              Right records = decodeRecords (BSC.pack input) :: Either String (V.Vector TestRecord)
          V.length records `shouldBe` 2
          records V.! 0 `shouldBe` TestRecord "foo" 1
          records V.! 1 `shouldBe` TestRecord "bar" 2
      ]


roundtripTests :: Spec
roundtripTests =
  describe "Roundtrip" $
    sequence_
      [ it "Encode and decode objects" $ do
          let vals =
                V.fromList
                  [ Aeson.object [("a", Aeson.Number 1)]
                  , Aeson.object [("b", Aeson.String "hello")]
                  ]
              encoded = encode vals
              Right decoded = decode encoded
          decoded `shouldBe` vals
      , it "Encode and decode records" $ do
          let records = V.fromList [TestRecord "x" 1, TestRecord "y" 2]
              encoded = encodeRecords records
              Right decoded = decodeRecords encoded :: Either String (V.Vector TestRecord)
          decoded `shouldBe` records
      , it "Roundtrip with random objects" $ property $ do
          n <- forAll $ Gen.int (Range.linear 1 20)
          vals <- forAll $ V.replicateM n $ do
            k <- Gen.text (Range.linear 1 10) Gen.alpha
            v <- Gen.int (Range.linear (-1000) 1000)
            pure $ Aeson.object [(Key.fromText k, Aeson.Number (fromIntegral v))]
          let encoded = encode vals
          case decode encoded of
            Left err -> do
              annotate err
              failure
            Right decoded -> decoded === vals
      ]


largeFileTests :: Spec
largeFileTests =
  describe "Large files" $
    sequence_
      [ it "10k lines" $ do
          let mkLine i = BSC.pack $ "{\"id\":" ++ show i ++ "}"
              input = BS.intercalate "\n" [mkLine i | i <- [1 .. 10000 :: Int]]
              Right vals = decode input
          V.length vals `shouldBe` 10000
      ]
