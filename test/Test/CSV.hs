module Test.CSV (csvTests) where

import CSV.Class
import CSV.Decode
import CSV.Encode
import CSV.Value
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


csvTests :: Spec
csvTests =
  describe "CSV" $
    sequence_
      [ parseTests
      , quoteTests
      , tsvTests
      , emptyFieldTests
      , roundtripTests
      , streamTests
      , genericTests
      ]


parseTests :: Spec
parseTests =
  describe "Basic parsing" $
    sequence_
      [ it "Simple CSV with header" $ do
          let input = "name,age\nAlice,30\nBob,25"
              Right doc = decode defaultCSV (BSC.pack input)
          csvHeader doc `shouldBe` Just (V.fromList ["name", "age"])
          csvRows doc
            `shouldBe` V.fromList
              [ V.fromList ["Alice", "30"]
              , V.fromList ["Bob", "25"]
              ]
      , it "CSV without header" $ do
          let cfg = defaultCSV {csvHasHeader = False}
              input = "Alice,30\nBob,25"
              Right doc = decode cfg (BSC.pack input)
          csvHeader doc `shouldBe` Nothing
          csvRows doc
            `shouldBe` V.fromList
              [ V.fromList ["Alice", "30"]
              , V.fromList ["Bob", "25"]
              ]
      , it "Single row" $ do
          let input = "a,b,c"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          csvRows doc `shouldBe` V.fromList [V.fromList ["a", "b", "c"]]
      , it "CRLF line endings" $ do
          let input = "x,y\r\n1,2\r\n3,4"
              Right doc = decode defaultCSV (BSC.pack input)
          csvHeader doc `shouldBe` Just (V.fromList ["x", "y"])
          csvRows doc
            `shouldBe` V.fromList
              [ V.fromList ["1", "2"]
              , V.fromList ["3", "4"]
              ]
      ]


quoteTests :: Spec
quoteTests =
  describe "Quoted fields" $
    sequence_
      [ it "Field with embedded comma" $ do
          let input = "a,\"hello, world\",c"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          csvRows doc `shouldBe` V.fromList [V.fromList ["a", "hello, world", "c"]]
      , it "Escaped quotes (doubled)" $ do
          let input = "\"she said \"\"hi\"\"\""
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          V.head (csvRows doc) `shouldBe` V.fromList ["she said \"hi\""]
      , it "Quoted field with newline" $ do
          let input = "\"line1\nline2\",b"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          V.head (csvRows doc) `shouldBe` V.fromList ["line1\nline2", "b"]
      , it "Empty quoted field" $ do
          let input = "\"\",b"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          V.head (csvRows doc) `shouldBe` V.fromList ["", "b"]
      ]


tsvTests :: Spec
tsvTests =
  describe "TSV" $
    sequence_
      [ it "Tab-separated values" $ do
          let input = "name\tage\nAlice\t30\nBob\t25"
              Right doc = decode defaultTSV (BSC.pack input)
          csvHeader doc `shouldBe` Just (V.fromList ["name", "age"])
          csvRows doc
            `shouldBe` V.fromList
              [ V.fromList ["Alice", "30"]
              , V.fromList ["Bob", "25"]
              ]
      , it "TSV with quoted tab" $ do
          let input = "\"a\tb\"\tc"
              cfg = defaultTSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          V.head (csvRows doc) `shouldBe` V.fromList ["a\tb", "c"]
      ]


emptyFieldTests :: Spec
emptyFieldTests =
  describe "Empty fields" $
    sequence_
      [ it "Empty fields at various positions" $ do
          let input = ",b,\na,,c"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          csvRows doc
            `shouldBe` V.fromList
              [ V.fromList ["", "b", ""]
              , V.fromList ["a", "", "c"]
              ]
      , it "All empty fields" $ do
          let input = ",,"
              cfg = defaultCSV {csvHasHeader = False}
              Right doc = decode cfg (BSC.pack input)
          csvRows doc `shouldBe` V.fromList [V.fromList ["", "", ""]]
      ]


roundtripTests :: Spec
roundtripTests =
  describe "Roundtrip" $
    sequence_
      [ it "CSV encode-decode roundtrip" $ property $ do
          nRows <- forAll $ Gen.int (Range.linear 1 20)
          nCols <- forAll $ Gen.int (Range.linear 1 5)
          rows <-
            forAll $
              V.replicateM nRows $
                V.replicateM nCols (Gen.text (Range.linear 0 20) Gen.alphaNum)
          let cfg = defaultCSV {csvHasHeader = False}
              doc = CSVDocument Nothing rows
              encoded = encode cfg doc
          case decode cfg encoded of
            Left err -> do
              annotate err
              failure
            Right doc' -> csvRows doc' === csvRows doc
      , it "CSV with header roundtrip" $ property $ do
          nCols <- forAll $ Gen.int (Range.linear 1 5)
          header <- forAll $ V.replicateM nCols (Gen.text (Range.linear 1 10) Gen.alpha)
          nRows <- forAll $ Gen.int (Range.linear 0 10)
          rows <-
            forAll $
              V.replicateM nRows $
                V.replicateM nCols (Gen.text (Range.linear 0 15) Gen.alphaNum)
          let doc = CSVDocument (Just header) rows
              encoded = encode defaultCSV doc
          case decode defaultCSV encoded of
            Left err -> do
              annotate err
              failure
            Right doc' -> do
              csvHeader doc' === csvHeader doc
              csvRows doc' === csvRows doc
      ]


streamTests :: Spec
streamTests =
  describe "Streaming" $
    sequence_
      [ it "Streaming decode collects all rows" $ do
          let input = "h1,h2\na,1\nb,2\nc,3"
              ref =
                V.fromList
                  [ V.fromList ["a", "1"]
                  , V.fromList ["b", "2"]
                  , V.fromList ["c", "3"]
                  ]
          collected <- newIORef []
          result <- decodeStream defaultCSV (BSC.pack input) $ \row ->
            modifyIORef' collected (row :)
          result `shouldBe` Right ()
          rows <- reverse <$> readIORef collected
          V.fromList rows `shouldBe` ref
      ]


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  }
  deriving stock (Show, Eq, Generic)


instance ToCSV Person where
  toCSVRow p = V.fromList [personName p, T.pack (show (personAge p))]


instance FromCSV Person where
  fromCSVRow v
    | V.length v < 2 = Left "not enough fields for Person"
    | otherwise = do
        let name = v V.! 0
        age <- case reads (T.unpack (v V.! 1)) of
          [(n, "")] -> Right n
          _ -> Left "cannot parse age"
        Right (Person name age)


genericTests :: Spec
genericTests =
  describe "Generic deriving" $
    sequence_
      [ it "Person record from CSV" $ do
          let input = "name,age\nAlice,30\nBob,25"
              Right persons = decodeRecords defaultCSV (BSC.pack input) :: Either String (V.Vector Person)
          persons `shouldBe` V.fromList [Person "Alice" 30, Person "Bob" 25]
      , it "Person record roundtrip" $ do
          let cfg = defaultCSV {csvHasHeader = False}
              persons = V.fromList [Person "Alice" 30, Person "Bob" 25]
              encoded = encodeRecords cfg persons
              Right decoded = decodeRecords cfg encoded :: Either String (V.Vector Person)
          decoded `shouldBe` persons
      ]
