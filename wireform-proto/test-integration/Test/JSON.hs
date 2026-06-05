module Test.JSON (jsonTests) where

import qualified Data.Bifunctor
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as AesonKM
import Data.Aeson.Types qualified as AesonT
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Short qualified as SBS
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Proto.Internal.JSON
import Test.Syd
import Test.Syd.Hedgehog ()


jsonTests :: Spec
jsonTests =
  describe
    "JSON representation helpers" $ sequence_
    [ describe
        "Strict ByteString (base64)" $ sequence_
        [ it "roundtrip via Value" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let val = protoBytesToJSON bs
            parsed <- evalEither (AesonT.parseEither protoBytesFromJSON val)
            parsed === bs
        , it "encodes as base64 string" $ do
            let val = protoBytesToJSON (BS.pack [0x00, 0xFF, 0x42])
            case val of
              Aeson.String _ -> pure ()
              other -> expectationFailure ("Expected String, got " <> show other)
        , it "field helper roundtrip" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let (_, val) = bytesFieldToJSON "data" bs
                obj = mkObj [("data", val)]
            result <- evalEither (AesonT.parseEither (parseBytesFieldMaybe obj) "data")
            result === Just bs
        ]
    , describe
        "Lazy ByteString (base64)" $ sequence_
        [ it "roundtrip via Value" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let lbs = BL.fromStrict bs
                val = protoLazyBytesToJSON lbs
            parsed <- evalEither (AesonT.parseEither protoLazyBytesFromJSON val)
            parsed === lbs
        , it "field helper roundtrip" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let lbs = BL.fromStrict bs
                (_, val) = lazyBytesFieldToJSON "blob" lbs
                obj = mkObj [("blob", val)]
            result <- evalEither (AesonT.parseEither (parseLazyBytesFieldMaybe obj) "blob")
            result === Just lbs
        ]
    , describe
        "ShortByteString (base64)" $ sequence_
        [ it "roundtrip via Value" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let sbs = SBS.toShort bs
                val = protoShortBytesToJSON sbs
            parsed <- evalEither (AesonT.parseEither protoShortBytesFromJSON val)
            parsed === sbs
        , it "field helper roundtrip" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let sbs = SBS.toShort bs
                (_, val) = shortBytesFieldToJSON "compact" sbs
                obj = mkObj [("compact", val)]
            result <- evalEither (AesonT.parseEither (parseShortBytesFieldMaybe obj) "compact")
            result === Just sbs
        ]
    , describe
        "Strict Text" $ sequence_
        [ it ".=: / parseFieldMaybe roundtrip" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
            let (_, val) = "name" .=: t
                obj = mkObj [("name", val)]
            result <- evalEither (AesonT.parseEither (parseFieldMaybe obj) "name")
            result === Just t
        ]
    , describe
        "Lazy Text" $ sequence_
        [ it "field helper roundtrip" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
            let lt = TL.fromStrict t
                (_, val) = lazyTextFieldToJSON "desc" lt
                obj = mkObj [("desc", val)]
            result <- evalEither (AesonT.parseEither (parseLazyTextFieldMaybe obj) "desc")
            result === Just lt
        ]
    , describe
        "ShortByteString as text (UTF-8)" $ sequence_
        [ it "field helper roundtrip" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
            let sbs = SBS.toShort (TE.encodeUtf8 t)
                (_, val) = shortTextFieldToJSON "tag" sbs
                obj = mkObj [("tag", val)]
            result <- evalEither (AesonT.parseEither (parseShortTextFieldMaybe obj) "tag")
            result === Just sbs
        ]
    , describe
        "Haskell String" $ sequence_
        [ it "field helper roundtrip" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
            let s = T.unpack t
                (_, val) = hsStringFieldToJSON "label" s
                obj = mkObj [("label", val)]
            result <- evalEither (AesonT.parseEither (parseHsStringFieldMaybe obj) "label")
            result === Just s
        ]
    , describe
        "Missing / null field handling" $ sequence_
        [ it "parseBytesFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseBytesFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseBytesFieldMaybe null -> Nothing" $ do
            let obj = mkObj [("x", Aeson.Null)]
            runParserOK (parseBytesFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseLazyBytesFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseLazyBytesFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseShortBytesFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseShortBytesFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseLazyTextFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseLazyTextFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseShortTextFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseShortTextFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        , it "parseHsStringFieldMaybe missing -> Nothing" $ do
            let obj = mkObj []
            runParserOK (parseHsStringFieldMaybe obj "x") >>= (`shouldBe` Nothing)
        ]
    , describe
        "Type mismatch errors" $ sequence_
        [ it "parseBytesFieldMaybe non-string -> fail" $
            assertParserFails (parseBytesFieldMaybe (mkObj [("x", Aeson.Number 42)]) "x")
        , it "parseLazyTextFieldMaybe non-string -> fail" $
            assertParserFails (parseLazyTextFieldMaybe (mkObj [("x", Aeson.Bool True)]) "x")
        , it "parseShortTextFieldMaybe non-string -> fail" $
            assertParserFails (parseShortTextFieldMaybe (mkObj [("x", Aeson.Number 1)]) "x")
        , it "parseHsStringFieldMaybe non-string -> fail" $
            assertParserFails (parseHsStringFieldMaybe (mkObj [("x", Aeson.Array mempty)]) "x")
        ]
    , describe
        "Cross-representation consistency" $ sequence_
        [ it "all bytes reps produce same base64" $ property $ do
            bs <- forAll $ Gen.bytes (Range.linear 0 200)
            let strict = protoBytesToJSON bs
                lazy = protoLazyBytesToJSON (BL.fromStrict bs)
                short = protoShortBytesToJSON (SBS.toShort bs)
            strict === lazy
            strict === short
        , it "all text reps produce same JSON string" $ property $ do
            t <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
            let (_, strictVal) = "k" .=: t
                (_, lazyVal) = lazyTextFieldToJSON "k" (TL.fromStrict t)
                (_, shortVal) = shortTextFieldToJSON "k" (SBS.toShort (TE.encodeUtf8 t))
                (_, stringVal) = hsStringFieldToJSON "k" (T.unpack t)
            strictVal === lazyVal
            strictVal === shortVal
            strictVal === stringVal
        ]
    , describe
        "Map representations" $ sequence_
        [ describe
            "Ordered Map (Map.Map)" $ sequence_
            [ it "ordMapToJSON roundtrip" $ property $ do
                keys <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.text (Range.linear 1 20) Gen.alphaNum)
                vals <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.int32 (Range.linear (-1000) 1000))
                let m = Map.fromList (zip keys vals)
                    encoded = ordMapToJSON m
                parsed <- evalEither (AesonT.parseEither parseOrdMapFromJSON encoded)
                parsed === m
            , it "ordMapToJSON empty" $ do
                let m = Map.empty :: Map.Map Text Int
                    encoded = ordMapToJSON m
                encoded `shouldBe` Aeson.object []
            , it "ordMapToJSON preserves entries" $ do
                let m = Map.fromList [("a" :: Text, 1 :: Int), ("b", 2)]
                    Aeson.Object o = ordMapToJSON m
                AesonT.parseEither (parseOrdMapFromJSON . Aeson.Object) o `shouldBe` Right m
            ]
        , describe
            "HashMap" $ sequence_
            [ it "hashMapToJSON roundtrip" $ property $ do
                keys <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.text (Range.linear 1 20) Gen.alphaNum)
                vals <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.int32 (Range.linear (-1000) 1000))
                let m = HM.fromList (zip keys vals)
                    encoded = hashMapToJSON m
                parsed <- evalEither (AesonT.parseEither parseHashMapFromJSON encoded)
                parsed === m
            , it "hashMapToJSON empty" $ do
                let m = HM.empty :: HM.HashMap Text Int
                    encoded = hashMapToJSON m
                encoded `shouldBe` Aeson.object []
            , it "hashMapToJSON preserves entries" $ do
                let m = HM.fromList [("x" :: Text, True), ("y", False)]
                    Aeson.Object o = hashMapToJSON m
                AesonT.parseEither (parseHashMapFromJSON . Aeson.Object) o `shouldBe` Right m
            ]
        , describe
            "Cross-representation consistency" $ sequence_
            [ it "ordMap and hashMap produce same JSON" $ property $ do
                keys <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.text (Range.linear 1 20) Gen.alphaNum)
                vals <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.int32 (Range.linear (-1000) 1000))
                let ordM = Map.fromList (zip keys vals)
                    hashM = HM.fromList (zip keys vals)
                    ordJSON = ordMapToJSON ordM
                    hashJSON = hashMapToJSON hashM
                normalizeObject ordJSON === normalizeObject hashJSON
            , it "ordMap JSON parses as hashMap and vice versa" $ property $ do
                keys <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.text (Range.linear 1 20) Gen.alphaNum)
                vals <-
                  forAll $
                    Gen.list
                      (Range.linear 0 10)
                      (Gen.int32 (Range.linear (-1000) 1000))
                let ordM = Map.fromList (zip keys vals)
                    encoded = ordMapToJSON ordM
                parsedAsHash <- evalEither (AesonT.parseEither parseHashMapFromJSON encoded)
                Map.fromList (HM.toList parsedAsHash) === ordM
            ]
        , it "parseOrdMapFromJSON non-object -> fail" $
            assertParserFails (parseOrdMapFromJSON (Aeson.String "nope") :: AesonT.Parser (Map.Map Text Int))
        , it "parseHashMapFromJSON non-object -> fail" $
            assertParserFails (parseHashMapFromJSON (Aeson.Number 42) :: AesonT.Parser (HM.HashMap Text Int))
        ]
    ]


mkObj :: [(Text, Aeson.Value)] -> Aeson.Object
mkObj kvs = case Aeson.object (fmap (\(k, v) -> AesonKey.fromText k Aeson..= v) kvs) of
  Aeson.Object o -> o
  _ -> error "impossible"


runParserOK :: (Show a, Eq a) => AesonT.Parser a -> IO a
runParserOK p = case AesonT.parseEither (const p) () of
  Right a -> pure a
  Left e -> expectationFailure ("Parser failed: " <> e) >> error "unreachable"


assertParserFails :: AesonT.Parser a -> IO ()
assertParserFails p = case AesonT.parseEither (const p) () of
  Left _ -> pure ()
  Right _ -> expectationFailure "Expected parser to fail"


normalizeObject :: Aeson.Value -> Map.Map Text Aeson.Value
normalizeObject (Aeson.Object o) =
  Map.fromList (fmap (Data.Bifunctor.first AesonKey.toText) (AesonKM.toList o))
normalizeObject _ = Map.empty
