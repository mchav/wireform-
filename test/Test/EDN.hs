module Test.EDN (ednTests) where

import Data.Aeson qualified as Aeson
import Data.Vector qualified as V
import EDN.Decode (decode, decodeBS)
import EDN.Encode (encode, encodeBS)
import EDN.JSON (fromJSON, toJSON)
import EDN.Value qualified as E
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()


ednTests :: Spec
ednTests =
  describe "EDN" $
    sequence_
      [ parseTests
      , encodeTests
      , roundtripTests
      , propertyRoundtrips
      , edgeCases
      , jsonTests
      ]


parseTests :: Spec
parseTests =
  describe "Parse known EDN strings" $
    sequence_
      [ it "nil" $
          decode "nil" `shouldBe` Right E.Nil
      , it "42" $
          decode "42" `shouldBe` Right (E.Integer 42)
      , it "-17" $
          decode "-17" `shouldBe` Right (E.Integer (-17))
      , it "0" $
          decode "0" `shouldBe` Right (E.Integer 0)
      , it "3.14" $
          decode "3.14" `shouldBe` Right (E.Float 3.14)
      , it "1.0e10" $
          decode "1.0e10" `shouldBe` Right (E.Float 1.0e10)
      , it "\"hello\"" $
          decode "\"hello\"" `shouldBe` Right (E.String "hello")
      , it "string with escapes" $
          decode "\"a\\nb\\tc\"" `shouldBe` Right (E.String "a\nb\tc")
      , it ":keyword" $
          decode ":keyword" `shouldBe` Right (E.Keyword Nothing "keyword")
      , it ":ns/key" $
          decode ":ns/key" `shouldBe` Right (E.Keyword (Just "ns") "key")
      , it "\\newline" $
          decode "\\newline" `shouldBe` Right (E.Char '\n')
      , it "\\space" $
          decode "\\space" `shouldBe` Right (E.Char ' ')
      , it "\\tab" $
          decode "\\tab" `shouldBe` Right (E.Char '\t')
      , it "\\return" $
          decode "\\return" `shouldBe` Right (E.Char '\r')
      , it "\\a" $
          decode "\\a" `shouldBe` Right (E.Char 'a')
      , it "(1 2 3)" $
          decode "(1 2 3)" `shouldBe` Right (E.List (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))
      , it "[1 \"two\" :three]" $
          decode "[1 \"two\" :three]"
            `shouldBe` Right
              ( E.Vector
                  ( V.fromList
                      [E.Integer 1, E.String "two", E.Keyword Nothing "three"]
                  )
              )
      , it "{:a 1 :b 2}" $
          decode "{:a 1 :b 2}"
            `shouldBe` Right
              ( E.Map
                  ( V.fromList
                      [(E.Keyword Nothing "a", E.Integer 1), (E.Keyword Nothing "b", E.Integer 2)]
                  )
              )
      , it "#{1 2 3}" $
          decode "#{1 2 3}" `shouldBe` Right (E.Set (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))
      , it "#inst \"1985-04-12T23:20:50.52Z\"" $
          decode "#inst \"1985-04-12T23:20:50.52Z\""
            `shouldBe` Right (E.Tagged "" "inst" (E.String "1985-04-12T23:20:50.52Z"))
      , it "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\"" $
          decode "#uuid \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
            `shouldBe` Right (E.Tagged "" "uuid" (E.String "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"))
      , it "#myapp/Person {:name \"Joe\"}" $
          decode "#myapp/Person {:name \"Joe\"}"
            `shouldBe` Right
              ( E.Tagged
                  "myapp"
                  "Person"
                  ( E.Map
                      ( V.fromList
                          [(E.Keyword Nothing "name", E.String "Joe")]
                      )
                  )
              )
      , it "comments" $
          decode "; this is a comment\n42" `shouldBe` Right (E.Integer 42)
      , it "commas as whitespace" $
          decode "[1, 2, 3]"
            `shouldBe` Right
              ( E.Vector
                  ( V.fromList
                      [E.Integer 1, E.Integer 2, E.Integer 3]
                  )
              )
      , it "nested structure" $
          decode "{:users [{:name \"Alice\"} {:name \"Bob\"}]}"
            `shouldBe` Right
              ( E.Map
                  ( V.fromList
                      [
                        ( E.Keyword Nothing "users"
                        , E.Vector
                            ( V.fromList
                                [ E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Alice")])
                                , E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Bob")])
                                ]
                            )
                        )
                      ]
                  )
              )
      , it "##Inf" $
          case decode "##Inf" of
            Right (E.Float d) -> (isInfinite d && d > 0) `shouldBe` True
            other -> expectationFailure $ "expected Float Inf, got: " ++ show other
      , it "##-Inf" $
          case decode "##-Inf" of
            Right (E.Float d) -> (isInfinite d && d < 0) `shouldBe` True
            other -> expectationFailure $ "expected Float -Inf, got: " ++ show other
      , it "##NaN" $
          case decode "##NaN" of
            Right (E.Float d) -> (isNaN d) `shouldBe` True
            other -> expectationFailure $ "expected Float NaN, got: " ++ show other
      , it "discard: [1 #_ 2 3]" $
          decode "[1 #_ 2 3]" `shouldBe` Right (E.Vector (V.fromList [E.Integer 1, E.Integer 3]))
      , it "symbol" $
          decode "foo" `shouldBe` Right (E.Symbol Nothing "foo")
      , it "namespaced symbol" $
          decode "my-ns/bar" `shouldBe` Right (E.Symbol (Just "my-ns") "bar")
      , it "true" $
          decode "true" `shouldBe` Right (E.Bool True)
      , it "false" $
          decode "false" `shouldBe` Right (E.Bool False)
      , it "empty list" $
          decode "()" `shouldBe` Right (E.List V.empty)
      , it "empty vector" $
          decode "[]" `shouldBe` Right (E.Vector V.empty)
      , it "empty map" $
          decode "{}" `shouldBe` Right (E.Map V.empty)
      , it "empty set" $
          decode "#{}" `shouldBe` Right (E.Set V.empty)
      ]


encodeTests :: Spec
encodeTests =
  describe "Encode" $
    sequence_
      [ it "nil" $
          encode E.Nil `shouldBe` "nil"
      , it "true" $
          encode (E.Bool True) `shouldBe` "true"
      , it "false" $
          encode (E.Bool False) `shouldBe` "false"
      , it "integer" $
          encode (E.Integer 42) `shouldBe` "42"
      , it "negative integer" $
          encode (E.Integer (-17)) `shouldBe` "-17"
      , it "string" $
          encode (E.String "hello") `shouldBe` "\"hello\""
      , it "string with escapes" $
          encode (E.String "a\nb") `shouldBe` "\"a\\nb\""
      , it "keyword" $
          encode (E.Keyword Nothing "foo") `shouldBe` ":foo"
      , it "namespaced keyword" $
          encode (E.Keyword (Just "ns") "key") `shouldBe` ":ns/key"
      , it "char newline" $
          encode (E.Char '\n') `shouldBe` "\\newline"
      , it "char a" $
          encode (E.Char 'a') `shouldBe` "\\a"
      , it "list" $
          encode (E.List (V.fromList [E.Integer 1, E.Integer 2])) `shouldBe` "(1 2)"
      , it "vector" $
          encode (E.Vector (V.fromList [E.Integer 1, E.Integer 2])) `shouldBe` "[1 2]"
      , it "map" $
          encode (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)])) `shouldBe` "{:a 1}"
      , it "set" $
          encode (E.Set (V.fromList [E.Integer 1, E.Integer 2])) `shouldBe` "#{1 2}"
      , it "tagged" $
          encode (E.Tagged "" "inst" (E.String "1985")) `shouldBe` "#inst \"1985\""
      , it "namespaced tagged" $
          encode (E.Tagged "myapp" "Person" (E.String "Joe")) `shouldBe` "#myapp/Person \"Joe\""
      , it "##Inf" $ do
          let t = encode (E.Float (1 / 0))
          t `shouldBe` "##Inf"
      , it "##-Inf" $ do
          let t = encode (E.Float (-1 / 0))
          t `shouldBe` "##-Inf"
      , it "##NaN" $ do
          let t = encode (E.Float (0 / 0))
          t `shouldBe` "##NaN"
      , it "encodeBS produces UTF-8" $ do
          let bs = encodeBS (E.String "hello")
          bs `shouldBe` "\"hello\""
      ]


roundtripTests :: Spec
roundtripTests =
  describe "Roundtrip (encode then decode)" $
    sequence_
      [ it "nil" $ rt E.Nil
      , it "true" $ rt (E.Bool True)
      , it "false" $ rt (E.Bool False)
      , it "integer" $ rt (E.Integer 42)
      , it "negative integer" $ rt (E.Integer (-100))
      , it "zero" $ rt (E.Integer 0)
      , it "float" $ rt (E.Float 3.14)
      , it "string" $ rt (E.String "hello world")
      , it "char" $ rt (E.Char 'x')
      , it "char newline" $ rt (E.Char '\n')
      , it "char space" $ rt (E.Char ' ')
      , it "keyword" $ rt (E.Keyword Nothing "key")
      , it "namespaced keyword" $ rt (E.Keyword (Just "ns") "key")
      , it "symbol" $ rt (E.Symbol Nothing "sym")
      , it "namespaced symbol" $ rt (E.Symbol (Just "ns") "sym")
      , it "empty list" $ rt (E.List V.empty)
      , it "list" $ rt (E.List (V.fromList [E.Integer 1, E.Integer 2, E.Integer 3]))
      , it "empty vector" $ rt (E.Vector V.empty)
      , it "vector" $ rt (E.Vector (V.fromList [E.Integer 1, E.String "two", E.Keyword Nothing "three"]))
      , it "empty map" $ rt (E.Map V.empty)
      , it "map" $ rt (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)]))
      , it "empty set" $ rt (E.Set V.empty)
      , it "set" $ rt (E.Set (V.fromList [E.Integer 1, E.Integer 2]))
      , it "tagged" $ rt (E.Tagged "" "inst" (E.String "1985"))
      , it "namespaced tagged" $ rt (E.Tagged "myapp" "Person" (E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Joe")])))
      , it "nested" $
          rt
            ( E.Map
                ( V.fromList
                    [
                      ( E.Keyword Nothing "users"
                      , E.Vector
                          ( V.fromList
                              [ E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Alice")])
                              , E.Map (V.fromList [(E.Keyword Nothing "name", E.String "Bob")])
                              ]
                          )
                      )
                    ]
                )
            )
      ]
  where
    rt val = decode (encode val) `shouldBe` Right val


propertyRoundtrips :: Spec
propertyRoundtrips =
  describe "Property roundtrips" $
    sequence_
      [ it "Integer roundtrip" $ property $ do
          n <- forAll $ Gen.integral (Range.linear (-10000) 10000)
          let val = E.Integer n
          decode (encode val) === Right val
      , it "String roundtrip" $ property $ do
          t <- forAll $ Gen.text (Range.linear 0 100) Gen.alphaNum
          let val = E.String t
          decode (encode val) === Right val
      , it "Vector of integers roundtrip" $ property $ do
          ns <-
            forAll $
              Gen.list (Range.linear 0 30) $
                Gen.integral (Range.linear (-1000) 1000)
          let val = E.Vector (V.fromList (map E.Integer ns))
          decode (encode val) === Right val
      , it "Keyword roundtrip" $ property $ do
          name <- forAll $ Gen.text (Range.linear 1 20) Gen.alpha
          let val = E.Keyword Nothing name
          decode (encode val) === Right val
      , it "Bool roundtrip" $ property $ do
          b <- forAll Gen.bool
          let val = E.Bool b
          decode (encode val) === Right val
      ]


edgeCases :: Spec
edgeCases =
  describe "Edge cases" $
    sequence_
      [ it "large integer" $ do
          let val = E.Integer 999999999999999999
          decode (encode val) `shouldBe` Right val
      , it "negative large integer" $ do
          let val = E.Integer (-999999999999999999)
          decode (encode val) `shouldBe` Right val
      , it "deeply nested" $ do
          let nest 0 = E.Integer 42
              nest n = E.Vector (V.singleton (nest (n - 1)))
              val = nest (15 :: Int)
          decode (encode val) `shouldBe` Right val
      , it "string with all escape types" $ do
          let val = E.String "tab:\tnewline:\nreturn:\rquote:\"backslash:\\"
          decode (encode val) `shouldBe` Right val
      , it "multiple comments" $
          decode "; comment 1\n; comment 2\n42" `shouldBe` Right (E.Integer 42)
      , it "whitespace variations" $
          decode "  \t\n  42  " `shouldBe` Right (E.Integer 42)
      , it "map with mixed value types" $ do
          let val =
                E.Map
                  ( V.fromList
                      [ (E.Keyword Nothing "int", E.Integer 1)
                      , (E.Keyword Nothing "str", E.String "hello")
                      , (E.Keyword Nothing "bool", E.Bool True)
                      , (E.Keyword Nothing "nil", E.Nil)
                      ]
                  )
          decode (encode val) `shouldBe` Right val
      , it "discard in map" $
          decode "{:a 1 #_ :b #_ 2 :c 3}"
            `shouldBe` Right
              ( E.Map
                  ( V.fromList
                      [(E.Keyword Nothing "a", E.Integer 1), (E.Keyword Nothing "c", E.Integer 3)]
                  )
              )
      , it "decodeBS" $
          decodeBS "42" `shouldBe` Right (E.Integer 42)
      , it "empty input" $
          case decode "" of
            Left _ -> pure ()
            Right _ -> expectationFailure "expected error on empty input"
      ]


jsonTests :: Spec
jsonTests =
  describe "JSON conversion" $
    sequence_
      [ it "Nil to JSON" $
          toJSON E.Nil `shouldBe` Aeson.Null
      , it "Bool to JSON" $ do
          toJSON (E.Bool True) `shouldBe` Aeson.Bool True
          toJSON (E.Bool False) `shouldBe` Aeson.Bool False
      , it "Integer to JSON" $
          toJSON (E.Integer 42) `shouldBe` Aeson.Number 42
      , it "String to JSON" $
          toJSON (E.String "hello") `shouldBe` Aeson.String "hello"
      , it "Char to JSON" $
          toJSON (E.Char 'x') `shouldBe` Aeson.String "x"
      , it "Keyword to JSON" $
          toJSON (E.Keyword Nothing "name") `shouldBe` Aeson.String ":name"
      , it "Symbol to JSON" $
          toJSON (E.Symbol Nothing "foo") `shouldBe` Aeson.String "foo"
      , it "Vector to JSON" $
          toJSON (E.Vector (V.fromList [E.Integer 1, E.Integer 2]))
            `shouldBe` Aeson.Array (V.fromList [Aeson.Number 1, Aeson.Number 2])
      , it "Set to JSON" $
          toJSON (E.Set (V.fromList [E.Integer 1]))
            `shouldBe` Aeson.Array (V.fromList [Aeson.Number 1])
      , it "Tagged to JSON" $ do
          let json = toJSON (E.Tagged "" "inst" (E.String "1985"))
          case json of
            Aeson.Object _ -> pure ()
            _ -> expectationFailure "expected JSON object for tagged"
      , it "Map with keyword keys to JSON object" $ do
          let json = toJSON (E.Map (V.fromList [(E.Keyword Nothing "a", E.Integer 1)]))
          case json of
            Aeson.Object _ -> pure ()
            _ -> expectationFailure "expected JSON object for keyword-keyed map"
      , it "fromJSON null" $
          fromJSON Aeson.Null `shouldBe` E.Nil
      , it "fromJSON bool" $
          fromJSON (Aeson.Bool True) `shouldBe` E.Bool True
      , it "fromJSON string" $
          fromJSON (Aeson.String "hi") `shouldBe` E.String "hi"
      , it "fromJSON integer" $
          fromJSON (Aeson.Number 42) `shouldBe` E.Integer 42
      , it "fromJSON array" $
          fromJSON (Aeson.Array (V.fromList [Aeson.Number 1]))
            `shouldBe` E.Vector (V.fromList [E.Integer 1])
      ]
