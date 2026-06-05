{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.TOML (tomlTests) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Test.Syd

import TOML.Value
import TOML.Encode
import TOML.Decode
import TOML.Class

tomlTests :: Spec
tomlTests = describe "TOML" $ sequence_
  [ basicTypeTests
  , tableTests
  , arrayTests
  , stringTests
  , integerTests
  , classTests
  , genericTests
  , roundtripTests
  ]

basicTypeTests :: Spec
basicTypeTests = describe "Basic types" $ sequence_
  [ it "parse string" $ do
      let Right val = decode "key = \"hello\""
      lookupKey "key" val `shouldBe` Just (TString "hello")

  , it "parse integer" $ do
      let Right val = decode "key = 42"
      lookupKey "key" val `shouldBe` Just (TInteger 42)

  , it "parse negative integer" $ do
      let Right val = decode "key = -17"
      lookupKey "key" val `shouldBe` Just (TInteger (-17))

  , it "parse float" $ do
      let Right val = decode "key = 3.14"
      case lookupKey "key" val of
        Just (TFloat d) -> (abs (d - 3.14) < 0.001) `shouldBe` True
        _ -> expectationFailure "expected TFloat"

  , it "parse bool true" $ do
      let Right val = decode "key = true"
      lookupKey "key" val `shouldBe` Just (TBool True)

  , it "parse bool false" $ do
      let Right val = decode "key = false"
      lookupKey "key" val `shouldBe` Just (TBool False)

  , it "parse inf" $ do
      let Right val = decode "key = inf"
      case lookupKey "key" val of
        Just (TFloat d) -> (isInfinite d && d > 0) `shouldBe` True
        _ -> expectationFailure "expected TFloat inf"

  , it "parse -inf" $ do
      let Right val = decode "key = -inf"
      case lookupKey "key" val of
        Just (TFloat d) -> (isInfinite d && d < 0) `shouldBe` True
        _ -> expectationFailure "expected TFloat -inf"

  , it "parse nan" $ do
      let Right val = decode "key = nan"
      case lookupKey "key" val of
        Just (TFloat d) -> (isNaN d) `shouldBe` True
        _ -> expectationFailure "expected TFloat nan"

  , it "parse datetime" $ do
      let Right val = decode "key = 2024-01-15T10:30:00Z"
      case lookupKey "key" val of
        Just (TDateTime _) -> pure () :: IO ()
        _ -> expectationFailure "expected TDateTime"

  , it "parse date" $ do
      let Right val = decode "key = 2024-01-15"
      case lookupKey "key" val of
        Just (TDate _) -> pure () :: IO ()
        _ -> expectationFailure "expected TDate"

  , it "comment handling" $ do
      let Right val = decode "key = 42 # this is a comment"
      lookupKey "key" val `shouldBe` Just (TInteger 42)

  , it "empty lines and comments" $ do
      let input = T.unlines
            [ "# Comment"
            , ""
            , "key1 = 1"
            , "# Another comment"
            , "key2 = 2"
            ]
      let Right val = decode input
      lookupKey "key1" val `shouldBe` Just (TInteger 1)
      lookupKey "key2" val `shouldBe` Just (TInteger 2)
  ]

tableTests :: Spec
tableTests = describe "Tables" $ sequence_
  [ it "simple table" $ do
      let input = T.unlines
            [ "[server]"
            , "host = \"localhost\""
            , "port = 8080"
            ]
      let Right val = decode input
      case lookupKey "server" val of
        Just (TTable kvs) -> do
          lookupInTable "host" kvs `shouldBe` Just (TString "localhost")
          lookupInTable "port" kvs `shouldBe` Just (TInteger 8080)
        _ -> expectationFailure "expected TTable"

  , it "nested tables" $ do
      let input = T.unlines
            [ "[database.connection]"
            , "host = \"db.example.com\""
            , "port = 5432"
            ]
      let Right val = decode input
      case lookupKey "database" val of
        Just (TTable outer) ->
          case lookupInTable "connection" outer of
            Just (TTable inner) ->
              lookupInTable "host" inner `shouldBe` Just (TString "db.example.com")
            _ -> expectationFailure "expected nested TTable"
        _ -> expectationFailure "expected TTable"

  , it "inline table" $ do
      let Right val = decode "point = {x = 1, y = 2}"
      case lookupKey "point" val of
        Just (TTable kvs) -> do
          lookupInTable "x" kvs `shouldBe` Just (TInteger 1)
          lookupInTable "y" kvs `shouldBe` Just (TInteger 2)
        _ -> expectationFailure "expected TTable"
  ]

arrayTests :: Spec
arrayTests = describe "Arrays" $ sequence_
  [ it "simple array" $ do
      let Right val = decode "arr = [1, 2, 3]"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.toList vs `shouldBe` [TInteger 1, TInteger 2, TInteger 3]
        _ -> expectationFailure "expected TArray"

  , it "empty array" $ do
      let Right val = decode "arr = []"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.null vs `shouldBe` True
        _ -> expectationFailure "expected TArray"

  , it "array of strings" $ do
      let Right val = decode "arr = [\"a\", \"b\", \"c\"]"
      case lookupKey "arr" val of
        Just (TArray vs) -> V.toList vs `shouldBe` [TString "a", TString "b", TString "c"]
        _ -> expectationFailure "expected TArray"

  , it "array of tables" $ do
      let input = T.unlines
            [ "[[products]]"
            , "name = \"Hammer\""
            , ""
            , "[[products]]"
            , "name = \"Nail\""
            ]
      let Right val = decode input
      case lookupKey "products" val of
        Just (TTable _) -> pure () :: IO ()
        _ -> pure () :: IO ()
  ]

stringTests :: Spec
stringTests = describe "Strings" $ sequence_
  [ it "basic string" $ do
      let Right val = decode "s = \"hello world\""
      lookupKey "s" val `shouldBe` Just (TString "hello world")

  , it "literal string" $ do
      let Right val = decode "s = 'hello world'"
      lookupKey "s" val `shouldBe` Just (TString "hello world")

  , it "escape sequences in basic string" $ do
      let Right val = decode "s = \"hello\\nworld\""
      lookupKey "s" val `shouldBe` Just (TString "hello\nworld")

  , it "literal string preserves backslash" $ do
      let Right val = decode "s = 'hello\\nworld'"
      lookupKey "s" val `shouldBe` Just (TString "hello\\nworld")
  ]

integerTests :: Spec
integerTests = describe "Integer formats" $ sequence_
  [ it "hex integer" $ do
      let Right val = decode "n = 0xFF"
      lookupKey "n" val `shouldBe` Just (TInteger 255)

  , it "octal integer" $ do
      let Right val = decode "n = 0o77"
      lookupKey "n" val `shouldBe` Just (TInteger 63)

  , it "binary integer" $ do
      let Right val = decode "n = 0b1010"
      lookupKey "n" val `shouldBe` Just (TInteger 10)

  , it "integer with underscores" $ do
      let Right val = decode "n = 1_000_000"
      lookupKey "n" val `shouldBe` Just (TInteger 1000000)

  , it "positive integer with +" $ do
      let Right val = decode "n = +42"
      lookupKey "n" val `shouldBe` Just (TInteger 42)
  ]

classTests :: Spec
classTests = describe "Class instances" $ sequence_
  [ it "Text roundtrip" $ do
      let val = "hello" :: Text
      fromTOML (toTOML val) `shouldBe` Right val

  , it "Int roundtrip" $ do
      let val = 42 :: Int
      fromTOML (toTOML val) `shouldBe` Right val

  , it "Bool roundtrip" $ do
      fromTOML (toTOML True) `shouldBe` Right True

  , it "Double roundtrip" $ do
      let val = 3.14 :: Double
      fromTOML (toTOML val) `shouldBe` Right val

  , it "List roundtrip" $ do
      let val = [1, 2, 3] :: [Int]
      fromTOML (toTOML val) `shouldBe` Right val
  ]

data Config = Config
  { title :: !Text
  , port :: !Int
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToTOML, FromTOML)

genericTests :: Spec
genericTests = describe "Generic deriving" $ sequence_
  [ it "record to TOML" $ do
      let cfg = Config "My App" 8080
          val = toTOML cfg
      case val of
        TTable kvs -> do
          lookupInTable "title" kvs `shouldBe` Just (TString "My App")
          lookupInTable "port" kvs `shouldBe` Just (TInteger 8080)
        _ -> expectationFailure "expected TTable"

  , it "record roundtrip" $ do
      let cfg = Config "Test" 3000
      fromTOML (toTOML cfg) `shouldBe` Right cfg
  ]

roundtripTests :: Spec
roundtripTests = describe "Roundtrip" $ sequence_
  [ it "encode then decode preserves values" $ do
      let val = TTable (V.fromList
            [ ("name", TString "test")
            , ("count", TInteger 42)
            , ("enabled", TBool True)
            ])
      let encoded = TOML.Encode.encode val
      case decode encoded of
        Right val2 -> do
          lookupKey "name" val2 `shouldBe` Just (TString "test")
          lookupKey "count" val2 `shouldBe` Just (TInteger 42)
          lookupKey "enabled" val2 `shouldBe` Just (TBool True)
        Left err -> expectationFailure $ "decode failed: " ++ err

  , it "array encode then decode" $ do
      let val = TTable (V.fromList
            [ ("items", TArray (V.fromList [TInteger 1, TInteger 2, TInteger 3]))
            ])
      let encoded = TOML.Encode.encode val
      case decode encoded of
        Right val2 ->
          case lookupKey "items" val2 of
            Just (TArray vs) -> V.toList vs `shouldBe` [TInteger 1, TInteger 2, TInteger 3]
            _ -> expectationFailure "expected TArray"
        Left err -> expectationFailure $ "decode failed: " ++ err
  ]

-- Helpers

lookupKey :: Text -> Value -> Maybe Value
lookupKey key (TTable kvs) = lookupInTable key kvs
lookupKey _ _ = Nothing

lookupInTable :: Text -> V.Vector (Text, Value) -> Maybe Value
lookupInTable key kvs = go 0
  where
    len = V.length kvs
    go i
      | i >= len = Nothing
      | (k, v) <- kvs V.! i, k == key = Just v
      | otherwise = go (i + 1)
