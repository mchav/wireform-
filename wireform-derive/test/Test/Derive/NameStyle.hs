{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.NameStyle (tests) where

import qualified Data.Char as Char
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Wireform.Derive.NameStyle

tests :: TestTree
tests = testGroup "NameStyle"
  [ testGroup "applyStyle / golden"
      [ testCase "snake personName"
          (applyStyle SnakeCase "personName"  @?= "person_name")
      , testCase "snake HTTPRequest"
          (applyStyle SnakeCase "HTTPRequest" @?= "http_request")
      , testCase "snake httpRequest"
          (applyStyle SnakeCase "httpRequest" @?= "http_request")
      , testCase "snake-of-snake personName"
          (applyStyle SnakeCase "person_name" @?= "person_name")
      , testCase "kebab personName"
          (applyStyle KebabCase "personName"  @?= "person-name")
      , testCase "kebab HTTPRequest"
          (applyStyle KebabCase "HTTPRequest" @?= "http-request")
      , testCase "camel of snake"
          (applyStyle CamelCase  "person_name" @?= "personName")
      , testCase "camel idempotent"
          (applyStyle CamelCase  "personName"  @?= "personName")
      , testCase "pascal of snake"
          (applyStyle PascalCase "person_name" @?= "PersonName")
      , testCase "pascal idempotent"
          (applyStyle PascalCase "PersonName"  @?= "PersonName")
      , testCase "upper snake"
          (applyStyle UpperSnake "personName"  @?= "PERSON_NAME")
      , testCase "upper kebab"
          (applyStyle UpperKebab "personName"  @?= "PERSON-NAME")
      , testCase "strip prefix present"
          (applyStyle (StripPrefix "person") "personName" @?= "Name")
      , testCase "strip prefix absent"
          (applyStyle (StripPrefix "other") "personName" @?= "personName")
      , testCase "strip prefix CI"
          (applyStyle (StripPrefixCI "PERSON") "personName" @?= "Name")
      , testCase "strip suffix present"
          (applyStyle (StripSuffix "Name") "personName" @?= "person")
      , testCase "compose strip + snake"
          (applyStyle
              (StripPrefix "person" `andThen` SnakeCase)
              "personHttpRequest"
              @?= "http_request")
      , testCase "replace"
          (applyStyle (Replace "Name" "Label") "personName" @?= "personLabel")
      , testCase "drop / take"
          (applyStyle (DropChars 6 `andThen` TakeChars 2) "personName" @?= "Na")
      , testCase "Idiomatic falls through to NoStyle without resolution"
          (applyStyle Idiomatic "personName" @?= "personName")
      ]

  , testGroup "Idiomatic resolution"
      [ testCase "JSON resolves to CamelCase"
          (applyStyle (resolveIdiomatic "json" Idiomatic) "person_name"
             @?= "personName")
      , testCase "EDN resolves to KebabCase"
          (applyStyle (resolveIdiomatic "edn" Idiomatic) "personName"
             @?= "person-name")
      , testCase "Proto resolves to CamelCase"
          (applyStyle (resolveIdiomatic "proto" Idiomatic) "person_name"
             @?= "personName")
      , testCase "TOML resolves to SnakeCase"
          (applyStyle (resolveIdiomatic "toml" Idiomatic) "personName"
             @?= "person_name")
      , testCase "YAML resolves to KebabCase"
          (applyStyle (resolveIdiomatic "yaml" Idiomatic) "personName"
             @?= "person-name")
      , testCase "XML resolves to PascalCase"
          (applyStyle (resolveIdiomatic "xml" Idiomatic) "person_name"
             @?= "PersonName")
      , testCase "CBOR resolves to NoStyle (verbatim)"
          (applyStyle (resolveIdiomatic "cbor" Idiomatic) "personName"
             @?= "personName")
      , testCase "compose preserves outer style"
          (applyStyle (resolveIdiomatic "edn"
              (StripPrefix "person" `andThen` Idiomatic))
              "personHttpRequest"
              @?= "http-request")
      ]

  , testGroup "properties"
      [ testProperty "NoStyle is identity" $ property $ do
          t <- forAll genIdent
          applyStyle NoStyle t === t

      , testProperty "Compose NoStyle a == a" $ property $ do
          t <- forAll genIdent
          applyStyle (Compose NoStyle SnakeCase) t === applyStyle SnakeCase t

      , testProperty "snake then camel preserves alpha" $ property $ do
          t <- forAll genCamelIdent
          let snaked = applyStyle SnakeCase t
              roundTripped = applyStyle CamelCase snaked
          T.toLower roundTripped === T.toLower t

      , testProperty "snake produces lowercase output" $ property $ do
          t <- forAll genIdent
          let s = applyStyle SnakeCase t
          assert (T.all (\c -> not (Char.isUpper c)) s)

      , testProperty "kebab produces lowercase output" $ property $ do
          t <- forAll genIdent
          let s = applyStyle KebabCase t
          assert (T.all (\c -> not (Char.isUpper c)) s)

      , testProperty "applyStyle is total (never bottom)" $ property $ do
          t <- forAll genIdent
          let len = T.length (applyStyle SnakeCase t)
          assert (len >= 0)
      ]
  ]

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- Identifier-like text: letters, digits, underscores; non-empty.
genIdent :: MonadGen m => m T.Text
genIdent = T.pack <$> Gen.list (Range.linear 1 20)
  (Gen.frequency
    [ (5, Gen.alpha)
    , (1, Gen.element ['_'])
    , (1, Gen.digit)
    ])

-- camelCase identifier: starts lower, no underscores, alphanumeric only.
genCamelIdent :: MonadGen m => m T.Text
genCamelIdent = do
  first <- Gen.lower
  rest  <- Gen.list (Range.linear 0 12) Gen.alphaNum
  pure (T.pack (first : rest))
