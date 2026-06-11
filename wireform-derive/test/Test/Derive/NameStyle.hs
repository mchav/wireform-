{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.NameStyle (tests) where

import Data.Char qualified as Char
import Data.Text qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Syd
import Test.Syd.Hedgehog ()
import Wireform.Derive.NameStyle


tests :: Spec
tests =
  describe "NameStyle" $
    sequence_
      [ describe "applyStyle / golden" $
          sequence_
            [ it
                "snake personName"
                (applyStyle SnakeCase "personName" `shouldBe` "person_name")
            , it
                "snake HTTPRequest"
                (applyStyle SnakeCase "HTTPRequest" `shouldBe` "http_request")
            , it
                "snake httpRequest"
                (applyStyle SnakeCase "httpRequest" `shouldBe` "http_request")
            , it
                "snake-of-snake personName"
                (applyStyle SnakeCase "person_name" `shouldBe` "person_name")
            , it
                "kebab personName"
                (applyStyle KebabCase "personName" `shouldBe` "person-name")
            , it
                "kebab HTTPRequest"
                (applyStyle KebabCase "HTTPRequest" `shouldBe` "http-request")
            , it
                "camel of snake"
                (applyStyle CamelCase "person_name" `shouldBe` "personName")
            , it
                "camel idempotent"
                (applyStyle CamelCase "personName" `shouldBe` "personName")
            , it
                "pascal of snake"
                (applyStyle PascalCase "person_name" `shouldBe` "PersonName")
            , it
                "pascal idempotent"
                (applyStyle PascalCase "PersonName" `shouldBe` "PersonName")
            , it
                "upper snake"
                (applyStyle UpperSnake "personName" `shouldBe` "PERSON_NAME")
            , it
                "upper kebab"
                (applyStyle UpperKebab "personName" `shouldBe` "PERSON-NAME")
            , it
                "strip prefix present"
                (applyStyle (StripPrefix "person") "personName" `shouldBe` "Name")
            , it
                "strip prefix absent"
                (applyStyle (StripPrefix "other") "personName" `shouldBe` "personName")
            , it
                "strip prefix CI"
                (applyStyle (StripPrefixCI "PERSON") "personName" `shouldBe` "Name")
            , it
                "strip suffix present"
                (applyStyle (StripSuffix "Name") "personName" `shouldBe` "person")
            , it
                "compose strip + snake"
                ( applyStyle
                    (StripPrefix "person" `andThen` SnakeCase)
                    "personHttpRequest"
                    `shouldBe` "http_request"
                )
            , it
                "replace"
                (applyStyle (Replace "Name" "Label") "personName" `shouldBe` "personLabel")
            , it
                "drop / take"
                (applyStyle (DropChars 6 `andThen` TakeChars 2) "personName" `shouldBe` "Na")
            , it
                "Idiomatic falls through to NoStyle without resolution"
                (applyStyle Idiomatic "personName" `shouldBe` "personName")
            ]
      , describe "Idiomatic resolution" $
          sequence_
            [ it
                "JSON resolves to CamelCase"
                ( applyStyle (resolveIdiomatic "json" Idiomatic) "person_name"
                    `shouldBe` "personName"
                )
            , it
                "EDN resolves to KebabCase"
                ( applyStyle (resolveIdiomatic "edn" Idiomatic) "personName"
                    `shouldBe` "person-name"
                )
            , it
                "Proto resolves to CamelCase"
                ( applyStyle (resolveIdiomatic "proto" Idiomatic) "person_name"
                    `shouldBe` "personName"
                )
            , it
                "TOML resolves to SnakeCase"
                ( applyStyle (resolveIdiomatic "toml" Idiomatic) "personName"
                    `shouldBe` "person_name"
                )
            , it
                "YAML resolves to KebabCase"
                ( applyStyle (resolveIdiomatic "yaml" Idiomatic) "personName"
                    `shouldBe` "person-name"
                )
            , it
                "XML resolves to PascalCase"
                ( applyStyle (resolveIdiomatic "xml" Idiomatic) "person_name"
                    `shouldBe` "PersonName"
                )
            , it
                "CBOR resolves to NoStyle (verbatim)"
                ( applyStyle (resolveIdiomatic "cbor" Idiomatic) "personName"
                    `shouldBe` "personName"
                )
            , it
                "compose preserves outer style"
                ( applyStyle
                    ( resolveIdiomatic
                        "edn"
                        (StripPrefix "person" `andThen` Idiomatic)
                    )
                    "personHttpRequest"
                    `shouldBe` "http-request"
                )
            ]
      , describe "properties" $
          sequence_
            [ it "NoStyle is identity" $ property $ do
                t <- forAll genIdent
                applyStyle NoStyle t === t
            , it "Compose NoStyle a == a" $ property $ do
                t <- forAll genIdent
                applyStyle (Compose NoStyle SnakeCase) t === applyStyle SnakeCase t
            , it "snake then camel preserves alpha" $ property $ do
                t <- forAll genCamelIdent
                let snaked = applyStyle SnakeCase t
                    roundTripped = applyStyle CamelCase snaked
                T.toLower roundTripped === T.toLower t
            , it "snake produces lowercase output" $ property $ do
                t <- forAll genIdent
                let s = applyStyle SnakeCase t
                assert (T.all (\c -> not (Char.isUpper c)) s)
            , it "kebab produces lowercase output" $ property $ do
                t <- forAll genIdent
                let s = applyStyle KebabCase t
                assert (T.all (\c -> not (Char.isUpper c)) s)
            , it "applyStyle is total (never bottom)" $ property $ do
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
genIdent =
  T.pack
    <$> Gen.list
      (Range.linear 1 20)
      ( Gen.frequency
          [ (5, Gen.alpha)
          , (1, Gen.element ['_'])
          , (1, Gen.digit)
          ]
      )


-- camelCase identifier: starts lower, no underscores, alphanumeric only.
genCamelIdent :: MonadGen m => m T.Text
genCamelIdent = do
  first <- Gen.lower
  rest <- Gen.list (Range.linear 0 12) Gen.alphaNum
  pure (T.pack (first : rest))
