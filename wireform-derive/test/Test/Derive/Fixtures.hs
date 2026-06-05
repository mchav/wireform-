{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Fixtures (tests) where

import Test.Syd

import qualified Test.Derive.Fixtures.Reified as R

tests :: Spec
tests = describe "ANN round-trip" $ sequence_
  [ describe "wire keys (literal renames)" $ sequence_
      [ it "personName JSON  -> \"name\""
          (R.personNameKeyJSON `shouldBe` "name")
      , it "personName CBOR  -> \"name\""
          (R.personNameKeyCBOR `shouldBe` "name")
      ]

  , describe "wire keys (renameStyle)" $ sequence_
      [ it "personAge JSON  -> \"person_age\" (literal from SnakeCase)"
          (R.personAgeKeyJSON `shouldBe` "person_age")
      , it "personAge Proto -> \"person_age\" (same; backend rename only on JSON-style)"
          (R.personAgeKeyProto `shouldBe` "person_age")
      ]

  , describe "wire keys (renameWith — runtime call)" $ sequence_
      [ it "personSSN JSON  -> \"_personssn\" (lowercase + underscore prefix)"
          (R.personSSNKeyJSON `shouldBe` "_personssn")
      , it "personSSN CBOR  -> \"_personssn\""
          (R.personSSNKeyCBOR `shouldBe` "_personssn")
      ]

  , describe "per-backend overrides" $ sequence_
      [ it "personSSN skipped in JSON (via disableFor)"
          (R.personSSNSkipJSON `shouldBe` True)
      , it "personSSN NOT skipped in CBOR"
          (R.personSSNSkipCBOR `shouldBe` False)
      , it "personAge tag visible only under proto"
          (R.personAgeTagProto `shouldBe` Just 7)
      , it "personAge has no tag under JSON"
          (R.personAgeTagJSON `shouldBe` Nothing)
      ]
  ]
