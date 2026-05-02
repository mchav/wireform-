{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Fixtures (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Test.Derive.Fixtures.Reified as R

tests :: TestTree
tests = testGroup "ANN round-trip"
  [ testGroup "wire keys (literal renames)"
      [ testCase "personName JSON  -> \"name\""
          (R.personNameKeyJSON @?= "name")
      , testCase "personName CBOR  -> \"name\""
          (R.personNameKeyCBOR @?= "name")
      ]

  , testGroup "wire keys (renameStyle)"
      [ testCase "personAge JSON  -> \"person_age\" (literal from SnakeCase)"
          (R.personAgeKeyJSON @?= "person_age")
      , testCase "personAge Proto -> \"person_age\" (same; backend rename only on JSON-style)"
          (R.personAgeKeyProto @?= "person_age")
      ]

  , testGroup "wire keys (renameWith — runtime call)"
      [ testCase "personSSN JSON  -> \"_personssn\" (lowercase + underscore prefix)"
          (R.personSSNKeyJSON @?= "_personssn")
      , testCase "personSSN CBOR  -> \"_personssn\""
          (R.personSSNKeyCBOR @?= "_personssn")
      ]

  , testGroup "per-backend overrides"
      [ testCase "personSSN skipped in JSON (via disableFor)"
          (R.personSSNSkipJSON @?= True)
      , testCase "personSSN NOT skipped in CBOR"
          (R.personSSNSkipCBOR @?= False)
      , testCase "personAge tag visible only under proto"
          (R.personAgeTagProto @?= Just 7)
      , testCase "personAge has no tag under JSON"
          (R.personAgeTagJSON @?= Nothing)
      ]
  ]
