{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Modifier (tests) where

import qualified Data.Map.Strict as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.ModifierInfo
import Wireform.Derive.NameStyle

tests :: TestTree
tests = testGroup "Modifier"
  [ testGroup "foldModifiers — happy path"
      [ testCase "empty input gives empty info" $
          foldModifiers backendJSON [] @?= Right (emptyModifierInfo backendJSON)

      , testCase "single rename" $
          fmap miRename (foldModifiers backendJSON [rename "name"])
            @?= Right (Just (RenameSpecLiteral "name"))

      , testCase "duplicate identical rename is no conflict" $
          fmap miRename (foldModifiers backendJSON
            [rename "name", rename "name"])
            @?= Right (Just (RenameSpecLiteral "name"))

      , testCase "skip flag" $
          fmap miSkip (foldModifiers backendJSON [skip])
            @?= Right True

      , testCase "tag set" $
          fmap miTag (foldModifiers backendJSON [tag 7])
            @?= Right (Just 7)

      , testCase "required + only set once" $
          fmap miRequired (foldModifiers backendJSON [required])
            @?= Right (Just True)

      , testCase "optional + only set once" $
          fmap miRequired (foldModifiers backendJSON [optional])
            @?= Right (Just False)
      ]

  , testGroup "foldModifiers — conflicts"
      [ testCase "two distinct renames conflict" $ do
          let r = foldModifiers backendJSON
                    [rename "a", rename "b"]
          case r of
            Left ConflictRename {} -> pure ()
            other -> fail ("expected ConflictRename, got " ++ show other)

      , testCase "two distinct tags conflict" $ do
          let r = foldModifiers backendJSON [tag 1, tag 2]
          case r of
            Left ConflictTag {} -> pure ()
            other -> fail ("expected ConflictTag, got " ++ show other)

      , testCase "required + optional conflict" $ do
          let r = foldModifiers backendJSON [required, optional]
          case r of
            Left ConflictRequired {} -> pure ()
            other -> fail ("expected ConflictRequired, got " ++ show other)

      , testCase "flatten + skip conflict" $ do
          let r = foldModifiers backendJSON [flatten, skip]
          case r of
            Left ConflictFlattenSkip -> pure ()
            other -> fail ("expected ConflictFlattenSkip, got " ++ show other)
      ]

  , testGroup "per-backend overrides"
      [ testCase "backend-only modifier applies for active backend" $
          fmap miRename
              (foldModifiers backendProto
                  [forBackend backendProto (rename "n")])
            @?= Right (Just (RenameSpecLiteral "n"))

      , testCase "backend-only modifier ignored for other backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [forBackend backendProto (rename "n")])
            @?= Right Nothing

      , testCase "per-backend rename shadows global, no conflict" $
          fmap miRename
              (foldModifiers backendProto
                  [ rename "global"
                  , forBackend backendProto (rename "scoped")
                  ])
            @?= Right (Just (RenameSpecLiteral "scoped"))

      , testCase "per-backend rename does not shadow on other backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [ rename "global"
                  , forBackend backendProto (rename "scoped")
                  ])
            @?= Right (Just (RenameSpecLiteral "global"))

      , testCase "disableFor on active backend marks skip" $
          fmap miSkip
              (foldModifiers backendCBOR
                  [ disableFor [backendCBOR, backendMsgPack] ])
            @?= Right True

      , testCase "disableFor on other backend has no effect" $
          fmap miSkip
              (foldModifiers backendJSON
                  [ disableFor [backendCBOR, backendMsgPack] ])
            @?= Right False

      , testCase "forBackends groups multiple modifiers" $ do
          let mi = foldModifiers backendProto
                     [ forBackends [backendProto] [rename "p", tag 9] ]
          fmap miRename mi @?= Right (Just (RenameSpecLiteral "p"))
          fmap miTag    mi @?= Right (Just 9)

      , testCase "forBackends does nothing for unlisted backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [ forBackends [backendProto] [rename "p"] ])
            @?= Right Nothing
      ]

  , testGroup "RenameSpec variants"
      [ testCase "rename literal" $
          fmap miRename (foldModifiers backendJSON [rename "n"])
            @?= Right (Just (RenameSpecLiteral "n"))

      , testCase "renameStyle preserved" $
          fmap miRename (foldModifiers backendJSON
              [renameStyle (StripPrefix "person")])
            @?= Right (Just (RenameSpecStyle (StripPrefix "person")))
      ]

  , testGroup "default rename keys"
      [ testCase "JSON default is camel"
          (defaultRenameForBackend backendJSON "person_name" @?= "personName")
      , testCase "EDN default is kebab"
          (defaultRenameForBackend backendEDN "personName"   @?= "person-name")
      , testCase "CBOR default is verbatim"
          (defaultRenameForBackend backendCBOR "personName"  @?= "personName")
      ]

  , testGroup "ModCustom accumulation"
      [ testCase "two custom payloads with same tag accumulate" $ do
          let mi = foldModifiers backendCBOR
                     [ customModifier "ext.foo" "x"
                     , customModifier "ext.foo" "y"
                     ]
          case mi of
            Right info -> do
              let xs = Map.findWithDefault [] "ext.foo" (miCustom info)
              assertEqual "accumulated count" 2 (length xs)
            Left e -> fail ("unexpected " ++ show e)

      , testCase "custom payloads with different tags don't conflict" $ do
          let mi = foldModifiers backendCBOR
                     [ customModifier "ext.foo" "x"
                     , customModifier "ext.bar" "y"
                     ]
          case mi of
            Right info -> do
              assertBool "foo present" (Map.member "ext.foo" (miCustom info))
              assertBool "bar present" (Map.member "ext.bar" (miCustom info))
            Left e -> fail ("unexpected " ++ show e)
      ]
  ]
