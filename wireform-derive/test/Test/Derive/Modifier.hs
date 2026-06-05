{-# LANGUAGE OverloadedStrings #-}

module Test.Derive.Modifier (tests) where

import qualified Data.Map.Strict as Map
import Test.Syd

import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.ModifierInfo
import Wireform.Derive.NameStyle

tests :: Spec
tests = describe "Modifier" $ sequence_
  [ describe "foldModifiers — happy path" $ sequence_
      [ it "empty input gives empty info" $
          foldModifiers backendJSON [] `shouldBe` Right (emptyModifierInfo backendJSON)

      , it "single rename" $
          fmap miRename (foldModifiers backendJSON [rename "name"])
            `shouldBe` Right (Just (RenameSpecLiteral "name"))

      , it "duplicate identical rename is no conflict" $
          fmap miRename (foldModifiers backendJSON
            [rename "name", rename "name"])
            `shouldBe` Right (Just (RenameSpecLiteral "name"))

      , it "skip flag" $
          fmap miSkip (foldModifiers backendJSON [skip])
            `shouldBe` Right True

      , it "tag set" $
          fmap miTag (foldModifiers backendJSON [tag 7])
            `shouldBe` Right (Just 7)

      , it "required + only set once" $
          fmap miRequired (foldModifiers backendJSON [required])
            `shouldBe` Right (Just True)

      , it "optional + only set once" $
          fmap miRequired (foldModifiers backendJSON [optional])
            `shouldBe` Right (Just False)
      ]

  , describe "foldModifiers — conflicts" $ sequence_
      [ it "two distinct renames conflict" $ do
          let r = foldModifiers backendJSON
                    [rename "a", rename "b"]
          case r of
            Left ConflictRename {} -> pure ()
            other -> expectationFailure ("expected ConflictRename, got " ++ show other)

      , it "two distinct tags conflict" $ do
          let r = foldModifiers backendJSON [tag 1, tag 2]
          case r of
            Left ConflictTag {} -> pure ()
            other -> expectationFailure ("expected ConflictTag, got " ++ show other)

      , it "required + optional conflict" $ do
          let r = foldModifiers backendJSON [required, optional]
          case r of
            Left ConflictRequired {} -> pure ()
            other -> expectationFailure ("expected ConflictRequired, got " ++ show other)

      , it "flatten + skip conflict" $ do
          let r = foldModifiers backendJSON [flatten, skip]
          case r of
            Left ConflictFlattenSkip -> pure ()
            other -> expectationFailure ("expected ConflictFlattenSkip, got " ++ show other)
      ]

  , describe "per-backend overrides" $ sequence_
      [ it "backend-only modifier applies for active backend" $
          fmap miRename
              (foldModifiers backendProto
                  [forBackend backendProto (rename "n")])
            `shouldBe` Right (Just (RenameSpecLiteral "n"))

      , it "backend-only modifier ignored for other backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [forBackend backendProto (rename "n")])
            `shouldBe` Right Nothing

      , it "per-backend rename shadows global, no conflict" $
          fmap miRename
              (foldModifiers backendProto
                  [ rename "global"
                  , forBackend backendProto (rename "scoped")
                  ])
            `shouldBe` Right (Just (RenameSpecLiteral "scoped"))

      , it "per-backend rename does not shadow on other backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [ rename "global"
                  , forBackend backendProto (rename "scoped")
                  ])
            `shouldBe` Right (Just (RenameSpecLiteral "global"))

      , it "disableFor on active backend marks skip" $
          fmap miSkip
              (foldModifiers backendCBOR
                  [ disableFor [backendCBOR, backendMsgPack] ])
            `shouldBe` Right True

      , it "disableFor on other backend has no effect" $
          fmap miSkip
              (foldModifiers backendJSON
                  [ disableFor [backendCBOR, backendMsgPack] ])
            `shouldBe` Right False

      , it "forBackends groups multiple modifiers" $ do
          let mi = foldModifiers backendProto
                     [ forBackends [backendProto] [rename "p", tag 9] ]
          fmap miRename mi `shouldBe` Right (Just (RenameSpecLiteral "p"))
          fmap miTag    mi `shouldBe` Right (Just 9)

      , it "forBackends does nothing for unlisted backend" $
          fmap miRename
              (foldModifiers backendJSON
                  [ forBackends [backendProto] [rename "p"] ])
            `shouldBe` Right Nothing
      ]

  , describe "RenameSpec variants" $ sequence_
      [ it "rename literal" $
          fmap miRename (foldModifiers backendJSON [rename "n"])
            `shouldBe` Right (Just (RenameSpecLiteral "n"))

      , it "renameStyle preserved" $
          fmap miRename (foldModifiers backendJSON
              [renameStyle (StripPrefix "person")])
            `shouldBe` Right (Just (RenameSpecStyle (StripPrefix "person")))
      ]

  , describe "default rename keys" $ sequence_
      [ it "JSON default is camel"
          (defaultRenameForBackend backendJSON "person_name" `shouldBe` "personName")
      , it "EDN default is kebab"
          (defaultRenameForBackend backendEDN "personName"   `shouldBe` "person-name")
      , it "CBOR default is verbatim"
          (defaultRenameForBackend backendCBOR "personName"  `shouldBe` "personName")
      ]

  , describe "ModCustom accumulation" $ sequence_
      [ it "two custom payloads with same tag accumulate" $ do
          let mi = foldModifiers backendCBOR
                     [ customModifier "ext.foo" "x"
                     , customModifier "ext.foo" "y"
                     ]
          case mi of
            Right info -> do
              let xs = Map.findWithDefault [] "ext.foo" (miCustom info)
              length xs `shouldBe` 2
            Left e -> expectationFailure ("unexpected " ++ show e)

      , it "custom payloads with different tags don't conflict" $ do
          let mi = foldModifiers backendCBOR
                     [ customModifier "ext.foo" "x"
                     , customModifier "ext.bar" "y"
                     ]
          case mi of
            Right info -> do
              (Map.member "ext.foo" (miCustom info)) `shouldBe` True
              (Map.member "ext.bar" (miCustom info)) `shouldBe` True
            Left e -> expectationFailure ("unexpected " ++ show e)
      ]
  ]
