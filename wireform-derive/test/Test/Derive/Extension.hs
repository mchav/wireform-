{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Round-trip tests for 'Wireform.Derive.Extension'. Two distinct
-- backend-defined modifier types coexist on the same 'Name' and are
-- recovered with full type fidelity.
module Test.Derive.Extension (tests) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Test.Syd

import Wireform.Derive.Backend (backendIceberg)
import Wireform.Derive.Extension
  ( BackendModifier (..)
  , extension
  , hasExtension
  , lookupExtension
  , lookupExtensions
  )
import Wireform.Derive.Modifier (Modifier)
import Wireform.Derive.ModifierInfo
  ( ModifierInfo (..)
  , emptyModifierInfo
  , foldModifiers
  )

-- ---------------------------------------------------------------------------
-- Two pretend backend extension vocabularies
-- ---------------------------------------------------------------------------

data IcebergFieldOpt
  = PartitionColumn
  | OptimisticTransform !Text
  deriving stock (Eq, Show, Read, Typeable, Generic)

instance BackendModifier IcebergFieldOpt where
  backendModifierTag _ = "wireform-iceberg.field-opt"

data XmlFieldOpt
  = AsAttribute
  | AsElement
  | NamespacedTo !Text
  deriving stock (Eq, Show, Read, Typeable, Generic)

instance BackendModifier XmlFieldOpt where
  backendModifierTag _ = "wireform-xml.field-opt"

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

tests :: Spec
tests = describe "Wireform.Derive.Extension" $ sequence_
  [ it "round-trip a single typed extension" $ do
      let m = extension PartitionColumn
      let mi = applyMods [m]
      lookupExtension @IcebergFieldOpt mi `shouldBe` Just PartitionColumn
      hasExtension    @IcebergFieldOpt mi `shouldBe` True

  , it "round-trip an extension with a Text payload" $ do
      let m = extension (OptimisticTransform (T.pack "year"))
      let mi = applyMods [m]
      lookupExtension @IcebergFieldOpt mi `shouldBe` Just (OptimisticTransform (T.pack "year"))

  , it "two extensions of the same type are stacked" $ do
      let ms =
            [ extension PartitionColumn
            , extension (OptimisticTransform (T.pack "month"))
            ]
      let mi = applyMods ms
      lookupExtensions @IcebergFieldOpt mi `shouldBe`
        [ PartitionColumn
        , OptimisticTransform (T.pack "month")
        ]

  , it "two extensions of distinct types coexist" $ do
      let ms = [ extension PartitionColumn
               , extension AsAttribute
               , extension (NamespacedTo (T.pack "ns0"))
               ]
      let mi = applyMods ms
      lookupExtension @IcebergFieldOpt mi `shouldBe` Just PartitionColumn
      lookupExtensions @XmlFieldOpt   mi `shouldBe`
        [AsAttribute, NamespacedTo (T.pack "ns0")]

  , it "absent extension is Nothing" $ do
      let mi = emptyModifierInfo backendIceberg
      lookupExtension @IcebergFieldOpt mi `shouldBe` Nothing
      hasExtension    @IcebergFieldOpt mi `shouldBe` False

  , it "miCustom keys match the BackendModifier tag" $ do
      let mi = applyMods [extension PartitionColumn, extension AsAttribute]
      Map.keys (miCustom mi) `shouldBe`
        [ "wireform-iceberg.field-opt"
        , "wireform-xml.field-opt"
        ]
  ]

-- | Fold the given modifiers as if they were attached to a name, so
-- the test mirrors what 'reifyModifierInfoFor' would produce.
applyMods :: [Modifier] -> ModifierInfo
applyMods ms = case foldModifiers backendIceberg ms of
  Right mi -> mi
  Left err -> error ("test fixture: foldModifiers raised " <> show err)
