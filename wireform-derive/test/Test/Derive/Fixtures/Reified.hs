{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- | Splices that exercise the public deriver API against the
annotated types in "Test.Derive.Fixtures.Types".

Rather than try to embed entire 'ModifierInfo' values (which would
require @Lift@ instances we deliberately do not provide), we splice
only the values per-format derivers actually consume:

* the wire-key 'Text' returned by 'renderWireKey'
* 'Bool's that summarise key flags after resolution.
-}
module Test.Derive.Fixtures.Reified (
  -- * Wire keys
  personNameKeyJSON,
  personNameKeyCBOR,
  personAgeKeyJSON,
  personAgeKeyProto,
  personSSNKeyJSON,
  personSSNKeyCBOR,

  -- * Resolved flags
  personSSNSkipJSON,
  personSSNSkipCBOR,
  personAgeTagProto,
  personAgeTagJSON,
) where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (lift)
import Test.Derive.Fixtures.Types qualified as F
import Wireform.Derive.Backend
import Wireform.Derive.ModifierInfo


-- ---------------------------------------------------------------------------
-- Wire keys
-- ---------------------------------------------------------------------------

personNameKeyJSON :: Text
personNameKeyJSON =
  $( do
       mi <- reifyModifierInfoFor backendJSON 'F.personName
       renderWireKey mi "personName"
   )


personNameKeyCBOR :: Text
personNameKeyCBOR =
  $( do
       mi <- reifyModifierInfoFor backendCBOR 'F.personName
       renderWireKey mi "personName"
   )


personAgeKeyJSON :: Text
personAgeKeyJSON =
  $( do
       mi <- reifyModifierInfoFor backendJSON 'F.personAge
       renderWireKey mi "personAge"
   )


personAgeKeyProto :: Text
personAgeKeyProto =
  $( do
       mi <- reifyModifierInfoFor backendProto 'F.personAge
       renderWireKey mi "personAge"
   )


personSSNKeyJSON :: Text
personSSNKeyJSON =
  $( do
       mi <- reifyModifierInfoFor backendJSON 'F.personSSN
       renderWireKey mi "personSSN"
   )


personSSNKeyCBOR :: Text
personSSNKeyCBOR =
  $( do
       mi <- reifyModifierInfoFor backendCBOR 'F.personSSN
       renderWireKey mi "personSSN"
   )


-- ---------------------------------------------------------------------------
-- Flags
-- ---------------------------------------------------------------------------

personSSNSkipJSON :: Bool
personSSNSkipJSON =
  $( do
       mi <- reifyModifierInfoFor backendJSON 'F.personSSN
       lift (miSkip mi)
   )


personSSNSkipCBOR :: Bool
personSSNSkipCBOR =
  $( do
       mi <- reifyModifierInfoFor backendCBOR 'F.personSSN
       lift (miSkip mi)
   )


personAgeTagProto :: Maybe Int
personAgeTagProto =
  $( do
       mi <- reifyModifierInfoFor backendProto 'F.personAge
       lift (miTag mi)
   )


personAgeTagJSON :: Maybe Int
personAgeTagJSON =
  $( do
       mi <- reifyModifierInfoFor backendJSON 'F.personAge
       lift (miTag mi)
   )
