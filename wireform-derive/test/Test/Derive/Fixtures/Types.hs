{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Annotated data types used to verify that 'ANN' pragmas survive
the TH reflection round-trip.

This module deliberately performs no splices: 'ANN' pragmas are
only visible to 'reifyAnnotations' /after/ the declaring module has
been compiled, so the splice that reifies them lives in
"Test.Derive.Fixtures.Reified".
-}
module Test.Derive.Fixtures.Types (
  Person (..),
  personNameRenamer,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Wireform.Derive.Backend
import Wireform.Derive.Modifier
import Wireform.Derive.NameStyle


-- | Test-only @Text -> Text@ function used by 'renameWith'.
personNameRenamer :: Text -> Text
personNameRenamer = T.cons '_' . T.toLower


data Person = Person
  { personName :: !Text
  , personAge :: !Int
  , personSSN :: !Text
  }
  deriving (Eq, Show)


-- | Type-level rename: identity (kept here for completeness).
{-# ANN type Person (rename "person") #-}


-- | Field-level customisations used by the round-trip tests.
{-# ANN personName (rename "name") #-}


{-# ANN personAge (renameStyle SnakeCase) #-}


{-# ANN personSSN (renameWith 'personNameRenamer) #-}


{- | Per-backend overrides: in JSON, 'personSSN' should be skipped;
in CBOR it should still be encoded.
-}
{-# ANN personSSN (disableFor [backendJSON]) #-}


-- | Demonstrate that several modifiers compose on a single name.
{-# ANN personAge (forBackend backendProto (tag 7)) #-}
