{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the ASN.1 deriver round-trip tests.
module Test.ASN1.Derive.Types
  ( Person (..)
  , Color (..)
  , Shape (..)
  , Wrapper (..)
  ) where

import Data.Text (Text)

import Wireform.Derive.Modifier

import ASN1.Derive (asn1ImplicitTag)

-- ---------------------------------------------------------------------------
-- Record with mixed scalars and a context-tagged field
-- ---------------------------------------------------------------------------

data Person = Person
  { personId    :: !Int
  , personName  :: !Text
  , personAdmin :: !Bool
  } deriving (Eq, Show)

-- The 'admin' flag rides as @[CONTEXT 0] BOOLEAN@.
{-# ANN personAdmin (asn1ImplicitTag 0) #-}

-- 'tag' modifier is irrelevant for record fields in ASN.1; include
-- one to confirm the deriver tolerates it without effect.
{-# ANN personId (tag 99) #-}

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtype Wrapper = Wrapper { unwrap :: Int }
  deriving stock (Eq, Show)

-- ---------------------------------------------------------------------------
-- Enum + Sum
-- ---------------------------------------------------------------------------

data Color = Red | Green | Blue
  deriving (Eq, Show)

data Shape
  = Origin
  | Square !Int
  | Rect !Int !Int
  deriving (Eq, Show)
