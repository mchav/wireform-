{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the Iceberg deriver tests.
module Test.Iceberg.Derive.Types
  ( Person (..)
  , Sale (..)
  , Tagged (..)
  , Variant (..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)

import Wireform.Derive.Modifier (rename, tag)

-- | Plain three-field record for the basic schema test.
data Person = Person
  { personName   :: !Text
  , personAge    :: !Int
  , personActive :: !Bool
  } deriving stock (Eq, Show)

-- | Record exercising 'rename' and a nullable column.
data Sale = Sale
  { saleAmount  :: !Int64
  , saleProduct :: !Text
  , saleRegion  :: !(Maybe Text)
  } deriving stock (Eq, Show)

{-# ANN saleAmount  (rename "amount")  #-}
{-# ANN saleProduct (rename "product") #-}

-- | Record exercising user-supplied 'tag' field IDs.
data Tagged = Tagged
  { taggedAlpha :: !Text
  , taggedBeta  :: !Int64
  } deriving stock (Eq, Show)

{-# ANN taggedAlpha (tag 100) #-}
{-# ANN taggedBeta  (tag 200) #-}

-- | Sum used to verify 'icebergSchemaFor' refuses non-record shapes.
data Variant = VariantA | VariantB !Int
  deriving stock (Eq, Show)
