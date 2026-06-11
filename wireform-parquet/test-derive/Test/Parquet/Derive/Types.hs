{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Annotated fixture types for the Parquet deriver tests.
module Test.Parquet.Derive.Types (
  Sale (..),
  OrderId (..),
  Order (..),
  Color (..),
) where

import Data.Int (Int64)
import Data.Text (Text)
import Wireform.Derive.Modifier (coerced, rename)


{- | Three flat fields exercising required, required, and nullable
leaves. The renames are picked up by the spliced schema.
-}
data Sale = Sale
  { saleAmount :: !Int64
  , saleProduct :: !Text
  , saleRegion :: !(Maybe Text)
  }
  deriving stock (Eq, Show)


{-# ANN saleAmount (rename "amount") #-}


{-# ANN saleProduct (rename "product") #-}


{- | Newtype around 'Int64' used through 'coerced' so the deriver
emits the underlying physical type without needing an instance on
'OrderId' itself.
-}
newtype OrderId = OrderId {unOrderId :: Int64}
  deriving stock (Eq, Show)


-- | One-field record exercising the 'coerced' modifier.
data Order = Order
  { orderId :: !OrderId
  }
  deriving stock (Eq, Show)


{-# ANN orderId (coerced ''Int64) #-}


{- | Multi-constructor sum used to verify 'deriveParquet' refuses
non-record shapes at splice time.
-}
data Color = Red | Green | Blue
  deriving stock (Eq, Show)
