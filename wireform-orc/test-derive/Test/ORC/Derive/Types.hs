{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the ORC deriver round-trip tests.
module Test.ORC.Derive.Types
  ( Sale (..)
  , OrderId (..)
  , defaultRegion
  , Shape (..)
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier

-- | A record exercising:
--
-- * Mixed scalar widths ('Int64' \/ 'Int32' \/ 'Double' \/ 'Bool').
-- * 'Text' (mapped to 'TKString').
-- * Nullable column via 'Maybe'.
-- * 'rename' (forces an explicit ORC column name).
-- * 'skip' + 'defaults' under @backendOrc@.
-- * 'coerced' newtype on 'orderId'.
data Sale = Sale
  { saleOrderId  :: !OrderId
  , saleAmount   :: !Double
  , saleQty      :: !(Maybe Int32)
  , saleNotes    :: !Text
  , saleActive   :: !Bool
  , saleRegion   :: !Text
  } deriving (Eq, Show)

-- | Default value for the skipped 'saleRegion' column. Reads pull
-- this back in on decode.
defaultRegion :: Text
defaultRegion = "<unknown>"

{-# ANN saleOrderId (forBackend backendOrc (coerced ''Int64)) #-}
{-# ANN saleAmount  (rename "amount") #-}
{-# ANN saleRegion  (forBackend backendOrc skip) #-}
{-# ANN saleRegion  (forBackend backendOrc (defaults 'defaultRegion)) #-}

-- | Newtype wrapping an 'Int64'. Used both as a coerced field
-- ('saleOrderId') and as a standalone leaf-class derivation
-- target — the deriver emits 'ToORCLeaf' \/ 'FromORCLeaf'
-- pass-through instances when 'deriveORC' is applied to it.
newtype OrderId = OrderId { unOrderId :: Int64 }
  deriving (Eq, Show)

-- | Splice-rejection fixture. 'deriveORC' on this type must 'fail'
-- because ORC does not have a value-level union representation.
data Shape
  = Circle !Double
  | Square !Double
  deriving (Eq, Show)
