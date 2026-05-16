{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Common.Quota
-- Description : Client-quota value types from @org.apache.kafka.common.quota@
--
-- Declarative records mirroring
-- @org.apache.kafka.common.quota.*@. These are the carrying-types
-- for the @alterClientQuotas@ / @describeClientQuotas@ admin RPCs.
module Kafka.Common.Quota
  ( -- * Entity
    ClientQuotaEntity (..)
  , clientQuotaEntity
    -- * Filter
  , ClientQuotaFilter (..)
  , ClientQuotaFilterComponent (..)
  , ClientQuotaMatch (..)
  , exactMatch
  , matchAnyName
  , defaultEntity
    -- * Alteration
  , ClientQuotaAlteration (..)
  , ClientQuotaOp (..)
  ) where

import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- Entity
----------------------------------------------------------------------

-- | A client-quota entity: a map from entity-type (@\"user\"@,
-- @\"client-id\"@, @\"ip\"@) to the specific name (or 'Nothing' for
-- the per-type default). Mirrors
-- @org.apache.kafka.common.quota.ClientQuotaEntity@.
--
-- Construct with 'clientQuotaEntity' so the @Nothing@ default
-- semantics are explicit.
newtype ClientQuotaEntity = ClientQuotaEntity
  { cqeEntries :: Map Text (Maybe Text)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

clientQuotaEntity :: [(Text, Maybe Text)] -> ClientQuotaEntity
clientQuotaEntity = ClientQuotaEntity . Map.fromList

----------------------------------------------------------------------
-- Filter
----------------------------------------------------------------------

-- | A component of a quota filter. Mirrors
-- @org.apache.kafka.common.quota.ClientQuotaFilterComponent@.
data ClientQuotaFilterComponent = ClientQuotaFilterComponent
  { cqfcEntityType :: !Text
  , cqfcMatchType  :: !ClientQuotaMatch
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | The matching mode of a 'ClientQuotaFilterComponent'.
data ClientQuotaMatch
  = MatchExact !Text
    -- ^ Match only the entity with this specific name.
  | MatchDefault
    -- ^ Match the per-type default entity.
  | MatchAny
    -- ^ Match every entity of this type.
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

exactMatch :: Text -> Text -> ClientQuotaFilterComponent
exactMatch ty nm = ClientQuotaFilterComponent ty (MatchExact nm)

matchAnyName :: Text -> ClientQuotaFilterComponent
matchAnyName ty = ClientQuotaFilterComponent ty MatchAny

defaultEntity :: Text -> ClientQuotaFilterComponent
defaultEntity ty = ClientQuotaFilterComponent ty MatchDefault

-- | A quota filter is a list of components plus a strictness flag.
-- Mirrors @org.apache.kafka.common.quota.ClientQuotaFilter@.
data ClientQuotaFilter = ClientQuotaFilter
  { cqfComponents :: ![ClientQuotaFilterComponent]
  , cqfStrict     :: !Bool
    -- ^ When 'True' only entities whose entity-types are exactly
    -- the components' types match. When 'False' entities with
    -- additional component types also match.
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- Alteration
----------------------------------------------------------------------

-- | A single quota change. 'Just' sets the value; 'Nothing'
-- removes it (matches Java's @Op(name, null)@).
data ClientQuotaOp = ClientQuotaOp
  { cqoKey   :: !Text
    -- ^ The quota name, e.g. @\"producer_byte_rate\"@.
  , cqoValue :: !(Maybe Double)
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | A request to alter the quotas attached to a single
-- 'ClientQuotaEntity'. Mirrors
-- @org.apache.kafka.common.quota.ClientQuotaAlteration@.
data ClientQuotaAlteration = ClientQuotaAlteration
  { cqaEntity :: !ClientQuotaEntity
  , cqaOps    :: ![ClientQuotaOp]
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable
