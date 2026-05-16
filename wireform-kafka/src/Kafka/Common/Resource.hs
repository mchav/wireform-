{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Common.Resource
-- Description : Resource pattern types from @org.apache.kafka.common.resource@
--
-- Declarative records mirroring
-- @org.apache.kafka.common.resource.*@. These are the targets of
-- the ACL admin RPCs; see "Kafka.Common.Acl".
module Kafka.Common.Resource
  ( -- * Resource type
    ResourceType (..)
    -- * Pattern type
  , PatternType (..)
    -- * Resource + pattern
  , Resource (..)
  , ResourcePattern (..)
    -- * Filter
  , ResourcePatternFilter (..)
  , anyResourcePatternFilter
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

----------------------------------------------------------------------
-- ResourceType
----------------------------------------------------------------------

-- | Kinds of resources an ACL can target. Mirrors
-- @org.apache.kafka.common.resource.ResourceType@.
data ResourceType
  = ResourceUnknown
  | ResourceAny
  | ResourceTopic
  | ResourceGroup
  | ResourceCluster
  | ResourceTransactionalId
  | ResourceDelegationToken
  | ResourceUser
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- PatternType
----------------------------------------------------------------------

-- | How an ACL pattern matches a resource name. Mirrors
-- @org.apache.kafka.common.resource.PatternType@.
data PatternType
  = PatternUnknown
  | PatternAny
  | PatternMatch
  | PatternLiteral
  | PatternPrefixed
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- Resource + ResourcePattern
----------------------------------------------------------------------

-- | A concrete cluster resource: type + name. Mirrors
-- @org.apache.kafka.common.resource.Resource@.
data Resource = Resource
  { resourceType :: !ResourceType
  , resourceName :: !Text
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | An ACL match pattern. Mirrors
-- @org.apache.kafka.common.resource.ResourcePattern@.
data ResourcePattern = ResourcePattern
  { rpResourceType :: !ResourceType
  , rpName         :: !Text
  , rpPatternType  :: !PatternType
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- ResourcePatternFilter
----------------------------------------------------------------------

-- | A filter that matches resource patterns. Mirrors
-- @org.apache.kafka.common.resource.ResourcePatternFilter@.
data ResourcePatternFilter = ResourcePatternFilter
  { rpfResourceType :: !ResourceType
  , rpfName         :: !(Maybe Text)
    -- ^ 'Nothing' matches any name.
  , rpfPatternType  :: !PatternType
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | The wildcard pattern filter: matches every resource.
anyResourcePatternFilter :: ResourcePatternFilter
anyResourcePatternFilter = ResourcePatternFilter
  { rpfResourceType = ResourceAny
  , rpfName         = Nothing
  , rpfPatternType  = PatternAny
  }
