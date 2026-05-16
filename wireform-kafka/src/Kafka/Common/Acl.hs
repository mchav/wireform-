{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Kafka.Common.Acl
-- Description : ACL value types from @org.apache.kafka.common.acl@
--
-- Declarative records mirroring
-- @org.apache.kafka.common.acl.*@. They're carrying-types for the
-- ACL admin RPCs (@createAcls@ / @describeAcls@ / @deleteAcls@);
-- the corresponding admin operations aren't yet wrapped at the
-- @Kafka.Client.AdminClient@ surface, but having the value types
-- in tree makes that follow-up a pure naming/wiring exercise.
module Kafka.Common.Acl
  ( -- * Permission / operation
    AclPermissionType (..)
  , AclOperation (..)
    -- * Access-control entry
  , AccessControlEntry (..)
  , AccessControlEntryFilter (..)
    -- * Binding
  , AclBinding (..)
  , AclBindingFilter (..)
    -- * Convenience
  , anyAccessControlEntryFilter
  , anyAclBindingFilter
  ) where

import Data.Hashable (Hashable)
import Data.Text (Text)
import GHC.Generics (Generic)

import Kafka.Common.Resource
  ( ResourcePattern
  , ResourcePatternFilter
  , anyResourcePatternFilter
  )

----------------------------------------------------------------------
-- Permission + operation
----------------------------------------------------------------------

-- | Whether an ACL grants or denies permission. Mirrors
-- @org.apache.kafka.common.acl.AclPermissionType@.
data AclPermissionType
  = AclUnknownPerm
  | AclAnyPerm
  | AclDeny
  | AclAllow
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | What operation an ACL applies to. Mirrors
-- @org.apache.kafka.common.acl.AclOperation@.
data AclOperation
  = AclUnknownOp
  | AclAnyOp
  | AclAll
  | AclRead
  | AclWrite
  | AclCreate
  | AclDelete
  | AclAlter
  | AclDescribe
  | AclClusterAction
  | AclDescribeConfigs
  | AclAlterConfigs
  | AclIdempotentWrite
  | AclCreateTokens
  | AclDescribeTokens
  | AclTwoPhaseCommit
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

----------------------------------------------------------------------
-- Entries
----------------------------------------------------------------------

-- | A single access-control entry: who, what, where (on a
-- resource), and whether allow/deny. Mirrors
-- @org.apache.kafka.common.acl.AccessControlEntry@.
data AccessControlEntry = AccessControlEntry
  { aceePrincipal      :: !Text
    -- ^ Java syntax: @"User:alice"@ / @"Group:admin"@.
  , aceeHost           :: !Text
    -- ^ Source host. Use @"*"@ for any.
  , aceeOperation      :: !AclOperation
  , aceePermissionType :: !AclPermissionType
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | A filter for matching access-control entries. Mirrors
-- @org.apache.kafka.common.acl.AccessControlEntryFilter@. The
-- wildcards @'Nothing'@ on each field accept anything.
data AccessControlEntryFilter = AccessControlEntryFilter
  { acefPrincipal      :: !(Maybe Text)
  , acefHost           :: !(Maybe Text)
  , acefOperation      :: !AclOperation
  , acefPermissionType :: !AclPermissionType
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | The wildcard entry filter: matches every entry.
anyAccessControlEntryFilter :: AccessControlEntryFilter
anyAccessControlEntryFilter = AccessControlEntryFilter
  { acefPrincipal      = Nothing
  , acefHost           = Nothing
  , acefOperation      = AclAnyOp
  , acefPermissionType = AclAnyPerm
  }

----------------------------------------------------------------------
-- Bindings
----------------------------------------------------------------------

-- | A binding of an 'AccessControlEntry' to a 'ResourcePattern'.
-- Mirrors @org.apache.kafka.common.acl.AclBinding@.
data AclBinding = AclBinding
  { aclbPattern :: !ResourcePattern
  , aclbEntry   :: !AccessControlEntry
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | A filter matching bindings. Mirrors
-- @org.apache.kafka.common.acl.AclBindingFilter@.
data AclBindingFilter = AclBindingFilter
  { aclbfPatternFilter :: !ResourcePatternFilter
  , aclbfEntryFilter   :: !AccessControlEntryFilter
  }
  deriving stock (Eq, Show, Ord, Generic)
  deriving anyclass Hashable

-- | The wildcard binding filter: matches every binding.
anyAclBindingFilter :: AclBindingFilter
anyAclBindingFilter = AclBindingFilter
  { aclbfPatternFilter = anyResourcePatternFilter
  , aclbfEntryFilter   = anyAccessControlEntryFilter
  }
