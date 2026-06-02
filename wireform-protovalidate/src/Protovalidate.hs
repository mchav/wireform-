-- | [protovalidate](https://protovalidate.com/) for the @wireform@ Protocol
-- Buffers stack: protobuf message validation driven by CEL.
--
-- protovalidate expresses validation rules as CEL expressions — both the
-- standard annotations (@(buf.validate.field).string.email@,
-- @(buf.validate.field).int32.gt@, …) and arbitrary custom logic
-- (@(buf.validate.field).cel@ / @(buf.validate.message).cel@). This package
-- supplies:
--
--   * the protovalidate CEL extension library ("Protovalidate.Library"):
--     @isEmail@, @isHostname@, @isIp@, @isIpPrefix@, @isUri@, @unique@, … ;
--   * the standard rules expressed as CEL ("Protovalidate.Rules"); and
--   * an evaluation engine ("Protovalidate.Eval") that binds each field value
--     to @this@ and its rules to @rules@, evaluates the applicable
--     constraints, and collects 'Violation's.
--
-- Messages are represented as CEL maps; "Protovalidate.Proto" bridges
-- @wireform-proto@ dynamic messages into that representation.
--
-- == Example
--
-- @
-- import Protovalidate
-- import CEL (Value (..), celMapFromList)
--
-- userRules :: MessageRules
-- userRules = messageRules
--   [ ("id",    fieldRules KString [uuid])
--   , ("age",   fieldRules KUint32 [lteV (VUInt 150)])
--   , ("email", fieldRules KString [email])
--   ]
--   [ -- message-level custom CEL
--     either (error . show) id
--       (mkConstraint "first_name_requires_last_name"
--                     "last_name must be present if first_name is present"
--                     "!has(this.first_name) || has(this.last_name)")
--   ]
--
-- validateUser :: Value -> [Violation]
-- validateUser msg = validate msg userRules
-- @
module Protovalidate
  (     -- * Validating
    validate
  , validateAt
  , validateIn
  , Violation (..)

    -- * Compiled / typed validation
  , Validator
  , compileValidator
  , compileValidatorIn
  , runValidator
  , validateValue
  , ToCel (..)
  , genericToCel

    -- * Reading rules from a @.proto@ (buf.validate annotations)
  , parseProtoRules
  , fileMessageRules
  , extractMessageRules

    -- * Reading rules from a compiled descriptor (buf.validate extension #1159)
  , fileRulesFromDescriptor
  , messageRulesFromDescriptor

    -- * Reifying rules as refinement types (refined)
  , refinedFieldType
  , refinedPredicate

    -- * Rules
  , RuleKind (..)
  , FieldRules (..)
  , MessageRules (..)
  , emptyFieldRules
  , fieldRules
  , messageRules

    -- * Rule-value builders
  , constV
  , ltV
  , lteV
  , gtV
  , gteV
  , inV
  , notInV
  , minLen
  , maxLen
  , lenV
  , minItems
  , maxItems
  , prefix
  , suffix
  , contains
  , pattern
  , email
  , hostname
  , ip
  , uri
  , uuid
  , unique
  , finite
  , definedOnly
  , oneofRequired
  , wellKnownRegex
  , mapKeys
  , mapValues
  , predefined

    -- * Constraints
  , Constraint (..)
  , mkConstraint

    -- * CEL environment / library
  , libraryEnv
  , withLibrary

    -- * Dynamic-message bridge
  , MessageSchema
  , FieldSchema (..)
  , FieldShape (..)
  , dynamicMessageToCel
  , dynamicValueToCel
  ) where

import Protovalidate.Class
  ( ToCel (..)
  , Validator
  , compileValidator
  , compileValidatorIn
  , genericToCel
  , runValidator
  , validateValue
  )
import Protovalidate.Constraint (Constraint (..), mkConstraint)
import Protovalidate.Descriptor (fileRulesFromDescriptor, messageRulesFromDescriptor)
import Protovalidate.Eval (validate, validateAt, validateIn)
import Protovalidate.Library (libraryEnv, withLibrary)
import Protovalidate.Proto
  ( FieldSchema (..)
  , FieldShape (..)
  , MessageSchema
  , dynamicMessageToCel
  , dynamicValueToCel
  )
import Protovalidate.Refined (refinedFieldType, refinedPredicate)
import Protovalidate.Rules
import Protovalidate.Schema (extractMessageRules, fileMessageRules, parseProtoRules)
import Protovalidate.Violation (Violation (..))
