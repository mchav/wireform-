{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Annotated fixture types for the Thrift deriver round-trip tests.
--
-- Thrift differs from CBOR / MsgPack: fields are identified by
-- numeric IDs (Int16) rather than text-string keys, so 'rename' and
-- 'renameStyle' are ignored on Thrift wire keys. The 'tag' modifier
-- overrides the otherwise-positional default.
module Test.Thrift.Derive.Types
  ( -- * Record (mixed default + tag-overridden field IDs)
    LogEntry (..)
  , defaultRequestId
    -- * Newtype
  , RequestId (..)
    -- * Enum (with explicit tags)
  , Severity (..)
    -- * Sum (Thrift union, sequential field IDs)
  , Event (..)
  ) where

import Data.Text (Text)

import Wireform.Derive.Backend
import Wireform.Derive.Modifier

-- ---------------------------------------------------------------------------
-- Record
-- ---------------------------------------------------------------------------

data LogEntry = LogEntry
  { logTimestamp :: !Int   -- field id 1 by default
  , logMessage   :: !Text  -- field id 2 by default
  , logCode      :: !Int   -- explicit tag 7
  , logRequestId :: !Text  -- skipped under Thrift, default value
  } deriving (Eq, Show)

defaultRequestId :: Text
defaultRequestId = "<unknown>"

{-# ANN logCode      (forBackend backendThrift (tag 7)) #-}
{-# ANN logRequestId (forBackend backendThrift skip) #-}
{-# ANN logRequestId (forBackend backendThrift (defaults 'defaultRequestId)) #-}

-- ---------------------------------------------------------------------------
-- Newtype
-- ---------------------------------------------------------------------------

newtype RequestId = RequestId { unRequestId :: Int }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Enum
-- ---------------------------------------------------------------------------

data Severity = Debug | Info | Warn | Critical
  deriving (Eq, Show)

{-# ANN Critical (forBackend backendThrift (tag 99)) #-}

-- ---------------------------------------------------------------------------
-- Sum (encoded as a Thrift union)
-- ---------------------------------------------------------------------------

data Event
  = EvHeartbeat
  | EvData !Int
  | EvAlert !Text !Int
  deriving (Eq, Show)

{-# ANN EvAlert (forBackend backendThrift (tag 10)) #-}
