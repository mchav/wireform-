{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | Apache Hudi timeline reader (skeleton).
--
-- Hudi tables sit on top of Parquet files plus a /timeline/
-- of @.hoodie/@ instant files. Each file is named
-- @<instantTime>.<action>.<state>@ where:
--
--   * @action@ is @commit@, @deltacommit@, @clean@,
--     @compaction@, @rollback@, @savepoint@, @restore@, or
--     @replacecommit@.
--   * @state@ is @requested@, @inflight@, or @completed@.
--
-- The completed instants describe which Parquet files
-- (Copy-on-Write tables) or Avro/Parquet log files
-- (Merge-on-Read tables) form the table's current view.
--
-- This module is a /skeleton/ exposing the action / state
-- enums and an instant-file name parser; full plumbing
-- (reading the JSON / Avro instant payloads, joining with
-- file slices, resolving record-level merges) is a follow-up.
module Hudi.Timeline
  ( Action (..)
  , State (..)
  , Instant (..)
  , parseInstantFileName
  ) where

import Data.Text (Text)
import qualified Data.Text as T

data Action
  = Commit
  | DeltaCommit
  | Clean
  | Compaction
  | Rollback
  | Savepoint
  | Restore
  | ReplaceCommit
  deriving (Show, Eq, Enum, Bounded)

data State
  = Requested
  | Inflight
  | Completed
  deriving (Show, Eq, Enum, Bounded)

-- | One entry in the @.hoodie/@ timeline.
data Instant = Instant
  { instantTime   :: !Text
    -- ^ Sortable timestamp string (typically
    -- @yyyyMMddHHmmssSSS@), used as the instant's id.
  , instantAction :: !Action
  , instantState  :: !State
  } deriving (Show, Eq)

-- | Parse a @.hoodie/@ instant file name into its components.
-- Returns 'Nothing' if the name doesn't match the
-- @<time>.<action>.<state>@ shape.
--
-- @
-- parseInstantFileName \"20240106120000000.commit.completed\"
--   == Just (Instant \"20240106120000000\" Commit Completed)
-- @
parseInstantFileName :: Text -> Maybe Instant
parseInstantFileName t = case T.splitOn "." t of
  [time, action, state] ->
    Instant time
      <$> parseAction action
      <*> parseState state
  _ -> Nothing

parseAction :: Text -> Maybe Action
parseAction = \case
  "commit"        -> Just Commit
  "deltacommit"   -> Just DeltaCommit
  "clean"         -> Just Clean
  "compaction"    -> Just Compaction
  "rollback"      -> Just Rollback
  "savepoint"     -> Just Savepoint
  "restore"       -> Just Restore
  "replacecommit" -> Just ReplaceCommit
  _               -> Nothing

parseState :: Text -> Maybe State
parseState = \case
  "requested" -> Just Requested
  "inflight"  -> Just Inflight
  "completed" -> Just Completed
  _           -> Nothing
