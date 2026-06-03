{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
Module      : Kafka.Serde.Proto.Buf.TH
Description : Compile-time auto-population of the Buf @commit@ metadata.
Copyright   : (c) 2025
License     : BSD-3-Clause

The @message@ header of "Kafka.Serde.Proto.Buf" is already derived
automatically from the type's 'Proto.Schema.ProtoMessage' instance. The
@commit@ header, however, is a build-time constant the caller would
otherwise paste in by hand and update on every @buf push@. This module
embeds it at compile time instead, so the serde is fully auto-populated.

= Recommended usage

Splice a complete header-carrying serde straight from your @buf.lock@:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
import           Kafka.Serde.Proto.Buf.TH (bufProtoSerdeFromLock)
import qualified Kafka.Topic as Topic

import qualified My.Proto.Generated as Pb

orders :: Topic.Topic Text Pb.OrderPlaced
orders =
  Topic.topic \"orders\" keySerde
    $(bufProtoSerdeFromLock \"buf.lock\" \"buf.build/acme/payments\")
@

The splice reads @buf.lock@ at compile time, finds the @commit@ pinned
for @buf.build/acme/payments@, and expands to
@'Kafka.Serde.Proto.Buf.bufProtoSerde' commit Nothing@. The file is
registered with 'addDependentFile', so editing @buf.lock@ (e.g. after
@buf mod update@) recompiles the call site. If you only need the commit
string, use 'bufCommitFromLock' (or 'bufCommitFromEnv' to read it from a
CI-populated environment variable).

= Where the commit comes from

@buf.lock@ pins each /dependency/ module to a BSR commit, so this works
when the schemas you produce live in a BSR module you depend on. For a
local module you have not pushed yet there is no commit to embed; use
'bufCommitFromEnv' with a value your CI sets after @buf push@.

Both @buf.lock@ schema versions are understood: v2 (@name:@ +
@commit:@) and v1 (@remote:@ \/ @owner:@ \/ @repository:@ +
@commit:@). The module name to look up is the full
@<remote>/<owner>/<repository>@, e.g. @buf.build/acme/payments@.
-}
module Kafka.Serde.Proto.Buf.TH
  ( -- * Splices
    bufProtoSerdeFromLock
  , bufCommitFromLock
  , bufCommitFromEnv
    -- * Pure @buf.lock@ lookup
  , lookupBufLockCommit
  ) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Language.Haskell.TH        (Exp (..), Lit (..), Q)
import           Language.Haskell.TH.Syntax (addDependentFile, runIO)

import           System.Directory           (doesFileExist)
import           System.Environment         (lookupEnv)

import           Kafka.Serde.Proto.Buf      (bufProtoSerde)

-- | Build a complete header-carrying serde by reading the BSR commit
-- for @moduleName@ from a @buf.lock@ at compile time. Expands to
-- @'Kafka.Serde.Proto.Buf.bufProtoSerde' commit Nothing@ (the @module@
-- header is left off — it is optional and only meaningful on
-- multi-module topics; use 'bufCommitFromLock' with an explicit
-- 'Kafka.Serde.Proto.Buf.bufProtoSerde' call if you need it).
--
-- The result is polymorphic in the message type, so annotate the call
-- site (usually via the surrounding 'Kafka.Topic.Topic').
bufProtoSerdeFromLock
  :: FilePath -- ^ Path to @buf.lock@ (relative to the package root).
  -> String   -- ^ Full Buf module name, e.g. @"buf.build/acme/payments"@.
  -> Q Exp
bufProtoSerdeFromLock lockPath moduleName = do
  commitE <- bufCommitFromLock lockPath moduleName
  pure (AppE (AppE (VarE 'bufProtoSerde) commitE) (ConE 'Nothing))

-- | Splice the BSR commit (as 'Text') pinned for @moduleName@ in a
-- @buf.lock@, read at compile time. Fails compilation with a clear
-- message if the file is missing or the module is not pinned.
bufCommitFromLock :: FilePath -> String -> Q Exp
bufCommitFromLock lockPath moduleName = do
  addDependentFile lockPath
  exists <- runIO (doesFileExist lockPath)
  if not exists
    then fail $
      "Kafka.Serde.Proto.Buf.TH.bufCommitFromLock: buf.lock not found at "
        <> lockPath
    else do
      contents <- runIO (TIO.readFile lockPath)
      case lookupBufLockCommit (T.pack moduleName) contents of
        Left err -> fail ("Kafka.Serde.Proto.Buf.TH.bufCommitFromLock: " <> err)
        Right c  -> pure (textLitE c)

-- | Splice the BSR commit (as 'Text') from an environment variable read
-- at compile time — handy when CI sets it (e.g. from @buf push@ output).
-- Fails compilation if the variable is unset.
bufCommitFromEnv :: String -> Q Exp
bufCommitFromEnv var = do
  m <- runIO (lookupEnv var)
  case m of
    Nothing ->
      fail $
        "Kafka.Serde.Proto.Buf.TH.bufCommitFromEnv: environment variable "
          <> var
          <> " is not set at compile time"
    Just v -> pure (textLitE (T.pack v))

-- | @T.pack \"\<commit\>\"@ as an 'Exp'. Built by hand (rather than via a
-- typed quote) so the module only needs @TemplateHaskellQuotes@.
textLitE :: Text -> Exp
textLitE t = AppE (VarE 'T.pack) (LitE (StringL (T.unpack t)))

-- | Find the BSR commit pinned for a module in @buf.lock@ contents.
-- Pure so it can be unit-tested without the TH machinery. Understands
-- the v2 (@name:@) and v1 (@remote:@\/@owner:@\/@repository:@) shapes.
-- Returns the first matching @commit:@ or a @Left@ describing the miss.
lookupBufLockCommit
  :: Text -- ^ Full module name, e.g. @"buf.build/acme/payments"@.
  -> Text -- ^ Raw @buf.lock@ contents.
  -> Either String Text
lookupBufLockCommit target contents = go (T.lines contents) []
  where
    go [] block =
      case checkBlock block of
        Just c -> Right c
        Nothing ->
          Left $
            "commit for buf module '" <> T.unpack target
              <> "' not found in buf.lock"
    go (l : ls) block =
      case T.stripPrefix "- " (T.strip l) of
        Just firstItem ->
          case checkBlock block of
            Just c  -> Right c
            Nothing -> go ls (consKV firstItem [])
        Nothing -> go ls (consKV (T.strip l) block)

    -- Prepend the parsed key/value of a line to the current dep block,
    -- ignoring lines that are not @key: value@ pairs.
    consKV s block = case parseKV s of
      Just kv -> kv : block
      Nothing -> block

    checkBlock kvs =
      if blockName kvs == Just target
        then lookup "commit" kvs
        else Nothing

parseKV :: Text -> Maybe (Text, Text)
parseKV t =
  let (k, rest) = T.breakOn ":" t
   in if T.null rest
        then Nothing
        else Just (T.strip k, dequote (T.strip (T.drop 1 rest)))

-- | Strip a single pair of surrounding quotes, if present.
dequote :: Text -> Text
dequote t = case T.uncons t of
  Just (q, body)
    | q == '"' || q == '\'' ->
        case T.unsnoc body of
          Just (inner, q') | q' == q -> inner
          _                          -> t
  _ -> t

-- | The module name a dep block identifies: the v2 @name@, or the v1
-- @remote/owner/repository@ triple joined with @\/@.
blockName :: [(Text, Text)] -> Maybe Text
blockName kvs = case lookup "name" kvs of
  Just n -> Just n
  Nothing ->
    case (lookup "remote" kvs, lookup "owner" kvs, lookup "repository" kvs) of
      (Just r, Just o, Just rp) -> Just (r <> "/" <> o <> "/" <> rp)
      _                         -> Nothing
