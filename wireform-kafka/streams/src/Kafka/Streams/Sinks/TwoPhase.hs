{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Kafka.Streams.Sinks.TwoPhase
-- Description : Two-phase-commit sink contract + EOS integration
--
-- Riffle §1 wants a Flink-class \"sink interface\" that survives the
-- same kinds of failures the producer/consumer protocol survives:
-- a sink should be able to /prepare/ a batch (a side-effect that is
-- visible to no consumer until commit), then /commit/ exactly when
-- the upstream Kafka transaction commits, or /abort/ when it aborts.
--
-- This module defines the contract a Riffle 2PC sink must satisfy
-- and exposes the wiring needed to bolt one onto the existing
-- 'Kafka.Streams.Runtime.EOS.EOSCoordinator' commit cycle without
-- changing the shape of the cycle itself.
--
-- == Reference implementations
--
-- For deterministic in-process testing the module also ships a
-- handful of reference sinks. They have the same correctness
-- properties as the real adapter targets (JDBC / Iceberg / S3 /
-- HTTP), but live in pure IO so the chaos suite can drive them:
--
--   * 'inMemoryTwoPhaseSink' — appends rows to an 'IORef',
--     prepare-buffers them, and only flushes on commit. Used by the
--     property tests.
--   * 'filesystemTwoPhaseSink' — writes each prepared batch to a
--     @prepared/\<txn\>.batch@ file, then renames into @committed/@
--     on commit (atomic on POSIX). This mirrors what an Iceberg or
--     S3 sink would do at the manifest / object-rename layer.
--   * 'httpEchoTwoPhaseSink' — buffers each prepared batch, only
--     fires the supplied @onCommit@ callback once the upstream
--     commit succeeds. This is the shape a 2PC HTTP sink takes
--     when the target endpoint supports prepare/commit semantics.
--
-- The real JDBC / Iceberg / S3 / HTTP adapters live in separate
-- packages (e.g. @wireform-jdbc@, @wireform-iceberg@) because they
-- depend on external runtimes; this module defines the contract
-- they satisfy.
--
-- == Commit-cycle wiring
--
-- 'withTwoPhaseSinks' composes a list of 'TwoPhaseSink's into an
-- existing 'EOSCoordinator' by extending its 'storeCommit' and
-- 'storeAbort' hooks. Sinks /prepare/ during the topology's
-- pre-commit drain (a single 'preCommit' hook), commit when the
-- coordinator's 'storeCommit' fires, and abort when 'storeAbort'
-- fires. The 'EOSCoordinator' guarantees these are mutually
-- exclusive and ordered (see 'runCommitCycle').
module Kafka.Streams.Sinks.TwoPhase
  ( -- * Contract
    TwoPhaseSink (..)
  , SinkTxnId (..)
  , SinkOutcome (..)
  , RecoveryDecision (..)
  , noopTwoPhaseSink
    -- * Reference sinks
  , inMemoryTwoPhaseSink
  , InMemorySinkState
  , readCommittedRows
  , readPreparedRows
  , filesystemTwoPhaseSink
  , httpEchoTwoPhaseSink
    -- * Coordinator wiring
  , withTwoPhaseSinks
    -- * Direct driving (for tests / non-EOS callers)
  , runSinkCommitCycle
    -- * Topology compile path (used by the Free DSL Prim)
  , compileSinkTwoPhase
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , modifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  , removeFile
  , renameFile
  )
import System.FilePath ((</>))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import Kafka.Streams.Runtime.EOS
  ( EOSCoordinator (..)
  )
import qualified Kafka.Streams.KStream as KS
import Kafka.Streams.Types (Record)

----------------------------------------------------------------------
-- Contract
----------------------------------------------------------------------

-- | Identity of one in-flight sink transaction. The coordinator
-- hands a fresh id to every commit cycle so a sink can recover
-- prepared-but-not-committed state on startup.
newtype SinkTxnId = SinkTxnId { unSinkTxnId :: Text }
  deriving stock (Eq, Ord, Show)

-- | Result of a 2PC step.
data SinkOutcome
  = SinkOK
  | SinkRetryable !Text
    -- ^ The downstream system is transiently unavailable. The
    -- coordinator will treat the cycle as 'CommitAborted' and the
    -- runtime will retry on the next cycle.
  | SinkFatal !Text
    -- ^ The sink has lost consistency between the prepared batch
    -- and the downstream system in a way that the runtime can't
    -- recover from automatically. The cycle becomes 'CommitFatal'.
  deriving stock (Eq, Show)

-- | What the runtime should do with a half-committed sink txn
-- discovered on startup via 'tpsRecover'. Mirrors Flink's
-- @TwoPhaseCommitSinkFunction.recoverAndCommit@ vs
-- @recoverAndAbort@ vs "give up" trichotomy.
data RecoveryDecision
  = CommitFromToken
    -- ^ The producer transaction had committed; finish the
    -- sink txn so the downstream sees the same data.
  | AbortFromToken
    -- ^ The producer transaction had aborted; roll back the
    -- sink txn so neither side keeps the data.
  | UnknownLeaveAsIs
    -- ^ The runtime can't tell; log the token and leave the
    -- sink alone for an operator to resolve manually.
  deriving stock (Eq, Show)

-- | A sink with two-phase commit semantics, parameterised by the
-- row type @r@. The contract:
--
--   * 'tpsPrepare' is invoked during the engine's pre-commit
--     drain. The sink must record the rows but must NOT make them
--     visible to downstream consumers.
--   * 'tpsCommit' is invoked /after/ the upstream Kafka
--     transaction commit succeeds. The sink atomically makes the
--     prepared batch visible.
--   * 'tpsAbort' is invoked when the upstream transaction aborts.
--     The sink discards the prepared batch.
--   * 'tpsRecover' is invoked once at startup. The sink should
--     surface any txns it sees prepared on the downstream system
--     so the runtime can either commit or roll them back depending
--     on the committed-offset checkpoint.
--
-- The implementation is responsible for idempotence: any of the
-- four operations may be re-invoked on retry, so each should be a
-- no-op when called against an already-completed txn.
data TwoPhaseSink r = TwoPhaseSink
  { tpsName    :: !Text
    -- ^ Stable identifier (for logging / metrics / failure
    -- attribution).
  , tpsStage   :: !(r -> IO ())
    -- ^ Per-record /stage/ hook. The topology's
    -- 'Kafka.Streams.Topology.Free.SinkTwoPhase' processor
    -- calls this for every record on the stream as it arrives.
    -- The sink decides whether to buffer in memory and flush
    -- on 'tpsPrepare', or to write speculatively and verify on
    -- prepare. Reference sinks ship the buffering variant.
  , tpsPrepare :: !(SinkTxnId -> [r] -> IO SinkOutcome)
    -- ^ Transition the staged rows + the explicit @[r]@ batch
    -- to the /prepared/ state. The @[r]@ argument is the
    -- caller-side batch (used by 'withTwoPhaseSinks' when it
    -- has its own row source via @rowsFor@); pass @[]@ if all
    -- rows arrive via 'tpsStage'. Sinks must merge the two
    -- sources before preparing.
  , tpsCommit  :: !(SinkTxnId -> IO SinkOutcome)
  , tpsAbort   :: !(SinkTxnId -> IO SinkOutcome)
  , tpsRecover :: !(IO [SinkTxnId])
    -- ^ List of txns currently in the @prepared@ state on the
    -- downstream. The runtime should call this on startup, decide
    -- per-txn whether to commit or abort (the per-token decision
    -- is the sink's via 'RecoveryDecision').
  }

-- | Sink that ignores every operation. Useful as a default.
noopTwoPhaseSink :: Text -> TwoPhaseSink r
noopTwoPhaseSink nm = TwoPhaseSink
  { tpsName    = nm
  , tpsStage   = \_   -> pure ()
  , tpsPrepare = \_ _ -> pure SinkOK
  , tpsCommit  = \_   -> pure SinkOK
  , tpsAbort   = \_   -> pure SinkOK
  , tpsRecover = pure []
  }

----------------------------------------------------------------------
-- In-memory reference sink
----------------------------------------------------------------------

-- | Observable state of an 'inMemoryTwoPhaseSink'. The chaos
-- suite reads this directly to assert invariants.
data InMemorySinkState r = InMemorySinkState
  { imsPrepared  :: !(IORef (Map SinkTxnId [r]))
  , imsCommitted :: !(IORef [r])
    -- ^ Append-only history of committed rows in commit order.
  }

readCommittedRows :: InMemorySinkState r -> IO [r]
readCommittedRows = fmap reverse . readIORef . imsCommitted
-- Stored in reverse-order for O(1) prepend on commit; reverse on
-- read for caller-friendly order.

readPreparedRows :: InMemorySinkState r -> IO (Map SinkTxnId [r])
readPreparedRows = readIORef . imsPrepared

-- | A sink that buffers prepared rows in memory and atomically
-- promotes them to a committed history on commit. Aborts discard
-- the prepared batch.
inMemoryTwoPhaseSink :: Text -> IO (TwoPhaseSink r, InMemorySinkState r)
inMemoryTwoPhaseSink nm = do
  prep   <- newIORef Map.empty
  done   <- newIORef []
  staged <- newIORef ([] :: [r])
  let s = InMemorySinkState prep done
  pure
    ( TwoPhaseSink
        { tpsName    = nm
        , tpsStage   = \r ->
            atomicModifyIORef' staged (\rs -> (r : rs, ()))
        , tpsPrepare = \txn extraRows -> do
            stagedRows <- atomicModifyIORef' staged
                            (\rs -> ([], reverse rs))
            let rows = stagedRows ++ extraRows
            atomicModifyIORef' prep (\m ->
              (Map.insert txn rows m, ()))
            pure SinkOK
        , tpsCommit  = \txn -> do
            mrows <- atomicModifyIORef' prep (\m ->
              (Map.delete txn m, Map.lookup txn m))
            case mrows of
              Nothing -> pure SinkOK  -- already committed: idempotent
              Just rs -> do
                modifyIORef' done (\xs -> reverse rs ++ xs)
                pure SinkOK
        , tpsAbort   = \txn -> do
            atomicModifyIORef' prep (\m ->
              (Map.delete txn m, ()))
            pure SinkOK
        , tpsRecover = Map.keys <$> readIORef prep
        }
    , s
    )

----------------------------------------------------------------------
-- Filesystem reference sink
----------------------------------------------------------------------

-- | A sink that materialises each prepared batch as a file in
-- @\<root\>/prepared/@ and renames it to @\<root\>/committed/@ on
-- commit. The rename is atomic on POSIX, so a crash between
-- 'tpsCommit' steps leaves the system in either the "prepared" or
-- "committed" state but never half-and-half. This is structurally
-- the same protocol Iceberg uses at the manifest layer and S3
-- sinks use at the object-naming layer.
--
-- The serialiser converts each row into a 'ByteString'; the on-disk
-- format is just the rows concatenated with newline separators.
-- Real Iceberg / S3 adapters override this with their wire format.
filesystemTwoPhaseSink
  :: forall r
   . FilePath
  -> (r -> ByteString)
  -> Text
  -> IO (TwoPhaseSink r)
filesystemTwoPhaseSink root encode nm = do
  createDirectoryIfMissing True (root </> "prepared")
  createDirectoryIfMissing True (root </> "committed")
  staged <- newIORef ([] :: [r])
  pure TwoPhaseSink
    { tpsName    = nm
    , tpsStage   = \r ->
        atomicModifyIORef' staged (\rs -> (r : rs, ()))
    , tpsPrepare = \(SinkTxnId txn) extraRows -> do
        stagedRows <- atomicModifyIORef' staged
                        (\rs -> ([], reverse rs))
        let rows = stagedRows ++ extraRows
            path = root </> "prepared" </> T.unpack txn <> ".batch"
            body = BS.intercalate "\n" (map encode rows)
        r <- try (BS.writeFile path body)
        case r of
          Right () -> pure SinkOK
          Left (e :: SomeException) ->
            pure (SinkRetryable ("write prepared: " <> T.pack (show e)))
    , tpsCommit  = \(SinkTxnId txn) -> do
        let src = root </> "prepared" </> T.unpack txn <> ".batch"
            dst = root </> "committed" </> T.unpack txn <> ".batch"
        exists <- doesFileExist src
        if not exists
          then pure SinkOK  -- already committed: idempotent
          else do
            r <- try (renameFile src dst)
            case r of
              Right () -> pure SinkOK
              Left (e :: SomeException) ->
                pure (SinkRetryable ("rename: " <> T.pack (show e)))
    , tpsAbort   = \(SinkTxnId txn) -> do
        let path = root </> "prepared" </> T.unpack txn <> ".batch"
        exists <- doesFileExist path
        when exists $ do
          r <- try (removeFile path)
          case r of
            Right () -> pure ()
            Left (_ :: SomeException) -> pure ()
        pure SinkOK
    , tpsRecover = do
        prepared <- listDirectory (root </> "prepared")
        pure [SinkTxnId (T.dropEnd 6 (T.pack p)) | p <- prepared]
        -- ".batch" suffix = 6 chars; we strip it.
    }

----------------------------------------------------------------------
-- HTTP echo / deferred-callback reference sink
----------------------------------------------------------------------

-- | A sink whose 'tpsPrepare' just buffers the rows in memory and
-- whose 'tpsCommit' fires a caller-supplied @onCommit@ callback
-- with the full batch. This mirrors a 2PC HTTP sink that uses a
-- two-phase protocol over POST: prepare with @\/prepare\/{txn}@,
-- commit with @\/commit\/{txn}@.
--
-- Tests can supply an @onCommit@ that captures the rows into a
-- 'TVar' or appends to an 'IORef' to assert delivery semantics.
httpEchoTwoPhaseSink
  :: forall r
   . Text
  -> ([r] -> IO ())
  -> IO (TwoPhaseSink r)
httpEchoTwoPhaseSink nm onCommit = do
  buf    <- newIORef (Map.empty :: Map SinkTxnId [r])
  staged <- newIORef ([] :: [r])
  pure TwoPhaseSink
    { tpsName    = nm
    , tpsStage   = \r ->
        atomicModifyIORef' staged (\rs -> (r : rs, ()))
    , tpsPrepare = \txn extraRows -> do
        stagedRows <- atomicModifyIORef' staged
                        (\rs -> ([], reverse rs))
        let rows = stagedRows ++ extraRows
        atomicModifyIORef' buf (\m -> (Map.insert txn rows m, ()))
        pure SinkOK
    , tpsCommit  = \txn -> do
        mrows <- atomicModifyIORef' buf (\m ->
          (Map.delete txn m, Map.lookup txn m))
        case mrows of
          Nothing -> pure SinkOK  -- already committed
          Just rs -> do
            r <- try (onCommit rs)
            case r of
              Right () -> pure SinkOK
              Left (e :: SomeException) ->
                pure (SinkRetryable ("onCommit: " <> T.pack (show e)))
    , tpsAbort   = \txn -> do
        atomicModifyIORef' buf (\m -> (Map.delete txn m, ()))
        pure SinkOK
    , tpsRecover = Map.keys <$> readIORef buf
    }

----------------------------------------------------------------------
-- Coordinator wiring
----------------------------------------------------------------------

-- | Extend an existing 'EOSCoordinator' with a list of 2PC sinks.
-- Hooks into the spec'd 5-step commit cycle as described in
-- Riffle \xc2\xa74:
--
-- @
-- beginTxn \xe2\x86\x92 flush \xe2\x86\x92 commitOffsets
--           \xe2\x86\x92 preCommit2PC      \xe2\x86\x90 we hook here
--           \xe2\x86\x92 commitTxn
--           \xe2\x86\x92 commit2PC         \xe2\x86\x90 and here
--           \xe2\x86\x92 storeCommit
-- @
--
-- Failure semantics, end-to-end:
--
--   * 'preCommit2PC' fails \xe2\x87\x92 producer txn aborts, every sink
--     that successfully prepared also aborts ('abort2PC').
--   * 'commitTxn' fails after a successful prepare \xe2\x87\x92 same
--     story: producer aborts, sinks roll back.
--   * 'commit2PC' fails AFTER the producer txn already
--     committed \xe2\x87\x92 unrecoverable. The cycle returns
--     'CommitFatal', the in-flight 'SinkTxnId' stays in the
--     sink's prepared map, and on restart the runtime calls
--     'tpsRecover' + 'tpsCommit' / 'tpsAbort' to resolve the
--     stranded txn.
--
-- The caller supplies a @rowsFor@ closure that yields the
-- per-sink batch of rows for the current commit cycle. This is
-- typically populated by the engine's pre-commit drain (e.g. an
-- @asyncMapValues@-style operator pushes finished rows into a
-- 'TQueue' that 'rowsFor' drains and returns).
withTwoPhaseSinks
  :: forall r
   . EOSCoordinator
  -> [TwoPhaseSink r]
  -> (SinkTxnId -> Text -> IO [r])
    -- ^ Fetch the rows queued for a particular sink (identified
    -- by 'tpsName') under a particular cycle's 'SinkTxnId'.
  -> IO Text
    -- ^ Generator for fresh 'SinkTxnId' text labels. The runtime
    -- typically derives this from @applicationId-instanceId-cycle@.
  -> IO EOSCoordinator
withTwoPhaseSinks base sinks rowsFor nextTxnLabel = do
  inFlight <- newIORef (Nothing :: Maybe SinkTxnId)
  let
    prepareAll txn = do
      let go [] = pure (Right ())
          go (s : rest) = do
            rows <- rowsFor txn (tpsName s)
            o <- tpsPrepare s txn rows
            case o of
              SinkOK            -> go rest
              SinkRetryable err -> pure (Left ("prepare "
                                       <> tpsName s <> ": " <> err))
              SinkFatal err     -> pure (Left ("prepare "
                                       <> tpsName s <> ": fatal: "
                                       <> err))
      go sinks
    commitAll txn = do
      let go [] = pure (Right ())
          go (s : rest) = do
            o <- tpsCommit s txn
            case o of
              SinkOK            -> go rest
              SinkRetryable err -> pure (Left ("commit "
                                       <> tpsName s <> ": " <> err))
              SinkFatal err     -> pure (Left ("commit "
                                       <> tpsName s <> ": fatal: "
                                       <> err))
      go sinks
    abortAll txn =
      forM_ sinks (\s -> tpsAbort s txn)
    freshTxn = do
      txnLabel <- nextTxnLabel
      let !txn = SinkTxnId txnLabel
      writeIORef inFlight (Just txn)
      pure txn
    currentTxn = readIORef inFlight
    clearTxn = writeIORef inFlight Nothing

  pure base
    { preCommit2PC = do
        txn <- freshTxn
        prep <- prepareAll txn
        case prep of
          Left err -> pure (Left err)
          Right () -> do
            innerR <- base.preCommit2PC
            case innerR of
              Left e -> pure (Left e)
              Right () -> pure (Right ())
    , commit2PC = do
        mTxn <- currentTxn
        case mTxn of
          Nothing  -> base.commit2PC
          Just txn -> do
            done <- commitAll txn
            clearTxn
            case done of
              Left err -> pure (Left err)
              Right () -> base.commit2PC
    , abort2PC = do
        mTxn <- currentTxn
        case mTxn of
          Just txn -> abortAll txn
          Nothing  -> pure ()
        clearTxn
        base.abort2PC
    }

----------------------------------------------------------------------
-- Direct (no-EOS) sink driver for tests
----------------------------------------------------------------------

-- | Run a single 2PC cycle directly against a sink, for tests
-- that don't want to spin up an 'EOSCoordinator'. The supplied
-- @body@ produces the rows to prepare; the result of the upstream
-- "commit" determines whether the sink commits or aborts.
runSinkCommitCycle
  :: TwoPhaseSink r
  -> SinkTxnId
  -> IO ([r], Bool)
     -- ^ @(rows, upstreamCommitOK)@. If 'True', the sink will be
     -- committed; if 'False', aborted.
  -> IO (Either Text ())
runSinkCommitCycle sink txn body = do
  (rows, ok) <- body
  p <- tpsPrepare sink txn rows
  case p of
    SinkRetryable e -> pure (Left ("prepare retry: " <> e))
    SinkFatal e     -> pure (Left ("prepare fatal: " <> e))
    SinkOK
      | ok -> do
          c <- tpsCommit sink txn
          case c of
            SinkOK            -> pure (Right ())
            SinkRetryable e   -> pure (Left ("commit retry: " <> e))
            SinkFatal e       -> pure (Left ("commit fatal: " <> e))
      | otherwise -> do
          _ <- tpsAbort sink txn
          pure (Right ())

----------------------------------------------------------------------
-- Compile path for the SinkTwoPhase Prim
----------------------------------------------------------------------

-- | Compile a 'TwoPhaseSink' attached to a 'KStream' into a
-- foreach processor: every record on the stream gets staged via
-- 'tpsStage'. The sink's per-record buffer is consumed on the
-- next 'tpsPrepare' invocation (driven by the 'EOSCoordinator''s
-- 'preCommit2PC' hook through 'withTwoPhaseSinks').
--
-- The caller is responsible for wiring the sink into the
-- coordinator separately. The compile path here only owns the
-- per-record staging.
compileSinkTwoPhase
  :: forall k v opaque
   . opaque
    -- ^ The topology builder. The compile path doesn't need it
    -- directly — 'KS.foreachStream' picks the builder out of the
    -- 'KStream' — but the Free DSL passes it positionally, so
    -- we accept and ignore it here to keep the call shape
    -- uniform with 'Sink' \/ 'SinkExtracted'.
  -> TwoPhaseSink (Record k v)
  -> KS.KStream k v
  -> IO ()
compileSinkTwoPhase _ sink =
  KS.foreachStream (tpsStage sink)
