{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Client.Group
Description : High-level consumer-group API ("just give me my records")

The producer side of Kafka has always been the easy half — pick a
topic, hand it bytes, get an ack. The consumer side is harder because
of consumer-group coordination, rebalancing, offset management, and
the fact that "process every record once" is a surprisingly load-
bearing promise. This module hides all of that behind a small set of
high-level entry points.

= The five-second tour

The shortest-possible Kafka consumer in this library:

@
import qualified Wireform.Kafka.Group as Kafka

main :: IO ()
main =
  Kafka.runConsumer
    Kafka.defaultGroupConfig
      { Kafka.gcBootstrapBrokers = [\"broker-1:9092\", \"broker-2:9092\"]
      , Kafka.gcGroupId          = \"my-service\"
      , Kafka.gcTopics           = [\"events\"]
      }
    $ \\rec -> do
        putStrLn $ \"got \" <> show (Kafka.crKey rec) <> \" -> \" <> show (Kafka.crValue rec)
@

That's the whole API surface needed for the common case:

* connect to the cluster,
* join the group as a member of @my-service@,
* receive partition assignments and resume from the last committed
  offsets,
* fan records into the handler one at a time,
* commit offsets after each successful handler invocation,
* gracefully leave the group on @SIGINT@ / exception / clean exit.

If you want batch-at-a-time delivery (almost always the right choice
for throughput), use 'runBatchedConsumer'. If you want to drive the
loop yourself, use 'withGroupConsumer' and call 'pollOnce' / 'commit'
in your own monad.

= Configuration

'GroupConfig' wraps the underlying 'ConsumerConfig' and adds the
high-level options the bracket needs (which brokers to bootstrap from,
which topics to subscribe to, error-handling policy). 'defaultGroupConfig'
is a sensible starting point — a 5-second auto-commit interval, range
assignment, latest @auto.offset.reset@.

= Error handling

Each handler invocation runs inside a SomeException catch. The
default 'gcOnError' logs the exception to @stderr@ and re-raises it,
which terminates the loop cleanly. To keep going past a failing
record (skip-and-log semantics), set 'gcOnError = SkipRecord'. To
shut the loop down on the first error, set 'gcOnError = StopLoop'.

= Offsets

By default we commit offsets synchronously after each handler call
(i.e. after each batch in the batched API). This trades a little
throughput for "at least once with no duplicates on a clean shutdown"
semantics. Set 'gcCommitMode' to 'CommitAsync' for higher throughput
(at-least-once with no on-shutdown commit guarantee), or 'CommitManual'
to take ownership yourself.
-}
module Kafka.Client.Group
  ( -- * Configuration
    GroupConfig(..)
  , defaultGroupConfig
  , ErrorPolicy(..)
  , CommitMode(..)
    -- * High-level handlers
  , runConsumer
  , runBatchedConsumer
    -- * Bracket-style API for custom loops
  , GroupConsumer
  , withGroupConsumer
  , pollOnce
  , commit
  , currentAssignment
  , underlyingConsumer
    -- * Convenience re-exports
  , C.ConsumerRecord(..)
  , C.TopicPartition(..)
  , C.AssignmentStrategy(..)
  , C.OffsetResetStrategy(..)
    -- * Auth helpers (re-exported)
  , SASL.SaslConfig(..)
  , Scram.ScramAlgo(..)
  , Iam.AwsCredentials(..)
  , Iam.AwsCredentialsProvider(..)
  , OAuth.OAuthToken(..)
  , OAuth.OAuthTokenProvider(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( SomeException
  , bracket
  , catch
  , throwIO
  )
import Control.Monad (forM_, unless, when)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified System.IO as IO

import qualified Kafka.Client.Consumer as C
import qualified Kafka.Network.Auth.AwsMskIam as Iam
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Network.Auth.Scram as Scram
import qualified Kafka.Network.Connection as Conn
import qualified Network.TLS as TLS

-- | What to do when the user-provided handler throws.
data ErrorPolicy
  = -- | Print the exception to @stderr@ and re-raise it; the loop
    --   exits and 'runConsumer' rethrows so the caller can decide
    --   whether to restart.
    LogAndRaise
  | -- | Print the exception to @stderr@ and keep going on the next
    --   record. Offsets for the failing record are still committed
    --   (it counts as "delivered"); use a dead-letter queue or
    --   in-handler retry if you need different semantics.
    SkipRecord
  | -- | Print the exception to @stderr@ and exit the loop cleanly
    --   (does not re-raise).
    StopLoop
  | -- | Run a custom handler. Return 'True' to keep going, 'False'
    --   to stop the loop.
    CustomError !(SomeException -> IO Bool)

-- | When to commit offsets back to the broker.
data CommitMode
  = -- | After every successful handler invocation (or batch). Slowest
    --   but the safest default — guarantees at-least-once with the
    --   smallest possible duplicate window on crash.
    CommitSync
  | -- | Fire-and-forget after every successful handler invocation.
    --   Higher throughput; on a hard crash you may reprocess up to a
    --   commit-interval's worth of records.
    CommitAsync
  | -- | Don't commit at all from the loop. The caller owns offset
    --   commits via 'commit' or via the configured auto-commit timer.
    CommitManual

-- | High-level consumer configuration.
data GroupConfig = GroupConfig
  { gcBootstrapBrokers   :: ![Text]
  , gcGroupId            :: !Text
  , gcTopics             :: ![Text]
  , gcClientId           :: !Text
  , gcSessionTimeoutMs   :: !Int
  , gcMaxPollIntervalMs  :: !Int
  , gcMaxPollRecords     :: !Int
  , gcPollTimeoutMs      :: !Int
    -- ^ How long a single 'C.poll' is allowed to block server-side
    --   when there are no records yet. Defaults to 1000 ms.
  , gcAutoOffsetReset    :: !C.OffsetResetStrategy
  , gcAssignmentStrategy :: !C.AssignmentStrategy
  , gcCommitMode         :: !CommitMode
  , gcOnError            :: !ErrorPolicy
  , gcOnPollError        :: !(String -> IO ())
    -- ^ What to do when the underlying poll itself fails (network
    --   blip, broker rejection, etc.). Default: 'TIO.hPutStrLn'
    --   stderr and back off briefly. Returning normally signals
    --   "retry"; throw to stop the loop.
  , gcCloseTimeoutMs     :: !Int
  , gcUseTls             :: !Bool
    -- ^ Whether to wrap the broker connection in TLS. Defaults to
    --   'False' for local development; flip to 'True' for any
    --   production / cloud broker. AWS MSK IAM (and Confluent
    --   Cloud's PLAIN \/ OAUTHBEARER) /require/ TLS.
  , gcTlsParams          :: !(Maybe TLS.ClientParams)
    -- ^ Custom TLS parameters. When 'Nothing' but 'gcUseTls' is
    --   'True' we fall back to 'Conn.defaultTlsSettings' against the
    --   first bootstrap broker hostname (system trust store, strong
    --   ciphers, hostname verification on).
  , gcSasl               :: !(Maybe SASL.SaslConfig)
    -- ^ SASL mechanism to use after the connection is up. 'Nothing'
    --   means \"no SASL\" (i.e. the broker is configured for
    --   PLAINTEXT or SSL-only auth).
  }

-- | Sensible defaults: localhost broker, no topics yet (the caller is
-- expected to fill at least 'gcGroupId' and 'gcTopics'), 5-second
-- session timeout, sync commits.
defaultGroupConfig :: GroupConfig
defaultGroupConfig = GroupConfig
  { gcBootstrapBrokers   = ["localhost:9092"]
  , gcGroupId            = ""
  , gcTopics             = []
  , gcClientId           = "wireform-kafka"
  , gcSessionTimeoutMs   = 10000
  , gcMaxPollIntervalMs  = 300000
  , gcMaxPollRecords     = 500
  , gcPollTimeoutMs      = 1000
  , gcAutoOffsetReset    = C.Latest
  , gcAssignmentStrategy = C.RangeAssignment
  , gcCommitMode         = CommitSync
  , gcOnError            = LogAndRaise
  , gcOnPollError        = \msg -> do
      TIO.hPutStrLn IO.stderr (T.pack ("[wireform-kafka] poll error: " <> msg))
      threadDelay 250000  -- back off 250ms
  , gcCloseTimeoutMs     = 30000
  , gcUseTls             = False
  , gcTlsParams          = Nothing
  , gcSasl               = Nothing
  }

-- | Opaque handle for the bracket-style API. Wraps the raw
-- 'C.Consumer' along with the user's 'GroupConfig' so the
-- helper functions can read the right policies.
data GroupConsumer = GroupConsumer
  { gcConsumer :: !C.Consumer
  , gcConfig   :: !GroupConfig
  }

-- | Get the underlying low-level consumer for advanced use cases
-- (manual seeking, pausing partitions, etc.).
underlyingConsumer :: GroupConsumer -> C.Consumer
underlyingConsumer = gcConsumer

-- | Bracket: open the consumer, join the group, run the body, leave
-- the group + close on the way out (commits a final batch on
-- 'CommitSync', cancels the heartbeat thread, closes connections).
--
-- Throws an 'IOError' if the broker can't be reached or the
-- subscription fails — the caller can catch and retry the whole
-- bracket if needed.
withGroupConsumer
  :: GroupConfig
  -> (GroupConsumer -> IO a)
  -> IO a
withGroupConsumer cfg@GroupConfig{..} body = do
  validateConfig cfg
  bracket open close $ \gc -> do
    subResult <- C.subscribe (gcConsumer gc) gcTopics
    case subResult of
      Left err -> throwIO $ userError ("wireform-kafka: subscribe failed: " <> err)
      Right () -> body gc
  where
    open = do
      let connBase = Conn.defaultConnectionConfig
            { Conn.connUseTls      = gcUseTls
            , Conn.connTlsSettings = case gcTlsParams of
                Just p  -> Just p
                Nothing -> case gcBootstrapBrokers of
                  -- Fall back to a sensible default keyed off the
                  -- first bootstrap broker hostname. We strip the
                  -- ":port" if present.
                  (b:_) | gcUseTls ->
                    let hostOnly = T.unpack (T.takeWhile (/= ':') b)
                    in Just (Conn.defaultTlsSettings hostOnly)
                  _ -> Nothing
            , Conn.connSasl        = gcSasl
            , Conn.connClientId    = gcClientId
            }
          ccfg = C.defaultConsumerConfig
            { C.consumerClientId            = gcClientId
            , C.consumerGroupId             = gcGroupId
            , C.consumerSessionTimeoutMs    = gcSessionTimeoutMs
            , C.consumerMaxPollIntervalMs   = gcMaxPollIntervalMs
            , C.consumerMaxPollRecords      = gcMaxPollRecords
            , C.consumerAutoOffsetReset     = gcAutoOffsetReset
            , C.consumerAssignmentStrategy  = gcAssignmentStrategy
            , C.consumerAutoCommit          = case gcCommitMode of
                CommitManual -> True   -- let the broker-side timer drive it
                _            -> False  -- the loop owns commits
            , C.consumerConnectionConfig    = connBase
            }
      r <- C.createConsumer gcBootstrapBrokers gcGroupId ccfg
      case r of
        Left err  -> throwIO $ userError ("wireform-kafka: createConsumer failed: " <> err)
        Right con -> pure GroupConsumer { gcConsumer = con, gcConfig = cfg }

    close gc = C.closeConsumerWithTimeout (gcConsumer gc) gcCloseTimeoutMs

-- | A single 'C.poll' against the underlying consumer.
pollOnce :: GroupConsumer -> IO [C.ConsumerRecord]
pollOnce GroupConsumer{..} = do
  r <- C.poll gcConsumer (gcPollTimeoutMs gcConfig)
  case r of
    Right xs -> pure xs
    Left err -> do
      gcOnPollError gcConfig err
      pure []

-- | Commit current offsets according to the configured 'CommitMode'.
-- 'CommitManual' is a no-op (the caller is expected to drive their
-- own commits).
commit :: GroupConsumer -> IO ()
commit gc@GroupConsumer{gcConfig = GroupConfig{..}} =
  case gcCommitMode of
    CommitManual -> pure ()
    CommitAsync  -> do
      r <- C.commitAsync (gcConsumer gc)
      case r of
        Right () -> pure ()
        Left err -> gcOnPollError ("commitAsync: " <> err)
    CommitSync -> do
      r <- C.commitSync (gcConsumer gc)
      case r of
        Right () -> pure ()
        Left err -> gcOnPollError ("commitSync: " <> err)

-- | Currently-assigned partitions for this consumer. Useful for
-- rebalance-aware logging.
currentAssignment :: GroupConsumer -> IO [C.TopicPartition]
currentAssignment GroupConsumer{..} = C.assignment gcConsumer

-- | Run a consumer loop: call the handler once per record, commit
-- after each (per the configured 'CommitMode'), back off briefly when
-- the broker hands us nothing, exit cleanly on graceful close.
--
-- The bracket guarantees we leave the group + close connections + run
-- a final commit (in 'CommitSync' mode) when the body returns or
-- throws.
runConsumer
  :: GroupConfig
  -> (C.ConsumerRecord -> IO ())
  -> IO ()
runConsumer cfg handler =
  withGroupConsumer cfg $ \gc -> do
    keepGoing <- newIORef True
    loop gc keepGoing
  where
    loop gc keepGoing = do
      go <- readIORef keepGoing
      when go $ do
        records <- pollOnce gc
        case records of
          [] -> do
            -- Nothing right now; tiny back-off so we don't spin.
            threadDelay 50000  -- 50 ms
            loop gc keepGoing
          rs -> do
            anyHandled <- newIORef False
            forM_ rs $ \rec -> do
              keep <- runHandler (gcConfig gc) handler rec
              writeIORef anyHandled True
              unless keep $ writeIORef keepGoing False
            commit gc
            loop gc keepGoing

-- | Same as 'runConsumer' but the handler receives a whole batch at a
-- time (whatever 'C.poll' returned). Use this for higher-throughput
-- workloads where per-record work amortises well.
--
-- The whole batch is treated as one commit unit: if the handler
-- throws, no offsets are committed for that batch. The 'ErrorPolicy'
-- still controls whether the loop stops.
runBatchedConsumer
  :: GroupConfig
  -> (V.Vector C.ConsumerRecord -> IO ())
  -> IO ()
runBatchedConsumer cfg handler =
  withGroupConsumer cfg $ \gc -> do
    keepGoing <- newIORef True
    loop gc keepGoing
  where
    loop gc keepGoing = do
      go <- readIORef keepGoing
      when go $ do
        records <- pollOnce gc
        case records of
          [] -> do
            threadDelay 50000
            loop gc keepGoing
          rs -> do
            outcome <- (Right <$> handler (V.fromList rs))
                        `catch` \(e :: SomeException) -> pure (Left e)
            case outcome of
              Right () -> commit gc >> loop gc keepGoing
              Left e   -> do
                keep <- handleError (gcConfig gc) e
                if keep
                  then loop gc keepGoing
                  else pure ()

runHandler
  :: GroupConfig
  -> (C.ConsumerRecord -> IO ())
  -> C.ConsumerRecord
  -> IO Bool
runHandler GroupConfig{gcOnError = policy} h rec =
  (h rec >> pure True) `catch` \(e :: SomeException) ->
    handlePolicy policy e

handleError :: GroupConfig -> SomeException -> IO Bool
handleError GroupConfig{gcOnError = policy} = handlePolicy policy

handlePolicy :: ErrorPolicy -> SomeException -> IO Bool
handlePolicy policy e = case policy of
  LogAndRaise -> do
    logErr e
    throwIO e
  SkipRecord -> do
    logErr e
    pure True
  StopLoop -> do
    logErr e
    pure False
  CustomError k -> k e
  where
    logErr ex = TIO.hPutStrLn IO.stderr $ T.pack
      ("[wireform-kafka] handler exception: " <> show ex)

-- | Cheap sanity checks before we even open a network connection.
validateConfig :: GroupConfig -> IO ()
validateConfig GroupConfig{..} = do
  when (null gcBootstrapBrokers) $
    throwIO $ userError "wireform-kafka: gcBootstrapBrokers must be non-empty"
  when (T.null gcGroupId) $
    throwIO $ userError "wireform-kafka: gcGroupId must be non-empty"
  when (null gcTopics) $
    throwIO $ userError "wireform-kafka: gcTopics must be non-empty"
