{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Kafka.Client.Env
Description : Read standard Kafka @KAFKA_*@ environment variables
Copyright   : (c) 2025
License     : BSD-3-Clause
Maintainer  : kafka-native

This module lets a 'Conn.ConnectionConfig' / 'P.ProducerConfig' /
'C.ConsumerConfig' pick up the standard @KAFKA_*@ environment
variables that the broader Kafka ecosystem (Confluent's Docker
images, @kcat@, the various librdkafka-based language bindings,
@confluentinc/kafka-python@, the Java client when launched from
the @kafka-console-*@ scripts, …) has converged on.

The convention is: take the librdkafka / JVM @CONFIGURATION.md@
property name (e.g. @bootstrap.servers@), uppercase it, replace
each @.@ with @_@, and prefix with @KAFKA_@. So

    bootstrap.servers       -> KAFKA_BOOTSTRAP_SERVERS
    client.id               -> KAFKA_CLIENT_ID
    security.protocol       -> KAFKA_SECURITY_PROTOCOL
    sasl.mechanism          -> KAFKA_SASL_MECHANISM
    sasl.username           -> KAFKA_SASL_USERNAME
    sasl.password           -> KAFKA_SASL_PASSWORD
    group.id                -> KAFKA_GROUP_ID
    auto.offset.reset       -> KAFKA_AUTO_OFFSET_RESET
    enable.auto.commit      -> KAFKA_ENABLE_AUTO_COMMIT
    compression.type        -> KAFKA_COMPRESSION_TYPE
    acks                    -> KAFKA_ACKS
    enable.idempotence      -> KAFKA_ENABLE_IDEMPOTENCE
    transactional.id        -> KAFKA_TRANSACTIONAL_ID
    …

The module has two layers:

  * 'parseKafkaEnv' takes a pure @Text -> Maybe Text@ lookup and
    returns a typed 'KafkaEnv' (or a list of parse errors). This
    is the layer the tests drive directly; nothing in here
    touches @IO@.
  * 'loadKafkaEnv' / 'applyKafkaEnv*' / 'consumerConfigFromEnv'
    /etc/ are the @IO@ wrappers that consult
    'System.Environment.getEnvironment'.

The overlay functions /never overwrite a field with a default/:
they only touch a field when the corresponding environment
variable is set, so the caller can compose env on top of any
'Conn.defaultConnectionConfig' \/ 'P.defaultProducerConfig' \/
'C.defaultConsumerConfig' starting point (or on top of an
already-customised config) and get the obvious "env-wins-over-
code-defaults" precedence.

= SASL caveats

@KAFKA_SECURITY_PROTOCOL@ accepts @PLAINTEXT@, @SSL@,
@SASL_PLAINTEXT@, @SASL_SSL@. When the protocol asks for SASL,
@KAFKA_SASL_MECHANISM@ is required. For the credential-based
mechanisms (@PLAIN@, @SCRAM-SHA-256@, @SCRAM-SHA-512@) we
construct a 'SASL.SaslConfig' from @KAFKA_SASL_USERNAME@ /
@KAFKA_SASL_PASSWORD@.

@OAUTHBEARER@, @AWS_MSK_IAM@, and @GSSAPI@ can't be set up from
flat env vars alone (they need a token provider /
credentials-provider callback / Kerberos ticket cache) so we
emit a 'ConfigError' pointing the caller at the corresponding
@SASL.Sasl*@ constructor.

= TLS caveats

When the protocol implies TLS (@SSL@, @SASL_SSL@) we set
'Conn.connUseTls' to 'True' and, if 'Conn.connTlsSettings' was
unset, install 'Conn.defaultTlsSettings' against the first
bootstrap broker host parsed from @KAFKA_BOOTSTRAP_SERVERS@.
If the caller has already filled in 'Conn.connTlsSettings' (e.g.
with a pinned CA bundle) we leave it alone. If there are no
bootstrap brokers in the env we leave TLS settings unset; the
caller is then expected to either set them programmatically or
let the connect attempt surface "TLS enabled but no TLS settings
provided".
-}
module Kafka.Client.Env
  ( -- * Typed env snapshot
    KafkaEnv (..)
  , emptyKafkaEnv
  , SecurityProtocol (..)
  , SaslMechanism (..)
  , EnvAcks (..)
  , EnvOffsetReset (..)
  , EnvIsolationLevel (..)
  , EnvAssignmentStrategy (..)
    -- * Parsing
  , parseKafkaEnv
  , parseKafkaEnvList
    -- * IO entry points
  , loadKafkaEnv
  , bootstrapServersFromEnv
    -- * Overlay onto an existing connection config
    -- (Producer- and Consumer-specific overlays live alongside
    -- the corresponding config types in
    -- "Kafka.Client.Producer" and "Kafka.Client.Consumer" to
    -- keep this module free of an import cycle.)
  , applyKafkaEnvToConnectionConfig
  , connectionConfigFromEnv
    -- * Internals exposed for tests
  , parseBool
  , parseAcks
  , parseCompression
  , parseSecurityProtocol
  , parseSaslMechanism
  , parseAutoOffsetReset
  , parseIsolationLevel
  , parseAssignmentStrategy
  , parseBrokerAddressFamily
  , parseDnsLookupMode
  , parseBootstrapServers
  ) where

import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified System.Environment as Env

import Kafka.Client.ConfigValidation (ConfigError (..))
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.Scram as Scram
import qualified Kafka.Network.Connection as Conn

------------------------------------------------------------------------
-- Typed snapshot of the relevant env vars
------------------------------------------------------------------------

-- | @security.protocol@ — controls TLS + whether a SASL handshake
-- runs after the TCP handshake.
data SecurityProtocol
  = SecPlaintext
  | SecSsl
  | SecSaslPlaintext
  | SecSaslSsl
  deriving (Eq, Show)

-- | @sasl.mechanism@ — the SASL mechanism we negotiate with the
-- broker. Mirrors 'SASL.SaslMechanismName' but covers the two
-- mechanisms that can be /fully/ configured from flat env vars
-- (PLAIN, SCRAM-*) plus tags for the three that can't, so we can
-- give a precise error pointing the caller at the right
-- constructor.
data SaslMechanism
  = MechPlain
  | MechScramSha256
  | MechScramSha512
  | MechOAuthBearer
  | MechAwsMskIam
  | MechGssapi
  deriving (Eq, Show)

-- | Acks setting parsed from @KAFKA_ACKS@. Kept neutral (rather
-- than as a 'Kafka.Client.Producer.DeliveryGuarantee') so the
-- Env module doesn't pull the high-level Producer module into
-- its import graph; the conversion to 'DeliveryGuarantee'
-- happens in 'Kafka.Client.Producer.applyKafkaEnvToProducerConfig'.
data EnvAcks
  = EnvAcksZero        -- ^ acks=0 (fire-and-forget; 'AtMostOnce')
  | EnvAcksOne         -- ^ acks=1 (leader ACK; 'AtLeastOnce')
  | EnvAcksAll         -- ^ acks=-1\/all (all ISRs; 'ExactlyOnce')
  deriving (Eq, Show)

-- | @auto.offset.reset@ parsed value. Mirrors
-- 'Kafka.Client.Consumer.OffsetResetStrategy' but lives here so
-- Env stays consumer-agnostic.
data EnvOffsetReset
  = EnvOffsetEarliest
  | EnvOffsetLatest
  | EnvOffsetNone
  deriving (Eq, Show)

-- | @isolation.level@ parsed value.
data EnvIsolationLevel
  = EnvReadUncommitted
  | EnvReadCommitted
  deriving (Eq, Show)

-- | @partition.assignment.strategy@ parsed value. The two
-- JVM-only "cooperative-sticky" variants collapse onto
-- 'EnvAssignSticky' since the underlying client supports a
-- single sticky path.
data EnvAssignmentStrategy
  = EnvAssignRange
  | EnvAssignRoundRobin
  | EnvAssignSticky
  deriving (Eq, Show)

-- | A typed view of every env var this module recognises. All
-- fields are 'Maybe': absent means "no override, keep the
-- existing config value".
data KafkaEnv = KafkaEnv
  { -- Connection / global
    envBootstrapServers              :: !(Maybe [Text])
  , envClientId                      :: !(Maybe Text)
  , envSecurityProtocol              :: !(Maybe SecurityProtocol)
  , envSaslMechanism                 :: !(Maybe SaslMechanism)
  , envSaslUsername                  :: !(Maybe Text)
  , envSaslPassword                  :: !(Maybe Text)
  , envSaslOAuthBearerToken          :: !(Maybe Text)
  , envRequestTimeoutMs              :: !(Maybe Int)
  , envSocketTimeoutMs               :: !(Maybe Int)
  , envSocketKeepaliveEnable         :: !(Maybe Bool)
  , envSocketNagleDisable            :: !(Maybe Bool)
  , envSocketSendBufferBytes         :: !(Maybe Int)
  , envSocketReceiveBufferBytes      :: !(Maybe Int)
  , envReconnectBackoffMs            :: !(Maybe Int)
  , envReconnectBackoffMaxMs         :: !(Maybe Int)
  , envConnectionsMaxIdleMs          :: !(Maybe Int)
  , envMessageMaxBytes               :: !(Maybe Int)
  , envReceiveMessageMaxBytes        :: !(Maybe Int)
  , envMetadataMaxAgeMs              :: !(Maybe Int)
  , envBrokerAddressFamily           :: !(Maybe Conn.BrokerAddressFamily)
  , envClientDnsLookup               :: !(Maybe Conn.DnsLookupMode)
    -- Producer
  , envAcks                          :: !(Maybe EnvAcks)
  , envCompressionType               :: !(Maybe Compression.CompressionCodec)
  , envCompressionLevel              :: !(Maybe Int)
  , envBatchSize                     :: !(Maybe Int)
  , envLingerMs                      :: !(Maybe Int)
  , envMaxInFlightRequestsPerConn    :: !(Maybe Int)
  , envRetries                       :: !(Maybe Int)
  , envRetryBackoffMs                :: !(Maybe Int)
  , envRetryBackoffMaxMs             :: !(Maybe Int)
  , envDeliveryTimeoutMs             :: !(Maybe Int)
  , envMaxRequestSize                :: !(Maybe Int)
  , envEnableIdempotence             :: !(Maybe Bool)
  , envTransactionalId               :: !(Maybe Text)
  , envTransactionTimeoutMs          :: !(Maybe Int)
    -- Consumer
  , envGroupId                       :: !(Maybe Text)
  , envGroupInstanceId               :: !(Maybe Text)
  , envEnableAutoCommit              :: !(Maybe Bool)
  , envAutoCommitIntervalMs          :: !(Maybe Int)
  , envAutoOffsetReset               :: !(Maybe EnvOffsetReset)
  , envSessionTimeoutMs              :: !(Maybe Int)
  , envHeartbeatIntervalMs           :: !(Maybe Int)
  , envMaxPollRecords                :: !(Maybe Int)
  , envMaxPollIntervalMs             :: !(Maybe Int)
  , envIsolationLevel                :: !(Maybe EnvIsolationLevel)
  , envFetchMinBytes                 :: !(Maybe Int)
  , envFetchMaxBytes                 :: !(Maybe Int)
  , envFetchMaxWaitMs                :: !(Maybe Int)
  , envFetchMessageMaxBytes          :: !(Maybe Int)
  , envClientRack                    :: !(Maybe Text)
  , envPartitionAssignmentStrategy   :: !(Maybe EnvAssignmentStrategy)
  , envCheckCrcs                     :: !(Maybe Bool)
  } deriving (Eq, Show)

-- | Snapshot with every field unset. Useful as a base for tests
-- and for callers that want to construct a 'KafkaEnv' by hand
-- (e.g. pulling overrides from a non-env source).
emptyKafkaEnv :: KafkaEnv
emptyKafkaEnv = KafkaEnv
  { envBootstrapServers              = Nothing
  , envClientId                      = Nothing
  , envSecurityProtocol              = Nothing
  , envSaslMechanism                 = Nothing
  , envSaslUsername                  = Nothing
  , envSaslPassword                  = Nothing
  , envSaslOAuthBearerToken          = Nothing
  , envRequestTimeoutMs              = Nothing
  , envSocketTimeoutMs               = Nothing
  , envSocketKeepaliveEnable         = Nothing
  , envSocketNagleDisable            = Nothing
  , envSocketSendBufferBytes         = Nothing
  , envSocketReceiveBufferBytes      = Nothing
  , envReconnectBackoffMs            = Nothing
  , envReconnectBackoffMaxMs         = Nothing
  , envConnectionsMaxIdleMs          = Nothing
  , envMessageMaxBytes               = Nothing
  , envReceiveMessageMaxBytes        = Nothing
  , envMetadataMaxAgeMs              = Nothing
  , envBrokerAddressFamily           = Nothing
  , envClientDnsLookup               = Nothing
  , envAcks                          = Nothing
  , envCompressionType               = Nothing
  , envCompressionLevel              = Nothing
  , envBatchSize                     = Nothing
  , envLingerMs                      = Nothing
  , envMaxInFlightRequestsPerConn    = Nothing
  , envRetries                       = Nothing
  , envRetryBackoffMs                = Nothing
  , envRetryBackoffMaxMs             = Nothing
  , envDeliveryTimeoutMs             = Nothing
  , envMaxRequestSize                = Nothing
  , envEnableIdempotence             = Nothing
  , envTransactionalId               = Nothing
  , envTransactionTimeoutMs          = Nothing
  , envGroupId                       = Nothing
  , envGroupInstanceId               = Nothing
  , envEnableAutoCommit              = Nothing
  , envAutoCommitIntervalMs          = Nothing
  , envAutoOffsetReset               = Nothing
  , envSessionTimeoutMs              = Nothing
  , envHeartbeatIntervalMs           = Nothing
  , envMaxPollRecords                = Nothing
  , envMaxPollIntervalMs             = Nothing
  , envIsolationLevel                = Nothing
  , envFetchMinBytes                 = Nothing
  , envFetchMaxBytes                 = Nothing
  , envFetchMaxWaitMs                = Nothing
  , envFetchMessageMaxBytes          = Nothing
  , envClientRack                    = Nothing
  , envPartitionAssignmentStrategy   = Nothing
  , envCheckCrcs                     = Nothing
  }

------------------------------------------------------------------------
-- Parsing helpers
------------------------------------------------------------------------

-- | Lookup table from @KAFKA_*@ variable name to handler. Each
-- handler receives the (already-trimmed) value and either fills
-- a field of the accumulator or appends a 'ConfigError'.
type FieldParser = Text -> KafkaEnv -> Either ConfigError KafkaEnv

-- | Parse a flat list of @(name, value)@ pairs into a 'KafkaEnv'.
-- Pairs whose @name@ is not in the recognised set are ignored
-- silently — we don't want to fail when the process environment
-- contains unrelated variables that happen to start with @KAFKA_@
-- (Confluent's Docker images set dozens of broker-side
-- @KAFKA_*@ variables that are meaningless to a client).
parseKafkaEnvList :: [(Text, Text)] -> Either [ConfigError] KafkaEnv
parseKafkaEnvList = parseKafkaEnv . tableLookup
  where
    tableLookup pairs =
      let m = Map.fromList (fmap (\(k, v) -> (T.toUpper k, v)) pairs)
      in \k -> Map.lookup (T.toUpper k) m

-- | Pure parser: takes a lookup function (typically
-- @Map.lookup . normalise@) and produces either a populated
-- 'KafkaEnv' or every parse error encountered. The lookup
-- function should return 'Nothing' for unset variables and
-- 'Just' the raw value (we strip surrounding whitespace
-- ourselves).
parseKafkaEnv :: (Text -> Maybe Text) -> Either [ConfigError] KafkaEnv
parseKafkaEnv look0 =
  let look name = case look0 name of
        Nothing -> Nothing
        Just t  ->
          let trimmed = T.strip t
          in if T.null trimmed then Nothing else Just trimmed
      go (envAcc, errAcc) (name, parser) = case look name of
        Nothing -> (envAcc, errAcc)
        Just v  -> case parser v envAcc of
          Right env' -> (env', errAcc)
          Left  err  -> (envAcc, err : errAcc)
      (finalEnv, finalErrs) = foldl go (emptyKafkaEnv, []) fieldParsers
  in case reverse finalErrs of
       []   -> Right finalEnv
       errs -> Left errs

-- | The full mapping from @KAFKA_*@ env var name to the handler
-- that decodes its value and slots it into the accumulator. New
-- env vars get added here; that keeps the table next to the
-- handler logic and out of the user-facing API.
fieldParsers :: [(Text, FieldParser)]
fieldParsers =
  [ ("KAFKA_BOOTSTRAP_SERVERS", \v env ->
      case parseBootstrapServers v of
        Left e  -> Left (ConfigError "bootstrap.servers" (T.pack e))
        Right s -> Right env { envBootstrapServers = Just s })
  , ("KAFKA_CLIENT_ID", textField "client.id" (\v env -> env { envClientId = Just v }))
  , ("KAFKA_SECURITY_PROTOCOL", \v env ->
      case parseSecurityProtocol v of
        Left e  -> Left (ConfigError "security.protocol" (T.pack e))
        Right s -> Right env { envSecurityProtocol = Just s })
  , ("KAFKA_SASL_MECHANISM", \v env ->
      case parseSaslMechanism v of
        Left e  -> Left (ConfigError "sasl.mechanism" (T.pack e))
        Right s -> Right env { envSaslMechanism = Just s })
  , ("KAFKA_SASL_USERNAME", textField "sasl.username"
      (\v env -> env { envSaslUsername = Just v }))
    -- @sasl.jaas.config@ in the JVM client is the historical way
    -- to pass username/password; we expose a flat alias.
  , ("KAFKA_SASL_PLAIN_USERNAME", textField "sasl.username"
      (\v env -> env { envSaslUsername = Just v }))
  , ("KAFKA_SASL_PASSWORD", textField "sasl.password"
      (\v env -> env { envSaslPassword = Just v }))
  , ("KAFKA_SASL_PLAIN_PASSWORD", textField "sasl.password"
      (\v env -> env { envSaslPassword = Just v }))
  , ("KAFKA_SASL_OAUTH_BEARER_TOKEN", textField "sasl.oauthbearer.token"
      (\v env -> env { envSaslOAuthBearerToken = Just v }))
  , ("KAFKA_REQUEST_TIMEOUT_MS", intField "request.timeout.ms"
      (\n env -> env { envRequestTimeoutMs = Just n }))
  , ("KAFKA_SOCKET_TIMEOUT_MS", intField "socket.timeout.ms"
      (\n env -> env { envSocketTimeoutMs = Just n }))
  , ("KAFKA_SOCKET_KEEPALIVE_ENABLE", boolField "socket.keepalive.enable"
      (\b env -> env { envSocketKeepaliveEnable = Just b }))
  , ("KAFKA_SOCKET_NAGLE_DISABLE", boolField "socket.nagle.disable"
      (\b env -> env { envSocketNagleDisable = Just b }))
  , ("KAFKA_SOCKET_SEND_BUFFER_BYTES", intField "socket.send.buffer.bytes"
      (\n env -> env { envSocketSendBufferBytes = Just n }))
  , ("KAFKA_SOCKET_RECEIVE_BUFFER_BYTES", intField "socket.receive.buffer.bytes"
      (\n env -> env { envSocketReceiveBufferBytes = Just n }))
  , ("KAFKA_RECONNECT_BACKOFF_MS", intField "reconnect.backoff.ms"
      (\n env -> env { envReconnectBackoffMs = Just n }))
  , ("KAFKA_RECONNECT_BACKOFF_MAX_MS", intField "reconnect.backoff.max.ms"
      (\n env -> env { envReconnectBackoffMaxMs = Just n }))
  , ("KAFKA_CONNECTIONS_MAX_IDLE_MS", intField "connections.max.idle.ms"
      (\n env -> env { envConnectionsMaxIdleMs = Just n }))
  , ("KAFKA_MESSAGE_MAX_BYTES", intField "message.max.bytes"
      (\n env -> env { envMessageMaxBytes = Just n }))
  , ("KAFKA_RECEIVE_MESSAGE_MAX_BYTES", intField "receive.message.max.bytes"
      (\n env -> env { envReceiveMessageMaxBytes = Just n }))
  , ("KAFKA_METADATA_MAX_AGE_MS", intField "metadata.max.age.ms"
      (\n env -> env { envMetadataMaxAgeMs = Just n }))
  , ("KAFKA_BROKER_ADDRESS_FAMILY", \v env ->
      case parseBrokerAddressFamily v of
        Left e  -> Left (ConfigError "broker.address.family" (T.pack e))
        Right f -> Right env { envBrokerAddressFamily = Just f })
  , ("KAFKA_CLIENT_DNS_LOOKUP", \v env ->
      case parseDnsLookupMode v of
        Left e  -> Left (ConfigError "client.dns.lookup" (T.pack e))
        Right d -> Right env { envClientDnsLookup = Just d })
    -- Producer
  , ("KAFKA_ACKS", \v env ->
      case parseAcks v of
        Left e  -> Left (ConfigError "acks" (T.pack e))
        Right a -> Right env { envAcks = Just a })
  , ("KAFKA_COMPRESSION_TYPE", \v env ->
      case parseCompression v of
        Left e  -> Left (ConfigError "compression.type" (T.pack e))
        Right c -> Right env { envCompressionType = Just c })
  , ("KAFKA_COMPRESSION_LEVEL", intField "compression.level"
      (\n env -> env { envCompressionLevel = Just n }))
  , ("KAFKA_BATCH_SIZE", intField "batch.size"
      (\n env -> env { envBatchSize = Just n }))
  , ("KAFKA_LINGER_MS", intField "linger.ms"
      (\n env -> env { envLingerMs = Just n }))
  , ("KAFKA_MAX_IN_FLIGHT_REQUESTS_PER_CONNECTION",
      intField "max.in.flight.requests.per.connection"
        (\n env -> env { envMaxInFlightRequestsPerConn = Just n }))
  , ("KAFKA_RETRIES", intField "retries"
      (\n env -> env { envRetries = Just n }))
  , ("KAFKA_RETRY_BACKOFF_MS", intField "retry.backoff.ms"
      (\n env -> env { envRetryBackoffMs = Just n }))
  , ("KAFKA_RETRY_BACKOFF_MAX_MS", intField "retry.backoff.max.ms"
      (\n env -> env { envRetryBackoffMaxMs = Just n }))
  , ("KAFKA_DELIVERY_TIMEOUT_MS", intField "delivery.timeout.ms"
      (\n env -> env { envDeliveryTimeoutMs = Just n }))
  , ("KAFKA_MAX_REQUEST_SIZE", intField "max.request.size"
      (\n env -> env { envMaxRequestSize = Just n }))
  , ("KAFKA_ENABLE_IDEMPOTENCE", boolField "enable.idempotence"
      (\b env -> env { envEnableIdempotence = Just b }))
  , ("KAFKA_TRANSACTIONAL_ID", textField "transactional.id"
      (\v env -> env { envTransactionalId = Just v }))
  , ("KAFKA_TRANSACTION_TIMEOUT_MS", intField "transaction.timeout.ms"
      (\n env -> env { envTransactionTimeoutMs = Just n }))
    -- Consumer
  , ("KAFKA_GROUP_ID", textField "group.id"
      (\v env -> env { envGroupId = Just v }))
  , ("KAFKA_GROUP_INSTANCE_ID", textField "group.instance.id"
      (\v env -> env { envGroupInstanceId = Just v }))
  , ("KAFKA_ENABLE_AUTO_COMMIT", boolField "enable.auto.commit"
      (\b env -> env { envEnableAutoCommit = Just b }))
  , ("KAFKA_AUTO_COMMIT_INTERVAL_MS", intField "auto.commit.interval.ms"
      (\n env -> env { envAutoCommitIntervalMs = Just n }))
  , ("KAFKA_AUTO_OFFSET_RESET", \v env ->
      case parseAutoOffsetReset v of
        Left e  -> Left (ConfigError "auto.offset.reset" (T.pack e))
        Right r -> Right env { envAutoOffsetReset = Just r })
  , ("KAFKA_SESSION_TIMEOUT_MS", intField "session.timeout.ms"
      (\n env -> env { envSessionTimeoutMs = Just n }))
  , ("KAFKA_HEARTBEAT_INTERVAL_MS", intField "heartbeat.interval.ms"
      (\n env -> env { envHeartbeatIntervalMs = Just n }))
  , ("KAFKA_MAX_POLL_RECORDS", intField "max.poll.records"
      (\n env -> env { envMaxPollRecords = Just n }))
  , ("KAFKA_MAX_POLL_INTERVAL_MS", intField "max.poll.interval.ms"
      (\n env -> env { envMaxPollIntervalMs = Just n }))
  , ("KAFKA_ISOLATION_LEVEL", \v env ->
      case parseIsolationLevel v of
        Left e  -> Left (ConfigError "isolation.level" (T.pack e))
        Right i -> Right env { envIsolationLevel = Just i })
  , ("KAFKA_FETCH_MIN_BYTES", intField "fetch.min.bytes"
      (\n env -> env { envFetchMinBytes = Just n }))
  , ("KAFKA_FETCH_MAX_BYTES", intField "fetch.max.bytes"
      (\n env -> env { envFetchMaxBytes = Just n }))
  , ("KAFKA_FETCH_MAX_WAIT_MS", intField "fetch.wait.max.ms"
      (\n env -> env { envFetchMaxWaitMs = Just n }))
  , ("KAFKA_FETCH_WAIT_MAX_MS", intField "fetch.wait.max.ms"
      (\n env -> env { envFetchMaxWaitMs = Just n }))
  , ("KAFKA_MAX_PARTITION_FETCH_BYTES", intField "max.partition.fetch.bytes"
      (\n env -> env { envFetchMessageMaxBytes = Just n }))
  , ("KAFKA_FETCH_MESSAGE_MAX_BYTES", intField "fetch.message.max.bytes"
      (\n env -> env { envFetchMessageMaxBytes = Just n }))
  , ("KAFKA_CLIENT_RACK", textField "client.rack"
      (\v env -> env { envClientRack = Just v }))
  , ("KAFKA_PARTITION_ASSIGNMENT_STRATEGY", \v env ->
      case parseAssignmentStrategy v of
        Left e  -> Left (ConfigError "partition.assignment.strategy" (T.pack e))
        Right s -> Right env { envPartitionAssignmentStrategy = Just s })
  , ("KAFKA_CHECK_CRCS", boolField "check.crcs"
      (\b env -> env { envCheckCrcs = Just b }))
  ]

textField :: Text -> (Text -> KafkaEnv -> KafkaEnv) -> FieldParser
textField _field setter v env = Right (setter v env)

intField :: Text -> (Int -> KafkaEnv -> KafkaEnv) -> FieldParser
intField field setter v env = case parseInt v of
  Left  e -> Left (ConfigError field (T.pack e))
  Right n -> Right (setter n env)

boolField :: Text -> (Bool -> KafkaEnv -> KafkaEnv) -> FieldParser
boolField field setter v env = case parseBool v of
  Left  e -> Left (ConfigError field (T.pack e))
  Right b -> Right (setter b env)

-- | Parse a decimal integer with an optional leading sign. We
-- intentionally do /not/ accept @0x@ / @0o@ / scientific notation
-- here — librdkafka's parser takes the same conservative view, so
-- @KAFKA_BATCH_SIZE=16k@ correctly fails rather than silently
-- truncating.
parseInt :: Text -> Either String Int
parseInt t = case TR.signed TR.decimal t of
  Right (n, rest) | T.null (T.strip rest) -> Right n
  _ -> Left ("expected an integer, got " ++ show (T.unpack t))

-- | Parse a boolean. Accepts the librdkafka set ("true", "false",
-- "1", "0") plus the YAML\/Confluent set ("yes", "no", "on",
-- "off"). Case-insensitive.
parseBool :: Text -> Either String Bool
parseBool t = case map toLower (T.unpack (T.strip t)) of
  "true"  -> Right True
  "1"     -> Right True
  "yes"   -> Right True
  "on"    -> Right True
  "false" -> Right False
  "0"     -> Right False
  "no"    -> Right False
  "off"   -> Right False
  s       -> Left ("expected a boolean (true|false|1|0|yes|no|on|off), got " ++ show s)

-- | Parse the @acks@ property. @0@ -> 'EnvAcksZero',
-- @1@ -> 'EnvAcksOne', @-1@ or @all@ -> 'EnvAcksAll'.
-- The eventual mapping to a producer's 'DeliveryGuarantee'
-- happens in 'Kafka.Client.Producer.applyKafkaEnvToProducerConfig'.
parseAcks :: Text -> Either String EnvAcks
parseAcks t = case map toLower (T.unpack (T.strip t)) of
  "0"   -> Right EnvAcksZero
  "1"   -> Right EnvAcksOne
  "-1"  -> Right EnvAcksAll
  "all" -> Right EnvAcksAll
  s     -> Left ("expected one of 0/1/-1/all, got " ++ show s)

-- | Parse @compression.type@. Delegates to
-- 'Compression.parseCompressionCodec'.
parseCompression :: Text -> Either String Compression.CompressionCodec
parseCompression t = case Compression.parseCompressionCodec t of
  Just c  -> Right c
  Nothing -> Left ("expected one of none/gzip/snappy/lz4/zstd, got "
                   ++ show (T.unpack t))

parseSecurityProtocol :: Text -> Either String SecurityProtocol
parseSecurityProtocol t = case map toLower (T.unpack (T.strip t)) of
  "plaintext"      -> Right SecPlaintext
  "ssl"            -> Right SecSsl
  "sasl_plaintext" -> Right SecSaslPlaintext
  "sasl_ssl"       -> Right SecSaslSsl
  s -> Left ("expected one of PLAINTEXT/SSL/SASL_PLAINTEXT/SASL_SSL, got " ++ show s)

parseSaslMechanism :: Text -> Either String SaslMechanism
parseSaslMechanism t = case map toLower (T.unpack (T.strip t)) of
  "plain"          -> Right MechPlain
  "scram-sha-256"  -> Right MechScramSha256
  "scram_sha_256"  -> Right MechScramSha256
  "scram-sha-512"  -> Right MechScramSha512
  "scram_sha_512"  -> Right MechScramSha512
  "oauthbearer"    -> Right MechOAuthBearer
  "aws_msk_iam"    -> Right MechAwsMskIam
  "aws-msk-iam"    -> Right MechAwsMskIam
  "gssapi"         -> Right MechGssapi
  s -> Left ("expected one of PLAIN/SCRAM-SHA-256/SCRAM-SHA-512/OAUTHBEARER/AWS_MSK_IAM/GSSAPI, got " ++ show s)

parseAutoOffsetReset :: Text -> Either String EnvOffsetReset
parseAutoOffsetReset t = case map toLower (T.unpack (T.strip t)) of
  "earliest" -> Right EnvOffsetEarliest
  "latest"   -> Right EnvOffsetLatest
  "none"     -> Right EnvOffsetNone
  s -> Left ("expected one of earliest/latest/none, got " ++ show s)

parseIsolationLevel :: Text -> Either String EnvIsolationLevel
parseIsolationLevel t = case map toLower (T.unpack (T.strip t)) of
  "read_uncommitted" -> Right EnvReadUncommitted
  "read_committed"   -> Right EnvReadCommitted
  s -> Left ("expected one of read_uncommitted/read_committed, got " ++ show s)

-- | Accept both the short names ('range', 'roundrobin', 'sticky')
-- and the JVM-style class names ('org.apache.kafka.clients.consumer.RangeAssignor'
-- etc.) so a config tuned for the JVM client transfers across
-- unchanged.
parseAssignmentStrategy :: Text -> Either String EnvAssignmentStrategy
parseAssignmentStrategy t =
  let lo = map toLower (T.unpack (T.strip t))
  in case lo of
       "range"                                                 -> Right EnvAssignRange
       "org.apache.kafka.clients.consumer.rangeassignor"       -> Right EnvAssignRange
       "roundrobin"                                            -> Right EnvAssignRoundRobin
       "round_robin"                                           -> Right EnvAssignRoundRobin
       "round-robin"                                           -> Right EnvAssignRoundRobin
       "org.apache.kafka.clients.consumer.roundrobinassignor"  -> Right EnvAssignRoundRobin
       "sticky"                                                -> Right EnvAssignSticky
       "org.apache.kafka.clients.consumer.stickyassignor"      -> Right EnvAssignSticky
       "cooperative-sticky"                                    -> Right EnvAssignSticky
       "cooperative_sticky"                                    -> Right EnvAssignSticky
       "org.apache.kafka.clients.consumer.cooperativestickyassignor"
                                                               -> Right EnvAssignSticky
       _ -> Left ("expected one of range/roundrobin/sticky, got " ++ show (T.unpack t))

parseBrokerAddressFamily :: Text -> Either String Conn.BrokerAddressFamily
parseBrokerAddressFamily t = case map toLower (T.unpack (T.strip t)) of
  "any"  -> Right Conn.BrokerAddressAny
  "v4"   -> Right Conn.BrokerAddressIPv4
  "ipv4" -> Right Conn.BrokerAddressIPv4
  "v6"   -> Right Conn.BrokerAddressIPv6
  "ipv6" -> Right Conn.BrokerAddressIPv6
  s      -> Left ("expected one of any/v4/v6, got " ++ show s)

parseDnsLookupMode :: Text -> Either String Conn.DnsLookupMode
parseDnsLookupMode t = case map toLower (T.unpack (T.strip t)) of
  "use_all_dns_ips"                       -> Right Conn.DnsUseAllDnsIps
  "use-all-dns-ips"                       -> Right Conn.DnsUseAllDnsIps
  "resolve_canonical_bootstrap_servers_only"
                                          -> Right Conn.DnsResolveCanonicalBootstrapServersOnly
  "resolve-canonical-bootstrap-servers-only"
                                          -> Right Conn.DnsResolveCanonicalBootstrapServersOnly
  "default"                               -> Right Conn.DnsResolveCanonicalBootstrapServersOnly
  s -> Left ("expected one of use_all_dns_ips/resolve_canonical_bootstrap_servers_only, got " ++ show s)

-- | Split a comma- or whitespace-separated list of @host:port@
-- entries. Empty entries are skipped. Whitespace around each
-- entry is trimmed. We /don't/ validate the @host:port@ shape
-- here — the connection layer's parser does that, and reporting
-- "parse error in your bootstrap list" twice would be noisy.
parseBootstrapServers :: Text -> Either String [Text]
parseBootstrapServers t =
  let isSep c = c == ',' || c == ' ' || c == '\t' || c == '\n' || c == '\r'
      raw = T.split isSep t
      nonEmpty = filter (not . T.null) (fmap T.strip raw)
  in if null nonEmpty
       then Left "bootstrap.servers must contain at least one host:port"
       else Right nonEmpty

------------------------------------------------------------------------
-- IO entry points
------------------------------------------------------------------------

-- | Read every @KAFKA_*@ variable from 'Env.getEnvironment' and
-- parse it into a typed 'KafkaEnv'. Returns 'Left' with a list
-- of every parse error so the caller can surface them all.
loadKafkaEnv :: IO (Either [ConfigError] KafkaEnv)
loadKafkaEnv = do
  pairs <- Env.getEnvironment
  let txt = fmap (\(k, v) -> (T.pack k, T.pack v)) pairs
  pure (parseKafkaEnvList txt)

-- | Convenience: pull just the bootstrap server list from the
-- process environment, ignoring everything else. Useful for
-- callers that hand-build their config but want @KAFKA_BOOTSTRAP_SERVERS@
-- to seed the broker list.
bootstrapServersFromEnv :: IO (Maybe [Text])
bootstrapServersFromEnv = do
  mv <- Env.lookupEnv "KAFKA_BOOTSTRAP_SERVERS"
  case mv of
    Nothing -> pure Nothing
    Just raw -> case parseBootstrapServers (T.pack raw) of
      Left  _ -> pure Nothing
      Right s -> pure (Just s)

------------------------------------------------------------------------
-- Overlays
------------------------------------------------------------------------

-- | Apply a parsed 'KafkaEnv' onto a 'Conn.ConnectionConfig'.
-- Only fields whose corresponding env var was set get touched.
-- A 'Left' is returned when the env asks for something the
-- connection layer can't represent (e.g. @SASL_SSL@ with
-- @KAFKA_SASL_MECHANISM=OAUTHBEARER@ — that needs a token
-- provider we can't pull from a flat env var).
applyKafkaEnvToConnectionConfig
  :: KafkaEnv
  -> Conn.ConnectionConfig
  -> Either [ConfigError] Conn.ConnectionConfig
applyKafkaEnvToConnectionConfig env cfg0 =
  let cfg1 = overlayPlainFields env cfg0
  in case applySecurityProtocol env cfg1 of
       Left errs  -> Left errs
       Right cfg2 -> Right cfg2

-- | All the connection-level knobs that are pure numeric or
-- enum-style overrides (no SASL/TLS bookkeeping needed). Split
-- out so 'applyKafkaEnvToConnectionConfig' stays readable.
overlayPlainFields :: KafkaEnv -> Conn.ConnectionConfig -> Conn.ConnectionConfig
overlayPlainFields KafkaEnv{..} cfg = cfg
  { Conn.connClientId =
      fromMaybe (Conn.connClientId cfg) envClientId
  , Conn.connReadTimeout =
      fromMaybe (Conn.connReadTimeout cfg)
        (fmap (\ms -> max 1 (ms `div` 1000)) envSocketTimeoutMs)
  , Conn.connWriteTimeout =
      fromMaybe (Conn.connWriteTimeout cfg)
        (fmap (\ms -> max 1 (ms `div` 1000)) envSocketTimeoutMs)
  , Conn.connRequestTimeoutMs =
      fromMaybe (Conn.connRequestTimeoutMs cfg) envRequestTimeoutMs
  , Conn.connSocketKeepalive =
      fromMaybe (Conn.connSocketKeepalive cfg) envSocketKeepaliveEnable
  , Conn.connSocketNagleDisable =
      fromMaybe (Conn.connSocketNagleDisable cfg) envSocketNagleDisable
  , Conn.connSocketSendBuffer =
      fromMaybe (Conn.connSocketSendBuffer cfg) envSocketSendBufferBytes
  , Conn.connSocketReceiveBuffer =
      fromMaybe (Conn.connSocketReceiveBuffer cfg) envSocketReceiveBufferBytes
  , Conn.connRetryDelay =
      fromMaybe (Conn.connRetryDelay cfg) envReconnectBackoffMs
  , Conn.connBackoffMaxMs =
      fromMaybe (Conn.connBackoffMaxMs cfg) envReconnectBackoffMaxMs
  , Conn.connMaxIdleMs =
      fromMaybe (Conn.connMaxIdleMs cfg) envConnectionsMaxIdleMs
  , Conn.connMessageMaxBytes =
      fromMaybe (Conn.connMessageMaxBytes cfg) envMessageMaxBytes
  , Conn.connReceiveMessageMaxBytes =
      fromMaybe (Conn.connReceiveMessageMaxBytes cfg) envReceiveMessageMaxBytes
  , Conn.connMetadataMaxAgeMs =
      fromMaybe (Conn.connMetadataMaxAgeMs cfg) envMetadataMaxAgeMs
  , Conn.connBrokerAddressFamily =
      fromMaybe (Conn.connBrokerAddressFamily cfg) envBrokerAddressFamily
  , Conn.connDnsLookup =
      fromMaybe (Conn.connDnsLookup cfg) envClientDnsLookup
  }

-- | Translate the @security.protocol@ + @sasl.*@ env tuple into
-- 'Conn.connUseTls' / 'Conn.connTlsSettings' / 'Conn.connSasl'.
-- The two sources of error here are:
--
--   * a SASL_* protocol with no mechanism set, or a mechanism
--     that needs out-of-band setup (OAUTHBEARER, AWS_MSK_IAM,
--     GSSAPI);
--   * a credential mechanism (PLAIN, SCRAM-*) with one of the
--     username/password halves missing.
applySecurityProtocol
  :: KafkaEnv
  -> Conn.ConnectionConfig
  -> Either [ConfigError] Conn.ConnectionConfig
applySecurityProtocol env@KafkaEnv{..} cfg = case envSecurityProtocol of
  Nothing -> case buildSaslOnly env of
    Right msasl -> Right cfg { Conn.connSasl = mergeMaybe (Conn.connSasl cfg) msasl }
    Left errs   -> Left errs
  Just SecPlaintext ->
    case buildSaslOnly env of
      Right msasl -> Right cfg
        { Conn.connUseTls = False
        , Conn.connSasl   = mergeMaybe (Conn.connSasl cfg) msasl
        }
      Left errs   -> Left errs
  Just SecSsl ->
    Right (withTls env cfg)
  Just SecSaslPlaintext ->
    case buildSaslRequired env of
      Right sasl -> Right cfg
        { Conn.connUseTls = False
        , Conn.connSasl   = Just sasl
        }
      Left errs  -> Left errs
  Just SecSaslSsl ->
    case buildSaslRequired env of
      Right sasl -> Right ((withTls env cfg) { Conn.connSasl = Just sasl })
      Left errs  -> Left errs

mergeMaybe :: Maybe a -> Maybe a -> Maybe a
mergeMaybe existing override = case override of
  Just _  -> override
  Nothing -> existing

-- | Build a SASL config when the user gave us a mechanism but no
-- explicit security protocol (e.g. SASL over plain TCP for
-- testing). Returns 'Nothing' when no SASL env var is set,
-- 'Left' when something is half-set in an unresolvable way.
buildSaslOnly :: KafkaEnv -> Either [ConfigError] (Maybe SASL.SaslConfig)
buildSaslOnly env@KafkaEnv{..} = case envSaslMechanism of
  Nothing -> Right Nothing
  Just _  -> case buildSaslRequired env of
    Right sasl -> Right (Just sasl)
    Left errs  -> Left errs

-- | Build a SASL config knowing the user definitely wants one
-- (the security protocol was SASL_*).
buildSaslRequired :: KafkaEnv -> Either [ConfigError] SASL.SaslConfig
buildSaslRequired KafkaEnv{..} = case envSaslMechanism of
  Nothing -> Left
    [ ConfigError "sasl.mechanism"
        "must be set when security.protocol is SASL_PLAINTEXT or SASL_SSL" ]
  Just MechPlain -> credMech "PLAIN" SASL.SaslPlain envSaslUsername envSaslPassword
  Just MechScramSha256 ->
    credMech "SCRAM-SHA-256" (SASL.SaslScram Scram.ScramSHA256)
             envSaslUsername envSaslPassword
  Just MechScramSha512 ->
    credMech "SCRAM-SHA-512" (SASL.SaslScram Scram.ScramSHA512)
             envSaslUsername envSaslPassword
  Just MechOAuthBearer -> case envSaslOAuthBearerToken of
    Just tok ->
      Right (SASL.SaslOAuthBearer (OAuth.OAuthStaticToken (OAuth.OAuthToken tok Nothing Nothing)))
    Nothing -> Left
      [ ConfigError "sasl.oauthbearer.token"
          ("must be set for sasl.mechanism=OAUTHBEARER, or build a "
            <> "SASL.SaslOAuthBearer with your token provider and set "
            <> "ConnectionConfig.connSasl programmatically") ]
  Just MechAwsMskIam -> Left
    [ ConfigError "sasl.mechanism"
        ("AWS_MSK_IAM cannot be configured from env vars alone; "
          <> "build a SASL.SaslAwsMskIam with an AwsCredentialsProvider and "
          <> "set ConnectionConfig.connSasl programmatically") ]
  Just MechGssapi -> Left
    [ ConfigError "sasl.mechanism"
        ("GSSAPI is not supported by wireform-kafka's built-in handshake; "
          <> "supply a custom SaslMechanismImpl") ]
  where
    credMech :: Text
             -> (Text -> Text -> SASL.SaslConfig)
             -> Maybe Text
             -> Maybe Text
             -> Either [ConfigError] SASL.SaslConfig
    credMech name mk muser mpass = case (muser, mpass) of
      (Just u, Just p) -> Right (mk u p)
      (Nothing, _) -> Left
        [ ConfigError "sasl.username"
            ("must be set for sasl.mechanism=" <> name) ]
      (_, Nothing) -> Left
        [ ConfigError "sasl.password"
            ("must be set for sasl.mechanism=" <> name) ]

-- | Enable TLS. If the caller already set 'Conn.connTlsSettings'
-- (e.g. with a pinned CA bundle) we keep it. Otherwise we fill
-- in 'Conn.defaultTlsSettings' against the first env-provided
-- bootstrap broker so SNI / hostname verification has something
-- to match. With no bootstrap brokers we leave the settings
-- 'Nothing' — the connection layer will then refuse to connect
-- with a clear error, which is preferable to silently disabling
-- hostname verification.
withTls :: KafkaEnv -> Conn.ConnectionConfig -> Conn.ConnectionConfig
withTls KafkaEnv{..} cfg = cfg
  { Conn.connUseTls      = True
  , Conn.connTlsSettings = case Conn.connTlsSettings cfg of
      Just existing -> Just existing
      Nothing -> case envBootstrapServers of
        Just (b:_) -> Just (Conn.defaultTlsSettings (hostOf b))
        _ -> Nothing
  }
  where
    hostOf :: Text -> String
    hostOf t = T.unpack (T.takeWhile (/= ':') t)

------------------------------------------------------------------------
-- ConnectionConfig overlay IO wrapper
------------------------------------------------------------------------

-- | Read the process environment, parse it, and overlay it onto
-- the supplied 'Conn.ConnectionConfig'. Combines 'loadKafkaEnv'
-- and 'applyKafkaEnvToConnectionConfig'.
connectionConfigFromEnv
  :: Conn.ConnectionConfig
  -> IO (Either [ConfigError] Conn.ConnectionConfig)
connectionConfigFromEnv cfg = do
  r <- loadKafkaEnv
  case r of
    Left errs -> pure (Left errs)
    Right env -> pure (applyKafkaEnvToConnectionConfig env cfg)
