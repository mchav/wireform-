{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the @KAFKA_*@ env-var loader in
-- "Kafka.Client.Env".
--
-- The unit under test is the pure 'parseKafkaEnvList' /
-- 'applyKafkaEnvTo*' surface; the IO-flavoured wrappers
-- ('loadKafkaEnv', 'producerConfigFromEnv', …) are thin shims on
-- top so we exercise them indirectly by passing the env in as
-- a list.
module Client.EnvSpec (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))

import qualified Data.Text as T
import qualified System.Environment as SE

import qualified Kafka.Client.Consumer as C
import Kafka.Client.Consumer (applyKafkaEnvToConsumerConfig)
import qualified Kafka.Client.Env as Env
import Kafka.Client.Env
  ( KafkaEnv (..)
  , SecurityProtocol (..)
  , SaslMechanism (..)
  , EnvAcks (..)
  , EnvOffsetReset (..)
  , applyKafkaEnvToConnectionConfig
  , parseKafkaEnvList
  )
import qualified Kafka.Client.Producer as P
import Kafka.Client.Producer (applyKafkaEnvToProducerConfig)
import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Network.Auth.OAuthBearer as OAuth
import qualified Kafka.Network.Auth.SASL as SASL
import qualified Kafka.Network.Auth.Scram as Scram
import qualified Kafka.Network.Connection as Conn

import Kafka.Client.ConfigValidation (ConfigError (..))

tests :: TestTree
tests = testGroup "Kafka.Client.Env"
  [ testGroup "scalar parsers"
      [ testCase "parseBool accepts true/false/1/0/yes/no/on/off" prop_parseBool
      , testCase "parseBool rejects garbage" prop_parseBoolBad
      , testCase "parseAcks accepts 0/1/-1/all" prop_parseAcks
      , testCase "parseCompression delegates to codec parser" prop_parseCompression
      , testCase "parseBootstrapServers splits and trims" prop_parseBootstrapServers
      , testCase "parseBootstrapServers rejects empty string" prop_parseBootstrapServersEmpty
      ]
  , testGroup "parseKafkaEnvList"
      [ testCase "empty env yields empty KafkaEnv" prop_emptyEnv
      , testCase "ignores unrelated KAFKA_* variables" prop_ignoresUnknown
      , testCase "lookup is case-insensitive" prop_caseInsensitive
      , testCase "trims whitespace" prop_trimsWhitespace
      , testCase "reports parse errors with the librdkafka field name"
          prop_reportsParseErrors
      , testCase "accumulates multiple parse errors" prop_accumulatesErrors
      , testCase "fills every relevant field" prop_fullEnv
      ]
  , testGroup "applyKafkaEnvToConnectionConfig"
      [ testCase "absent vars leave defaults intact" prop_connDefaults
      , testCase "client.id is overridden" prop_connClientId
      , testCase "socket timeout overrides read/write" prop_connSocketTimeout
      , testCase "SSL flips connUseTls and supplies default TLS settings"
          prop_connSsl
      , testCase "SSL preserves caller-supplied TLS settings" prop_connSslPreservesTls
      , testCase "SASL_SSL with PLAIN sets connSasl and TLS"
          prop_connSaslSsl
      , testCase "SASL_SSL with SCRAM-SHA-512 picks the right algo"
          prop_connSaslScramSha512
      , testCase "SASL_PLAINTEXT skips TLS but sets connSasl"
          prop_connSaslPlaintext
      , testCase "SASL_SSL without mechanism is an error"
          prop_connSaslSslNoMechanism
      , testCase "SASL_SSL with PLAIN and missing password is an error"
          prop_connSaslMissingPassword
      , testCase "SASL_SSL with OAUTHBEARER and static token sets connSasl"
          prop_connSaslOAuthStaticToken
      , testCase "SASL_SSL with OAUTHBEARER and no token is rejected with guidance"
          prop_connSaslOAuthMissingToken
      , testCase "SASL_SSL with AWS_MSK_IAM is rejected with guidance"
          prop_connSaslAwsRejected
      ]
  , testGroup "applyKafkaEnvToProducerConfig"
      [ testCase "absent vars leave defaults intact" prop_prodDefaults
      , testCase "overrides batch.size + linger.ms + compression"
          prop_prodOverrides
      , testCase "acks=all maps to ExactlyOnce" prop_prodAcksAll
      , testCase "enable.idempotence=true flips the flag" prop_prodIdempotence
      , testCase "transactional.id is propagated" prop_prodTransactional
      ]
  , testGroup "applyKafkaEnvToConsumerConfig"
      [ testCase "absent vars leave defaults intact" prop_consDefaults
      , testCase "group.id + group.instance.id are propagated"
          prop_consGroupId
      , testCase "auto.offset.reset=earliest is applied" prop_consAutoOffsetReset
      , testCase "isolation.level=read_committed is applied"
          prop_consIsolation
      , testCase "session/heartbeat overrides apply" prop_consTimeouts
      , testCase "partition.assignment.strategy accepts JVM class names"
          prop_consAssignmentJvmName
      , testCase "consumer pulls connection-level env via embedded ConnectionConfig"
          prop_consInheritsConnection
      ]
  , testGroup "createProducer / createConsumer"
      [ testCase "createProducer reads KAFKA_BOOTSTRAP_SERVERS from the env"
          prop_createProducerReadsBootstrap
      , testCase "createConsumer reads KAFKA_BOOTSTRAP_SERVERS from the env"
          prop_createConsumerReadsBootstrap
      , testCase "createProducer surfaces malformed env-var values"
          prop_createProducerMalformedEnv
      ]
  ]

------------------------------------------------------------------
-- Scalar parsers
------------------------------------------------------------------

prop_parseBool :: Assertion
prop_parseBool = do
  Env.parseBool "true"  @?= Right True
  Env.parseBool "TRUE"  @?= Right True
  Env.parseBool "1"     @?= Right True
  Env.parseBool "yes"   @?= Right True
  Env.parseBool "on"    @?= Right True
  Env.parseBool "false" @?= Right False
  Env.parseBool "0"     @?= Right False
  Env.parseBool "no"    @?= Right False
  Env.parseBool "off"   @?= Right False

prop_parseBoolBad :: Assertion
prop_parseBoolBad = case Env.parseBool "maybe" of
  Left _  -> pure ()
  Right b -> assertFailure ("expected Left, got Right " <> show b)

prop_parseAcks :: Assertion
prop_parseAcks = do
  Env.parseAcks "0"   @?= Right EnvAcksZero
  Env.parseAcks "1"   @?= Right EnvAcksOne
  Env.parseAcks "-1"  @?= Right EnvAcksAll
  Env.parseAcks "all" @?= Right EnvAcksAll
  Env.parseAcks "ALL" @?= Right EnvAcksAll

prop_parseCompression :: Assertion
prop_parseCompression = do
  Env.parseCompression "gzip"   @?= Right Compression.Gzip
  Env.parseCompression "ZSTD"   @?= Right Compression.Zstd
  Env.parseCompression "snappy" @?= Right Compression.Snappy
  Env.parseCompression "lz4"    @?= Right Compression.Lz4
  Env.parseCompression "none"   @?= Right Compression.NoCompression

prop_parseBootstrapServers :: Assertion
prop_parseBootstrapServers = do
  Env.parseBootstrapServers "a:1, b:2 ,c:3"
    @?= Right ["a:1", "b:2", "c:3"]
  Env.parseBootstrapServers "a:1  b:2\tc:3"
    @?= Right ["a:1", "b:2", "c:3"]

prop_parseBootstrapServersEmpty :: Assertion
prop_parseBootstrapServersEmpty = case Env.parseBootstrapServers "   " of
  Left _  -> pure ()
  Right s -> assertFailure ("expected Left, got " <> show s)

------------------------------------------------------------------
-- parseKafkaEnvList
------------------------------------------------------------------

prop_emptyEnv :: Assertion
prop_emptyEnv = case parseKafkaEnvList [] of
  Right env -> env @?= Env.emptyKafkaEnv
  Left  e   -> assertFailure ("expected Right, got " <> show e)

prop_ignoresUnknown :: Assertion
prop_ignoresUnknown =
  -- Confluent's Docker images set tons of KAFKA_BROKER_*,
  -- KAFKA_LISTENERS, KAFKA_ZOOKEEPER_CONNECT, … - all of which
  -- target the broker, not the client. We must ignore them.
  case parseKafkaEnvList
         [ ("KAFKA_LISTENERS", "PLAINTEXT://0.0.0.0:9092")
         , ("KAFKA_BROKER_ID", "1")
         , ("KAFKA_ZOOKEEPER_CONNECT", "zookeeper:2181")
         , ("PATH", "/usr/bin")
         , ("KAFKA_CLIENT_ID", "alice")
         ] of
    Right env -> envClientId env @?= Just "alice"
    Left e    -> assertFailure ("expected Right, got " <> show e)

prop_caseInsensitive :: Assertion
prop_caseInsensitive = case parseKafkaEnvList
         [ ("kafka_client_id", "casey")
         , ("Kafka_Group_Id", "g1")
         ] of
    Right env -> do
      envClientId env @?= Just "casey"
      envGroupId  env @?= Just "g1"
    Left e -> assertFailure ("expected Right, got " <> show e)

prop_trimsWhitespace :: Assertion
prop_trimsWhitespace = case parseKafkaEnvList
         [ ("KAFKA_CLIENT_ID", "  ringo  ")
         , ("KAFKA_BATCH_SIZE", " 4096 ")
         ] of
    Right env -> do
      envClientId  env @?= Just "ringo"
      envBatchSize env @?= Just 4096
    Left e -> assertFailure ("expected Right, got " <> show e)

prop_reportsParseErrors :: Assertion
prop_reportsParseErrors = case parseKafkaEnvList
         [ ("KAFKA_BATCH_SIZE", "not-a-number") ] of
    Left [ConfigError field _] -> field @?= "batch.size"
    other -> assertFailure ("expected one batch.size error, got " <> show other)

prop_accumulatesErrors :: Assertion
prop_accumulatesErrors = case parseKafkaEnvList
         [ ("KAFKA_BATCH_SIZE", "x")
         , ("KAFKA_LINGER_MS", "y")
         , ("KAFKA_ENABLE_AUTO_COMMIT", "maybe")
         ] of
    Left errs -> length errs @?= 3
    Right _   -> assertFailure "expected Left with 3 errors"

prop_fullEnv :: Assertion
prop_fullEnv = case parseKafkaEnvList
         [ ("KAFKA_BOOTSTRAP_SERVERS", "b1:9092,b2:9092")
         , ("KAFKA_CLIENT_ID", "ci")
         , ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
         , ("KAFKA_SASL_MECHANISM", "SCRAM-SHA-256")
         , ("KAFKA_SASL_USERNAME", "u")
         , ("KAFKA_SASL_PASSWORD", "p")
         , ("KAFKA_SASL_OAUTH_BEARER_TOKEN", "oauth-token")
         , ("KAFKA_ACKS", "all")
         , ("KAFKA_COMPRESSION_TYPE", "zstd")
         , ("KAFKA_GROUP_ID", "grp")
         , ("KAFKA_AUTO_OFFSET_RESET", "earliest")
         ] of
    Right env -> do
      envBootstrapServers env @?= Just ["b1:9092", "b2:9092"]
      envClientId         env @?= Just "ci"
      envSecurityProtocol env @?= Just SecSaslSsl
      envSaslMechanism    env @?= Just MechScramSha256
      envSaslUsername     env @?= Just "u"
      envSaslPassword     env @?= Just "p"
      envSaslOAuthBearerToken env @?= Just "oauth-token"
      envAcks             env @?= Just EnvAcksAll
      envCompressionType  env @?= Just Compression.Zstd
      envGroupId          env @?= Just "grp"
      envAutoOffsetReset  env @?= Just EnvOffsetEarliest
    Left e -> assertFailure ("expected Right, got " <> show e)

------------------------------------------------------------------
-- ConnectionConfig overlay
------------------------------------------------------------------

baseConn :: Conn.ConnectionConfig
baseConn = Conn.defaultConnectionConfig

prop_connDefaults :: Assertion
prop_connDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToConnectionConfig env baseConn of
    Right cfg -> do
      Conn.connClientId            cfg @?= Conn.connClientId baseConn
      Conn.connUseTls              cfg @?= Conn.connUseTls baseConn
      Conn.connSasl                cfg `assertSameSaslNothing` Conn.connSasl baseConn
      Conn.connRequestTimeoutMs    cfg @?= Conn.connRequestTimeoutMs baseConn
    Left e -> assertFailure ("expected Right, got " <> show e)

prop_connClientId :: Assertion
prop_connClientId = withEnv [("KAFKA_CLIENT_ID", "rocky")] $ \env ->
  case applyKafkaEnvToConnectionConfig env baseConn of
    Right cfg -> Conn.connClientId cfg @?= "rocky"
    Left e    -> assertFailure ("expected Right, got " <> show e)

prop_connSocketTimeout :: Assertion
prop_connSocketTimeout =
  withEnv [("KAFKA_SOCKET_TIMEOUT_MS", "45000")] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> do
        Conn.connReadTimeout  cfg @?= 45
        Conn.connWriteTimeout cfg @?= 45
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSsl :: Assertion
prop_connSsl =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SSL")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker.example.com:9093")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> do
        Conn.connUseTls cfg @?= True
        assertBool "connTlsSettings populated from bootstrap host"
          (case Conn.connTlsSettings cfg of
             Just _  -> True
             Nothing -> False)
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSslPreservesTls :: Assertion
prop_connSslPreservesTls = do
  let pinned = Conn.defaultTlsSettings "pinned.example.com"
      cfg0   = baseConn { Conn.connTlsSettings = Just pinned }
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SSL")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker.example.com:9093")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env cfg0 of
      Right cfg -> do
        Conn.connUseTls cfg @?= True
        -- We can't inspect the inner ClientParams equality easily,
        -- but at minimum the slot is occupied.
        assertBool "connTlsSettings retained"
          (case Conn.connTlsSettings cfg of
             Just _  -> True
             Nothing -> False)
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSaslSsl :: Assertion
prop_connSaslSsl =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "user")
    , ("KAFKA_SASL_PASSWORD", "pass")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker:9093")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> do
        Conn.connUseTls cfg @?= True
        case Conn.connSasl cfg of
          Just (SASL.SaslPlain u p) -> do
            u @?= "user"
            p @?= "pass"
          other -> assertFailure ("expected SaslPlain, got " <> show (saslDescr other))
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSaslScramSha512 :: Assertion
prop_connSaslScramSha512 =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "SCRAM-SHA-512")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_SASL_PASSWORD", "p")
    , ("KAFKA_BOOTSTRAP_SERVERS", "broker:9093")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> case Conn.connSasl cfg of
        Just (SASL.SaslScram Scram.ScramSHA512 _ _) -> pure ()
        other -> assertFailure ("expected SCRAM-SHA-512, got " <> show (saslDescr other))
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSaslPlaintext :: Assertion
prop_connSaslPlaintext =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_PLAINTEXT")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_SASL_PASSWORD", "p")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> do
        Conn.connUseTls cfg @?= False
        case Conn.connSasl cfg of
          Just (SASL.SaslPlain _ _) -> pure ()
          other -> assertFailure ("expected SaslPlain, got " <> show (saslDescr other))
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSaslSslNoMechanism :: Assertion
prop_connSaslSslNoMechanism =
  withEnv [("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Left [ConfigError field _] -> field @?= "sasl.mechanism"
      other -> assertFailure ("expected sasl.mechanism error, got "
                               <> describeResult other)

prop_connSaslMissingPassword :: Assertion
prop_connSaslMissingPassword =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "PLAIN")
    , ("KAFKA_SASL_USERNAME", "u")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Left [ConfigError field _] -> field @?= "sasl.password"
      other -> assertFailure ("expected sasl.password error, got "
                              <> describeResult other)

prop_connSaslOAuthStaticToken :: Assertion
prop_connSaslOAuthStaticToken =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "OAUTHBEARER")
    , ("KAFKA_SASL_OAUTH_BEARER_TOKEN", "static-token")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Right cfg -> case Conn.connSasl cfg of
        Just (SASL.SaslOAuthBearer (OAuth.OAuthStaticToken tok)) ->
          OAuth.oauthTokenBytes tok @?= "static-token"
        other -> assertFailure ("expected static OAUTHBEARER, got "
                                <> show (saslDescr other))
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_connSaslOAuthMissingToken :: Assertion
prop_connSaslOAuthMissingToken =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "OAUTHBEARER")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Left [ConfigError field msg] -> do
        field @?= "sasl.oauthbearer.token"
        assertBool "msg mentions OAUTHBEARER"
          ("OAUTHBEARER" `T.isInfixOf` msg)
      other -> assertFailure ("expected sasl.oauthbearer.token error, got "
                              <> describeResult other)

prop_connSaslAwsRejected :: Assertion
prop_connSaslAwsRejected =
  withEnv
    [ ("KAFKA_SECURITY_PROTOCOL", "SASL_SSL")
    , ("KAFKA_SASL_MECHANISM", "AWS_MSK_IAM")
    , ("KAFKA_BOOTSTRAP_SERVERS", "h:1")
    ] $ \env ->
    case applyKafkaEnvToConnectionConfig env baseConn of
      Left [ConfigError field msg] -> do
        field @?= "sasl.mechanism"
        assertBool "msg mentions AWS_MSK_IAM"
          ("AWS_MSK_IAM" `T.isInfixOf` msg)
      other -> assertFailure ("expected sasl.mechanism error, got "
                              <> describeResult other)

------------------------------------------------------------------
-- ProducerConfig overlay
------------------------------------------------------------------

baseProd :: P.ProducerConfig
baseProd = P.defaultProducerConfig

prop_prodDefaults :: Assertion
prop_prodDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToProducerConfig env baseProd of
    Right cfg -> do
      P.producerClientId    cfg @?= P.producerClientId baseProd
      P.producerCompression cfg @?= P.producerCompression baseProd
      P.producerBatchSize   cfg @?= P.producerBatchSize baseProd
      P.producerLingerMs    cfg @?= P.producerLingerMs baseProd
      P.producerIdempotent  cfg @?= P.producerIdempotent baseProd
    Left e -> assertFailure ("expected Right, got " <> show e)

prop_prodOverrides :: Assertion
prop_prodOverrides =
  withEnv
    [ ("KAFKA_BATCH_SIZE", "32768")
    , ("KAFKA_LINGER_MS", "20")
    , ("KAFKA_COMPRESSION_TYPE", "lz4")
    , ("KAFKA_RETRIES", "7")
    ] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> do
        P.producerBatchSize   cfg @?= 32768
        P.producerLingerMs    cfg @?= 20
        P.producerCompression cfg @?= Compression.Lz4
        P.producerRetries     cfg @?= 7
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_prodAcksAll :: Assertion
prop_prodAcksAll =
  withEnv [("KAFKA_ACKS", "all")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerDelivery cfg @?= P.ExactlyOnce
      Left e    -> assertFailure ("expected Right, got " <> show e)

prop_prodIdempotence :: Assertion
prop_prodIdempotence =
  withEnv [("KAFKA_ENABLE_IDEMPOTENCE", "true")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerIdempotent cfg @?= True
      Left e    -> assertFailure ("expected Right, got " <> show e)

prop_prodTransactional :: Assertion
prop_prodTransactional =
  withEnv [("KAFKA_TRANSACTIONAL_ID", "tx-1")] $ \env ->
    case applyKafkaEnvToProducerConfig env baseProd of
      Right cfg -> P.producerTransactional cfg @?= Just "tx-1"
      Left e    -> assertFailure ("expected Right, got " <> show e)

------------------------------------------------------------------
-- ConsumerConfig overlay
------------------------------------------------------------------

baseCons :: C.ConsumerConfig
baseCons = C.defaultConsumerConfig

prop_consDefaults :: Assertion
prop_consDefaults = withEnv [] $ \env ->
  case applyKafkaEnvToConsumerConfig env baseCons of
    Right cfg -> do
      C.consumerClientId  cfg @?= C.consumerClientId baseCons
      C.consumerGroupId   cfg @?= C.consumerGroupId baseCons
      C.consumerAutoOffsetReset cfg @?= C.consumerAutoOffsetReset baseCons
    Left e -> assertFailure ("expected Right, got " <> show e)

prop_consGroupId :: Assertion
prop_consGroupId =
  withEnv
    [ ("KAFKA_GROUP_ID", "team-a")
    , ("KAFKA_GROUP_INSTANCE_ID", "pod-1")
    ] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> do
        C.consumerGroupId         cfg @?= "team-a"
        C.consumerGroupInstanceId cfg @?= Just "pod-1"
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_consAutoOffsetReset :: Assertion
prop_consAutoOffsetReset =
  withEnv [("KAFKA_AUTO_OFFSET_RESET", "earliest")] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> C.consumerAutoOffsetReset cfg @?= C.Earliest
      Left e    -> assertFailure ("expected Right, got " <> show e)

prop_consIsolation :: Assertion
prop_consIsolation =
  withEnv [("KAFKA_ISOLATION_LEVEL", "read_committed")] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> C.consumerIsolationLevel cfg @?= C.ReadCommitted
      Left e    -> assertFailure ("expected Right, got " <> show e)

prop_consTimeouts :: Assertion
prop_consTimeouts =
  withEnv
    [ ("KAFKA_SESSION_TIMEOUT_MS", "60000")
    , ("KAFKA_HEARTBEAT_INTERVAL_MS", "5000")
    , ("KAFKA_MAX_POLL_INTERVAL_MS", "120000")
    , ("KAFKA_MAX_POLL_RECORDS", "250")
    ] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> do
        C.consumerSessionTimeoutMs    cfg @?= 60_000
        C.consumerHeartbeatIntervalMs cfg @?= 5_000
        C.consumerMaxPollIntervalMs   cfg @?= 120_000
        C.consumerMaxPollRecords      cfg @?= 250
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_consAssignmentJvmName :: Assertion
prop_consAssignmentJvmName =
  withEnv
    [ ("KAFKA_PARTITION_ASSIGNMENT_STRATEGY"
      , "org.apache.kafka.clients.consumer.RoundRobinAssignor")
    ] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg ->
        C.consumerAssignmentStrategy cfg @?= C.RoundRobinAssignment
      Left e -> assertFailure ("expected Right, got " <> show e)

prop_consInheritsConnection :: Assertion
prop_consInheritsConnection =
  withEnv
    [ ("KAFKA_REQUEST_TIMEOUT_MS", "12345")
    , ("KAFKA_CLIENT_ID", "consumer-a")
    ] $ \env ->
    case applyKafkaEnvToConsumerConfig env baseCons of
      Right cfg -> do
        C.consumerClientId cfg @?= "consumer-a"
        Conn.connRequestTimeoutMs (C.consumerConnectionConfig cfg) @?= 12345
      Left e -> assertFailure ("expected Right, got " <> show e)

------------------------------------------------------------------
-- createProducer / createConsumer wiring
------------------------------------------------------------------

-- | Set a KAFKA_* env var for the body of the test, then unset
-- it. Uses 'setEnv' / 'unsetEnv' from "System.Environment".
withTempEnv :: [(String, String)] -> IO a -> IO a
withTempEnv kvs body = do
  old <- mapM (\(k, _) -> (,) k <$> SE.lookupEnv k) kvs
  mapM_ (\(k, v) -> SE.setEnv k v) kvs
  r <- body
  mapM_ restore old
  pure r
  where
    restore (k, Nothing) = SE.unsetEnv k
    restore (k, Just v)  = SE.setEnv k v

-- | createProducer should pick up @KAFKA_BOOTSTRAP_SERVERS@ when
-- the positional arg is left empty. We point it at a port that
-- nothing is listening on; the connect attempt fails, but the
-- failure message contains the host:port the env var supplied,
-- proving the env was consulted.
prop_createProducerReadsBootstrap :: Assertion
prop_createProducerReadsBootstrap =
  withTempEnv [("KAFKA_BOOTSTRAP_SERVERS", "127.0.0.1:1")] $ do
    r <- P.createProducer [] P.defaultProducerConfig
    case r of
      Left msg -> assertBool
        ("expected error to mention 127.0.0.1, got " <> msg)
        ("127.0.0.1" `isInfixOfS` msg)
      Right _ -> assertFailure
        "expected createProducer to fail connecting to 127.0.0.1:1"

prop_createConsumerReadsBootstrap :: Assertion
prop_createConsumerReadsBootstrap =
  withTempEnv [("KAFKA_BOOTSTRAP_SERVERS", "127.0.0.1:1")] $ do
    r <- C.createConsumer [] "" C.defaultConsumerConfig
    case r of
      Left msg -> assertBool
        ("expected error to mention 127.0.0.1, got " <> msg)
        ("127.0.0.1" `isInfixOfS` msg)
      Right _ -> assertFailure
        "expected createConsumer to fail connecting to 127.0.0.1:1"

prop_createProducerMalformedEnv :: Assertion
prop_createProducerMalformedEnv =
  withTempEnv [("KAFKA_BATCH_SIZE", "not-a-number")] $ do
    r <- P.createProducer ["127.0.0.1:1"] P.defaultProducerConfig
    case r of
      Left msg -> assertBool
        ("expected the rendered error to mention batch.size, got " <> msg)
        ("batch.size" `isInfixOfS` msg)
      Right _ -> assertFailure
        "expected createProducer to refuse the malformed env"

-- | 'Data.List.isInfixOf' specialised for 'String'. Avoids
-- pulling 'Text.isInfixOf' just for the literal substring match.
isInfixOfS :: String -> String -> Bool
isInfixOfS needle haystack = go haystack
  where
    n = length needle
    go [] = False
    go xs@(_:rest)
      | take n xs == needle = True
      | otherwise           = go rest

------------------------------------------------------------------
-- Test helpers
------------------------------------------------------------------

-- | Parse the env table and hand the resulting 'KafkaEnv' to the
-- continuation. We fail the test if parsing itself returns
-- errors, since every test that uses this helper exercises the
-- /successful/ path of 'parseKafkaEnvList'.
withEnv :: [(String, String)] -> (KafkaEnv -> Assertion) -> Assertion
withEnv kvs k = case parseKafkaEnvList (fmap (\(a, b) -> (T.pack a, T.pack b)) kvs) of
  Right env -> k env
  Left  errs -> assertFailure ("parseKafkaEnvList failed: " <> show errs)

-- | Compare two SASL configs by structural intent. We don't get
-- an 'Eq' instance for 'SASL.SaslConfig' (it contains function
-- callbacks for OAUTHBEARER) so we settle for 'Nothing == Nothing'.
assertSameSaslNothing :: Maybe SASL.SaslConfig -> Maybe SASL.SaslConfig -> Assertion
assertSameSaslNothing Nothing Nothing = pure ()
assertSameSaslNothing a       b       =
  assertFailure ("expected both Nothing, got "
                  <> show (fmap saslName a) <> " / "
                  <> show (fmap saslName b))

saslName :: SASL.SaslConfig -> String
saslName SASL.SaslPlain{}       = "PLAIN"
saslName SASL.SaslPlainWithAuthzid{} = "PLAIN"
saslName (SASL.SaslScram Scram.ScramSHA256 _ _) = "SCRAM-SHA-256"
saslName (SASL.SaslScram Scram.ScramSHA512 _ _) = "SCRAM-SHA-512"
saslName SASL.SaslOAuthBearer{} = "OAUTHBEARER"
saslName SASL.SaslOAuthBearerWithExtensions{} = "OAUTHBEARER"
saslName SASL.SaslAwsMskIam{}   = "AWS_MSK_IAM"
saslName SASL.SaslGssapi        = "GSSAPI"

saslDescr :: Maybe SASL.SaslConfig -> String
saslDescr Nothing  = "Nothing"
saslDescr (Just s) = "Just " <> saslName s

-- | We can't simply 'show' the @Either [ConfigError] ConnectionConfig@
-- because 'ConnectionConfig' embeds 'TLS.ClientParams' / function
-- callbacks that don't have 'Show' instances. Reduce to a tag.
describeResult :: Either [ConfigError] a -> String
describeResult (Right _)   = "Right <config>"
describeResult (Left errs) = "Left " <> show errs
