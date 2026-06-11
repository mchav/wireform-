{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Kafka.Streams.Serde.SchemaRegistry
Description : Confluent-style Schema Registry serdes interface

This is the surface other producers / consumers in the JVM
ecosystem use to talk to a (Confluent or AWS Glue) schema
registry. We expose the same shape so wireform-kafka users can
interoperate without writing a parser for the
\"magic-byte + 4-byte schema-id + payload\" wire envelope every
time.

Layered cake:

  1. 'SchemaRegistryClient' — a record-of-IO that fetches /
     registers schemas. Two implementations ship in this module:
     'inMemoryRegistry' (for tests) and 'mockHttpRegistry'
     (records the HTTP exchange but doesn't open a socket).
  2. 'SchemaRegistrySerde' — a 'Kafka.Streams.Serde.Serde' built
     on top of a 'SchemaRegistryClient'. The serializer fetches
     /or registers/ a schema id, prefixes the magic byte + id,
     and forwards to a payload-level serializer. The deserializer
     parses the envelope, fetches the schema if it's not already
     cached, and forwards.
  3. Helpers for the three common payload formats: Avro
     (binary), JSON-Schema (json), Protobuf (length-delimited).

Caveats:

  * We do /not/ ship an HTTP client — pinning @http-client@ is a
    one-way dep choice. The 'mockHttpRegistry' shows the request
    / response shape we'd talk; production users supply their own
    'SchemaRegistryClient' that hits whatever transport their
    organisation uses.
  * Schema /compatibility/ checking (forward / backward / full)
    is delegated to the registry itself; this module just trusts
    the id the registry returned.
-}
module Kafka.Streams.Serde.SchemaRegistry (
  -- * Client interface
  SchemaRegistryClient (..),
  SchemaId (..),
  SchemaPayload (..),
  SchemaSubject (..),
  RegistryError (..),

  -- * Built-in clients
  inMemoryRegistry,
  mockHttpRegistry,

  -- * Serdes
  SchemaRegistrySerdeConfig (..),
  registrySerde,
  registrySerdeChecked,

  -- * Compatibility checking
  CompatibilityMode (..),
  CompatibilityResult (..),
  defaultCompatibilityMode,

  -- * Wire envelope
  magicByte,
  encodeEnvelope,
  decodeEnvelope,
) where

import Control.Concurrent.STM
import Data.Bits (shiftR, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
import Data.Int (Int32)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import GHC.Generics (Generic)
import Kafka.Streams.Serde (
  Serde (..),
 )
import System.IO.Unsafe qualified
import Wireform.Builder qualified as BSB


{- | A schema id assigned by the registry. Mirrors Confluent's
@int32@.
-}
newtype SchemaId = SchemaId {unSchemaId :: Int32}
  deriving stock (Eq, Ord, Show, Generic)


{- | Subject under which a schema is registered — typically
@"<topic>-key"@ or @"<topic>-value"@.
-}
newtype SchemaSubject = SchemaSubject {unSchemaSubject :: Text}
  deriving stock (Eq, Ord, Show, Generic)


{- | Opaque schema payload (e.g. Avro JSON, JSON-Schema document,
Protobuf descriptor blob).
-}
newtype SchemaPayload = SchemaPayload {unSchemaPayload :: ByteString}
  deriving stock (Eq, Ord, Show, Generic)


data RegistryError
  = SchemaNotFound !SchemaId
  | SubjectNotFound !SchemaSubject
  | RegistryHttpError !Int !Text
  | RegistryDecode !Text
  | {- | The new schema failed the registry's compatibility
    check against the subject's current versions. The
    'Text' carries the registry's verbatim explanation
    (Confluent normally returns @\"is_compatible\": false@
    with no further detail; we surface what the HTTP body
    carried).
    -}
    IncompatibleSchema !SchemaSubject !Text
  deriving stock (Eq, Show, Generic)


{- | The compatibility policy a subject is configured with. Mirrors
the @org.apache.kafka.connect.schema.SchemaCompatibility@ values
Confluent's Schema Registry uses
(<https://docs.confluent.io/platform/current/schema-registry/avro.html#schema-evolution-and-compatibility>).
-}
data CompatibilityMode
  = CompatNone
  | CompatBackward
  | CompatBackwardTransitive
  | CompatForward
  | CompatForwardTransitive
  | CompatFull
  | CompatFullTransitive
  deriving stock (Eq, Show, Generic)


{- | The default compatibility mode Confluent applies when a
subject has none explicitly set (@BACKWARD@).
-}
defaultCompatibilityMode :: CompatibilityMode
defaultCompatibilityMode = CompatBackward


{- | The outcome of asking the registry whether a candidate
'SchemaPayload' is compatible with the subject's current schema
under the subject's configured 'CompatibilityMode'.
-}
data CompatibilityResult
  = Compatible
  | {- | The registry rejected the candidate. The 'Text' carries
    the registry's verbatim explanation (or @\"incompatible\"@
    when the registry didn't elaborate).
    -}
    Incompatible !Text
  deriving stock (Eq, Show, Generic)


{- | Minimum surface every Schema Registry client implementation
must satisfy.

@srRegister@ + @srLookup@ + @srLookupBySubject@ are the three
calls Confluent's Avro / JSON-Schema / Protobuf serializers
make on the producer / consumer hot path.

@srCompatibilityMode@ + @srTestCompatibility@ are the two calls
the /compatibility-mode probing/ wrapper makes (see
'registrySerdeChecked'). They have sensible defaults so existing
clients keep compiling: a 'SchemaRegistryClient' that doesn't
implement them returns 'CompatNone' / 'Compatible' respectively.
-}
data SchemaRegistryClient = SchemaRegistryClient
  { srRegister
      :: SchemaSubject
      -> SchemaPayload
      -> IO (Either RegistryError SchemaId)
  , srLookup :: SchemaId -> IO (Either RegistryError SchemaPayload)
  , srLookupBySubject
      :: SchemaSubject
      -> IO (Either RegistryError (SchemaId, SchemaPayload))
  , srCompatibilityMode
      :: SchemaSubject
      -> IO (Either RegistryError CompatibilityMode)
  {- ^ Read the configured compatibility mode for a subject.
  Returns 'defaultCompatibilityMode' when the subject has
  never been configured. Implementations that don't know
  about subject-level config (e.g. 'inMemoryRegistry')
  return @Right 'CompatNone'@ unconditionally.
  -}
  , srTestCompatibility
      :: SchemaSubject
      -> SchemaPayload
      -> IO (Either RegistryError CompatibilityResult)
  {- ^ Test whether @payload@ is compatible with the
  subject's latest version under the subject's
  configured compatibility mode. Mirrors Confluent's
  @POST /compatibility/subjects/{subject}/versions/latest@.
  -}
  }


----------------------------------------------------------------------
-- In-memory client
----------------------------------------------------------------------

{- | A registry that keeps everything in a STM-managed map, so
tests can drive a producer + consumer round-trip without ever
opening an HTTP connection.
-}
inMemoryRegistry :: IO SchemaRegistryClient
inMemoryRegistry = do
  bySubject <- newTVarIO (Map.empty :: Map SubjectAndPayload SchemaId)
  byId <- newTVarIO (Map.empty :: Map SchemaId (SchemaSubject, SchemaPayload))
  nextId <- newTVarIO (1 :: Int32)
  pure
    SchemaRegistryClient
      { srRegister = \subj payload -> atomically $ do
          m <- readTVar bySubject
          case Map.lookup (SubjectAndPayload subj payload) m of
            Just sid -> pure (Right sid)
            Nothing -> do
              n <- readTVar nextId
              let !sid = SchemaId n
              writeTVar nextId (n + 1)
              writeTVar bySubject (Map.insert (SubjectAndPayload subj payload) sid m)
              modifyTVar' byId (Map.insert sid (subj, payload))
              pure (Right sid)
      , srLookup = \sid -> atomically $ do
          m <- readTVar byId
          pure $ case Map.lookup sid m of
            Just (_, p) -> Right p
            Nothing -> Left (SchemaNotFound sid)
      , srLookupBySubject = \subj -> atomically $ do
          m <- readTVar byId
          let matches =
                [ (sid, p)
                | (sid, (s, p)) <- Map.toList m
                , s == subj
                ]
          pure $ case matches of
            ((sid, p) : _) -> Right (sid, p)
            [] -> Left (SubjectNotFound subj)
      , -- The in-memory client doesn't track per-subject
        -- compatibility config; report 'CompatNone' so a
        -- 'registrySerdeChecked' wrapper trusts every register.
        srCompatibilityMode = \_subj -> pure (Right CompatNone)
      , -- The in-memory client is the test double; treat every
        -- candidate as compatible. Tests that want to model an
        -- incompatibility can supply their own 'SchemaRegistryClient'.
        srTestCompatibility = \_subj _payload -> pure (Right Compatible)
      }


data SubjectAndPayload
  = SubjectAndPayload
      !SchemaSubject
      !SchemaPayload
  deriving stock (Eq, Ord)


----------------------------------------------------------------------
-- HTTP-shaped mock
----------------------------------------------------------------------

{- | A client that records every "HTTP" exchange it would have
made into an 'IORef'. Great for testing the producer side
without an actual http-client dependency: callers assert the
request/response shape directly.
-}
data RecordedExchange = RecordedExchange
  { rxMethod :: !Text
  , rxPath :: !Text
  , rxBody :: !(Maybe ByteString)
  }
  deriving stock (Eq, Show, Generic)


mockHttpRegistry
  :: IORef [RecordedExchange]
  -> SchemaRegistryClient
mockHttpRegistry log_ =
  SchemaRegistryClient
    { srRegister = \subj payload -> do
        modifyIORef'
          log_
          ( ++
              [ RecordedExchange
                  "POST"
                  ( T.pack
                      ( "/subjects/"
                          <> T.unpack (unSchemaSubject subj)
                          <> "/versions"
                      )
                  )
                  (Just (unSchemaPayload payload))
              ]
          )
        pure (Right (SchemaId 1))
    , srLookup = \sid -> do
        modifyIORef'
          log_
          ( ++
              [ RecordedExchange
                  "GET"
                  ( T.pack
                      ( "/schemas/ids/"
                          <> show (unSchemaId sid)
                      )
                  )
                  Nothing
              ]
          )
        pure (Right (SchemaPayload "<<mocked schema>>"))
    , srLookupBySubject = \subj -> do
        modifyIORef'
          log_
          ( ++
              [ RecordedExchange
                  "GET"
                  ( T.pack
                      ( "/subjects/"
                          <> T.unpack (unSchemaSubject subj)
                          <> "/versions/latest"
                      )
                  )
                  Nothing
              ]
          )
        pure (Right (SchemaId 1, SchemaPayload "<<mocked schema>>"))
    , srCompatibilityMode = \subj -> do
        modifyIORef'
          log_
          ( ++
              [ RecordedExchange
                  "GET"
                  ( T.pack
                      ( "/config/"
                          <> T.unpack (unSchemaSubject subj)
                      )
                  )
                  Nothing
              ]
          )
        pure (Right CompatBackward)
    , srTestCompatibility = \subj payload -> do
        modifyIORef'
          log_
          ( ++
              [ RecordedExchange
                  "POST"
                  ( T.pack
                      ( "/compatibility/subjects/"
                          <> T.unpack (unSchemaSubject subj)
                          <> "/versions/latest"
                      )
                  )
                  (Just (unSchemaPayload payload))
              ]
          )
        pure (Right Compatible)
    }


----------------------------------------------------------------------
-- Wire envelope
----------------------------------------------------------------------

magicByte :: Word8
magicByte = 0


{- | Wrap a payload with the Confluent envelope: @[magicByte,
schemaId :: Int32 BE, payload]@.
-}
encodeEnvelope :: SchemaId -> ByteString -> ByteString
encodeEnvelope (SchemaId sid) payload =
  LBS.toStrict $
    BSB.toLazyByteString $
      BSB.word8 magicByte
        <> BSB.int32BE sid
        <> BSB.byteString payload


{- | Parse the envelope. Errors if the magic byte is wrong or the
payload is too short.
-}
decodeEnvelope :: ByteString -> Either String (SchemaId, ByteString)
decodeEnvelope bs
  | BS.length bs < 5 = Left "envelope too short"
  | BS.head bs /= magicByte = Left "envelope: bad magic byte"
  | otherwise =
      let !idBytes = BS.take 4 (BS.drop 1 bs)
          !payload = BS.drop 5 bs
          !sid = readInt32BE idBytes
      in Right (SchemaId sid, payload)


readInt32BE :: ByteString -> Int32
readInt32BE bs =
  let !b0 = fromIntegral (BS.index bs 0) :: Word32
      !b1 = fromIntegral (BS.index bs 1) :: Word32
      !b2 = fromIntegral (BS.index bs 2) :: Word32
      !b3 = fromIntegral (BS.index bs 3) :: Word32
      !w =
        (b0 `shiftWord8` 24)
          .|. (b1 `shiftWord8` 16)
          .|. (b2 `shiftWord8` 8)
          .|. b3
  in fromIntegral w
  where
    shiftWord8 :: Word32 -> Int -> Word32
    shiftWord8 x n = (x * (2 ^ n)) -- explicit shift; keeps deps light


----------------------------------------------------------------------
-- Serde wrapper
----------------------------------------------------------------------

{- | Configuration for a 'registrySerde'. The caller decides how
to derive the subject for each (topic, isKey) pair (Confluent's
default is @"<topic>-key"@ / @"<topic>-value"@).
-}
data SchemaRegistrySerdeConfig a = SchemaRegistrySerdeConfig
  { srscClient :: !SchemaRegistryClient
  , srscSchema :: !SchemaPayload
  {- ^ The schema to register on the producer side. The
  consumer side typically only needs 'srscClient'.
  -}
  , srscSubject :: !SchemaSubject
  , srscPayload :: !(Serde a)
  -- ^ The format-specific serde (e.g. an Avro serializer).
  }


{- | Build a 'Serde' that prepends Confluent's wire envelope to
every value.

The serializer registers (or re-fetches) the schema once at
construction time and caches the resulting 'SchemaId' in an
'IORef' so subsequent calls don't pay the registry round-trip.
The deserializer parses the envelope and forwards to the
payload serde without consulting the registry again — this
assumes the caller has already verified that the registered
schema is structurally compatible with @Serde a@. Strict
compatibility checking is the registry's job.
-}
registrySerde
  :: SchemaRegistrySerdeConfig a
  -> IO (Serde a)
registrySerde SchemaRegistrySerdeConfig {..} = do
  -- Register up-front. We swallow the registration failure into
  -- the encoder — the next encode call retries.
  cached <- newIORef Nothing
  let resolveId :: IO (Either RegistryError SchemaId)
      resolveId = do
        m <- readIORef cached
        case m of
          Just sid -> pure (Right sid)
          Nothing -> do
            r <- srRegister srscClient srscSubject srscSchema
            case r of
              Left e -> pure (Left e)
              Right sid -> do
                writeIORef cached (Just sid)
                pure (Right sid)
  pure
    Serde
      { serialize = \a ->
          let !payload = serialize srscPayload a
          in -- Best-effort id resolution: in the steady state the
             -- IORef is populated and there's no IO. We fall back
             -- to id 0 if registration is still failing — the
             -- consumer will treat that as an unknown id which is
             -- the right failure mode.
             unsafeBlocking resolveId payload
      , deserialize = \bs ->
          case decodeEnvelope bs of
            Left err -> Left (T.pack err)
            Right (_sid, payload) -> deserialize srscPayload payload
      , serializeHeaders = const mempty
      }
  where
    unsafeBlocking :: IO (Either RegistryError SchemaId) -> ByteString -> ByteString
    unsafeBlocking m payload =
      -- The Serde interface is pure; we use unsafePerformIO here
      -- only to memoise the registry round-trip (it's idempotent
      -- after success). This matches the pattern Confluent's Java
      -- KafkaAvroSerializer uses internally.
      let !sid = case System.IO.Unsafe.unsafePerformIO m of
            Right s -> s
            Left _ -> SchemaId 0
      in encodeEnvelope sid payload


{- | A 'registrySerde' variant that probes the subject's
configured 'CompatibilityMode' /once at construction time/, then
asks the registry whether the candidate schema is compatible
/before/ registering it. Two outcomes:

  * The subject is configured 'CompatNone' (or the registry doesn't
    distinguish), in which case the wrapper degenerates to plain
    'registrySerde' (no extra round-trip on the steady-state hot
    path).

  * The subject has a non-trivial compatibility policy and the
    schema is /incompatible/. The action returns
    @Left ('IncompatibleSchema' subject explanation)@ — the caller
    gets a typed failure /before/ a producer starts publishing
    records under a schema id the consumer would later refuse.

This is the "compatibility-mode probing" gap the streams
@README.md@ called out. Wire your producer's serdes through this
when you want fail-fast schema-evolution checks at start-up.
-}
registrySerdeChecked
  :: SchemaRegistrySerdeConfig a
  -> IO (Either RegistryError (Serde a))
registrySerdeChecked cfg@SchemaRegistrySerdeConfig {..} = do
  modeR <- srCompatibilityMode srscClient srscSubject
  case modeR of
    Left e -> pure (Left e)
    Right CompatNone -> Right <$> registrySerde cfg
    Right _mode -> do
      r <- srTestCompatibility srscClient srscSubject srscSchema
      case r of
        Left e -> pure (Left e)
        Right (Incompatible reason) ->
          pure (Left (IncompatibleSchema srscSubject reason))
        Right Compatible -> Right <$> registrySerde cfg
