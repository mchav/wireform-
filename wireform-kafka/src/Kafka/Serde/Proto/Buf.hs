{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Kafka.Serde.Proto.Buf
Description : Buf Schema Registry header-discrimination layer over 'Kafka.Serde.Proto'.
Copyright   : (c) 2025
License     : BSD-3-Clause

A thin, schema-identity layer built /on top of/ the bare-bytes
"Kafka.Serde.Proto" codec. It follows the
@bufbuild/bsr-kafka-serde@ convention (the same one Bufstream and the
@bsr-kafka-serde-{java,go}@ libraries use): the Protobuf message body
is written unchanged, and the schema identity travels in Kafka record
__headers__, not in the payload.

= Recommended usage

Produce side — the schema identity rides /in the serde/. Build the
value serde with 'bufProtoSerde' and the ordinary
'Kafka.Client.Producer.publish' attaches the @message@ + @commit@
headers automatically; there is no bespoke produce helper:

@
import qualified Kafka                 as Kafka
import qualified Kafka.Topic           as Topic
import qualified Kafka.Serde           as Serde
import           Kafka.Serde.Proto.Buf (bufProtoSerde)

import qualified My.Proto.Generated as Pb  -- @wireform-proto@-generated

orders :: Topic.'Topic.Topic' Text Pb.OrderPlaced
orders = Topic.'Topic.topic' \"orders\" Serde.'Serde.textSerde' (bufProtoSerde bsrCommit Nothing)

main =
  Kafka.'Kafka.withProducer' [\"localhost:9092\"] Kafka.'Kafka.defaultProducerConfig' $ \\p ->
    Kafka.'Kafka.publish' p orders (Just \"order-42\") order  -- headers stamped automatically
@

The header plumbing ('identityOf' \/ 'bufSchemaHeaders' \/
'addBufSchemaHeaders') is still exported for callers that construct
records by hand (e.g. multi-module topics, or attaching the @module@
header).

Consume side — read the @message@ header off the record and route to a
typed decoder. Wrap the record's @headers@ list with 'Kafka.Headers.fromList'
first:

@
case dispatch handlers (H.fromList rec.headers) rec.value of
  Right r                    -> handleResult r
  Left (UnknownType fqn)     -> ...   -- header present, no handler
  Left (HeaderError e)       -> ...   -- header missing / not UTF-8
  Left (TypeMismatch e a)    -> ...   -- 'decodeAs' only
  Left (DecodeFailure msg)   -> ...   -- wire decode failed
 where
  handlers =
    [ Handler (Proxy :: Proxy Pb.OrderPlaced)  onPlaced
    , Handler (Proxy :: Proxy Pb.OrderShipped) onShipped
    ]
@

= Wire format

The record __value is exactly__ 'Kafka.Serde.Proto.encodeProto' — the
raw Protobuf message body. There is __no magic byte, no schema-ID
prefix, and no Confluent Schema Registry envelope__. This is
deliberately /not/ the @KafkaProtobufSerializer@ framing produced by the
Confluent JVM serializer (and exposed in this repo as
@Kafka.Streams.Serde.SchemaRegistry@); none of that machinery is used
here. This module is the Buf-convention, header-discriminated default:
the schema identity lives in the three @buf.registry.value.schema.*@
headers, and a single topic may carry multiple top-level message types
distinguished by the @message@ header.

= Why the headers live in the serde

'Kafka.Serde.Serde' carries a 'Kafka.Serde.serializeHeaders' channel
alongside its value codec: the record headers a value contributes when
produced through the typed 'Kafka.Client.Producer.publish' path. The
serialized value bytes stay exactly 'Kafka.Serde.Proto.encodeProto' —
the headers ride on the 'Kafka.Client.Producer.ProducerRecord', not in
the payload. 'bufProtoSerde' is therefore just 'Kafka.Serde.Proto.protoSerde'
with its header channel set to emit the schema identity; it wraps
(never replaces) the bare proto codec, and slots into any 'Kafka.Topic.Topic'.
-}
module Kafka.Serde.Proto.Buf
  ( -- * Header-carrying serde
    bufProtoSerde
    -- * Schema identity
  , SchemaIdentity (..)
  , fqn
  , identityOf
    -- * Header names
  , messageHeaderName
  , commitHeaderName
  , moduleHeaderName
    -- * Building headers by hand
  , bufSchemaHeaders
  , addBufSchemaHeaders
    -- * Consume side
  , readMessageHeader
  , BufHeaderError (..)
    -- * Header-discriminated dispatch
  , decodeAs
  , Handler (..)
  , dispatch
  , DispatchError (..)
  ) where

import           Data.Bifunctor      (first)
import           Data.ByteString     (ByteString)
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as TE

import qualified Kafka.Headers       as H
import           Kafka.Serde         (Serde, withHeaders)
import           Kafka.Serde.Proto   (decodeProto, protoSerde)
import           Proto.Decode        (MessageDecode)
import           Proto.Encode        (MessageEncode)
import           Proto.Schema        (ProtoMessage (..))

-- | 'Kafka.Serde.Proto.protoSerde' that additionally stamps the Buf
-- schema-identity headers (@message@ + @commit@, optionally @module@)
-- onto every record produced through the typed
-- 'Kafka.Client.Producer.publish' path. The serialized value bytes are
-- unchanged 'Kafka.Serde.Proto.encodeProto' output — the identity is
-- carried by the serde's 'Kafka.Serde.serializeHeaders' channel, not in
-- the payload.
--
-- Use it as a 'Kafka.Topic.Topic' value serde and produce as normal:
--
-- @
-- orders = Topic.'Topic.topic' \"orders\" keySerde (bufProtoSerde commit Nothing)
-- _ <- publish p orders (Just k) order   -- @message@ + @commit@ attached
-- @
--
-- The @commit@ is a build-time constant; nothing here contacts the BSR
-- at runtime. Pass @'Just' moduleRef@ for the optional @module@ header
-- on multi-module topics.
bufProtoSerde
  :: forall a. (MessageEncode a, MessageDecode a, ProtoMessage a)
  => Text       -- ^ BSR commit id (build-time constant).
  -> Maybe Text -- ^ Optional Buf module reference (multi-module topics).
  -> Serde a
bufProtoSerde commit mModule =
  withHeaders
    (const (bufSchemaHeaders (identityOf (Proxy :: Proxy a) commit mModule)))
    protoSerde

-- | The Buf schema identity carried in a record's headers. The
-- @message@ FQN is derived from the type's 'ProtoMessage' instance;
-- the @commit@ is a build-time constant the caller supplies (this
-- layer never contacts the BSR at runtime); @module@ is only emitted
-- for topics that carry messages from more than one Buf module.
data SchemaIdentity = SchemaIdentity
  { siMessageFQN :: !Text
    -- ^ Fully-qualified Protobuf message name, e.g. @"payments.v1.OrderPlaced"@.
  , siCommit     :: !Text
    -- ^ BSR commit id (dashless), a build-time constant.
  , siModule     :: !(Maybe Text)
    -- ^ Buf module reference; emit only on multi-module topics.
  }
  deriving stock (Eq, Show)

-- | Header name for the fully-qualified message type. Byte-identical
-- to @bufbuild/bsr-kafka-serde@ and Bufstream.
messageHeaderName :: Text
messageHeaderName = "buf.registry.value.schema.message"

-- | Header name for the BSR commit id.
commitHeaderName :: Text
commitHeaderName = "buf.registry.value.schema.commit"

-- | Header name for the optional Buf module reference.
moduleHeaderName :: Text
moduleHeaderName = "buf.registry.value.schema.module"

-- | The fully-qualified Protobuf message name for the @message@
-- header, sourced from the generated 'ProtoMessage' instance.
--
-- The @wireform-proto@ codegen emits 'protoMessageName' as the
-- /already/ fully-qualified name (e.g. @"payments.v1.OrderPlaced"@)
-- and 'protoPackageName' as the package alone (e.g. @"payments.v1"@).
-- This function returns 'protoMessageName' verbatim in that case, and
-- as a safety net also reconstructs the FQN for a hand-written
-- instance whose 'protoMessageName' is the bare type name with a
-- non-empty 'protoPackageName'. An empty package yields the bare name
-- with no leading dot.
fqn :: ProtoMessage a => Proxy a -> Text
fqn p =
  let name = protoMessageName p
      pkg  = protoPackageName p
   in if T.null pkg || (pkg <> ".") `T.isPrefixOf` name
        then name
        else pkg <> "." <> name

-- | Build a 'SchemaIdentity' from a 'ProtoMessage' type, a build-time
-- BSR commit, and an optional module reference.
identityOf :: ProtoMessage a => Proxy a -> Text -> Maybe Text -> SchemaIdentity
identityOf p commit mModule = SchemaIdentity (fqn p) commit mModule

-- | The Buf schema-identity header block for a message. Always emits
-- the @message@ and @commit@ headers; emits @module@ only when
-- 'siModule' is 'Just'.
bufSchemaHeaders :: SchemaIdentity -> H.Headers
bufSchemaHeaders (SchemaIdentity msg commit mMod) =
  let base =
        H.insertText commitHeaderName commit $
          H.insertText messageHeaderName msg H.empty
   in maybe base (\m -> H.insertText moduleHeaderName m base) mMod

-- | Merge the identity headers onto an existing header block,
-- appending after whatever the caller already had.
addBufSchemaHeaders :: SchemaIdentity -> H.Headers -> H.Headers
addBufSchemaHeaders ident hs = hs <> bufSchemaHeaders ident

-- | Failure modes for reading the @message@ header off a record.
data BufHeaderError
  = MissingMessageHeader
    -- ^ The @message@ header is absent.
  | MessageHeaderNotUtf8
    -- ^ The @message@ header value was not valid UTF-8.
  deriving stock (Eq, Show)

-- | Extract the fully-qualified message name from a record's headers.
readMessageHeader :: H.Headers -> Either BufHeaderError Text
readMessageHeader hs =
  case H.lookup messageHeaderName hs of
    Nothing -> Left MissingMessageHeader
    Just bs -> case TE.decodeUtf8' bs of
      Left _  -> Left MessageHeaderNotUtf8
      Right t -> Right t

-- | Reasons a header-discriminated decode can fail. Every failure mode
-- is a typed 'Left'; nothing crashes and nothing is silently dropped.
data DispatchError
  = HeaderError BufHeaderError
    -- ^ The @message@ header was missing or not UTF-8.
  | UnknownType Text
    -- ^ The @message@ header was present but no handler matched its FQN.
  | TypeMismatch Text Text
    -- ^ 'decodeAs' only: @TypeMismatch expected actual@ when the
    --   record's @message@ FQN differs from the requested type.
  | DecodeFailure Text
    -- ^ The Protobuf wire decode failed; carries the rendered
    --   @wireform-proto@ 'Proto.Decode.DecodeError'.
  deriving stock (Eq, Show)

-- | Decode a record's value /as/ a specific type, but only after
-- confirming the record's @message@ header names exactly that type.
-- A mismatch short-circuits with 'TypeMismatch' before any decode is
-- attempted, so a wrong-typed payload can never be mis-parsed.
decodeAs
  :: (MessageDecode a, ProtoMessage a)
  => Proxy a
  -> H.Headers
  -> ByteString
  -> Either DispatchError a
decodeAs p hs valueBytes = do
  actual <- first HeaderError (readMessageHeader hs)
  let expected = fqn p
  if actual /= expected
    then Left (TypeMismatch expected actual)
    else first (DecodeFailure . T.pack) (decodeProto valueBytes)

-- | A registered decoder for one message type, paired with a handler
-- that consumes the decoded value into the common result type @r@.
data Handler r
  = forall a. (MessageDecode a, ProtoMessage a) => Handler (Proxy a) (a -> r)

-- | Route a record to the handler whose registered type matches the
-- record's @message@ header, decode, and apply the handler.
--
-- This is the documented cost of header discrimination over a closed
-- @oneof@ sum: an FQN with no registered handler is a runtime outcome,
-- surfaced here as @'Left' ('UnknownType' fqn)@ — never a crash, never
-- a silent drop. A missing\/garbled header is
-- @'Left' ('HeaderError' …)@.
dispatch :: [Handler r] -> H.Headers -> ByteString -> Either DispatchError r
dispatch handlers hs valueBytes = do
  actual <- first HeaderError (readMessageHeader hs)
  go handlers actual
  where
    go [] actual = Left (UnknownType actual)
    go (Handler p f : rest) actual
      | fqn p == actual =
          case decodeProto valueBytes of
            Left e  -> Left (DecodeFailure (T.pack e))
            Right a -> Right (f a)
      | otherwise = go rest actual
