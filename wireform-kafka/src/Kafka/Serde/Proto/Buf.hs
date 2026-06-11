{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
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

When the record /key/ is itself a typed Protobuf message, the parallel
@buf.registry.key.schema.*@ headers carry its identity; use
'bufProtoKeySerde' as the topic key serde (or 'bufProtoSerdeFor'
'KeySchema' / the @*For@ header helpers) and the typed
'Kafka.Client.Producer.publish' path attaches the key headers alongside
any value headers.

For consuming a topic that carries many variant types, compile the
handlers once into a 'HandlerMap' with 'handlerMap' and route with
'dispatchWith' for O(1) lookup instead of the linear list 'dispatch'.

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
module Kafka.Serde.Proto.Buf (
  -- * Header-carrying serdes
  bufProtoSerde,
  bufProtoKeySerde,
  bufProtoSerdeFor,

  -- * Schema side
  SchemaSide (..),

  -- * Schema identity
  SchemaIdentity (..),
  fqn,
  identityOf,

  -- * Header names

  --

  {- | The value-side constants are byte-identical to
  @bufbuild/bsr-kafka-serde@ and Bufstream; the @*For@ functions
  and the @key*@ constants give the matching key-side names.
  -}
  messageHeaderName,
  commitHeaderName,
  moduleHeaderName,
  keyMessageHeaderName,
  keyCommitHeaderName,
  keyModuleHeaderName,
  messageHeaderNameFor,
  commitHeaderNameFor,
  moduleHeaderNameFor,

  -- * Building headers by hand
  bufSchemaHeaders,
  bufSchemaHeadersFor,
  addBufSchemaHeaders,
  addBufSchemaHeadersFor,

  -- * Consume side
  readMessageHeader,
  readMessageHeaderFor,
  BufHeaderError (..),

  -- * Header-discriminated dispatch
  decodeAs,
  decodeAsFor,
  Handler (..),
  DispatchError (..),

  -- ** List dispatch
  dispatch,
  dispatchFor,

  -- ** O(1) prebuilt-map dispatch
  HandlerMap,
  handlerMap,
  dispatchWith,
  dispatchWithFor,
) where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.HashMap.Strict qualified as HM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kafka.Headers qualified as H
import Kafka.Serde (Serde, withHeaders)
import Kafka.Serde.Proto (decodeProto, protoSerde)
import Proto.Decode (MessageDecode)
import Proto.Encode (MessageEncode)
import Proto.Schema (ProtoMessage (..))


{- | Which side of the record a schema identity describes. The Buf
convention uses a parallel set of headers for the key and the value:
@buf.registry.value.schema.*@ and @buf.registry.key.schema.*@.
-}
data SchemaSide
  = -- | @buf.registry.key.schema.*@
    KeySchema
  | -- | @buf.registry.value.schema.*@
    ValueSchema
  deriving stock (Eq, Show)


-- | The wire token for a side (@"key"@ \/ @"value"@).
sideToken :: SchemaSide -> Text
sideToken KeySchema = "key"
sideToken ValueSchema = "value"


{- | 'Kafka.Serde.Proto.protoSerde' that additionally stamps the Buf
schema-identity headers (@message@ + @commit@, optionally @module@)
onto every record produced through the typed
'Kafka.Client.Producer.publish' path. The serialized value bytes are
unchanged 'Kafka.Serde.Proto.encodeProto' output — the identity is
carried by the serde's 'Kafka.Serde.serializeHeaders' channel, not in
the payload.

This is the value-side serde (@'bufProtoSerdeFor' 'ValueSchema'@); use
it as a 'Kafka.Topic.Topic' value serde and produce as normal:

@
orders = Topic.'Topic.topic' \"orders\" keySerde (bufProtoSerde commit Nothing)
_ <- publish p orders (Just k) order   -- @message@ + @commit@ attached
@

The @commit@ is a build-time constant; nothing here contacts the BSR
at runtime. Pass @'Just' moduleRef@ for the optional @module@ header
on multi-module topics.
-}
bufProtoSerde
  :: (MessageEncode a, MessageDecode a, ProtoMessage a)
  => Text
  -- ^ BSR commit id (build-time constant).
  -> Maybe Text
  -- ^ Optional Buf module reference (multi-module topics).
  -> Serde a
bufProtoSerde = bufProtoSerdeFor ValueSchema


{- | Key-side counterpart of 'bufProtoSerde' (@'bufProtoSerdeFor'
'KeySchema'@): a serde for a Protobuf message used as the record
/key/, stamping the @buf.registry.key.schema.*@ headers. Use it as a
'Kafka.Topic.Topic' key serde when the key is itself a typed Protobuf
message; the typed 'Kafka.Client.Producer.publish' path attaches the
key headers automatically alongside any value headers.
-}
bufProtoKeySerde
  :: (MessageEncode a, MessageDecode a, ProtoMessage a)
  => Text
  -> Maybe Text
  -> Serde a
bufProtoKeySerde = bufProtoSerdeFor KeySchema


{- | 'bufProtoSerde' \/ 'bufProtoKeySerde' generalised over the
'SchemaSide'. Stamps the side-appropriate @buf.registry.<side>.schema.*@
headers; the value codec is the bare 'Kafka.Serde.Proto.protoSerde'.
-}
bufProtoSerdeFor
  :: forall a
   . (MessageEncode a, MessageDecode a, ProtoMessage a)
  => SchemaSide
  -> Text
  -- ^ BSR commit id (build-time constant).
  -> Maybe Text
  -- ^ Optional Buf module reference (multi-module topics).
  -> Serde a
bufProtoSerdeFor side commit mModule =
  withHeaders
    (const (bufSchemaHeadersFor side (identityOf (Proxy :: Proxy a) commit mModule)))
    protoSerde


{- | The Buf schema identity carried in a record's headers. The
@message@ FQN is derived from the type's 'ProtoMessage' instance;
the @commit@ is a build-time constant the caller supplies (this
layer never contacts the BSR at runtime); @module@ is only emitted
for topics that carry messages from more than one Buf module.
-}
data SchemaIdentity = SchemaIdentity
  { siMessageFQN :: !Text
  -- ^ Fully-qualified Protobuf message name, e.g. @"payments.v1.OrderPlaced"@.
  , siCommit :: !Text
  -- ^ BSR commit id (dashless), a build-time constant.
  , siModule :: !(Maybe Text)
  -- ^ Buf module reference; emit only on multi-module topics.
  }
  deriving stock (Eq, Show)


{- | @buf.registry.\<side\>.schema.message@ — the fully-qualified
message type header name for the given side.
-}
messageHeaderNameFor :: SchemaSide -> Text
messageHeaderNameFor s = "buf.registry." <> sideToken s <> ".schema.message"


-- | @buf.registry.\<side\>.schema.commit@ for the given side.
commitHeaderNameFor :: SchemaSide -> Text
commitHeaderNameFor s = "buf.registry." <> sideToken s <> ".schema.commit"


-- | @buf.registry.\<side\>.schema.module@ for the given side.
moduleHeaderNameFor :: SchemaSide -> Text
moduleHeaderNameFor s = "buf.registry." <> sideToken s <> ".schema.module"


{- | Value-side message header name (@buf.registry.value.schema.message@).
Byte-identical to @bufbuild/bsr-kafka-serde@ and Bufstream.
-}
messageHeaderName :: Text
messageHeaderName = messageHeaderNameFor ValueSchema


-- | Value-side commit header name (@buf.registry.value.schema.commit@).
commitHeaderName :: Text
commitHeaderName = commitHeaderNameFor ValueSchema


-- | Value-side module header name (@buf.registry.value.schema.module@).
moduleHeaderName :: Text
moduleHeaderName = moduleHeaderNameFor ValueSchema


-- | Key-side message header name (@buf.registry.key.schema.message@).
keyMessageHeaderName :: Text
keyMessageHeaderName = messageHeaderNameFor KeySchema


-- | Key-side commit header name (@buf.registry.key.schema.commit@).
keyCommitHeaderName :: Text
keyCommitHeaderName = commitHeaderNameFor KeySchema


-- | Key-side module header name (@buf.registry.key.schema.module@).
keyModuleHeaderName :: Text
keyModuleHeaderName = moduleHeaderNameFor KeySchema


{- | The fully-qualified Protobuf message name for the @message@
header, sourced from the generated 'ProtoMessage' instance.

The @wireform-proto@ codegen emits 'protoMessageName' as the
/already/ fully-qualified name (e.g. @"payments.v1.OrderPlaced"@)
and 'protoPackageName' as the package alone (e.g. @"payments.v1"@).
This function returns 'protoMessageName' verbatim in that case, and
as a safety net also reconstructs the FQN for a hand-written
instance whose 'protoMessageName' is the bare type name with a
non-empty 'protoPackageName'. An empty package yields the bare name
with no leading dot.
-}
fqn :: ProtoMessage a => Proxy a -> Text
fqn p =
  let name = protoMessageName p
      pkg = protoPackageName p
  in if T.null pkg || (pkg <> ".") `T.isPrefixOf` name
       then name
       else pkg <> "." <> name


{- | Build a 'SchemaIdentity' from a 'ProtoMessage' type, a build-time
BSR commit, and an optional module reference.
-}
identityOf :: ProtoMessage a => Proxy a -> Text -> Maybe Text -> SchemaIdentity
identityOf p commit mModule = SchemaIdentity (fqn p) commit mModule


{- | The value-side Buf schema-identity header block for a message
(@'bufSchemaHeadersFor' 'ValueSchema'@). Always emits the @message@
and @commit@ headers; emits @module@ only when 'siModule' is 'Just'.
-}
bufSchemaHeaders :: SchemaIdentity -> H.Headers
bufSchemaHeaders = bufSchemaHeadersFor ValueSchema


{- | 'bufSchemaHeaders' for an explicit 'SchemaSide', emitting the
@buf.registry.\<side\>.schema.*@ header block.
-}
bufSchemaHeadersFor :: SchemaSide -> SchemaIdentity -> H.Headers
bufSchemaHeadersFor side (SchemaIdentity msg commit mMod) =
  let base =
        H.insertText (commitHeaderNameFor side) commit $
          H.insertText (messageHeaderNameFor side) msg H.empty
  in maybe base (\m -> H.insertText (moduleHeaderNameFor side) m base) mMod


{- | Merge the value-side identity headers onto an existing header
block, appending after whatever the caller already had.
-}
addBufSchemaHeaders :: SchemaIdentity -> H.Headers -> H.Headers
addBufSchemaHeaders = addBufSchemaHeadersFor ValueSchema


-- | 'addBufSchemaHeaders' for an explicit 'SchemaSide'.
addBufSchemaHeadersFor :: SchemaSide -> SchemaIdentity -> H.Headers -> H.Headers
addBufSchemaHeadersFor side ident hs = hs <> bufSchemaHeadersFor side ident


-- | Failure modes for reading the @message@ header off a record.
data BufHeaderError
  = -- | The @message@ header is absent.
    MissingMessageHeader
  | -- | The @message@ header value was not valid UTF-8.
    MessageHeaderNotUtf8
  deriving stock (Eq, Show)


{- | Extract the fully-qualified message name from a record's value-side
headers (@'readMessageHeaderFor' 'ValueSchema'@).
-}
readMessageHeader :: H.Headers -> Either BufHeaderError Text
readMessageHeader = readMessageHeaderFor ValueSchema


{- | Extract the fully-qualified message name from the @message@ header
of the given 'SchemaSide'.
-}
readMessageHeaderFor :: SchemaSide -> H.Headers -> Either BufHeaderError Text
readMessageHeaderFor side hs =
  case H.lookup (messageHeaderNameFor side) hs of
    Nothing -> Left MissingMessageHeader
    Just bs -> case TE.decodeUtf8' bs of
      Left _ -> Left MessageHeaderNotUtf8
      Right t -> Right t


{- | Reasons a header-discriminated decode can fail. Every failure mode
is a typed 'Left'; nothing crashes and nothing is silently dropped.
-}
data DispatchError
  = -- | The @message@ header was missing or not UTF-8.
    HeaderError BufHeaderError
  | -- | The @message@ header was present but no handler matched its FQN.
    UnknownType Text
  | {- | 'decodeAs' only: @TypeMismatch expected actual@ when the
    record's @message@ FQN differs from the requested type.
    -}
    TypeMismatch Text Text
  | {- | The Protobuf wire decode failed; carries the rendered
    @wireform-proto@ 'Proto.Decode.DecodeError'.
    -}
    DecodeFailure Text
  deriving stock (Eq, Show)


{- | Decode a record's value /as/ a specific type, but only after
confirming the record's @message@ header names exactly that type.
A mismatch short-circuits with 'TypeMismatch' before any decode is
attempted, so a wrong-typed payload can never be mis-parsed.
-}
decodeAs
  :: (MessageDecode a, ProtoMessage a)
  => Proxy a
  -> H.Headers
  -> ByteString
  -> Either DispatchError a
decodeAs = decodeAsFor ValueSchema


-- | 'decodeAs' against the @message@ header of an explicit 'SchemaSide'.
decodeAsFor
  :: (MessageDecode a, ProtoMessage a)
  => SchemaSide
  -> Proxy a
  -> H.Headers
  -> ByteString
  -> Either DispatchError a
decodeAsFor side p hs bytes = do
  actual <- first HeaderError (readMessageHeaderFor side hs)
  let expected = fqn p
  if actual /= expected
    then Left (TypeMismatch expected actual)
    else first (DecodeFailure . T.pack) (decodeProto bytes)


{- | A registered decoder for one message type, paired with a handler
that consumes the decoded value into the common result type @r@.
-}
data Handler r
  = forall a. (MessageDecode a, ProtoMessage a) => Handler (Proxy a) (a -> r)


{- | Route a record to the handler whose registered type matches the
record's value-side @message@ header (@'dispatchFor' 'ValueSchema'@).

This is the documented cost of header discrimination over a closed
@oneof@ sum: an FQN with no registered handler is a runtime outcome,
surfaced here as @'Left' ('UnknownType' fqn)@ — never a crash, never
a silent drop. A missing\/garbled header is
@'Left' ('HeaderError' …)@.

This walks the handler list (O(n) in the number of registered types).
For a topic carrying many variants, build a 'HandlerMap' once with
'handlerMap' and route with 'dispatchWith' for O(1) lookup. (Note
that partially applying @'dispatchFor' side handlers@ and reusing the
resulting closure also shares a single 'HandlerMap'.)
-}
dispatch :: [Handler r] -> H.Headers -> ByteString -> Either DispatchError r
dispatch = dispatchFor ValueSchema


-- | 'dispatch' against the @message@ header of an explicit 'SchemaSide'.
dispatchFor :: SchemaSide -> [Handler r] -> H.Headers -> ByteString -> Either DispatchError r
dispatchFor side handlers = dispatchWithFor side (handlerMap handlers)


{- | A compiled dispatch table keyed on fully-qualified message name,
built once with 'handlerMap' and reused across records for O(1)
routing. Each entry decodes the value bytes and applies the
registered handler, yielding the common result type @r@.
-}
newtype HandlerMap r = HandlerMap (HM.HashMap Text (ByteString -> Either DispatchError r))


{- | Compile a list of 'Handler's into a 'HandlerMap'. On duplicate
FQNs the /first/ handler wins, matching the list-'dispatch'
semantics. Build this once (e.g. at consumer start-up) and reuse it
with 'dispatchWith'.
-}
handlerMap :: [Handler r] -> HandlerMap r
handlerMap = HandlerMap . HM.fromListWith (\_new old -> old) . fmap toEntry
  where
    toEntry :: Handler r -> (Text, ByteString -> Either DispatchError r)
    toEntry (Handler p f) =
      ( fqn p
      , \bytes -> case decodeProto bytes of
          Left e -> Left (DecodeFailure (T.pack e))
          Right a -> Right (f a)
      )


{- | Route a record through a prebuilt 'HandlerMap' on the value-side
@message@ header (@'dispatchWithFor' 'ValueSchema'@). O(1) in the
number of registered types.
-}
dispatchWith :: HandlerMap r -> H.Headers -> ByteString -> Either DispatchError r
dispatchWith = dispatchWithFor ValueSchema


{- | 'dispatchWith' against the @message@ header of an explicit
'SchemaSide'.
-}
dispatchWithFor :: SchemaSide -> HandlerMap r -> H.Headers -> ByteString -> Either DispatchError r
dispatchWithFor side (HandlerMap m) hs bytes = do
  actual <- first HeaderError (readMessageHeaderFor side hs)
  case HM.lookup actual m of
    Nothing -> Left (UnknownType actual)
    Just dec -> dec bytes
