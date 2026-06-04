{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Tests for the Buf header-discrimination layer over the proto
-- serde ("Kafka.Serde.Proto.Buf"): the header-carrying 'bufProtoSerde',
-- the schema-identity header derivation, and the total
-- header-discriminated consume dispatch.
module Serde.ProtoBufSpec (tests) where

import           Data.Bifunctor       (first)
import           Data.ByteString      (ByteString)
import qualified Data.Map.Strict      as Map
import           Data.Proxy           (Proxy (..))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import qualified Data.Text.IO         as TIO
import           Data.Word            (Word64)

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.Hedgehog  (testProperty)
import           Test.Tasty.HUnit     (assertBool, testCase, (@?=))

import qualified Kafka.Headers        as H
import qualified Kafka.Serde          as Serde
import           Kafka.Serde.Proto    (encodeProto)
import           Kafka.Serde.Proto.Buf
import           Kafka.Serde.Proto.Buf.TH (bufCommitFromLock, bufProtoKeySerdeFromLock, bufProtoSerdeFromLock, lookupBufLockCommit)

import           Proto.Decode         (MessageDecode (..), getTagOr, getText, getVarint, skipField)
import           Proto.Encode         (MessageEncode (..), encodeFieldString, encodeFieldVarint)
import           Proto.Google.Protobuf.Timestamp (Timestamp)
import           Proto.Internal.Wire  (Tag (..))
import           Proto.Schema         (ProtoMessage (..))

----------------------------------------------------------------------
-- Two-message fixture: payments.v1.{OrderPlaced,OrderShipped}
----------------------------------------------------------------------

data OrderPlaced = OrderPlaced
  { opId  :: !Word64
  , opSku :: !Text
  }
  deriving stock (Eq, Show)

data OrderShipped = OrderShipped
  { osId      :: !Word64
  , osCarrier :: !Text
  }
  deriving stock (Eq, Show)

-- | A message with an empty package, to exercise the no-leading-dot
-- branch of 'fqn'.
newtype BareEvent = BareEvent Word64
  deriving stock (Eq, Show)

instance MessageEncode OrderPlaced where
  buildMessage m =
    (if opId m /= 0 then encodeFieldVarint 1 (opId m) else mempty)
      <> (if opSku m /= "" then encodeFieldString 2 (opSku m) else mempty)

instance MessageDecode OrderPlaced where
  messageDecoder = loop 0 ""
    where
      loop !i !sku = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (OrderPlaced i sku)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v sku
            2 -> getText >>= \v -> loop i v
            _ -> skipField wt >> loop i sku
  {-# INLINE messageDecoder #-}

instance ProtoMessage OrderPlaced where
  protoMessageName _ = "payments.v1.OrderPlaced"
  protoPackageName _ = "payments.v1"
  protoDefaultValue = OrderPlaced 0 ""
  protoFieldDescriptors _ = Map.empty

instance MessageEncode OrderShipped where
  buildMessage m =
    (if osId m /= 0 then encodeFieldVarint 1 (osId m) else mempty)
      <> (if osCarrier m /= "" then encodeFieldString 2 (osCarrier m) else mempty)

instance MessageDecode OrderShipped where
  messageDecoder = loop 0 ""
    where
      loop !i !carrier = do
        mt <- getTagOr
        case mt of
          Nothing -> pure (OrderShipped i carrier)
          Just (Tag fn wt) -> case fn of
            1 -> getVarint >>= \v -> loop v carrier
            2 -> getText >>= \v -> loop i v
            _ -> skipField wt >> loop i carrier
  {-# INLINE messageDecoder #-}

instance ProtoMessage OrderShipped where
  protoMessageName _ = "payments.v1.OrderShipped"
  protoPackageName _ = "payments.v1"
  protoDefaultValue = OrderShipped 0 ""
  protoFieldDescriptors _ = Map.empty

instance ProtoMessage BareEvent where
  protoMessageName _ = "BareEvent"
  protoDefaultValue = BareEvent 0
  protoFieldDescriptors _ = Map.empty

----------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------

genOrderPlaced :: Gen OrderPlaced
genOrderPlaced =
  OrderPlaced
    <$> Gen.word64 (Range.linear 0 1000000)
    <*> Gen.text (Range.linear 0 32) Gen.alphaNum

genOrderShipped :: Gen OrderShipped
genOrderShipped =
  OrderShipped
    <$> Gen.word64 (Range.linear 0 1000000)
    <*> Gen.text (Range.linear 0 32) Gen.alphaNum

genCommit :: Gen Text
genCommit = Gen.text (Range.linear 1 40) Gen.hexit

----------------------------------------------------------------------
-- Dispatch target
----------------------------------------------------------------------

data Routed
  = RPlaced OrderPlaced
  | RShipped OrderShipped
  deriving stock (Eq, Show)

handlers :: [Handler Routed]
handlers =
  [ Handler (Proxy :: Proxy OrderPlaced) RPlaced
  , Handler (Proxy :: Proxy OrderShipped) RShipped
  ]

placedHeaders :: Text -> H.Headers
placedHeaders c = bufSchemaHeaders (identityOf (Proxy :: Proxy OrderPlaced) c Nothing)

shippedHeaders :: Text -> H.Headers
shippedHeaders c = bufSchemaHeaders (identityOf (Proxy :: Proxy OrderShipped) c Nothing)

----------------------------------------------------------------------
-- Tests
----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Kafka.Serde.Proto.Buf"
    [ testGroup
        "header names are byte-identical to the Buf convention"
        [ testCase "message" $ messageHeaderName @?= "buf.registry.value.schema.message"
        , testCase "commit" $ commitHeaderName @?= "buf.registry.value.schema.commit"
        , testCase "module" $ moduleHeaderName @?= "buf.registry.value.schema.module"
        ]
    , testGroup
        "fully-qualified name"
        [ testCase "packaged message keeps its FQN" $
            fqn (Proxy :: Proxy OrderPlaced) @?= "payments.v1.OrderPlaced"
        , testCase "empty package => no leading dot" $
            fqn (Proxy :: Proxy BareEvent) @?= "BareEvent"
        , -- Guard against a regression of the package-doubling bug on a
          -- real codegen-emitted instance (protoMessageName is already
          -- fully qualified, so fqn must not prepend the package again).
          testCase "real generated instance (google.protobuf.Timestamp) is not doubled" $ do
            fqn (Proxy :: Proxy Timestamp) @?= "google.protobuf.Timestamp"
            protoMessageName (Proxy :: Proxy Timestamp) @?= "google.protobuf.Timestamp"
            protoPackageName (Proxy :: Proxy Timestamp) @?= "google.protobuf"
        ]
    , testGroup
        "bufSchemaHeaders"
        [ testCase "Nothing module => exactly message + commit" $ do
            let hs = placedHeaders "abc123"
            H.lookup messageHeaderName hs @?= Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
            H.lookup commitHeaderName hs @?= Just (TE.encodeUtf8 "abc123")
            H.lookup moduleHeaderName hs @?= Nothing
            H.length hs @?= 2
        , testCase "Just module => message + commit + module" $ do
            let hs =
                  bufSchemaHeaders
                    (identityOf (Proxy :: Proxy OrderPlaced) "abc123" (Just "buf.build/acme/payments"))
            H.lookup moduleHeaderName hs @?= Just (TE.encodeUtf8 "buf.build/acme/payments")
            H.length hs @?= 3
        ]
    , testGroup
        "bufProtoSerde carries the identity in the serde"
        [ testProperty "value bytes are the bare encodeProto output (no prefix)" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              Serde.serialize (bufProtoSerde c Nothing :: Serde.Serde OrderPlaced) x === encodeProto x
        , testProperty "serde headers equal the hand-built identity headers" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              Serde.serializeHeaders (bufProtoSerde c Nothing :: Serde.Serde OrderPlaced) x
                === placedHeaders c
        , testProperty "deserialize round-trips through the serde" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              let s = bufProtoSerde c Nothing :: Serde.Serde OrderPlaced
              Serde.deserialize s (Serde.serialize s x) === Right x
        ]
    , testGroup
        "decodeAs"
        [ testProperty "round-trips a matching type" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              decodeAs (Proxy :: Proxy OrderPlaced) (placedHeaders c) (encodeProto x) === Right x
        , testProperty "the commit header round-trips unmodified" $
            property $ do
              c <- forAll genCommit
              H.lookup commitHeaderName (placedHeaders c) === Just (TE.encodeUtf8 c)
        , testCase "wrong header type => TypeMismatch, no decode attempted" $ do
            let bytes = encodeProto (OrderShipped 7 "u:ups")
            decodeAs (Proxy :: Proxy OrderPlaced) (shippedHeaders "c1") bytes
              @?= Left (TypeMismatch "payments.v1.OrderPlaced" "payments.v1.OrderShipped")
        , testCase "missing header => HeaderError MissingMessageHeader" $
            decodeAs (Proxy :: Proxy OrderPlaced) H.empty (encodeProto (OrderPlaced 1 "x"))
              @?= Left (HeaderError MissingMessageHeader)
        ]
    , testGroup
        "dispatch (header-discriminated, total)"
        [ testProperty "routes a mixed stream to the right handler" $
            property $ do
              c <- forAll genCommit
              p <- forAll genOrderPlaced
              s <- forAll genOrderShipped
              dispatch handlers (placedHeaders c) (encodeProto p) === Right (RPlaced p)
              dispatch handlers (shippedHeaders c) (encodeProto s) === Right (RShipped s)
        , testCase "unregistered FQN => UnknownType (no crash, no drop)" $ do
            let hs = H.singleton messageHeaderName (TE.encodeUtf8 "payments.v1.Unknown")
            dispatch handlers hs "anything"
              @?= Left (UnknownType "payments.v1.Unknown")
        , testCase "missing header => HeaderError MissingMessageHeader" $
            dispatch handlers H.empty "anything"
              @?= Left (HeaderError MissingMessageHeader)
        , testCase "non-UTF-8 header => HeaderError MessageHeaderNotUtf8" $ do
            let hs = H.singleton messageHeaderName invalidUtf8
            first (const ()) (readMessageHeader hs) @?= Left ()
            dispatch handlers hs "anything"
              @?= Left (HeaderError MessageHeaderNotUtf8)
        ]
    , testGroup
        "Template Haskell auto-population (buf.lock)"
        [ testGroup
            "lookupBufLockCommit (pure)"
            [ testCase "v2 name: entry" $
                lookupBufLockCommit "buf.build/acme/payments" v2Lock
                  @?= Right "0a1b2c3d"
            , testCase "second dep in the file" $
                lookupBufLockCommit "buf.build/googleapis/googleapis" v2Lock
                  @?= Right "ffffffff"
            , testCase "v1 remote/owner/repository entry" $
                lookupBufLockCommit "buf.build/acme/payments" v1Lock
                  @?= Right "11112222"
            , testCase "unknown module => Left" $
                assertBool "expected Left" $
                  case lookupBufLockCommit "buf.build/nope/nope" v2Lock of
                    Left _  -> True
                    Right _ -> False
            ]
        , testGroup
            "compile-time splice (committed fixture buf.lock)"
            [ testCase "bufCommitFromLock embeds the pinned commit" $
                splicedCommit @?= "0a1b2c3d4e5f60718293a4b5c6d7e8f9"
            , testCase "bufProtoSerdeFromLock stamps commit + message headers" $ do
                let hs = Serde.serializeHeaders splicedSerde (OrderPlaced 1 "sku")
                H.lookup commitHeaderName hs
                  @?= Just (TE.encodeUtf8 "0a1b2c3d4e5f60718293a4b5c6d7e8f9")
                H.lookup messageHeaderName hs
                  @?= Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
                H.lookup moduleHeaderName hs @?= Nothing
            , testCase "spliced serde still emits the bare encodeProto value" $ do
                let x = OrderPlaced 42 "widget"
                Serde.serialize splicedSerde x @?= encodeProto x
            , testCase "bufProtoKeySerdeFromLock stamps key-side commit + message" $ do
                let hs = Serde.serializeHeaders splicedKeySerde (OrderPlaced 1 "sku")
                H.lookup keyCommitHeaderName hs
                  @?= Just (TE.encodeUtf8 "0a1b2c3d4e5f60718293a4b5c6d7e8f9")
                H.lookup keyMessageHeaderName hs
                  @?= Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
            ]
        , testGroup
            "real buf-generated buf.lock (buf CLI 1.70.0)"
            [ testCase "parses a real v2 buf.lock (buf dep update)" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v2.lock"
                lookupBufLockCommit "buf.build/bufbuild/protovalidate" contents
                  @?= Right realCommit
            , testCase "parses a real v1 buf.lock (remote/owner/repository)" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v1.lock"
                lookupBufLockCommit "buf.build/bufbuild/protovalidate" contents
                  @?= Right realCommit
            , testCase "unknown module in a real buf.lock => Left" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v2.lock"
                assertBool "expected Left" $
                  case lookupBufLockCommit "buf.build/acme/nope" contents of
                    Left _  -> True
                    Right _ -> False
            , testCase "bufCommitFromLock splices the real commit at compile time" $
                realSplicedCommit @?= realCommit
            ]
        ]
    , testGroup
        "key-side schema identity"
        [ testCase "key header names are byte-identical to the convention" $ do
            keyMessageHeaderName @?= "buf.registry.key.schema.message"
            keyCommitHeaderName @?= "buf.registry.key.schema.commit"
            keyModuleHeaderName @?= "buf.registry.key.schema.module"
        , testCase "messageHeaderNameFor agrees with the side constants" $ do
            messageHeaderNameFor ValueSchema @?= messageHeaderName
            messageHeaderNameFor KeySchema @?= keyMessageHeaderName
        , testProperty "bufProtoKeySerde stamps key-side headers (not value-side)" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              let s  = bufProtoKeySerde c Nothing :: Serde.Serde OrderPlaced
                  hs = Serde.serializeHeaders s x
              H.lookup keyMessageHeaderName hs === Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
              H.lookup keyCommitHeaderName hs === Just (TE.encodeUtf8 c)
              H.lookup messageHeaderName hs === Nothing
              -- the value bytes are still the bare encodeProto output
              Serde.serialize s x === encodeProto x
        , testCase "decodeAsFor KeySchema reads the key header; value-side read misses" $ do
            let x  = OrderPlaced 5 "abc"
                hs = bufSchemaHeadersFor KeySchema
                       (identityOf (Proxy :: Proxy OrderPlaced) "k1" Nothing)
            decodeAsFor KeySchema (Proxy :: Proxy OrderPlaced) hs (encodeProto x) @?= Right x
            decodeAs (Proxy :: Proxy OrderPlaced) hs (encodeProto x)
              @?= Left (HeaderError MissingMessageHeader)
        ]
    , testGroup
        "O(1) prebuilt-map dispatch"
        [ testProperty "dispatchWith matches list dispatch on a mixed stream" $
            property $ do
              c <- forAll genCommit
              p <- forAll genOrderPlaced
              s <- forAll genOrderShipped
              let hm = handlerMap handlers
              dispatchWith hm (placedHeaders c) (encodeProto p) === Right (RPlaced p)
              dispatchWith hm (shippedHeaders c) (encodeProto s) === Right (RShipped s)
        , testCase "unregistered FQN => UnknownType" $ do
            let hm = handlerMap handlers
                hs = H.singleton messageHeaderName (TE.encodeUtf8 "payments.v1.Nope")
            dispatchWith hm hs "x" @?= Left (UnknownType "payments.v1.Nope")
        , testCase "missing header => HeaderError MissingMessageHeader" $
            dispatchWith (handlerMap handlers) H.empty "x"
              @?= Left (HeaderError MissingMessageHeader)
        , testCase "first handler wins on a duplicate FQN" $ do
            let hm =
                  handlerMap
                    [ Handler (Proxy :: Proxy OrderPlaced) (\_ -> RPlaced (OrderPlaced 111 ""))
                    , Handler (Proxy :: Proxy OrderPlaced) (\_ -> RPlaced (OrderPlaced 222 ""))
                    ]
            dispatchWith hm (placedHeaders "c") (encodeProto (OrderPlaced 1 "x"))
              @?= Right (RPlaced (OrderPlaced 111 ""))
        ]
    ]

-- Compile-time splices: the commit and the full serde are populated
-- from the committed fixture @test/Serde/buf.lock@ at build time. The
-- path is relative to the package root (where cabal invokes GHC).
splicedCommit :: Text
splicedCommit = $(bufCommitFromLock "test/Serde/buf.lock" "buf.build/acme/payments")

splicedSerde :: Serde.Serde OrderPlaced
splicedSerde = $(bufProtoSerdeFromLock "test/Serde/buf.lock" "buf.build/acme/payments")

splicedKeySerde :: Serde.Serde OrderPlaced
splicedKeySerde = $(bufProtoKeySerdeFromLock "test/Serde/buf.lock" "buf.build/acme/payments")

-- Commit spliced at compile time from a /real/ buf.lock generated by the
-- buf CLI (v1.70.0) via @buf dep update@ against
-- @buf.build/bufbuild/protovalidate@. Committed verbatim as a fixture.
realSplicedCommit :: Text
realSplicedCommit =
  $(bufCommitFromLock "test/Serde/buf.real.v2.lock" "buf.build/bufbuild/protovalidate")

-- The commit buf pinned for protovalidate when the fixtures were
-- generated. Stable as long as the committed buf.lock files are.
realCommit :: Text
realCommit = "50325440f8f24053b047484a6bf60b76"

-- Inline fixtures for the pure parser (kept separate from the on-disk
-- fixture used by the splice).
v2Lock :: Text
v2Lock =
  T.unlines
    [ "# Generated by buf. DO NOT EDIT."
    , "version: v2"
    , "deps:"
    , "  - name: buf.build/acme/payments"
    , "    commit: 0a1b2c3d"
    , "    digest: shake256:abcdef"
    , "  - name: buf.build/googleapis/googleapis"
    , "    commit: ffffffff"
    , "    digest: shake256:123456"
    ]

v1Lock :: Text
v1Lock =
  T.unlines
    [ "# Generated by buf. DO NOT EDIT."
    , "version: v1"
    , "deps:"
    , "  - remote: buf.build"
    , "    owner: acme"
    , "    repository: payments"
    , "    commit: 11112222"
    , "    digest: shake256:abcdef"
    ]

-- | A byte sequence that is not valid UTF-8 (lone continuation byte).
invalidUtf8 :: ByteString
invalidUtf8 = "\xff\xfe"
