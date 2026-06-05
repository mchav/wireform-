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
import qualified Data.ByteString      as BS
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
import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Kafka.Headers        as H
import qualified Kafka.Serde          as Serde
import           Kafka.Serde.Proto    (encodeProto)
import           Kafka.Serde.Proto.Buf
import           Kafka.Serde.Proto.Buf.TH (bufCommitFromLock, bufProtoKeySerdeFromLock, bufProtoSerdeFromLock, lookupBufLockCommit)

import           Proto.Decode         (MessageDecode (..), getTagOr, getText, getVarint, skipField)
import           Proto.Encode         (MessageEncode (..), encodeFieldString, encodeFieldVarint)
import           Proto.Google.Protobuf.Timestamp (Timestamp (..))
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

tests :: Spec
tests =
  describe
    "Kafka.Serde.Proto.Buf" $ sequence_
    [ describe
        "header names are byte-identical to the Buf convention" $ sequence_
        [ it "message" $ messageHeaderName `shouldBe` "buf.registry.value.schema.message"
        , it "commit" $ commitHeaderName `shouldBe` "buf.registry.value.schema.commit"
        , it "module" $ moduleHeaderName `shouldBe` "buf.registry.value.schema.module"
        ]
    , describe
        "fully-qualified name" $ sequence_
        [ it "packaged message keeps its FQN" $
            fqn (Proxy :: Proxy OrderPlaced) `shouldBe` "payments.v1.OrderPlaced"
        , it "empty package => no leading dot" $
            fqn (Proxy :: Proxy BareEvent) `shouldBe` "BareEvent"
        , -- Guard against a regression of the package-doubling bug on a
          -- real codegen-emitted instance (protoMessageName is already
          -- fully qualified, so fqn must not prepend the package again).
          it "real generated instance (google.protobuf.Timestamp) is not doubled" $ do
            fqn (Proxy :: Proxy Timestamp) `shouldBe` "google.protobuf.Timestamp"
            protoMessageName (Proxy :: Proxy Timestamp) `shouldBe` "google.protobuf.Timestamp"
            protoPackageName (Proxy :: Proxy Timestamp) `shouldBe` "google.protobuf"
        ]
    , describe
        "bufSchemaHeaders" $ sequence_
        [ it "Nothing module => exactly message + commit" $ do
            let hs = placedHeaders "abc123"
            H.lookup messageHeaderName hs `shouldBe` Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
            H.lookup commitHeaderName hs `shouldBe` Just (TE.encodeUtf8 "abc123")
            H.lookup moduleHeaderName hs `shouldBe` Nothing
            H.length hs `shouldBe` 2
        , it "Just module => message + commit + module" $ do
            let hs =
                  bufSchemaHeaders
                    (identityOf (Proxy :: Proxy OrderPlaced) "abc123" (Just "buf.build/acme/payments"))
            H.lookup moduleHeaderName hs `shouldBe` Just (TE.encodeUtf8 "buf.build/acme/payments")
            H.length hs `shouldBe` 3
        ]
    , describe
        "bufProtoSerde carries the identity in the serde" $ sequence_
        [ it "value bytes are the bare encodeProto output (no prefix)" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              Serde.serialize (bufProtoSerde c Nothing :: Serde.Serde OrderPlaced) x === encodeProto x
        , it "serde headers equal the hand-built identity headers" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              Serde.serializeHeaders (bufProtoSerde c Nothing :: Serde.Serde OrderPlaced) x
                === placedHeaders c
        , it "deserialize round-trips through the serde" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              let s = bufProtoSerde c Nothing :: Serde.Serde OrderPlaced
              Serde.deserialize s (Serde.serialize s x) === Right x
        ]
    , describe
        "decodeAs" $ sequence_
        [ it "round-trips a matching type" $
            property $ do
              x <- forAll genOrderPlaced
              c <- forAll genCommit
              decodeAs (Proxy :: Proxy OrderPlaced) (placedHeaders c) (encodeProto x) === Right x
        , it "the commit header round-trips unmodified" $
            property $ do
              c <- forAll genCommit
              H.lookup commitHeaderName (placedHeaders c) === Just (TE.encodeUtf8 c)
        , it "wrong header type => TypeMismatch, no decode attempted" $ do
            let bytes = encodeProto (OrderShipped 7 "u:ups")
            decodeAs (Proxy :: Proxy OrderPlaced) (shippedHeaders "c1") bytes
              `shouldBe` Left (TypeMismatch "payments.v1.OrderPlaced" "payments.v1.OrderShipped")
        , it "missing header => HeaderError MissingMessageHeader" $
            decodeAs (Proxy :: Proxy OrderPlaced) H.empty (encodeProto (OrderPlaced 1 "x"))
              `shouldBe` Left (HeaderError MissingMessageHeader)
        ]
    , describe
        "dispatch (header-discriminated, total)" $ sequence_
        [ it "routes a mixed stream to the right handler" $
            property $ do
              c <- forAll genCommit
              p <- forAll genOrderPlaced
              s <- forAll genOrderShipped
              dispatch handlers (placedHeaders c) (encodeProto p) === Right (RPlaced p)
              dispatch handlers (shippedHeaders c) (encodeProto s) === Right (RShipped s)
        , it "unregistered FQN => UnknownType (no crash, no drop)" $ do
            let hs = H.singleton messageHeaderName (TE.encodeUtf8 "payments.v1.Unknown")
            dispatch handlers hs "anything"
              `shouldBe` Left (UnknownType "payments.v1.Unknown")
        , it "missing header => HeaderError MissingMessageHeader" $
            dispatch handlers H.empty "anything"
              `shouldBe` Left (HeaderError MissingMessageHeader)
        , it "non-UTF-8 header => HeaderError MessageHeaderNotUtf8" $ do
            let hs = H.singleton messageHeaderName invalidUtf8
            first (const ()) (readMessageHeader hs) `shouldBe` Left ()
            dispatch handlers hs "anything"
              `shouldBe` Left (HeaderError MessageHeaderNotUtf8)
        ]
    , describe
        "Template Haskell auto-population (buf.lock)" $ sequence_
        [ describe
            "lookupBufLockCommit (pure)" $ sequence_
            [ it "v2 name: entry" $
                lookupBufLockCommit "buf.build/acme/payments" v2Lock
                  `shouldBe` Right "0a1b2c3d"
            , it "second dep in the file" $
                lookupBufLockCommit "buf.build/googleapis/googleapis" v2Lock
                  `shouldBe` Right "ffffffff"
            , it "v1 remote/owner/repository entry" $
                lookupBufLockCommit "buf.build/acme/payments" v1Lock
                  `shouldBe` Right "11112222"
            , it "unknown module => Left" $
                (case lookupBufLockCommit "buf.build/nope/nope" v2Lock of
                    Left _  -> True
                    Right _ -> False) `shouldBe` True
            ]
        , describe
            "compile-time splice (committed fixture buf.lock)" $ sequence_
            [ it "bufCommitFromLock embeds the pinned commit" $
                splicedCommit `shouldBe` "0a1b2c3d4e5f60718293a4b5c6d7e8f9"
            , it "bufProtoSerdeFromLock stamps commit + message headers" $ do
                let hs = Serde.serializeHeaders splicedSerde (OrderPlaced 1 "sku")
                H.lookup commitHeaderName hs
                  `shouldBe` Just (TE.encodeUtf8 "0a1b2c3d4e5f60718293a4b5c6d7e8f9")
                H.lookup messageHeaderName hs
                  `shouldBe` Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
                H.lookup moduleHeaderName hs `shouldBe` Nothing
            , it "spliced serde still emits the bare encodeProto value" $ do
                let x = OrderPlaced 42 "widget"
                Serde.serialize splicedSerde x `shouldBe` encodeProto x
            , it "bufProtoKeySerdeFromLock stamps key-side commit + message" $ do
                let hs = Serde.serializeHeaders splicedKeySerde (OrderPlaced 1 "sku")
                H.lookup keyCommitHeaderName hs
                  `shouldBe` Just (TE.encodeUtf8 "0a1b2c3d4e5f60718293a4b5c6d7e8f9")
                H.lookup keyMessageHeaderName hs
                  `shouldBe` Just (TE.encodeUtf8 "payments.v1.OrderPlaced")
            ]
        , describe
            "real buf-generated buf.lock (buf CLI 1.70.0)" $ sequence_
            [ it "parses a real v2 buf.lock (buf dep update)" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v2.lock"
                lookupBufLockCommit "buf.build/bufbuild/protovalidate" contents
                  `shouldBe` Right realCommit
            , it "parses a real v1 buf.lock (remote/owner/repository)" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v1.lock"
                lookupBufLockCommit "buf.build/bufbuild/protovalidate" contents
                  `shouldBe` Right realCommit
            , it "unknown module in a real buf.lock => Left" $ do
                contents <- TIO.readFile "test/Serde/buf.real.v2.lock"
                (case lookupBufLockCommit "buf.build/acme/nope" contents of
                    Left _  -> True
                    Right _ -> False) `shouldBe` True
            , it "bufCommitFromLock splices the real commit at compile time" $
                realSplicedCommit `shouldBe` realCommit
            ]
        ]
    , describe
        "cross-language interop (bufbuild/bsr-kafka-serde-go v0.3.0)" $ sequence_
        -- Golden record under test/Serde/interop/ was produced by the
        -- real Go bsr-kafka-serde-go Serde.Serialize of a
        -- google.protobuf.Timestamp{1700000000,123} (static commit
        -- resolver). These assertions prove the Haskell and Go layers
        -- agree byte-for-byte on the wire.
        [ it "Go-produced headers match the Buf convention" $ do
            (hdrs, _) <- loadGoRecord
            H.lookup messageHeaderName hdrs
              `shouldBe` Just (TE.encodeUtf8 "google.protobuf.Timestamp")
            H.lookup commitHeaderName hdrs
              `shouldBe` Just (TE.encodeUtf8 goInteropCommit)
        , it "Haskell decodeAs consumes the Go-produced record" $ do
            (hdrs, val) <- loadGoRecord
            decodeAs (Proxy :: Proxy Timestamp) hdrs val `shouldBe` Right goTimestamp
        , it "Haskell dispatch routes the Go-produced record" $ do
            (hdrs, val) <- loadGoRecord
            dispatch [Handler (Proxy :: Proxy Timestamp) id] hdrs val
              `shouldBe` Right goTimestamp
        , it "Haskell bufProtoSerde produces byte-identical value + headers" $ do
            (hdrs, val) <- loadGoRecord
            let s = bufProtoSerde goInteropCommit Nothing :: Serde.Serde Timestamp
                hsk = Serde.serializeHeaders s goTimestamp
            -- value bytes identical to Go's proto.Marshal output
            Serde.serialize s goTimestamp `shouldBe` val
            -- the two identity headers match Go's, by name + value
            H.lookup messageHeaderName hsk `shouldBe` H.lookup messageHeaderName hdrs
            H.lookup commitHeaderName hsk `shouldBe` H.lookup commitHeaderName hdrs
        ]
    , describe
        "key-side schema identity" $ sequence_
        [ it "key header names are byte-identical to the convention" $ do
            keyMessageHeaderName `shouldBe` "buf.registry.key.schema.message"
            keyCommitHeaderName `shouldBe` "buf.registry.key.schema.commit"
            keyModuleHeaderName `shouldBe` "buf.registry.key.schema.module"
        , it "messageHeaderNameFor agrees with the side constants" $ do
            messageHeaderNameFor ValueSchema `shouldBe` messageHeaderName
            messageHeaderNameFor KeySchema `shouldBe` keyMessageHeaderName
        , it "bufProtoKeySerde stamps key-side headers (not value-side)" $
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
        , it "decodeAsFor KeySchema reads the key header; value-side read misses" $ do
            let x  = OrderPlaced 5 "abc"
                hs = bufSchemaHeadersFor KeySchema
                       (identityOf (Proxy :: Proxy OrderPlaced) "k1" Nothing)
            decodeAsFor KeySchema (Proxy :: Proxy OrderPlaced) hs (encodeProto x) `shouldBe` Right x
            decodeAs (Proxy :: Proxy OrderPlaced) hs (encodeProto x)
              `shouldBe` Left (HeaderError MissingMessageHeader)
        ]
    , describe
        "O(1) prebuilt-map dispatch" $ sequence_
        [ it "dispatchWith matches list dispatch on a mixed stream" $
            property $ do
              c <- forAll genCommit
              p <- forAll genOrderPlaced
              s <- forAll genOrderShipped
              let hm = handlerMap handlers
              dispatchWith hm (placedHeaders c) (encodeProto p) === Right (RPlaced p)
              dispatchWith hm (shippedHeaders c) (encodeProto s) === Right (RShipped s)
        , it "unregistered FQN => UnknownType" $ do
            let hm = handlerMap handlers
                hs = H.singleton messageHeaderName (TE.encodeUtf8 "payments.v1.Nope")
            dispatchWith hm hs "x" `shouldBe` Left (UnknownType "payments.v1.Nope")
        , it "missing header => HeaderError MissingMessageHeader" $
            dispatchWith (handlerMap handlers) H.empty "x"
              `shouldBe` Left (HeaderError MissingMessageHeader)
        , it "first handler wins on a duplicate FQN" $ do
            let hm =
                  handlerMap
                    [ Handler (Proxy :: Proxy OrderPlaced) (\_ -> RPlaced (OrderPlaced 111 ""))
                    , Handler (Proxy :: Proxy OrderPlaced) (\_ -> RPlaced (OrderPlaced 222 ""))
                    ]
            dispatchWith hm (placedHeaders "c") (encodeProto (OrderPlaced 1 "x"))
              `shouldBe` Right (RPlaced (OrderPlaced 111 ""))
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

-- | The Timestamp the Go serde was given (and that its golden record
-- decodes back to).
goTimestamp :: Timestamp
goTimestamp = Timestamp 1700000000 123 []

-- | The static commit the Go serde's resolver returned.
goInteropCommit :: Text
goInteropCommit = "0a1b2c3d4e5f60718293a4b5c6d7e8f9"

-- | Load the golden record produced by the real Go @bsr-kafka-serde-go@
-- serializer: the bare value bytes and the @(name, value)@ headers it
-- attached (one @name\\tvalue@ line each).
loadGoRecord :: IO (H.Headers, ByteString)
loadGoRecord = do
  val <- BS.readFile "test/Serde/interop/go-timestamp.bin"
  raw <- TIO.readFile "test/Serde/interop/go-timestamp.headers"
  let parseLine l =
        let (k, v) = T.breakOn "\t" l
         in (T.strip k, TE.encodeUtf8 (T.strip (T.drop 1 v)))
      hdrs =
        H.fromList
          (fmap parseLine (filter (not . T.null) (T.lines raw)))
  pure (hdrs, val)

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
