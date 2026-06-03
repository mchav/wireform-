{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.Text.Encoding   as TE
import           Data.Word            (Word64)

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range
import           Test.Tasty           (TestTree, testGroup)
import           Test.Tasty.Hedgehog  (testProperty)
import           Test.Tasty.HUnit     (testCase, (@?=))

import qualified Kafka.Headers        as H
import qualified Kafka.Serde          as Serde
import           Kafka.Serde.Proto    (encodeProto)
import           Kafka.Serde.Proto.Buf

import           Proto.Decode         (MessageDecode (..), getTagOr, getText, getVarint, skipField)
import           Proto.Encode         (MessageEncode (..), encodeFieldString, encodeFieldVarint)
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
    ]

-- | A byte sequence that is not valid UTF-8 (lone continuation byte).
invalidUtf8 :: ByteString
invalidUtf8 = "\xff\xfe"
