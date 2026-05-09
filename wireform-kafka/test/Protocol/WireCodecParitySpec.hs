{-# LANGUAGE OverloadedStrings #-}

-- | Byte-level parity between the native 'Wire' codec
-- ('runEncodeVer' dispatching through 'WireCodec') and the legacy
-- 'Serial'-shape baseline ('runEncodeVerSerial').
--
-- The native codec is supplied by hand-edited Wire blocks in the
-- @Generated/RequestHeader@ / @Generated/ResponseHeader@ /
-- @Generated/ApiVersionsRequest@ modules. Every encode/decode the
-- runtime issues for those messages now goes through the native
-- pokes; the legacy 'Serial' encoders / decoders are still around as
-- a baseline. This test asserts that the two paths produce exactly
-- the same bytes for any value, in either direction.
module Protocol.WireCodecParitySpec (tests) where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32)
import qualified Data.Vector as V
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.HUnit (testCase, (@?=))

import qualified Kafka.Protocol.Generated.ApiVersionsRequest as AVR
import qualified Kafka.Protocol.Generated.MetadataRequest as MR
import qualified Kafka.Protocol.Generated.RequestHeader as RH
import qualified Kafka.Protocol.Generated.ResponseHeader as RsH
import qualified Kafka.Protocol.Primitives as P
import qualified Kafka.Protocol.Wire.Codec as WC

tests :: TestTree
tests = testGroup "Wire vs Serial codec parity (native WireCodec dispatch)"
  [ testGroup "RequestHeader"
      [ testProperty "encode == encodeSerial (any version, any msg)"
          prop_requestHeader_encodeEq
      , testProperty "decode . encode == id (native path)"
          prop_requestHeader_roundTripNative
      , testProperty "decodeNative bytes ≡ decodeSerial bytes"
          prop_requestHeader_decodeEq
      , testCase "v2 sample byte exact (api=18, ver=3, corr=42, cid=\"abc\")"
          unit_requestHeader_v2Sample
      ]
  , testGroup "ResponseHeader"
      [ testProperty "encode == encodeSerial (any version, any corrId)"
          prop_responseHeader_encodeEq
      , testProperty "decode . encode == id (native path)"
          prop_responseHeader_roundTripNative
      , testCase "v0 / v1 sample bytes exact"
          unit_responseHeader_samples
      ]
  , testGroup "ApiVersionsRequest"
      [ testProperty "encode == encodeSerial (v3+ shape)"
          prop_apiVersionsRequest_encodeEq
      , testProperty "decode . encode == id (native path)"
          prop_apiVersionsRequest_roundTripNative
      ]
  , testGroup "MetadataRequest (Serial-shim sanity check)"
      -- 'MetadataRequest' carries a nested-struct array, so the
      -- WireGenerator can't (yet) emit a native codec for it. The
      -- generated module wires it up via 'WC.serialShimCodec' —
      -- this test checks that a shim-routed message round-trips
      -- byte-identically with the legacy Serial path. If anything
      -- about 'serialShimCodec' drifts (buffer sizing, slice
      -- arithmetic, error wrapping), this will catch it without
      -- needing to touch every shim-using module.
      [ testCase "v9 round-trips through the shim"
          unit_metadataRequest_shimRoundTrip
      ]
  ]

------------------------------------------------------------------
-- RequestHeader
------------------------------------------------------------------

genRequestHeaderVersion :: Gen Int16
genRequestHeaderVersion = Gen.element [1, 2]

genRequestHeader :: Gen RH.RequestHeader
genRequestHeader = do
  ak  <- Gen.int16 (Range.linear 0 64)
  av  <- Gen.int16 (Range.linear 0 16)
  cid <- Gen.int32 (Range.linear 0 1000)
  client <- Gen.text (Range.linear 0 32) Gen.alphaNum
  pure RH.RequestHeader
    { RH.requestHeaderRequestApiKey     = ak
    , RH.requestHeaderRequestApiVersion = av
    , RH.requestHeaderCorrelationId     = cid
    , RH.requestHeaderClientId          = P.mkKafkaString client
    }

prop_requestHeader_encodeEq :: Property
prop_requestHeader_encodeEq = property $ do
  v   <- forAll genRequestHeaderVersion
  msg <- forAll genRequestHeader
  let !native = WC.runEncodeVer       RH.encodeRequestHeader v msg
      !serial = WC.runEncodeVerSerial RH.encodeRequestHeader v msg
  native === serial

prop_requestHeader_roundTripNative :: Property
prop_requestHeader_roundTripNative = property $ do
  v   <- forAll genRequestHeaderVersion
  msg <- forAll genRequestHeader
  let !bs = WC.runEncodeVer RH.encodeRequestHeader v msg
  case WC.runDecodeVer RH.decodeRequestHeader v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg

prop_requestHeader_decodeEq :: Property
prop_requestHeader_decodeEq = property $ do
  v   <- forAll genRequestHeaderVersion
  msg <- forAll genRequestHeader
  -- Encode via the legacy 'Serial' path so the bytes are guaranteed
  -- broker-shaped, then decode through both codecs and assert agreement.
  let !bs       = WC.runEncodeVerSerial RH.encodeRequestHeader v msg
      !native   = WC.runDecodeVer       RH.decodeRequestHeader v bs
      !serial   = WC.runDecodeVerSerial RH.decodeRequestHeader v bs
  native === serial

unit_requestHeader_v2Sample :: IO ()
unit_requestHeader_v2Sample = do
  let msg = RH.RequestHeader
        { RH.requestHeaderRequestApiKey     = 18
        , RH.requestHeaderRequestApiVersion = 3
        , RH.requestHeaderCorrelationId     = 42
        , RH.requestHeaderClientId          = P.mkKafkaString "abc"
        }
      v       = 2
      !native = WC.runEncodeVer       RH.encodeRequestHeader v msg
      !serial = WC.runEncodeVerSerial RH.encodeRequestHeader v msg
  native @?= serial
  -- Sanity: the byte layout for a v2 RequestHeader is
  -- 2 (apiKey) + 2 (apiVersion) + 4 (correlation) + 2+len (clientId
  -- as INT16-prefixed string, since 'flexibleVersions: none' applies)
  -- + 1 (empty tagged-fields trailer).
  BS.length native @?= 2 + 2 + 4 + 2 + 3 + 1

------------------------------------------------------------------
-- ResponseHeader
------------------------------------------------------------------

genResponseHeaderVersion :: Gen Int16
genResponseHeaderVersion = Gen.element [0, 1]

genResponseHeader :: Gen RsH.ResponseHeader
genResponseHeader = do
  cid <- Gen.int32 (Range.linear 0 (1024 * 1024))
  pure RsH.ResponseHeader { RsH.responseHeaderCorrelationId = cid }

prop_responseHeader_encodeEq :: Property
prop_responseHeader_encodeEq = property $ do
  v   <- forAll genResponseHeaderVersion
  msg <- forAll genResponseHeader
  let !native = WC.runEncodeVer       RsH.encodeResponseHeader v msg
      !serial = WC.runEncodeVerSerial RsH.encodeResponseHeader v msg
  native === serial

prop_responseHeader_roundTripNative :: Property
prop_responseHeader_roundTripNative = property $ do
  v   <- forAll genResponseHeaderVersion
  msg <- forAll genResponseHeader
  let !bs = WC.runEncodeVer RsH.encodeResponseHeader v msg
  case WC.runDecodeVer RsH.decodeResponseHeader v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg

unit_responseHeader_samples :: IO ()
unit_responseHeader_samples = do
  let msg = RsH.ResponseHeader { RsH.responseHeaderCorrelationId = 0xDEADBEEF }
  let v0_native = WC.runEncodeVer       RsH.encodeResponseHeader 0 msg
      v0_serial = WC.runEncodeVerSerial RsH.encodeResponseHeader 0 msg
  v0_native @?= v0_serial
  BS.length v0_native @?= 4
  let v1_native = WC.runEncodeVer       RsH.encodeResponseHeader 1 msg
      v1_serial = WC.runEncodeVerSerial RsH.encodeResponseHeader 1 msg
  v1_native @?= v1_serial
  BS.length v1_native @?= 4 + 1  -- + empty tagged-fields trailer

------------------------------------------------------------------
-- ApiVersionsRequest
------------------------------------------------------------------

genApiVersionsRequestVersion :: Gen Int16
genApiVersionsRequestVersion = Gen.element [3, 4]

genApiVersionsRequest :: Gen AVR.ApiVersionsRequest
genApiVersionsRequest = do
  name <- Gen.text (Range.linear 0 24) Gen.alphaNum
  ver  <- Gen.text (Range.linear 0 24) Gen.alphaNum
  pure AVR.ApiVersionsRequest
    { AVR.apiVersionsRequestClientSoftwareName    = P.mkKafkaString name
    , AVR.apiVersionsRequestClientSoftwareVersion = P.mkKafkaString ver
    -- KIP-1242 tagged fields (v5+ in the wire spec). At v3-4 (the
    -- versions exercised below) these aren't on the wire, so the
    -- decoder fills them with the field's Haskell default (Int32 0,
    -- nullable string Null). Match those here so the round-trip
    -- @rt === msg@ assertion holds.
    , AVR.apiVersionsRequestClusterId             = P.KafkaString P.Null
    , AVR.apiVersionsRequestNodeId                = 0
    }

prop_apiVersionsRequest_encodeEq :: Property
prop_apiVersionsRequest_encodeEq = property $ do
  v   <- forAll genApiVersionsRequestVersion
  msg <- forAll genApiVersionsRequest
  let !native = WC.runEncodeVer       AVR.encodeApiVersionsRequest v msg
      !serial = WC.runEncodeVerSerial AVR.encodeApiVersionsRequest v msg
  native === serial

prop_apiVersionsRequest_roundTripNative :: Property
prop_apiVersionsRequest_roundTripNative = property $ do
  v   <- forAll genApiVersionsRequestVersion
  msg <- forAll genApiVersionsRequest
  let !bs = WC.runEncodeVer AVR.encodeApiVersionsRequest v msg
  case WC.runDecodeVer AVR.decodeApiVersionsRequest v bs of
    Left err -> annotate err >> failure
    Right rt -> rt === msg

-- 'Int32' kept imported so the type annotations above stay tidy
-- without dragging in the full @Data.Int@ when cabal's import-pruner
-- gets aggressive about unused imports.
_keepInt32 :: Int32
_keepInt32 = 0

------------------------------------------------------------------
-- Serial-shim sanity check
------------------------------------------------------------------

unit_metadataRequest_shimRoundTrip :: IO ()
unit_metadataRequest_shimRoundTrip = do
  -- Empty topic list — decodes the same shape via either path,
  -- exercises the array length-prefix + the four trailing booleans.
  let msg = MR.MetadataRequest
        { MR.metadataRequestTopics =
            P.mkKafkaArray (mempty :: V.Vector MR.MetadataRequestTopic)
        , MR.metadataRequestAllowAutoTopicCreation             = True
        , MR.metadataRequestIncludeClusterAuthorizedOperations = False
        , MR.metadataRequestIncludeTopicAuthorizedOperations   = True
        }
      v = 9
      !shim   = WC.runEncodeVer       MR.encodeMetadataRequest v msg
      !serial = WC.runEncodeVerSerial MR.encodeMetadataRequest v msg
  shim @?= serial
  case WC.runDecodeVer MR.decodeMetadataRequest v shim of
    Left err -> error ("decodeMetadataRequest failed: " <> err)
    Right rt -> rt @?= msg
