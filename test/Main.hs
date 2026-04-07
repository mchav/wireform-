module Main (main) where

import Test.Tasty

import Test.Parser (parserTests)
import Test.Wire (wireTests)
import Test.Roundtrip (roundtripTests)
import Test.CodeGen (codeGenTests)
import Test.WellKnown (wellKnownTests)
import Test.WellKnownUtil (wellKnownUtilTests)
import Test.PrintInspect (printInspectTests)
import Test.Compat (compatTests)
import Test.Schema (schemaTests)
import Test.Options (optionsTests)
import Test.Lens (lensTests)
import Test.StreamCodec (streamCodecTests)
import Test.JSON (jsonTests)
import Test.Hooks (hooksTests)
import Test.TDP (tdpTests)
import Test.Thrift (thriftTests)
import Test.Avro (avroTests)
import Test.GRPC (grpcTests)
import Test.MsgPack (msgPackTests)
import Test.CBOR (cborTests)
import Test.BSON (bsonTests)
import Test.Ion (ionTests)
import Test.CapnProto (capnProtoTests)
import Test.FlatBuffers (flatBuffersTests)
import Test.Iceberg (icebergTests)
import Test.Bond (bondTests)

main :: IO ()
main = defaultMain $ testGroup "hs-proto"
  [ parserTests
  , wireTests
  , roundtripTests
  , codeGenTests
  , wellKnownTests
  , wellKnownUtilTests
  , printInspectTests
  , compatTests
  , schemaTests
  , optionsTests
  , lensTests
  , streamCodecTests
  , jsonTests
  , hooksTests
  , tdpTests
  , avroTests
  , thriftTests
  , grpcTests
  , msgPackTests
  , cborTests
  , bsonTests
  , ionTests
  , capnProtoTests
  , flatBuffersTests
  , icebergTests
  , bondTests
  ]
