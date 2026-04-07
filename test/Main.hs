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
import Test.ASN1 (asn1Tests)
import Test.Parquet (parquetTests)
import Test.Pickle (pickleTests)
import Test.Arrow (arrowTests)
import Test.EDN (ednTests)
import Test.MsgPackRPC (msgPackRPCTests)
import Test.CBORDiagnostic (cborDiagnosticTests)
import Test.Streaming (streamingTests)
import Test.Class (classTests)
import Test.AvroContainer (avroContainerTests)
import Test.ThriftParser (thriftParserTests)
import Test.AvroSchemaParse (avroSchemaParseTests)
import Test.BondParser (bondParserTests)
import Test.AvroCodeGen (avroCodeGenTests)
import Test.ThriftCodeGen (thriftCodeGenTests)
import Test.Resolver (resolverTests)

main :: IO ()
main = defaultMain $ testGroup "wireform"
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
  , asn1Tests
  , parquetTests
  , pickleTests
  , arrowTests
  , ednTests
  , classTests
  , msgPackRPCTests
  , cborDiagnosticTests
  , streamingTests
  , avroContainerTests
  , thriftParserTests
  , avroSchemaParseTests
  , bondParserTests
  , avroCodeGenTests
  , thriftCodeGenTests
  , resolverTests
  ]
