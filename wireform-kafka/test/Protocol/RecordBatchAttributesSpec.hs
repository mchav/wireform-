{-# LANGUAGE OverloadedStrings #-}

-- | Bit-level tests for the @attributes@ field of @RecordBatch@.
--
-- Ported from the JVM client's @org.apache.kafka.common.record.
-- DefaultRecordBatchTest@ + spot checks against the on-wire bit
-- positions documented in the
-- [Kafka protocol guide](https://kafka.apache.org/protocol.html#protocol_messages):
--
-- @
--   bit 0..2 : compression codec (0=none, 1=gzip, 2=snappy, 3=lz4, 4=zstd)
--   bit 3    : timestamp type    (0=CreateTime, 1=LogAppendTime)
--   bit 4    : isTransactional
--   bit 5    : isControl
--   bit 6    : hasDeleteHorizon
--   bit 7..15: unused
-- @
--
-- Any change that flips a bit position WILL silently corrupt every
-- record batch we send and every batch we decode, so locking them
-- down with explicit bit-position assertions is cheap insurance.
module Protocol.RecordBatchAttributesSpec (tests) where

import Data.Bits ((.&.))
import Data.Int (Int16)

import Hedgehog
import qualified Hedgehog.Gen as Gen

import Test.Syd
import Test.Syd.Hedgehog ()

import qualified Kafka.Compression.Types as Compression
import qualified Kafka.Protocol.RecordBatch as RB

tests :: Spec
tests = describe "RecordBatch attributes (bit-level)" $ sequence_
  [ describe "compression codec bits 0-2" $ sequence_
      [ it "NoCompression -> 0x00"
          (encodeOnly noCompression `shouldBe` 0x00)
      , it "Gzip          -> 0x01"
          (encodeOnly (noCompression { RB.attrCompressionType = Compression.Gzip })
             `shouldBe` 0x01)
      , it "Snappy        -> 0x02"
          (encodeOnly (noCompression { RB.attrCompressionType = Compression.Snappy })
             `shouldBe` 0x02)
      , it "Lz4           -> 0x03"
          (encodeOnly (noCompression { RB.attrCompressionType = Compression.Lz4 })
             `shouldBe` 0x03)
      , it "Zstd          -> 0x04"
          (encodeOnly (noCompression { RB.attrCompressionType = Compression.Zstd })
             `shouldBe` 0x04)
      ]

  , it "timestamp type bit 3" $ do
      encodeOnly (noCompression { RB.attrTimestampType = RB.CreateTime })
        `shouldBe` 0x00
      encodeOnly (noCompression { RB.attrTimestampType = RB.LogAppendTime })
        `shouldBe` 0x08

  , it "transactional bit 4" $ do
      encodeOnly (noCompression { RB.attrIsTransactional = True })
        `shouldBe` 0x10

  , it "control bit 5" $ do
      encodeOnly (noCompression { RB.attrIsControl = True })
        `shouldBe` 0x20

  , it "delete-horizon bit 6 (KIP-534)" $ do
      encodeOnly (noCompression { RB.attrHasDeleteHorizon = True })
        `shouldBe` 0x40

  , it "all flags + zstd composes correctly" $ do
      let attrs = RB.Attributes
            { RB.attrCompressionType = Compression.Zstd
            , RB.attrTimestampType   = RB.LogAppendTime
            , RB.attrIsTransactional = True
            , RB.attrIsControl       = True
            , RB.attrHasDeleteHorizon = True
            }
      encodeOnly attrs
        `shouldBe` (0x04 + 0x08 + 0x10 + 0x20 + 0x40)  -- 0x7C

  , describe "round-trip" $ sequence_
      [ it "decode . encode == id (any combination)"
          prop_attrs_round_trip
      , it "encode never sets bits 7..15"
          prop_attrs_no_high_bits
      ]
  ]

------------------------------------------------------------------
-- helpers
------------------------------------------------------------------

noCompression :: RB.Attributes
noCompression = RB.defaultAttributes

encodeOnly :: RB.Attributes -> Int16
encodeOnly = RB.encodeAttributes

------------------------------------------------------------------
-- properties
------------------------------------------------------------------

genCodec :: Gen Compression.CompressionCodec
genCodec = Gen.element
  [ Compression.NoCompression
  , Compression.Gzip
  , Compression.Snappy
  , Compression.Lz4
  , Compression.Zstd
  ]

genAttrs :: Gen RB.Attributes
genAttrs = do
  c   <- genCodec
  tt  <- Gen.element [RB.CreateTime, RB.LogAppendTime]
  txn <- Gen.bool
  ctl <- Gen.bool
  dh  <- Gen.bool
  pure $ RB.Attributes c tt txn ctl dh

prop_attrs_round_trip :: Property
prop_attrs_round_trip = property $ do
  attrs <- forAll genAttrs
  let !w = RB.encodeAttributes attrs
  case RB.decodeAttributes w of
    Left err -> annotate err >> failure
    Right roundTripped -> roundTripped === attrs

prop_attrs_no_high_bits :: Property
prop_attrs_no_high_bits = property $ do
  attrs <- forAll genAttrs
  let !w = RB.encodeAttributes attrs
  -- Bits 7..15 must be zero. The on-wire format reserves them and
  -- the broker rejects batches that set any of them.
  (w .&. 0x7F) === w
