{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip tests for the new ORC date / timestamp / decimal writers
against the existing column readers.
-}
module Main (main) where

import Columnar.Predicate qualified as Pred
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Vector.Primitive qualified as VP
import Data.Vector.Unboxed qualified as VU
import Data.Word (Word64)
import ORC.Aggregate qualified as Agg
import ORC.BloomFilter qualified as BF
import ORC.Encryption qualified as Enc
import ORC.Footer (decodeColStats, encodeColStats)
import ORC.Read (
  ORCTimestamp (..),
  decodeDateColumn,
  decodeDecimalColumn,
  decodeStringColumn,
  decodeTimestampColumn,
 )
import ORC.Read qualified
import ORC.RowIndex qualified as RI
import ORC.Statistics qualified as Stats
import ORC.Types
import ORC.Types (
  BinaryStatistics (..),
  BucketStatistics (..),
  ColumnStatistics (..),
  DateStatistics (..),
  DecimalStatistics (..),
  DoubleStatistics (..),
  FooterEncryption (..),
  IntegerStatistics (..),
  ORCType (..),
  StatsKind (..),
  StringStatistics (..),
  TimestampStatistics (..),
  TypeKind (..),
  orcEncryption,
 )
import ORC.Write (
  StripeEncryption (..),
  decryptStripeStream,
  encodeDateColumn,
  encodeDecimalColumn,
  encodeORCNano,
  encodeStringDictColumn,
  encodeTimestampColumn,
  encryptStripeStreams,
 )
import ORC.Write qualified
import System.Exit (exitFailure)
import Wireform.Builder qualified as B


main :: IO ()
main = do
  let !dateVals = VP.fromList [0 :: Int32, 1, -1, 365, 18000, -100]
      !dateBs = encodeDateColumn dateVals
  case decodeDateColumn (VP.length dateVals) dateBs Nothing of
    Right vs -> do
      let actual = map (\x -> case x of Just v -> v; Nothing -> error "null") (V.toList vs)
      expect "date round-trip" (actual == VP.toList dateVals)
    Left e -> failTest ("decodeDateColumn: " ++ e)

  -- Timestamp: a few values with different trailing-zero shapes.
  let !secs = VP.fromList [0 :: Int64, 1700_000_000, -1, 12345]
      !nanos = VP.fromList [0 :: Int64, 500_000_000, 123, 999_999_999]
      (!secBs, !nanoBs) = encodeTimestampColumn secs nanos
  case decodeTimestampColumn (VP.length secs) secBs nanoBs Nothing of
    Right vs ->
      let pairs = zip (VP.toList secs) (VP.toList nanos)
          actual = [(s, n) | Just (ORCTimestamp s n) <- V.toList vs]
      in expect "timestamp round-trip" (actual == pairs)
    Left e -> failTest ("decodeTimestampColumn: " ++ e)

  -- Whole-file column encryption: build a file with two plaintext
  -- stripes, one encrypted under a 16-byte local key. The footer
  -- should round-trip the raw Encryption bytes verbatim, and the
  -- per-stripe streams should decrypt back to the originals.
  do
    let localKey = BS.replicate 16 0x5A
        !stripeKey = StripeEncryption {seLocalKey = localKey, seStripeId = 0}
        plain0 = VP.fromList ([1, 2, 3, 4, 5] :: [Int64])
        plain1 = VP.fromList ([10, 20, 30] :: [Int64])
        stream0 = ORC.Write.encodeIntColumn plain0 True
        stream1 = ORC.Write.encodeIntColumn plain1 True
        stripeData =
          V.fromList
            [ V.singleton (1 :: Word64, 0 :: Word64, stream0)
            , V.singleton (1 :: Word64, 0 :: Word64, stream1)
            ]
        encMeta =
          Enc.Encryption
            { Enc.encMasks = []
            , Enc.encKeys = []
            , Enc.encVariants = []
            , Enc.encKeyProvider = Enc.ProviderUnknown
            }
        types = V.singleton (ORCType TKLong V.empty V.empty)
    case ORC.Write.buildEncryptedORCFile
      types
      stripeData
      (V.fromList [Just stripeKey, Nothing])
      encMeta of
      Left e -> failTest ("buildEncryptedORCFile: " ++ e)
      Right file -> case ORC.Read.loadORCFile file of
        Left e -> failTest ("loadORCFile (encrypted): " ++ e)
        Right of_ -> do
          let footer = ORC.Read.ofFooter of_
          expect
            "encrypted ORC: footer round-trips Encryption field"
            ( case orcEncryption footer of
                Just (FooterEncryption bs) -> bs == Enc.encodeEncryption encMeta
                Nothing -> False
            )
          -- And the encrypted stripe's stream bytes differ from the
          -- plaintext original; decrypting them recovers it.
          let encryptedStream0 = case encryptStripeStreams
                stripeKey
                (V.unsafeIndex stripeData 0) of
                Left _ -> BS.empty
                Right enc -> let (_, _, bs) = V.unsafeIndex enc 0 in bs
          expect
            "encrypted ORC: encrypted stream differs from plaintext"
            (encryptedStream0 /= stream0)
          case decryptStripeStream stripeKey 0 encryptedStream0 of
            Left e -> failTest ("decryptStripeStream: " ++ e)
            Right p ->
              expect
                "encrypted ORC: stream decrypts to the original"
                (p == stream0)

  -- Reader-side Encryption parser: build a record with all four
  -- field kinds populated (masks + keys + variants + keyProvider),
  -- encode + decode round-trip, assert structural equality.
  do
    let encRich =
          Enc.Encryption
            { Enc.encMasks =
                [ Enc.DataMask
                    { Enc.dmName = BSC.pack "redact-email"
                    , Enc.dmParameters = [BSC.pack "keep=3"]
                    , Enc.dmColumns = [4, 7]
                    }
                ]
            , Enc.encKeys =
                [ Enc.EncryptionKey
                    { Enc.ekName = BSC.pack "kek-prod-1"
                    , Enc.ekVersion = 17
                    , Enc.ekAlgorithm = Enc.AES_CTR_256
                    }
                ]
            , Enc.encVariants =
                [ Enc.EncryptionVariant
                    { Enc.evRoot = 3
                    , Enc.evKey = 0
                    , Enc.evEncryptedKey = BS.pack [0x01, 0x02, 0x03, 0x04]
                    }
                ]
            , Enc.encKeyProvider = Enc.ProviderAwsKms
            }
        encBytes = Enc.encodeEncryption encRich
    case Enc.decodeEncryption encBytes of
      Left e -> failTest ("decodeEncryption: " ++ e)
      Right parsed ->
        expect
          "Encryption round-trip: encodeEncryption -> decodeEncryption"
          (parsed == encRich)

  -- DICTIONARY_V2 string column round-trip. The writer emits three
  -- streams (DATA = per-row indices, LENGTH = dict entry lengths,
  -- DICTIONARY_DATA = raw UTF-8 bytes); decodeStringColumn auto-
  -- dispatches to the dictionary decoder when the dictionary stream
  -- is non-empty.
  let !dictInput =
        V.fromList
          (map T.pack ["alpha", "beta", "alpha", "gamma", "beta", "alpha"])
      (!dictData, !dictLen, !dictDictBytes) = encodeStringDictColumn dictInput
  case decodeStringColumn (V.length dictInput) dictData dictLen dictDictBytes Nothing of
    Right vs ->
      let actual = [t | Just t <- V.toList vs]
      in expect "DICTIONARY_V2 string round-trip" (actual == V.toList dictInput)
    Left e -> failTest ("decodeStringColumn DICTIONARY_V2: " ++ e)

  -- Decimal64: just the unscaled integers.
  let !dec = VP.fromList [123 :: Int64, -456, 0, 999_999_999_999_999]
      !decBs = encodeDecimalColumn dec
  case decodeDecimalColumn (VP.length dec) 4 decBs Nothing of
    Right vs ->
      let actual = [v | Just v <- V.toList vs]
      in expect "decimal64 round-trip" (actual == VP.toList dec)
    Left e -> failTest ("decodeDecimalColumn: " ++ e)

  -- encodeORCNano: 500_000_000 has 8 trailing zeros; clamped to 7 by spec.
  expect "encodeORCNano(0) == 0" (encodeORCNano 0 == 0)
  expect
    "encodeORCNano(123) == 123 << 3"
    (encodeORCNano 123 == (123 * 8))
  expect
    "encodeORCNano clamps trailing-zero scale at 7"
    ( let v = encodeORCNano 500_000_000
          scale = v `mod` 8
      in scale == 7
    )

  -- BLOOM_FILTER_UTF8 round-trip: insert -> contains for present
  -- and absent strings, plus integer membership.
  let bf0 = BF.emptyBloom 1000 0.01
      bf = foldl (\acc s -> BF.insertString s acc) bf0 ["alpha", "beta", "gamma"]
  expect "bloom contains 'alpha'" (BF.containsString "alpha" bf)
  expect "bloom contains 'beta'" (BF.containsString "beta" bf)
  expect "bloom contains 'gamma'" (BF.containsString "gamma" bf)
  expect
    "bloom does NOT contain 'delta' (1% FPP, deterministic input)"
    (not (BF.containsString "delta" bf))

  let intBf =
        foldl
          (\acc i -> BF.insertInt64 i acc)
          (BF.emptyBloom 1000 0.01)
          [1, 7, 42, 999, -3]
  expect "int bloom contains 42" (BF.containsInt64 42 intBf)
  expect "int bloom does NOT contain 8000" (not (BF.containsInt64 8000 intBf))

  -- Wire encoding: BloomFilterIndex prefix is the proto length-delimited
  -- field 1; with two entries we should see two outer length prefixes.
  let twoEntries = BF.encodeBloomFilterIndex [bf, bf]
  expect "BloomFilterIndex non-empty" (BS.length twoEntries > 0)

  -- Wire-format split: the two BloomFilterKind variants must use
  -- \*different* protobuf field numbers for the bit-set so a Java
  -- reader keyed on the stream kind picks them up.
  --   * field 1 (numHashFunctions, varint)        -> tag 0x08
  --   * field 2 (legacy bitset, repeated fixed64) -> tag 0x11
  --   * field 3 (utf8bitset, bytes)               -> tag 0x1A
  -- For these inputs numHashFunctions fits in one varint byte, so the
  -- bit-set tag is at offset 2.
  let utf8Bs = BF.encodeBloomFilterAs BF.BloomFilterUtf8 bf
      legacyBs = BF.encodeBloomFilterAs BF.BloomFilterLegacy bf
      bitsetTag = (`BS.index` 2)
  expect
    "BloomFilter prefix: numHashFunctions on field 1 (tag 0x08)"
    (BS.head utf8Bs == 0x08 && BS.head legacyBs == 0x08)
  expect
    "UTF-8 bloom carries the bitset on field 3 (utf8bitset, tag 0x1A)"
    (bitsetTag utf8Bs == 0x1A)
  expect
    "legacy bloom carries the bitset on field 2 (unpacked fixed64, tag 0x11)"
    (bitsetTag legacyBs == 0x11)
  expect
    "UTF-8 and legacy encodings differ byte-for-byte"
    (utf8Bs /= legacyBs)
  -- The legacy unpacked-repeated layout writes one tag-prefixed 8-byte
  -- word per backing 'Word64', so the message size grows linearly:
  -- numHashFunctions header (2 bytes) + 9 bytes per backing word.
  expect
    "legacy encoding length = 2 + 9 * numWords"
    (BS.length legacyBs == 2 + 9 * VU.length (BF.bfBits bf))

  -- Reader-side: a UTF-8 inserter and an explicit-charset inserter
  -- given the same UTF-8 bytes must hit the exact same bit positions
  -- (i.e. the legacy bloom is a strict refinement, not a different
  -- hashing scheme).
  let bf1 = BF.insertString "café" (BF.emptyBloom 200 0.01)
      bf2 = BF.insertStringWith T.encodeUtf8 "café" (BF.emptyBloom 200 0.01)
  expect
    "insertString and insertStringWith encodeUtf8 agree bit-for-bit"
    (BF.bfBits bf1 == BF.bfBits bf2)

  -- Row index: encode two entries, verify the stream is non-empty and
  -- starts with the expected protobuf field-1 tag (length-delimited).
  let rie1 = RI.RowIndexEntry [0, 0] BS.empty
      rie2 = RI.RowIndexEntry [123, 4567] (BS.singleton 0x42)
      idx = RI.encodeRowIndex [rie1, rie2]
  expect "RowIndex non-empty" (BS.length idx > 0)
  expect
    "RowIndex starts with field-1 length-delimited tag"
    (BS.head idx == 0x0A)
  -- ORC.Statistics: predicate evaluator using the new sub-stats.
  do
    let !names = V.fromList ["n", "name"]
        !stats =
          V.fromList
            [ ColumnStatistics
                (Just 5)
                (Just False)
                (Just 40)
                (Just (SkInt (IntegerStatistics (Just 10) (Just 50) (Just 150))))
            , ColumnStatistics
                (Just 5)
                (Just False)
                Nothing
                ( Just
                    ( SkString
                        ( StringStatistics
                            (Just (T.pack "alpha"))
                            (Just (T.pack "delta"))
                            (Just 30)
                            (Just (T.pack "alpha"))
                            (Just (T.pack "delta"))
                        )
                    )
                )
            ]
        !pInRange = Stats.PCol "n" (Stats.PEq (Stats.PVInt64 25))
        !pBelow = Stats.PCol "n" (Stats.PEq (Stats.PVInt64 5))
        !pAbove = Stats.PCol "n" (Stats.PEq (Stats.PVInt64 100))
        !pStrIn = Stats.PCol "name" (Stats.PEq (Stats.PVText (T.pack "beta")))
        !pStrAfter = Stats.PCol "name" (Stats.PEq (Stats.PVText (T.pack "zeta")))
    expect
      "ORC.Statistics: PEq inside int range -> MaybeKeep"
      (Stats.evalStripe names stats pInRange == Stats.PMaybeKeep)
    expect
      "ORC.Statistics: PEq below int min -> Skip"
      (Stats.evalStripe names stats pBelow == Stats.PSkip)
    expect
      "ORC.Statistics: PEq above int max -> Skip"
      (Stats.evalStripe names stats pAbove == Stats.PSkip)
    expect
      "ORC.Statistics: PEq inside string range -> MaybeKeep"
      (Stats.evalStripe names stats pStrIn == Stats.PMaybeKeep)
    expect
      "ORC.Statistics: PEq above string max -> Skip"
      (Stats.evalStripe names stats pStrAfter == Stats.PSkip)

  -- ColumnStatistics round-trip: cover the int / double /
  -- string / date / timestamp / decimal / binary / bucket
  -- variants that the new ColumnStatistics decoder handles.
  do
    let cases =
          [
            ( "int"
            , ColumnStatistics
                (Just 5)
                (Just False)
                (Just 40)
                ( Just
                    ( SkInt
                        ( IntegerStatistics
                            (Just (-100))
                            (Just 200)
                            (Just 100)
                        )
                    )
                )
            )
          ,
            ( "double"
            , ColumnStatistics
                (Just 3)
                (Just True)
                (Just 24)
                ( Just
                    ( SkDouble
                        ( DoubleStatistics
                            (Just (-2.5))
                            (Just 99.99)
                            (Just 99.0)
                        )
                    )
                )
            )
          ,
            ( "string"
            , ColumnStatistics
                (Just 2)
                (Just False)
                Nothing
                ( Just
                    ( SkString
                        ( StringStatistics
                            (Just (T.pack "alpha"))
                            (Just (T.pack "zeta"))
                            (Just 9)
                            (Just (T.pack "alpha"))
                            (Just (T.pack "zeta"))
                        )
                    )
                )
            )
          ,
            ( "date"
            , ColumnStatistics
                (Just 4)
                (Just False)
                Nothing
                ( Just
                    ( SkDate
                        ( DateStatistics
                            (Just 19000)
                            (Just 19365)
                        )
                    )
                )
            )
          ,
            ( "ts"
            , ColumnStatistics
                (Just 1)
                (Just False)
                Nothing
                ( Just
                    ( SkTimestamp
                        ( TimestampStatistics
                            (Just 1700000000)
                            (Just 1700000005)
                            (Just 1700000000)
                            (Just 1700000005)
                        )
                    )
                )
            )
          ,
            ( "dec"
            , ColumnStatistics
                (Just 2)
                (Just False)
                Nothing
                ( Just
                    ( SkDecimal
                        ( DecimalStatistics
                            (Just (T.pack "1.23"))
                            (Just (T.pack "456.78"))
                            (Just (T.pack "458.01"))
                        )
                    )
                )
            )
          ,
            ( "bin"
            , ColumnStatistics
                (Just 3)
                (Just False)
                (Just 99)
                (Just (SkBinary (BinaryStatistics (Just 99))))
            )
          ,
            ( "bool"
            , ColumnStatistics
                (Just 5)
                (Just True)
                Nothing
                ( Just
                    ( SkBucket
                        ( BucketStatistics
                            (V.fromList [3, 2])
                        )
                    )
                )
            )
          ]
    flip mapM_ cases $ \(name, cs) -> do
      let !bytes = BL.toStrict (B.toLazyByteString (encodeColStats cs))
      case decodeColStats bytes of
        Right got
          | got == cs ->
              expect ("ColumnStatistics " ++ name ++ " round-trip") True
        Right got ->
          failTest $
            "ColumnStatistics "
              ++ name
              ++ " mismatch:\n got "
              ++ show got
              ++ "\n exp "
              ++ show cs
        Left e -> failTest $ "ColumnStatistics " ++ name ++ ": " ++ e

  -- Inverse: decode the encoded payload and confirm we get back
  -- the same entries (positions + statistics blob).
  case RI.decodeRowIndex idx of
    Left e -> failTest ("RI.decodeRowIndex: " ++ e)
    Right [d1, d2] -> do
      expect
        "decodeRowIndex: positions[0] roundtrips"
        (RI.riePositions d1 == RI.riePositions rie1)
      expect
        "decodeRowIndex: positions[1] roundtrips"
        (RI.riePositions d2 == RI.riePositions rie2)
      expect
        "decodeRowIndex: statistics[1] roundtrips"
        (RI.rieStatistics d2 == RI.rieStatistics rie2)
    Right other ->
      failTest $
        "decodeRowIndex: expected 2 entries, got "
          ++ show (length other)

  -- DECIMAL128 round-trip via encodeDecimalRawColumn / decodeDecimal128Stream
  -- with values that overflow Int64 to exercise the Integer path.
  let bigPos = 10 ^ (30 :: Int) + 7 -- 30-digit positive
      bigNeg = -(10 ^ (35 :: Int) + 1) -- 35-digit negative
      d128Vals = V.fromList ([0, 1, -1, 12345, bigPos, bigNeg] :: [Integer])
      (d128Data, _scaleStream) = ORC.Write.encodeDecimalRawColumn d128Vals 4
  case ORC.Read.decodeDecimal128Stream (V.length d128Vals) d128Data of
    Right vs ->
      expect
        "DECIMAL128 round-trip (incl. >Int64 magnitudes)"
        (vs == d128Vals)
    Left e -> failTest ("decodeDecimal128Stream: " ++ e)

  -- Encryption: AES-CTR encrypt/decrypt round-trip + per-stripe key
  -- derivation + protobuf encoders for the footer Encryption field.
  let key128 = BS.replicate 16 0x42
      stripeId = 7
      streamOff = 1024
      iv = Enc.deriveStreamIv stripeId streamOff
      plain = BS.pack [0 .. 63]
  case Enc.aesCtrXor key128 iv plain of
    Left e -> failTest ("AES-CTR encrypt: " ++ e)
    Right ct -> do
      expect
        "AES-CTR ciphertext length matches plaintext"
        (BS.length ct == BS.length plain)
      expect
        "AES-CTR ciphertext differs from plaintext"
        (ct /= plain)
      case Enc.aesCtrXor key128 iv ct of
        Right pt -> expect "AES-CTR round-trip" (pt == plain)
        Left e -> failTest ("AES-CTR decrypt: " ++ e)

  case Enc.encryptStripeKey key128 stripeId of
    Left e -> failTest ("encryptStripeKey: " ++ e)
    Right sk ->
      expect
        "encryptStripeKey returns a key the same size as the local key"
        (BS.length sk == BS.length key128)

  -- Protobuf encoders: just check they produce non-empty byte
  -- strings starting with the right field tag for the first
  -- present field. Full byte-compat against orc-java is exercised
  -- by the round-trip writer; here we just ensure the messages
  -- serialize.
  let dm = Enc.DataMask "redact" ["full"] [3, 4, 5]
      ek = Enc.EncryptionKey "kms-master" 1 Enc.AES_CTR_128
      ev = Enc.EncryptionVariant 3 0 (BS.pack [1, 2, 3, 4])
      enc = Enc.Encryption [dm] [ek] [ev] Enc.ProviderHadoop
      encBs = Enc.encodeEncryption enc
  expect
    "encodeEncryption produces non-empty bytes"
    (BS.length encBs > 0)
  expect
    "encodeDataMask first byte is field-1 length-delimited"
    (BS.head (Enc.encodeDataMask dm) == 0x0A)
  expect
    "encodeEncryptionKey first field is name (tag 1)"
    (BS.head (Enc.encodeEncryptionKey ek) == 0x0A)
  expect
    "encodeEncryptionVariant first field is root (tag 1, varint)"
    (BS.head (Enc.encodeEncryptionVariant ev) == 0x08)

  -- Bloom filter encode -> decode -> probe round-trip.
  --
  -- The property we want to pin: encoding a bloom and then
  -- decoding the bytes produces a structurally-identical
  -- bloom (same bitset, same numHashFunctions). That gives
  -- the read-side membership probes exactly the same answers
  -- as the writer's original. We deliberately do NOT assert
  -- "non-members reject" because bloom filters are allowed to
  -- false-positive; what we assert is that whatever answer
  -- the writer's bloom gave, the decoded one gives too.
  do
    let !members = ["alpha", "beta", "gamma"] :: [BS.ByteString]
        !ints = [1, 2, 3, 100, -5] :: [Int64]
        !probes = members ++ ["delta", "epsilon", "zeta"] -- mix
        !intProbes = ints ++ [42, 99, -1000]
        !bf0 = BF.emptyBloom 1024 4
        !bfBytes = foldr BF.insertBytes bf0 members
        !bfFull = foldr BF.insertInt64 bfBytes ints

    -- Modern (UTF8) variant
    let !encUtf8 = BF.encodeBloomFilterAs BF.BloomFilterUtf8 bfFull
    case BF.decodeBloomFilter encUtf8 of
      Left e -> failTest $ "decodeBloomFilter (utf8): " ++ e
      Right bfBack -> do
        expect
          "ORC bloom: utf8 encode/decode preserves numHashFunctions"
          (BF.bfNumHashFunctions bfBack == BF.bfNumHashFunctions bfFull)
        expect
          "ORC bloom: utf8 encode/decode preserves bitset"
          (BF.bfBits bfBack == BF.bfBits bfFull)
        expect
          "ORC bloom: bfCheckBytes agrees with original on every probe"
          ( all
              (\b -> BF.bfCheckBytes b bfBack == BF.bfCheckBytes b bfFull)
              probes
          )
        expect
          "ORC bloom: bfCheckLong agrees with original on every probe"
          ( all
              (\i -> BF.bfCheckLong i bfBack == BF.bfCheckLong i bfFull)
              intProbes
          )
        expect
          "ORC bloom: every inserted member is reported present"
          (all (`BF.bfCheckBytes` bfBack) members)
        expect
          "ORC bloom: every inserted integer is reported present"
          (all (`BF.bfCheckLong` bfBack) ints)

    -- Legacy variant: same data, different on-the-wire slot.
    let !encLegacy = BF.encodeBloomFilterAs BF.BloomFilterLegacy bfFull
    case BF.decodeBloomFilter encLegacy of
      Left e -> failTest $ "decodeBloomFilter (legacy): " ++ e
      Right bfBack ->
        expect
          "ORC bloom: legacy encode/decode round-trips bitset"
          (BF.bfBits bfBack == BF.bfBits bfFull)

    -- BloomFilterIndex of 3 entries
    let !idx = [bfFull, bf0, bfFull]
        !idxBytes = BF.encodeBloomFilterIndexAs BF.BloomFilterUtf8 idx
    case BF.decodeBloomFilterIndex idxBytes of
      Left e -> failTest $ "decodeBloomFilterIndex: " ++ e
      Right back -> do
        expect
          "ORC bloom: index entry count"
          (length back == length idx)
        expect
          "ORC bloom: index entries round-trip"
          ( and
              ( zipWith
                  ( \a b ->
                      BF.bfBits a == BF.bfBits b
                        && BF.bfNumHashFunctions a == BF.bfNumHashFunctions b
                  )
                  back
                  idx
              )
          )

  -- ORC RLE v2 sub-encoding decoder: hand-craft each
  -- sub-encoding's wire bytes and verify the decoder
  -- produces the expected values. The DIRECT path is the
  -- only one our writer emits today; the other three
  -- (SHORT_REPEAT / DELTA / PATCHED_BASE) are
  -- read-side-only, exercised when consuming files written
  -- by the Java/C++/Rust ORC writers.
  do
    -- SHORT_REPEAT: encoding=0, widthBytes 1, count=5 -> emit 0x42 5 times.
    --   first byte = 0b00_000_010 (encoding=0, widthBytes-1=0, count-3=2)
    --              = 0x02
    let !shortRepeatBytes = BS.pack [0x02, 0x42]
    case ORC.Read.decodeRLEv2Int False 5 shortRepeatBytes of
      Right vs ->
        expect
          "RLEv2 SHORT_REPEAT decodes 5x 0x42"
          (VP.toList vs == [0x42, 0x42, 0x42, 0x42, 0x42])
      Left e -> failTest $ "SHORT_REPEAT decode: " ++ e

    -- DIRECT: encoded width=8 (8 bits), len=5, values [1,2,3,4,5].
    --   encodedW for w=8: lookup table maps 8 -> 7 (per ORC v2 spec)
    --   first byte  = 0b01_xxxxx_L where xxxxx = encodedW (7), L = lenHigh
    --     w=8 -> encodedW=7. lenHigh=0. firstByte = 0b01_00111_0 = 0x4E
    --   second byte = (len-1) low 8 bits = 4
    --   data: 1 2 3 4 5 packed at 8 bits each = 5 bytes
    let !directBytes =
          BS.pack
            [ 0x4E -- 0100_1110  -> encoding=01, encodedW=7 (w=8), lenHigh=0
            , 0x04 -- len-1 = 4
            , 1
            , 2
            , 3
            , 4
            , 5
            ]
    case ORC.Read.decodeRLEv2Int False 5 directBytes of
      Right vs ->
        expect
          "RLEv2 DIRECT decodes 5 byte-wide values"
          (VP.toList vs == [1, 2, 3, 4, 5])
      Left e -> failTest $ "DIRECT decode: " ++ e

    -- DELTA: simplest case — base value 100, fixed delta 1,
    --   length 5 -> sequence [100, 101, 102, 103, 104].
    -- Header (2 bytes): encoding=11, deltaWidth=0 (no varying-deltas),
    --   length-1 = 4 (lo bits 8). first byte = 0b11_00000_0 = 0xC0,
    --   second byte = 4.
    -- Then base value as zigzag varint (signed=False so plain varint),
    --   then delta as zigzag varint.
    let !deltaBytes =
          BS.pack
            [ 0xC0 -- encoding=11, deltaWidth=0, lenHigh=0
            , 0x04 -- len-1 = 4
            , 100 -- base = 100 (varint)
            , 1 -- delta = 1 (signed varint -> zigzag 2)... ORC spec says delta is signed varint
            ]
    -- The delta is encoded as a zigzag varint: zigzag(1)=2.
    let !deltaBytes' = BS.pack [0xC0, 0x04, 100, 2]
    -- The unused 'deltaBytes' above kept for documentation;
    -- silence the warning by binding it to ().
    let !_ = deltaBytes
    case ORC.Read.decodeRLEv2Int False 5 deltaBytes' of
      Right vs ->
        expect
          "RLEv2 DELTA decodes monotonic +1 sequence"
          (VP.toList vs == [100, 101, 102, 103, 104])
      Left e -> failTest $ "DELTA decode: " ++ e

  -- ORC.Aggregate: pure stats arithmetic on a synthetic footer.
  -- The footer carries one stripe of 100 rows and per-leaf
  -- column statistics for one Int64 column with min=1, max=99,
  -- sum=1000, num_values=98 (2 nulls).
  do
    let !intStats =
          IntegerStatistics
            { isMinimum = Just 1
            , isMaximum = Just 99
            , isSum = Just 1000
            }
        !cs =
          ColumnStatistics
            { csNumberOfValues = Just 98
            , csHasNull = Just True
            , csBytesOnDisk = Nothing
            , csKind = Just (SkInt intStats)
            }
        !footer =
          ORCFooter
            { orcHeaderLength = 0
            , orcContentLength = 0
            , orcStripes = V.empty
            , orcTypes = V.empty
            , orcMetadata = V.empty
            , orcNumberOfRows = 100
            , orcStatistics = V.singleton cs
            , orcEncryption = Nothing
            }
    expect
      "ORC fileRowCount returns orcNumberOfRows"
      (Agg.fileRowCount footer == 100)
    expect
      "ORC columnNonNullCount reads csNumberOfValues"
      (Agg.columnNonNullCount footer 0 == Just 98)
    expect
      "ORC columnMin reads IntegerStatistics.minimum"
      (Agg.columnMin footer 0 == Just (Pred.PVInt64 1))
    expect
      "ORC columnMax reads IntegerStatistics.maximum"
      (Agg.columnMax footer 0 == Just (Pred.PVInt64 99))
    expect
      "ORC columnSum reads IntegerStatistics.sum"
      (Agg.columnSum footer 0 == Just (Pred.PVInt64 1000))
    expect
      "ORC columnSum returns Nothing for absent stats"
      (Agg.columnSum footer 1 == Nothing)

  putStrLn "All ORC writer tests passed."


expect :: String -> Bool -> IO ()
expect name True = putStrLn ("OK: " ++ name)
expect name False = do
  putStrLn ("FAIL: " ++ name)
  exitFailure


failTest :: String -> IO ()
failTest msg = do
  putStrLn ("FAIL: " ++ msg)
  exitFailure
