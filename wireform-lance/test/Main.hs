{-# LANGUAGE OverloadedStrings #-}

{- | Round-trip the 'Lance.Format' parser over a synthesised
Lance file. We don't have a real Lance writer in-tree (the
protobuf encoder for ColumnMetadata is downstream), but the
envelope + footer + offset-table layout is fully specified
and we can build a minimal-but-valid file by hand to drive
the reader end-to-end.
-}
module Main (main) where

import Data.ByteString qualified as BS
import Data.Vector qualified as V
import Data.Word (Word16, Word32, Word64)
import Lance.Format qualified as L
import Lance.IO qualified as LIO
import Test.Syd


main :: IO ()
main =
  sydTest $
    describe "wireform-lance" $
      sequence_
        [ it "envelope round-trip" envelopeTest
        , it "footer fields decoded" footerTest
        , it "column metadata extraction" columnMetaTest
        , it "missing trailing magic is rejected" missingTrailingMagicTest
        , it "file shorter than footer is rejected" tooShortTest
        , it "manifest filename round-trip" manifestNameRoundTrip
        ]


{- | Build a minimal Lance file:

  * one column metadata blob (3 dummy bytes 0xAA 0xBB 0xCC)
  * column offset table: one (u64 pos, u64 size) entry
  * empty global-buffers offset table
  * 40-byte footer pointing at the above

Total = 3 + 16 + 0 + 40 = 59 bytes.
-}
sampleFile :: BS.ByteString
sampleFile =
  BS.concat
    [ columnBlob
    , cmoTable
    , gboTable
    , footer
    ]
  where
    columnBlob = BS.pack [0xAA, 0xBB, 0xCC] -- offset 0..2
    cmoTable = packU64 0 <> packU64 3 -- offset 3..18
    gboTable = BS.empty -- offset 19..18 (no entries)
    footer =
      BS.concat
        [ packU64 (fromIntegral colMeta0Off) --  0..7
        , packU64 (fromIntegral cmoOff) --  8..15
        , packU64 (fromIntegral gboOff) -- 16..23
        , packU32 (fromIntegral numGlobal) -- 24..27
        , packU32 (fromIntegral numCols) -- 28..31
        , packU16 (fromIntegral majV) -- 32..33
        , packU16 (fromIntegral minV) -- 34..35
        , L.lanceMagic -- 36..39
        ]

    colMeta0Off = 0 :: Int
    cmoOff = 3 :: Int
    gboOff = 19 :: Int
    numGlobal = 0 :: Int
    numCols = 1 :: Int
    majV = 2 :: Int
    minV = 1 :: Int


envelopeTest :: IO ()
envelopeTest = case L.readLanceFile sampleFile of
  Right _ -> pure ()
  Left err -> expectationFailure ("expected Right, got " ++ err)


footerTest :: IO ()
footerTest = case L.parseFooter sampleFile of
  Left err -> expectationFailure err
  Right f -> do
    L.lfColumnMeta0Offset f `shouldBe` 0
    L.lfCMOTableOffset f `shouldBe` 3
    L.lfGBOTableOffset f `shouldBe` 19
    L.lfNumGlobalBuffers f `shouldBe` 0
    L.lfNumColumns f `shouldBe` 1
    L.lfMajorVersion f `shouldBe` 2
    L.lfMinorVersion f `shouldBe` 1


columnMetaTest :: IO ()
columnMetaTest = case L.readLanceFile sampleFile of
  Left err -> expectationFailure err
  Right lf -> do
    case L.parseColumnOffsetTable lf of
      Left err -> expectationFailure err
      Right tbl -> do
        V.length tbl `shouldBe` 1
        let s = V.head tbl
        L.csPosition s `shouldBe` 0
        L.csSize s `shouldBe` 3
    case L.extractColumnMetadataBytes lf 0 of
      Right bytes -> bytes `shouldBe` BS.pack [0xAA, 0xBB, 0xCC]
      Left err -> expectationFailure err


missingTrailingMagicTest :: IO ()
missingTrailingMagicTest =
  let n = BS.length sampleFile
      corrupt = BS.take (n - 4) sampleFile <> BS.pack [0x00, 0x00, 0x00, 0x00]
  in case L.readLanceFile corrupt of
       Left _ -> pure ()
       Right _ -> expectationFailure "expected Left for corrupted trailing magic"


tooShortTest :: IO ()
tooShortTest = case L.readLanceFile (BS.pack [0x4C, 0x41, 0x4E, 0x43]) of
  Left _ -> pure ()
  Right _ -> expectationFailure "expected Left for too-short file"


manifestNameRoundTrip :: IO ()
manifestNameRoundTrip = do
  -- pylance writes manifest-filename = (2^64 - 1 - version), so:
  LIO.decodeManifestFileName "18446744073709551614.manifest" `shouldBe` Just 1
  LIO.decodeManifestFileName "18446744073709551613.manifest" `shouldBe` Just 2
  LIO.decodeManifestFileName "0.manifest"
    `shouldBe` Just (maxBound :: Word64)
  -- Filename → version → filename.
  LIO.encodeManifestFileName 1 `shouldBe` "18446744073709551614.manifest"
  LIO.encodeManifestFileName 2 `shouldBe` "18446744073709551613.manifest"
  -- Reject malformed names.
  LIO.decodeManifestFileName "not-a-version.manifest" `shouldBe` Nothing
  LIO.decodeManifestFileName "1.json" `shouldBe` Nothing


-- ============================================================
-- Tiny LE encoders (avoid pulling in extra deps for tests).
-- ============================================================

packU64 :: Word64 -> BS.ByteString
packU64 w =
  BS.pack
    [ byte 0
    , byte 1
    , byte 2
    , byte 3
    , byte 4
    , byte 5
    , byte 6
    , byte 7
    ]
  where
    byte i = fromIntegral ((w `divInt` (256 ^ (i :: Int))) `modInt` 256)


packU32 :: Word32 -> BS.ByteString
packU32 w =
  BS.pack
    [byte 0, byte 1, byte 2, byte 3]
  where
    byte i = fromIntegral ((fromIntegral w :: Word64) `divInt` (256 ^ (i :: Int)) `modInt` 256)


packU16 :: Word16 -> BS.ByteString
packU16 w =
  BS.pack
    [byte 0, byte 1]
  where
    byte i = fromIntegral ((fromIntegral w :: Word64) `divInt` (256 ^ (i :: Int)) `modInt` 256)


divInt :: Word64 -> Word64 -> Word64
divInt = div


modInt :: Word64 -> Word64 -> Word64
modInt = mod
