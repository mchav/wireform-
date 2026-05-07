{-# LANGUAGE OverloadedStrings #-}
-- | Round-trip the 'Lance.Format' parser over a synthesised
-- Lance file. We don't have a real Lance writer in-tree (the
-- protobuf encoder for ColumnMetadata is downstream), but the
-- envelope + footer + offset-table layout is fully specified
-- and we can build a minimal-but-valid file by hand to drive
-- the reader end-to-end.
module Main (main) where

import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import qualified Data.Vector as V

import qualified Lance.Format as L

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "wireform-lance"
  [ testCase "envelope round-trip" envelopeTest
  , testCase "footer fields decoded" footerTest
  , testCase "column metadata extraction" columnMetaTest
  , testCase "missing leading magic is rejected" missingLeadingMagicTest
  , testCase "missing trailing magic is rejected" missingTrailingMagicTest
  , testCase "file shorter than footer is rejected" tooShortTest
  ]

-- | Build a minimal Lance file:
--
--   * leading "LANC" magic
--   * one column metadata blob (3 dummy bytes 0xAA 0xBB 0xCC)
--   * column offset table: one (u64 pos, u64 size) entry
--   * empty global-buffers offset table
--   * 40-byte footer pointing at the above
--
-- Total = 4 + 3 + 16 + 0 + 40 = 63 bytes.
sampleFile :: BS.ByteString
sampleFile = BS.concat
  [ leadingMagic
  , columnBlob
  , cmoTable
  , gboTable
  , footer
  ]
  where
    leadingMagic = L.lanceMagic                          -- offset 0..3
    columnBlob   = BS.pack [0xAA, 0xBB, 0xCC]            -- offset 4..6
    cmoTable     = packU64 4 <> packU64 3                -- offset 7..22
    gboTable     = BS.empty                              -- offset 23..22 (no entries)
    footer       =
      BS.concat
        [ packU64 (fromIntegral colMeta0Off)             --  0..7
        , packU64 (fromIntegral cmoOff)                  --  8..15
        , packU64 (fromIntegral gboOff)                  -- 16..23
        , packU32 (fromIntegral numGlobal)               -- 24..27
        , packU32 (fromIntegral numCols)                 -- 28..31
        , packU16 (fromIntegral majV)                    -- 32..33
        , packU16 (fromIntegral minV)                    -- 34..35
        , L.lanceMagic                                   -- 36..39
        ]

    colMeta0Off = 4 :: Int
    cmoOff      = 7 :: Int
    gboOff      = 23 :: Int
    numGlobal   = 0 :: Int
    numCols     = 1 :: Int
    majV        = 2 :: Int
    minV        = 1 :: Int

envelopeTest :: Assertion
envelopeTest = case L.readLanceFile sampleFile of
  Right _  -> pure ()
  Left err -> assertFailure ("expected Right, got " ++ err)

footerTest :: Assertion
footerTest = case L.parseFooter sampleFile of
  Left err -> assertFailure err
  Right f  -> do
    L.lfColumnMeta0Offset f @?= 4
    L.lfCMOTableOffset    f @?= 7
    L.lfGBOTableOffset    f @?= 23
    L.lfNumGlobalBuffers  f @?= 0
    L.lfNumColumns        f @?= 1
    L.lfMajorVersion      f @?= 2
    L.lfMinorVersion      f @?= 1

columnMetaTest :: Assertion
columnMetaTest = case L.readLanceFile sampleFile of
  Left err -> assertFailure err
  Right lf -> do
    case L.parseColumnOffsetTable lf of
      Left err -> assertFailure err
      Right tbl -> do
        V.length tbl @?= 1
        let s = V.head tbl
        L.csPosition s @?= 4
        L.csSize     s @?= 3
    case L.extractColumnMetadataBytes lf 0 of
      Right bytes -> bytes @?= BS.pack [0xAA, 0xBB, 0xCC]
      Left  err   -> assertFailure err

missingLeadingMagicTest :: Assertion
missingLeadingMagicTest =
  let corrupt = BS.cons 0x00 (BS.drop 1 sampleFile)
   in case L.readLanceFile corrupt of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left for corrupted leading magic"

missingTrailingMagicTest :: Assertion
missingTrailingMagicTest =
  let n       = BS.length sampleFile
      corrupt = BS.take (n - 4) sampleFile <> BS.pack [0x00, 0x00, 0x00, 0x00]
   in case L.readLanceFile corrupt of
        Left _  -> pure ()
        Right _ -> assertFailure "expected Left for corrupted trailing magic"

tooShortTest :: Assertion
tooShortTest = case L.readLanceFile (BS.pack [0x4C, 0x41, 0x4E, 0x43]) of
  Left _  -> pure ()
  Right _ -> assertFailure "expected Left for too-short file"

-- ============================================================
-- Tiny LE encoders (avoid pulling in extra deps for tests).
-- ============================================================

packU64 :: Word64 -> BS.ByteString
packU64 w = BS.pack
  [ byte 0, byte 1, byte 2, byte 3
  , byte 4, byte 5, byte 6, byte 7
  ]
  where byte i = fromIntegral ((w `divInt` (256 ^ (i :: Int))) `modInt` 256)

packU32 :: Word32 -> BS.ByteString
packU32 w = BS.pack
  [ byte 0, byte 1, byte 2, byte 3 ]
  where byte i = fromIntegral ((fromIntegral w :: Word64) `divInt` (256 ^ (i :: Int)) `modInt` 256)

packU16 :: Word16 -> BS.ByteString
packU16 w = BS.pack
  [ byte 0, byte 1 ]
  where byte i = fromIntegral ((fromIntegral w :: Word64) `divInt` (256 ^ (i :: Int)) `modInt` 256)

divInt :: Word64 -> Word64 -> Word64
divInt = div

modInt :: Word64 -> Word64 -> Word64
modInt = mod
