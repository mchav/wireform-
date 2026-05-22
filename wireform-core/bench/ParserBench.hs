{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import Data.Word
import Data.Int

import qualified Wireform.Parser as W
import qualified Wireform.Parser.Driver as W
import qualified FlatParse.Basic as FP

------------------------------------------------------------------------
-- Input generation
------------------------------------------------------------------------

-- | N copies of a 4-byte big-endian word
mkWord32Input :: Int -> ByteString
mkWord32Input n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word32BE (fromIntegral i) | i <- [0 .. n - 1] ]

-- | N copies of a single byte
mkByteInput :: Int -> ByteString
mkByteInput n = BS.replicate n 0x42

-- | Length-prefixed messages: 1-byte length + payload
-- Each message is 8 bytes of payload (length byte = 0x08)
mkLengthPrefixedInput :: Int -> ByteString
mkLengthPrefixedInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 8 <> BSB.byteString (BS.replicate 8 (fromIntegral i))
          | i <- [0 .. n - 1]
          ]

-- | ASCII decimal numbers separated by newlines
mkAsciiDecimalInput :: Int -> ByteString
mkAsciiDecimalInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.stringUtf8 (show i) <> BSB.word8 0x0A | i <- [0 .. n - 1] ]

-- | Alternating tag bytes: 0x01 or 0x02, each followed by a word32be
mkTaggedInput :: Int -> ByteString
mkTaggedInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 (if even i then 0x01 else 0x02)
            <> BSB.word32BE (fromIntegral i)
          | i <- [0 .. n - 1]
          ]

-- | UTF-8 text: ASCII (1-byte) characters
mkAsciiTextInput :: Int -> ByteString
mkAsciiTextInput n = BS.replicate n 0x61 -- 'a'

-- | UTF-8 text: 2-byte characters (Latin-1 supplement, e.g. é = C3 A9)
mkUtf8_2byteInput :: Int -> ByteString
mkUtf8_2byteInput n = LBS.toStrict . BSB.toLazyByteString $
  mconcat [ BSB.word8 0xC3 <> BSB.word8 0xA9 | _ <- [1..n] ]

------------------------------------------------------------------------
-- Wireform parsers
------------------------------------------------------------------------

type WP = W.Parser () 

wfWord32s :: Int -> WP ()
wfWord32s 0 = pure ()
wfWord32s n = do
  !_ <- W.anyWord32be
  wfWord32s (n - 1)
{-# INLINE wfWord32s #-}

wfBytes :: Int -> WP ()
wfBytes 0 = pure ()
wfBytes n = do
  !_ <- W.anyWord8
  wfBytes (n - 1)
{-# INLINE wfBytes #-}

wfLengthPrefixed :: Int -> WP ()
wfLengthPrefixed 0 = pure ()
wfLengthPrefixed n = do
  !len <- W.anyWord8
  !_ <- W.takeBs (fromIntegral len)
  wfLengthPrefixed (n - 1)
{-# INLINE wfLengthPrefixed #-}

wfAsciiDecimals :: Int -> WP ()
wfAsciiDecimals 0 = pure ()
wfAsciiDecimals n = do
  !_ <- W.anyAsciiDecimalWord
  W.word8 0x0A
  wfAsciiDecimals (n - 1)
{-# INLINE wfAsciiDecimals #-}

wfTagged :: Int -> WP ()
wfTagged 0 = pure ()
wfTagged n = do
  (W.word8 0x01 >> W.anyWord32be >> pure ())
    W.<|> (W.word8 0x02 >> W.anyWord32be >> pure ())
  wfTagged (n - 1)
{-# INLINE wfTagged #-}

wfAsciiChars :: Int -> WP ()
wfAsciiChars 0 = pure ()
wfAsciiChars n = do
  W.skipSatisfyAscii (\_ -> True)
  wfAsciiChars (n - 1)
{-# INLINE wfAsciiChars #-}

wfUtf8Chars :: Int -> WP ()
wfUtf8Chars 0 = pure ()
wfUtf8Chars n = do
  !_ <- W.anyChar
  wfUtf8Chars (n - 1)
{-# INLINE wfUtf8Chars #-}

------------------------------------------------------------------------
-- FlatParse parsers
------------------------------------------------------------------------

type FPP = FP.Parser ()

fpWord32s :: Int -> FPP ()
fpWord32s 0 = pure ()
fpWord32s n = do
  !_ <- FP.anyWord32be
  fpWord32s (n - 1)
{-# INLINE fpWord32s #-}

fpBytes :: Int -> FPP ()
fpBytes 0 = pure ()
fpBytes n = do
  !_ <- FP.anyWord8
  fpBytes (n - 1)
{-# INLINE fpBytes #-}

fpLengthPrefixed :: Int -> FPP ()
fpLengthPrefixed 0 = pure ()
fpLengthPrefixed n = do
  !len <- FP.anyWord8
  !_ <- FP.take (fromIntegral len)
  fpLengthPrefixed (n - 1)
{-# INLINE fpLengthPrefixed #-}

fpAsciiDecimals :: Int -> FPP ()
fpAsciiDecimals 0 = pure ()
fpAsciiDecimals n = do
  !_ <- FP.anyAsciiDecimalWord
  FP.word8 0x0A
  fpAsciiDecimals (n - 1)
{-# INLINE fpAsciiDecimals #-}

fpTagged :: Int -> FPP ()
fpTagged 0 = pure ()
fpTagged n = do
  (FP.word8 0x01 >> FP.anyWord32be >> pure ())
    FP.<|> (FP.word8 0x02 >> FP.anyWord32be >> pure ())
  fpTagged (n - 1)
{-# INLINE fpTagged #-}

fpAsciiChars :: Int -> FPP ()
fpAsciiChars 0 = pure ()
fpAsciiChars n = do
  FP.skipSatisfyAscii (\_ -> True)
  fpAsciiChars (n - 1)
{-# INLINE fpAsciiChars #-}

fpUtf8Chars :: Int -> FPP ()
fpUtf8Chars 0 = pure ()
fpUtf8Chars n = do
  !_ <- FP.anyChar
  fpUtf8Chars (n - 1)
{-# INLINE fpUtf8Chars #-}

------------------------------------------------------------------------
-- Runners
------------------------------------------------------------------------

runWF :: WP a -> ByteString -> a
runWF p bs = case W.parseByteString p bs of
  Right a -> a
  Left _  -> error "wireform parse failed"
{-# INLINE runWF #-}

runFP :: FPP a -> ByteString -> a
runFP p bs = case FP.runParser p bs of
  FP.OK a _ -> a
  _         -> error "flatparse parse failed"
{-# INLINE runFP #-}

------------------------------------------------------------------------
-- Benchmark harness
------------------------------------------------------------------------

main :: IO ()
main = do
  let !n = 100000

  -- Pre-generate inputs
  let !byteInput    = mkByteInput n
      !word32Input  = mkWord32Input n
      !lpInput      = mkLengthPrefixedInput n
      !decInput     = mkAsciiDecimalInput n
      !taggedInput  = mkTaggedInput n
      !asciiInput   = mkAsciiTextInput n
      !utf8Input    = mkUtf8_2byteInput n

  putStrLn $ "Input sizes:"
  putStrLn $ "  byte:     " <> show (BS.length byteInput) <> " bytes"
  putStrLn $ "  word32:   " <> show (BS.length word32Input) <> " bytes"
  putStrLn $ "  len-pfx:  " <> show (BS.length lpInput) <> " bytes"
  putStrLn $ "  decimal:  " <> show (BS.length decInput) <> " bytes"
  putStrLn $ "  tagged:   " <> show (BS.length taggedInput) <> " bytes"
  putStrLn $ "  ascii:    " <> show (BS.length asciiInput) <> " bytes"
  putStrLn $ "  utf8-2b:  " <> show (BS.length utf8Input) <> " bytes"

  defaultMain
    [ bgroup "anyWord8 x100k"
        [ bench "wireform" $ nf (runWF (wfBytes n)) byteInput
        , bench "flatparse" $ nf (runFP (fpBytes n)) byteInput
        ]
    , bgroup "anyWord32be x100k"
        [ bench "wireform" $ nf (runWF (wfWord32s n)) word32Input
        , bench "flatparse" $ nf (runFP (fpWord32s n)) word32Input
        ]
    , bgroup "length-prefixed messages x100k"
        [ bench "wireform" $ nf (runWF (wfLengthPrefixed n)) lpInput
        , bench "flatparse" $ nf (runFP (fpLengthPrefixed n)) lpInput
        ]
    , bgroup "ASCII decimal + newline x100k"
        [ bench "wireform" $ nf (runWF (wfAsciiDecimals n)) decInput
        , bench "flatparse" $ nf (runFP (fpAsciiDecimals n)) decInput
        ]
    , bgroup "tagged alternatives x100k"
        [ bench "wireform" $ nf (runWF (wfTagged n)) taggedInput
        , bench "flatparse" $ nf (runFP (fpTagged n)) taggedInput
        ]
    , bgroup "anyCharASCII x100k"
        [ bench "wireform" $ nf (runWF (wfAsciiChars n)) asciiInput
        , bench "flatparse" $ nf (runFP (fpAsciiChars n)) asciiInput
        ]
    , bgroup "anyChar (2-byte UTF-8) x100k"
        [ bench "wireform" $ nf (runWF (wfUtf8Chars n)) utf8Input
        , bench "flatparse" $ nf (runFP (fpUtf8Chars n)) utf8Input
        ]
    ]
