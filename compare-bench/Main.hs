{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Criterion benchmark: hs-proto vs proto-lens (real generated code).
--
-- Run: cabal bench compare-bench
module Main where

import Criterion.Main
import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import Data.Int (Int32, Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Data.ProtoLens (defMessage)
import qualified Data.ProtoLens as PLC
import Lens.Family2 ((&), (.~))

import qualified Proto.Bench as PL
import qualified Proto.Bench_Fields as F

import qualified Proto.Encode as H
import qualified Proto.Decode as H
import qualified Proto.SizedBuilder as SB
import HsProtoTypes

main :: IO ()
main = defaultMain
  [ bgroup "Small"
      [ bgroup "encode"
          [ bench "hs-proto"   $ nf H.encodeMessageSized smallHS
          , bench "proto-lens" $ nf PLC.encodeMessage smallPL
          ]
      , bgroup "decode"
          [ bench "hs-proto"   $ nf decSmallH smallBytes
          , bench "proto-lens" $ nf decSmallP smallBytes
          ]
      , bgroup "roundtrip"
          [ bench "hs-proto"   $ nf rtSmallH smallHS
          , bench "proto-lens" $ nf rtSmallP smallPL
          ]
      ]
  , bgroup "Medium"
      [ bgroup "encode"
          [ bench "hs-proto"   $ nf H.encodeMessageSized mediumHS
          , bench "proto-lens" $ nf PLC.encodeMessage mediumPL
          ]
      , bgroup "decode"
          [ bench "hs-proto"   $ nf decMediumH mediumBytes
          , bench "proto-lens" $ nf decMediumP mediumBytes
          ]
      , bgroup "roundtrip"
          [ bench "hs-proto"   $ nf rtMediumH mediumHS
          , bench "proto-lens" $ nf rtMediumP mediumPL
          ]
      ]
  , bgroup "Nested"
      [ bgroup "encode"
          [ bench "hs-proto"   $ nf H.encodeMessageSized nestedHS
          , bench "proto-lens" $ nf PLC.encodeMessage nestedPL
          ]
      , bgroup "decode"
          [ bench "hs-proto"   $ nf decNestedH nestedBytes
          , bench "proto-lens" $ nf decNestedP nestedBytes
          ]
      , bgroup "roundtrip"
          [ bench "hs-proto"   $ nf rtNestedH nestedHS
          , bench "proto-lens" $ nf rtNestedP nestedPL
          ]
      ]
  , bgroup "Repeated"
      [ bgroup "encode"
          [ bench "hs-proto"   $ nf H.encodeMessageSized repeatedHS
          , bench "proto-lens" $ nf PLC.encodeMessage repeatedPL
          ]
      , bgroup "decode"
          [ bench "hs-proto"   $ nf decRepH repeatedBytes
          , bench "proto-lens" $ nf decRepP repeatedBytes
          ]
      ]
  ]

-- Decode/roundtrip wrappers to avoid ambiguous types
decSmallH :: BS.ByteString -> Either H.DecodeError HSmall
decSmallH = H.decodeMessage
{-# NOINLINE decSmallH #-}
decSmallP :: BS.ByteString -> Either String PL.Small
decSmallP = PLC.decodeMessage
{-# NOINLINE decSmallP #-}
rtSmallH :: HSmall -> Either H.DecodeError HSmall
rtSmallH m = H.decodeMessage (H.encodeMessageSized m)
{-# NOINLINE rtSmallH #-}
rtSmallP :: PL.Small -> Either String PL.Small
rtSmallP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtSmallP #-}

decMediumH :: BS.ByteString -> Either H.DecodeError HMedium
decMediumH = H.decodeMessage
{-# NOINLINE decMediumH #-}
decMediumP :: BS.ByteString -> Either String PL.Medium
decMediumP = PLC.decodeMessage
{-# NOINLINE decMediumP #-}
rtMediumH :: HMedium -> Either H.DecodeError HMedium
rtMediumH m = H.decodeMessage (H.encodeMessageSized m)
{-# NOINLINE rtMediumH #-}
rtMediumP :: PL.Medium -> Either String PL.Medium
rtMediumP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtMediumP #-}

decNestedH :: BS.ByteString -> Either H.DecodeError HWithNested
decNestedH = H.decodeMessage
{-# NOINLINE decNestedH #-}
decNestedP :: BS.ByteString -> Either String PL.WithNested
decNestedP = PLC.decodeMessage
{-# NOINLINE decNestedP #-}
rtNestedH :: HWithNested -> Either H.DecodeError HWithNested
rtNestedH m = H.decodeMessage (H.encodeMessageSized m)
{-# NOINLINE rtNestedH #-}
rtNestedP :: PL.WithNested -> Either String PL.WithNested
rtNestedP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtNestedP #-}

decRepH :: BS.ByteString -> Either H.DecodeError HWithRepeated
decRepH = H.decodeMessage
{-# NOINLINE decRepH #-}
decRepP :: BS.ByteString -> Either String PL.WithRepeated
decRepP = PLC.decodeMessage
{-# NOINLINE decRepP #-}

-- hs-proto test values

smallHS :: HSmall
smallHS = HSmall 42 "hello world" True

mediumHS :: HMedium
mediumHS = HMedium "benchmark title" 100 3.14159 "payload\x00\x01\x02" True 1708000000 "a medium description" 0.75

nestedHS :: HWithNested
nestedHS = HWithNested 99 (Just (HSmall 1 "inner" True)) "outer label"

repeatedHS :: HWithRepeated
repeatedHS = HWithRepeated
  (V.fromList [1..50])
  (V.fromList (fmap (\i -> "tag_" <> T.pack (show i)) [1..20 :: Int]))
  (V.fromList [ HSmall (fromIntegral i) ("item" <> T.pack (show i)) (even i) | i <- [1..10 :: Int] ])

-- proto-lens test values (using the real generated field lenses)

smallPL :: PL.Small
smallPL = (defMessage :: PL.Small)
  & F.id .~ (42 :: Int64)
  & F.name .~ ("hello world" :: Text)
  & F.active .~ True

mediumPL :: PL.Medium
mediumPL = (defMessage :: PL.Medium)
  & F.title .~ ("benchmark title" :: Text)
  & F.count .~ (100 :: Int32)
  & F.score .~ (3.14159 :: Double)
  & F.payload .~ ("payload\x00\x01\x02" :: BS.ByteString)
  & F.enabled .~ True
  & F.timestamp .~ (1708000000 :: Int64)
  & F.description .~ ("a medium description" :: Text)
  & F.ratio .~ (0.75 :: Float)

nestedPL :: PL.WithNested
nestedPL =
  let inner = (defMessage :: PL.Small)
        & F.id .~ (1 :: Int64)
        & F.name .~ ("inner" :: Text)
        & F.active .~ True
  in (defMessage :: PL.WithNested)
    & F.id .~ (99 :: Int64)
    & F.inner .~ inner
    & F.label .~ ("outer label" :: Text)

repeatedPL :: PL.WithRepeated
repeatedPL =
  let mkItem :: Int -> PL.Small
      mkItem i = (defMessage :: PL.Small)
        & F.id .~ (fromIntegral i :: Int64)
        & F.name .~ (("item" <> T.pack (show i)) :: Text)
        & F.active .~ (even i :: Bool)
  in (defMessage :: PL.WithRepeated)
    & F.vec'values .~ VU.fromList ([1..50] :: [Int32])
    & F.vec'tags .~ V.fromList (fmap (\i -> ("tag_" <> T.pack (show i)) :: Text) [1..20 :: Int])
    & F.vec'items .~ V.fromList (fmap mkItem [1..10])

-- Pre-encoded bytes
smallBytes, mediumBytes, nestedBytes, repeatedBytes :: BS.ByteString
smallBytes = H.encodeMessageSized smallHS
mediumBytes = H.encodeMessageSized mediumHS
nestedBytes = H.encodeMessageSized nestedHS
repeatedBytes = H.encodeMessageSized repeatedHS
