{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Criterion benchmark: wireform vs proto-lens (real generated code).

Run: cabal bench compare-bench
-}
module Main where

import Control.DeepSeq (NFData (..))
import Criterion.Main
import Data.ByteString qualified as BS
import Data.Int (Int32, Int64)
import Data.ProtoLens (defMessage)
import Data.ProtoLens qualified as PLC
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as VU
import Lens.Family2 ((&), (.~))
import Proto.Bench qualified as PL
import Proto.Bench_Fields qualified as F
import Proto.Decode qualified as H
import Proto.Encode qualified as H
import WireformTypes


encSmallH :: HSmall -> BS.ByteString
encSmallH = directEncodeSmall
{-# NOINLINE encSmallH #-}


encMediumH :: HMedium -> BS.ByteString
encMediumH = directEncodeMedium
{-# NOINLINE encMediumH #-}


encNestedH :: HWithNested -> BS.ByteString
encNestedH = directEncodeNested
{-# NOINLINE encNestedH #-}


encRepH :: HWithRepeated -> BS.ByteString
encRepH = directEncodeRepeated
{-# NOINLINE encRepH #-}


-- Builder-based encoders (exact-size FixedBuffer via encodeMessageSized)
encSmallB :: HSmall -> BS.ByteString
encSmallB = H.encodeMessageSized
{-# NOINLINE encSmallB #-}


encMediumB :: HMedium -> BS.ByteString
encMediumB = H.encodeMessage
{-# NOINLINE encMediumB #-}


encNestedB :: HWithNested -> BS.ByteString
encNestedB = H.encodeMessage
{-# NOINLINE encNestedB #-}


encRepB :: HWithRepeated -> BS.ByteString
encRepB = H.encodeMessage
{-# NOINLINE encRepB #-}


-- Direct fast-builder encoders (no SizedBuilder wrapper)
encSmallFB :: HSmall -> BS.ByteString
encSmallFB = encodeSmallDirect
{-# NOINLINE encSmallFB #-}


encMediumFB :: HMedium -> BS.ByteString
encMediumFB = encodeMediumDirect
{-# NOINLINE encMediumFB #-}


main :: IO ()
main =
  defaultMain
    [ bgroup
        "Small"
        [ bgroup
            "encode"
            [ bench "wireform-direct" $ nf encSmallH smallHS
            , bench "wireform-builder" $ nf encSmallB smallHS
            , bench "wireform-fb-direct" $ nf encSmallFB smallHS
            , bench "proto-lens" $ nf PLC.encodeMessage smallPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decSmallH smallBytes
            , bench "proto-lens" $ nf decSmallP smallBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtSmallH smallHS
            , bench "proto-lens" $ nf rtSmallP smallPL
            ]
        ]
    , bgroup
        "Medium"
        [ bgroup
            "encode"
            [ bench "wireform-direct" $ nf encMediumH mediumHS
            , bench "wireform-builder" $ nf encMediumB mediumHS
            , bench "wireform-fb-direct" $ nf encMediumFB mediumHS
            , bench "proto-lens" $ nf PLC.encodeMessage mediumPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decMediumH mediumBytes
            , bench "proto-lens" $ nf decMediumP mediumBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtMediumH mediumHS
            , bench "proto-lens" $ nf rtMediumP mediumPL
            ]
        ]
    , bgroup
        "Nested"
        [ bgroup
            "encode"
            [ bench "wireform-direct" $ nf encNestedH nestedHS
            , bench "wireform-builder" $ nf encNestedB nestedHS
            , bench "proto-lens" $ nf PLC.encodeMessage nestedPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decNestedH nestedBytes
            , bench "proto-lens" $ nf decNestedP nestedBytes
            ]
        , bgroup
            "roundtrip"
            [ bench "wireform" $ nf rtNestedH nestedHS
            , bench "proto-lens" $ nf rtNestedP nestedPL
            ]
        ]
    , bgroup
        "Repeated"
        [ bgroup
            "encode"
            [ bench "wireform-direct" $ nf encRepH repeatedHS
            , bench "wireform-builder" $ nf encRepB repeatedHS
            , bench "proto-lens" $ nf PLC.encodeMessage repeatedPL
            ]
        , bgroup
            "decode"
            [ bench "wireform" $ nf decRepH repeatedBytes
            , bench "proto-lens" $ nf decRepP repeatedBytes
            ]
        ]
    ]


-- Decode wrappers — using fast Addr#-based decoders
decSmallH :: BS.ByteString -> Either H.DecodeError HSmall
decSmallH = fastDecodeSmall
{-# NOINLINE decSmallH #-}


decSmallP :: BS.ByteString -> Either String PL.Small
decSmallP = PLC.decodeMessage
{-# NOINLINE decSmallP #-}


rtSmallH :: HSmall -> Either H.DecodeError HSmall
rtSmallH m = H.decodeMessage (directEncodeSmall m)
{-# NOINLINE rtSmallH #-}


rtSmallP :: PL.Small -> Either String PL.Small
rtSmallP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtSmallP #-}


decMediumH :: BS.ByteString -> Either H.DecodeError HMedium
decMediumH = fastDecodeMedium
{-# NOINLINE decMediumH #-}


decMediumP :: BS.ByteString -> Either String PL.Medium
decMediumP = PLC.decodeMessage
{-# NOINLINE decMediumP #-}


rtMediumH :: HMedium -> Either H.DecodeError HMedium
rtMediumH m = H.decodeMessage (directEncodeMedium m)
{-# NOINLINE rtMediumH #-}


rtMediumP :: PL.Medium -> Either String PL.Medium
rtMediumP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtMediumP #-}


decNestedH :: BS.ByteString -> Either H.DecodeError HWithNested
decNestedH = fastDecodeNested
{-# NOINLINE decNestedH #-}


decNestedP :: BS.ByteString -> Either String PL.WithNested
decNestedP = PLC.decodeMessage
{-# NOINLINE decNestedP #-}


rtNestedH :: HWithNested -> Either H.DecodeError HWithNested
rtNestedH m = H.decodeMessage (directEncodeNested m)
{-# NOINLINE rtNestedH #-}


rtNestedP :: PL.WithNested -> Either String PL.WithNested
rtNestedP m = PLC.decodeMessage (PLC.encodeMessage m)
{-# NOINLINE rtNestedP #-}


decRepH :: BS.ByteString -> Either H.DecodeError HWithRepeated
decRepH = fastDecodeRepeated
{-# NOINLINE decRepH #-}


decRepP :: BS.ByteString -> Either String PL.WithRepeated
decRepP = PLC.decodeMessage
{-# NOINLINE decRepP #-}


-- wireform test values

smallHS :: HSmall
smallHS = HSmall 42 "hello world" True


mediumHS :: HMedium
mediumHS = HMedium "benchmark title" 100 3.14159 "payload\x00\x01\x02" True 1708000000 "a medium description" 0.75


nestedHS :: HWithNested
nestedHS = HWithNested 99 (Just (HSmall 1 "inner" True)) "outer label"


repeatedHS :: HWithRepeated
repeatedHS =
  HWithRepeated
    (VU.fromList [1 .. 50])
    (V.fromList (fmap (\i -> "tag_" <> T.pack (show i)) [1 .. 20 :: Int]))
    (V.fromList [HSmall (fromIntegral i) ("item" <> T.pack (show i)) (even i) | i <- [1 .. 10 :: Int]])


-- proto-lens test values (using the real generated field lenses)

smallPL :: PL.Small
smallPL =
  (defMessage :: PL.Small)
    & F.id .~ (42 :: Int64)
    & F.name .~ ("hello world" :: Text)
    & F.active .~ True


mediumPL :: PL.Medium
mediumPL =
  (defMessage :: PL.Medium)
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
  let inner =
        (defMessage :: PL.Small)
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
      mkItem i =
        (defMessage :: PL.Small)
          & F.id .~ (fromIntegral i :: Int64)
          & F.name .~ (("item" <> T.pack (show i)) :: Text)
          & F.active .~ (even i :: Bool)
  in (defMessage :: PL.WithRepeated)
      & F.vec'values .~ VU.fromList ([1 .. 50] :: [Int32])
      & F.vec'tags .~ V.fromList (fmap (\i -> ("tag_" <> T.pack (show i)) :: Text) [1 .. 20 :: Int])
      & F.vec'items .~ V.fromList (fmap mkItem [1 .. 10])


-- Pre-encoded bytes
smallBytes, mediumBytes, nestedBytes, repeatedBytes :: BS.ByteString
smallBytes = encSmallH smallHS
mediumBytes = encMediumH mediumHS
nestedBytes = encNestedH nestedHS
repeatedBytes = encRepH repeatedHS
