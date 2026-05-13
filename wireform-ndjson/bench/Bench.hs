{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-ndjson encode + decode hot paths.
Compares against the obvious baseline: aeson + manual newline
splitting. wireform-ndjson's value-add is the SIMD newline scanner;
this bench measures the gap.
-}
module Main (main) where

import Control.DeepSeq (NFData)
import Criterion.Main
import Data.Aeson qualified as A
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)
import NDJSON.Decode qualified as NJD
import NDJSON.Encode qualified as NJE


data LogEntry = LogEntry
  { ts :: !Text
  , level :: !Text
  , message :: !Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON, NFData)


mkRow :: Int -> LogEntry
mkRow i =
  LogEntry
    (T.pack ("2026-05-13T10:" <> show (i `mod` 60)))
    (case i `mod` 3 of 0 -> "INFO"; 1 -> "WARN"; _ -> "ERROR")
    (T.pack ("event #" <> show i))


small :: Vector LogEntry
small = V.fromList (map mkRow [1 .. 10])


medium :: Vector LogEntry
medium = V.fromList (map mkRow [1 .. 1000])


-- Aeson baseline: manual line splitting + per-line decode.
aesonDecode :: A.FromJSON a => ByteString -> [Either String a]
aesonDecode bs =
  [ A.eitherDecodeStrict line
  | line <- BS8.split '\n' bs
  , not (BS.null line)
  ]


aesonEncode :: A.ToJSON a => Vector a -> ByteString
aesonEncode = BS.intercalate "\n" . map (BSL.toStrict . A.encode) . V.toList


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bgroup
            "10 rows"
            [ bench "wireform-ndjson" $ nf NJE.encodeRecords small
            , bench "aeson + lines" $ nf aesonEncode small
            ]
        , bgroup
            "1000 rows"
            [ bench "wireform-ndjson" $ nf NJE.encodeRecords medium
            , bench "aeson + lines" $ nf aesonEncode medium
            ]
        ]
    , bgroup
        "decode"
        [ env (pure (NJE.encodeRecords small)) $ \bs ->
            bgroup
              "10 rows"
              [ bench "wireform-ndjson" $ nf (NJD.decodeRecords :: ByteString -> Either String (Vector LogEntry)) bs
              , bench "aeson + lines" $ nf (aesonDecode :: ByteString -> [Either String LogEntry]) bs
              ]
        , env (pure (NJE.encodeRecords medium)) $ \bs ->
            bgroup
              "1000 rows"
              [ bench "wireform-ndjson" $ nf (NJD.decodeRecords :: ByteString -> Either String (Vector LogEntry)) bs
              , bench "aeson + lines" $ nf (aesonDecode :: ByteString -> [Either String LogEntry]) bs
              ]
        ]
    ]
