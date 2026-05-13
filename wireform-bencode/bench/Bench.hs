{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Microbench for wireform-bencode encode + decode hot paths.
Test fixture is a torrent-flavoured record (announce + length +
piece length + a list of files).
-}
module Main (main) where

import Bencode.Class
import Control.DeepSeq (NFData)
import Criterion.Main
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.Generics (Generic)


data TorrentFile = TorrentFile
  { tfPath :: !Text
  , tfLength :: !Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToBencode, FromBencode, NFData)


data TorrentInfo = TorrentInfo
  { tiAnnounce :: !Text
  , tiCreatedBy :: !Text
  , tiPieceLen :: !Int
  , tiFiles :: !(Vector TorrentFile)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToBencode, FromBencode, NFData)


small :: TorrentInfo
small =
  TorrentInfo
    "http://tracker.example.com/announce"
    "wireform"
    16384
    (V.singleton (TorrentFile "data/file.bin" 1024))


medium :: TorrentInfo
medium =
  TorrentInfo
    "http://tracker.example.com/announce"
    "wireform"
    16384
    ( V.fromList
        [ TorrentFile (T.pack ("data/chunk-" <> show i <> ".bin")) (1024 * i)
        | i <- [1 .. 100 :: Int]
        ]
    )


main :: IO ()
main =
  defaultMain
    [ bgroup
        "encode"
        [ bench "single-file metainfo" $ nf encodeBencode small
        , bench "100-file metainfo" $ nf encodeBencode medium
        ]
    , bgroup
        "decode"
        [ env (pure (encodeBencode small)) $ \bs ->
            bench "single-file metainfo" $ nf (decodeBencode :: ByteString -> Either String TorrentInfo) bs
        , env (pure (encodeBencode medium)) $ \bs ->
            bench "100-file metainfo" $ nf (decodeBencode :: ByteString -> Either String TorrentInfo) bs
        ]
    ]
