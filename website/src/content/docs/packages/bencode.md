---
title: wireform-bencode
description: "BitTorrent Bencode encoding and decoding with TH deriving, sorted dictionary keys, and wireform-derive annotations."
sidebar:
  order: 15
---

`wireform-bencode` implements Bencode, the encoding used by BitTorrent for
`.torrent` files, DHT messages, and peer wire protocols. Bencode supports
byte strings, integers, lists, and dictionaries with a deliberately small
grammar. Use this package when you parse or produce BitTorrent metadata,
validate info hashes, or implement peer-facing tooling that must match the
on-wire Bencode layout exactly.

## Key features

- **Template Haskell deriving** via `deriveBencode` from `Bencode.Derive`, with
  `wireform-derive` annotations; Generic defaults (empty instances) work for
  simple uncustomized records
- **Sorted dictionary keys** enforced on encode and validated on decode, as
  required by BEP-3 for stable info hashes
- **Simple wire grammar** of strings, integers, lists, and dictionaries
- **Dynamic values** via the untyped `Value` ADT for `.torrent` inspection
- **Direct encoding** for buffer-oriented writes

## Basic usage

Model a metadata record and derive Bencode codecs. Record fields encode as
dictionary keys (field names as byte strings):

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module TorrentInfo where

import Bencode.Class (ToBencode, FromBencode, encodeBencode, decodeBencode)
import Bencode.Derive (deriveBencode)
import GHC.Generics (Generic)
import Data.Text (Text)

data FileInfo = FileInfo
  { fileLength :: !Int
  , filePath   :: !Text
  }
  deriving stock (Show, Eq, Generic)

data Info = Info
  { infoName     :: !Text
  , infoPieceLen :: !Int
  , infoFiles    :: ![FileInfo]
  }
  deriving stock (Show, Eq, Generic)

$(deriveBencode ''FileInfo)
$(deriveBencode ''Info)

encodeInfo :: Info -> ByteString
encodeInfo info = encodeBencode info

decodeInfo :: ByteString -> Either String Info
decodeInfo bs = decodeBencode bs
```

For simple records with no custom wire naming, Generic defaults also work:
declare empty `instance ToBencode FileInfo` / `FromBencode FileInfo` (and the
same for `Info`) after `deriving stock (Show, Eq, Generic)`. Field names go to
the wire verbatim and annotations are not supported.

The encoder sorts dictionary keys by raw byte order before writing. You can
pass key/value pairs in any order; the wire output is always canonical for
hashing:

```haskell
import Bencode.Encoding (dictFromList, int, encodingToByteString)
import Data.ByteString.Char8 qualified as BS8

canonicalDict :: ByteString
canonicalDict =
  encodingToByteString
    ( dictFromList
        [ (BS8.pack "zebra", int 1)
        , (BS8.pack "alpha", int 2)
        ]
    )
```

For ad hoc `.torrent` parsing, use the dynamic ADT and walk the structure:

```haskell
import Bencode.Value qualified as B
import Bencode.Decode (decode)
import Data.ByteString.Char8 qualified as BS8
import Data.Vector qualified as V

infoLength :: B.Value -> Maybe Integer
infoLength val =
  case val of
    B.BDict pairs ->
      case V.find ((== BS8.pack "length") . fst) (V.toList pairs) of
        Just (_, B.BInteger n) -> Just n
        _                      -> Nothing
    _ -> Nothing
```

## Notable modules

| Module | Purpose |
|--------|---------|
| `Bencode.Class` | `ToBencode` / `FromBencode`, `encodeBencode`, `decodeBencode` |
| `Bencode.Encode` / `Bencode.Decode` | Low-level encode and decode with sorted-key enforcement |
| `Bencode.Encoding` | Composable encoding builder for dictionaries and lists |
| `Bencode.Value` | Dynamic untyped `Value` ADT |
| `Bencode.Derive` | Template Haskell deriver with `wireform-derive` annotations |
