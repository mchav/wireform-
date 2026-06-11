{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Apache Puffin file format, the storage container used for Iceberg
statistics blobs (e.g. theta sketches, NDV) and v3 deletion vectors.

Specification: <https://iceberg.apache.org/puffin-spec/>.

The file is laid out as:

@
Header     : 4 bytes magic \"PFA1\"
Blobs      : concatenated blob bytes
Footer     : footer payload + 4 bytes magic \"PFA1\"
@

The footer is a JSON record describing each blob's offset, length, type,
snapshot id, sequence number, fields, and properties. This module
implements the binary header\/footer framing and the JSON footer encoder
and decoder.
-}
module Iceberg.Puffin (
  PuffinBlob (..),
  PuffinFooter (..),
  puffinMagic,
  writePuffin,
  readPuffin,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int32, Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word32)
import Wireform.Builder qualified as BB


-- | Single blob record inside a Puffin file.
data PuffinBlob = PuffinBlob
  { pbType :: !Text
  , pbFields :: !(Vector Int)
  , pbSnapshotId :: !Int64
  , pbSequenceNumber :: !Int64
  , pbProperties :: !(Map Text Text)
  , pbCompressionCodec :: !(Maybe Text)
  , pbData :: !ByteString
  }
  deriving (Show, Eq)


-- | Computed footer for an entire Puffin file.
data PuffinFooter = PuffinFooter
  { pfBlobs :: !(Vector PuffinBlob)
  , pfProperties :: !(Map Text Text)
  }
  deriving (Show, Eq)


-- | "PFA1": Puffin File Apache, version 1.
puffinMagic :: ByteString
puffinMagic = BS.pack [0x50, 0x46, 0x41, 0x31]


-- ============================================================
-- Writer
-- ============================================================

{- | Serialise a Puffin file. Each blob is written verbatim (compression
support is left to the caller — set 'pbCompressionCodec' to record
that the blob bytes were already compressed).
-}
writePuffin :: PuffinFooter -> ByteString
writePuffin pf =
  let
    -- assign offsets to each blob
    assigned = assignOffsets (BS.length puffinMagic) (V.toList (pfBlobs pf))
    builder =
      BB.byteString puffinMagic
        <> mconcat [BB.byteString (pbData b) | (b, _) <- assigned]
        <> footer
    footer = footerPayloadBuilder pf assigned
  in
    BL.toStrict (BB.toLazyByteString builder)


assignOffsets :: Int -> [PuffinBlob] -> [(PuffinBlob, (Int, Int))]
assignOffsets = go
  where
    go !_ [] = []
    go !off (b : bs) =
      let len = BS.length (pbData b)
      in (b, (off, len)) : go (off + len) bs


footerPayloadBuilder :: PuffinFooter -> [(PuffinBlob, (Int, Int))] -> BB.Builder
footerPayloadBuilder pf assigned =
  let json = footerJSON pf assigned
      payload = BL.toStrict (Aeson.encode json)
      payloadLen = BS.length payload
      flags = (0 :: Word32) -- bit 0 = footer compression; we leave it disabled
      bs =
        BB.byteString payload
          <> BB.word32LE flags
          <> BB.word32LE (fromIntegral payloadLen)
          <> BB.byteString puffinMagic
  in bs


footerJSON :: PuffinFooter -> [(PuffinBlob, (Int, Int))] -> Aeson.Value
footerJSON pf assigned =
  Aeson.Object $
    KM.fromList
      [ ("blobs", Aeson.Array (V.fromList [blobJSON b off len | (b, (off, len)) <- assigned]))
      ,
        ( "properties"
        , Aeson.Object $
            KM.fromList
              [(Key.fromText k, Aeson.String v) | (k, v) <- Map.toList (pfProperties pf)]
        )
      ]


blobJSON :: PuffinBlob -> Int -> Int -> Aeson.Value
blobJSON b off len =
  Aeson.Object $
    KM.fromList $
      [ ("type", Aeson.String (pbType b))
      , ("fields", Aeson.Array (V.map (Aeson.Number . fromIntegral) (pbFields b)))
      , ("snapshot-id", Aeson.Number (fromIntegral (pbSnapshotId b)))
      , ("sequence-number", Aeson.Number (fromIntegral (pbSequenceNumber b)))
      , ("offset", Aeson.Number (fromIntegral off))
      , ("length", Aeson.Number (fromIntegral len))
      ]
        ++ ( if Map.null (pbProperties b)
               then []
               else
                 [
                   ( "properties"
                   , Aeson.Object $
                       KM.fromList
                         [(Key.fromText k, Aeson.String v) | (k, v) <- Map.toList (pbProperties b)]
                   )
                 ]
           )
        ++ maybe [] (\c -> [("compression-codec", Aeson.String c)]) (pbCompressionCodec b)


-- ============================================================
-- Reader
-- ============================================================

{- | Parse a Puffin file. Returns the footer and reconstructs each blob's
payload bytes. The footer JSON is fully decoded; unknown blob properties
pass through untouched.
-}
readPuffin :: ByteString -> Either String PuffinFooter
readPuffin bs = do
  checkMagic
  let total = BS.length bs
      -- Tail layout: ... | flags(4) | payload-length(4) | magic(4)
      lengthBytes = BS.take 4 (BS.drop (total - 8) bs)
      footerLen = fromIntegral (readWord32LE lengthBytes) :: Int
      footerStart = total - 12 - footerLen
      footerBytes = BS.take footerLen (BS.drop footerStart bs)
  json <- case Aeson.eitherDecodeStrict footerBytes of
    Right v -> Right v
    Left e -> Left $ "Puffin footer is not valid JSON: " ++ e
  parseFooter bs json
  where
    checkMagic
      | BS.length bs < 12 = Left "Puffin file too small"
      | BS.take 4 bs /= puffinMagic = Left "missing leading PFA1 magic"
      | BS.take 4 (BS.drop (BS.length bs - 4) bs) /= puffinMagic = Left "missing trailing PFA1 magic"
      | otherwise = Right ()


readWord32LE :: ByteString -> Word32
readWord32LE w =
  let b0 = fromIntegral (BS.index w 0) :: Word32
      b1 = fromIntegral (BS.index w 1) :: Word32
      b2 = fromIntegral (BS.index w 2) :: Word32
      b3 = fromIntegral (BS.index w 3) :: Word32
  in b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16) .|. (b3 `shiftL` 24)


parseFooter :: ByteString -> Aeson.Value -> Either String PuffinFooter
parseFooter bs (Aeson.Object obj) = do
  blobs <- case KM.lookup "blobs" obj of
    Just (Aeson.Array arr) -> V.mapM (parseBlob bs) arr
    _ -> Left "Puffin footer missing 'blobs' array"
  props <- case KM.lookup "properties" obj of
    Just (Aeson.Object m) ->
      Map.fromList <$> mapM extractStringPair (KM.toList m)
    Just Aeson.Null -> Right Map.empty
    Nothing -> Right Map.empty
    _ -> Left "Puffin footer 'properties' must be an object"
  Right PuffinFooter {pfBlobs = blobs, pfProperties = props}
parseFooter _ _ = Left "Puffin footer must be a JSON object"


parseBlob :: ByteString -> Aeson.Value -> Either String PuffinBlob
parseBlob fileBs (Aeson.Object obj) = do
  ty <- requireString "type" obj
  fs <- case KM.lookup "fields" obj of
    Just (Aeson.Array arr) -> V.mapM intFromJSON arr
    _ -> Left "blob.fields must be an array"
  sid <- requireInt64 "snapshot-id" obj
  seqN <- requireInt64 "sequence-number" obj
  off <- requireInt "offset" obj
  len <- requireInt "length" obj
  let blobBytes = BS.take len (BS.drop off fileBs)
  props <- case KM.lookup "properties" obj of
    Just (Aeson.Object m) -> Map.fromList <$> mapM extractStringPair (KM.toList m)
    Just Aeson.Null -> Right Map.empty
    Nothing -> Right Map.empty
    _ -> Left "blob.properties must be an object"
  let codec = case KM.lookup "compression-codec" obj of
        Just (Aeson.String c) -> Just c
        _ -> Nothing
  Right
    PuffinBlob
      { pbType = ty
      , pbFields = fs
      , pbSnapshotId = sid
      , pbSequenceNumber = seqN
      , pbProperties = props
      , pbCompressionCodec = codec
      , pbData = blobBytes
      }
parseBlob _ _ = Left "Puffin blob entry must be a JSON object"


requireString :: Text -> KM.KeyMap Aeson.Value -> Either String Text
requireString k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.String s) -> Right s
  _ -> Left $ "missing string field: " ++ T.unpack k


requireInt :: Text -> KM.KeyMap Aeson.Value -> Either String Int
requireInt k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n :: Maybe Int of
    Just i -> Right i
    Nothing -> Left $ "field out of Int range: " ++ T.unpack k
  _ -> Left $ "missing numeric field: " ++ T.unpack k


requireInt64 :: Text -> KM.KeyMap Aeson.Value -> Either String Int64
requireInt64 k obj = case KM.lookup (Key.fromText k) obj of
  Just (Aeson.Number n) -> case toBoundedInteger n of
    Just i -> Right i
    Nothing -> Left $ "field out of Int64 range: " ++ T.unpack k
  _ -> Left $ "missing numeric field: " ++ T.unpack k


intFromJSON :: Aeson.Value -> Either String Int
intFromJSON (Aeson.Number n) = case toBoundedInteger n :: Maybe Int of
  Just i -> Right i
  Nothing -> Left "field id out of Int range"
intFromJSON _ = Left "field id must be numeric"


extractStringPair :: (Key.Key, Aeson.Value) -> Either String (Text, Text)
extractStringPair (k, Aeson.String v) = Right (Key.toText k, v)
extractStringPair (k, _) = Left $ "non-string property: " ++ T.unpack (Key.toText k)


-- These are not used at the moment but kept here so that future versions
-- which need them don't have to re-derive numeric helpers.
_unusedShifts :: (Word32, Int32)
_unusedShifts = (0 .&. 0, 0 `shiftR` 0)


_unusedTE :: Text -> ByteString
_unusedTE = TE.encodeUtf8
