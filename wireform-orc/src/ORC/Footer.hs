{-# LANGUAGE BangPatterns #-}
-- | Read/write Apache ORC file footer.
--
-- ORC file layout ends with:
--   [protobuf Footer] [protobuf PostScript] [1-byte postscript length]
--
-- The PostScript contains: footerLength, compression, compressionBlockSize,
-- version, and the magic string "ORC".
--
-- We encode the footer and postscript as protobuf messages using our
-- existing Proto.Wire.Encode / Proto.Wire.Decode infrastructure.
module ORC.Footer
  ( readORCFooter
  , writeORCFooter
  , readORCCompression
  , orcMagic
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)

import ORC.Types

orcMagic :: ByteString
orcMagic = "ORC"

readORCFooter :: ByteString -> Either String ORCFooter
readORCFooter bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          let !magic = psMagic ps
          if magic /= orcMagic
            then Left $ "ORC.Footer: invalid magic (expected ORC, got " ++ show magic ++ ")"
            else do
              let !footerLen = fromIntegral (psFooterLength ps) :: Int
                  !footerStart = psStart - footerLen
              if footerLen <= 0 || footerStart < 0
                then Left "ORC.Footer: invalid footer length"
                else do
                  let !footerBytes = BSU.unsafeTake footerLen (BSU.unsafeDrop footerStart bs)
                  decodeFooter footerBytes

-- | Extract the compression kind from the PostScript.
readORCCompression :: ByteString -> Either String CompressionKind
readORCCompression bs
  | BS.length bs < 4 = Left "ORC.Footer: input too short for compression read"
  | otherwise = do
      let !totalLen = BS.length bs
          !psLen = fromIntegral (BSU.unsafeIndex bs (totalLen - 1)) :: Int
      if psLen <= 0 || psLen >= totalLen - 1
        then Left "ORC.Footer: invalid postscript length"
        else do
          let !psStart = totalLen - 1 - psLen
              !psBytes = BSU.unsafeTake psLen (BSU.unsafeDrop psStart bs)
          ps <- decodePostScript psBytes
          case compressionFromInt (psCompression ps) of
            Just ck -> Right ck
            Nothing -> Left $ "ORC.Footer: unknown compression kind " ++ show (psCompression ps)

writeORCFooter :: ORCFooter -> ByteString
writeORCFooter footer =
  let !footerBytes = encodeFooter footer
      !footerLen = BS.length footerBytes
      !psBytes = encodePostScript PostScript
        { psFooterLength = fromIntegral footerLen
        , psCompression = 0
        , psCompressionBlockSize = 262144
        , psVersion = V.fromList [0, 12]
        , psMagic = orcMagic
        }
      !psLen = BS.length psBytes
  in BL.toStrict $ B.toLazyByteString $
       B.byteString footerBytes
       <> B.byteString psBytes
       <> B.word8 (fromIntegral psLen)

-- Internal PostScript type
data PostScript = PostScript
  { psFooterLength         :: !Word64
  , psCompression          :: !Word64
  , psCompressionBlockSize :: !Word64
  , psVersion              :: !(Vector Word32)
  , psMagic                :: !ByteString
  }

------------------------------------------------------------------------
-- Protobuf encoding (manual, matching ORC .proto field numbers)
------------------------------------------------------------------------

encodeFooter :: ORCFooter -> ByteString
encodeFooter f = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField 1 (orcHeaderLength f)
  , encodeVarintField 2 (orcContentLength f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim 3 (encodeStripeInfo s)) mempty (orcStripes f)
  , V.foldl' (\acc t -> acc <> encodeLengthDelim 4 (encodeORCType t)) mempty (orcTypes f)
  , V.foldl' (\acc (k, v) -> acc <> encodeLengthDelim 5 (encodeMetadataEntry k v)) mempty (orcMetadata f)
  , encodeVarintField 6 (orcNumberOfRows f)
  , V.foldl' (\acc s -> acc <> encodeLengthDelim 7 (encodeColStats s)) mempty (orcStatistics f)
  ]

encodeStripeInfo :: StripeInformation -> B.Builder
encodeStripeInfo si = mconcat
  [ encodeVarintField 1 (siOffset si)
  , encodeVarintField 2 (siIndexLength si)
  , encodeVarintField 3 (siDataLength si)
  , encodeVarintField 4 (siFooterLength si)
  , encodeVarintField 5 (siNumberOfRows si)
  ]

encodeORCType :: ORCType -> B.Builder
encodeORCType ot = mconcat
  [ encodeVarintField 1 (fromIntegral (typeKindToInt (otKind ot)) :: Word64)
  , V.foldl' (\acc st -> acc <> encodeVarintField 2 (fromIntegral st :: Word64)) mempty (otSubtypes ot)
  , V.foldl' (\acc fn -> acc <> encodeLengthDelim 3 (B.byteString (TE.encodeUtf8 fn))) mempty (otFieldNames ot)
  ]

encodeMetadataEntry :: T.Text -> ByteString -> B.Builder
encodeMetadataEntry k v = mconcat
  [ encodeLengthDelim 1 (B.byteString (TE.encodeUtf8 k))
  , encodeLengthDelim 2 (B.byteString v)
  ]

encodeColStats :: ColumnStatistics -> B.Builder
encodeColStats cs = mconcat
  [ maybe mempty (encodeVarintField 1) (csNumberOfValues cs)
  , maybe mempty (\b -> encodeVarintField 2 (if b then 1 else 0 :: Word64)) (csHasNull cs)
  , maybe mempty (encodeVarintField 3) (csBytesOnDisk cs)
  ]

encodePostScript :: PostScript -> ByteString
encodePostScript ps = BL.toStrict $ B.toLazyByteString $ mconcat
  [ encodeVarintField 1 (psFooterLength ps)
  , encodeVarintField 2 (psCompression ps)
  , encodeVarintField 3 (psCompressionBlockSize ps)
  , V.foldl' (\acc v -> acc <> encodeVarintField 4 (fromIntegral v :: Word64)) mempty (psVersion ps)
  , encodeLengthDelim 5 (B.byteString (psMagic ps))
  ]

------------------------------------------------------------------------
-- Protobuf decoding (manual varint + length-delimited)
------------------------------------------------------------------------

decodePostScript :: ByteString -> Either String PostScript
decodePostScript bs = go 0 (PostScript 0 0 0 V.empty BS.empty)
  where
    !len = BS.length bs
    go !off !ps
      | off >= len = Right ps
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do (v, off'') <- getVarint bs off' len; go off'' ps { psFooterLength = v }
            (2, 0) -> do (v, off'') <- getVarint bs off' len; go off'' ps { psCompression = v }
            (3, 0) -> do (v, off'') <- getVarint bs off' len; go off'' ps { psCompressionBlockSize = v }
            (4, 0) -> do (v, off'') <- getVarint bs off' len; go off'' ps { psVersion = V.snoc (psVersion ps) (fromIntegral v) }
            (5, 2) -> do (v, off'') <- getLenDelim bs off' len; go off'' ps { psMagic = v }
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' ps

decodeFooter :: ByteString -> Either String ORCFooter
decodeFooter bs = go 0 emptyFooter
  where
    !len = BS.length bs
    emptyFooter = ORCFooter 0 0 V.empty V.empty V.empty 0 V.empty
    go !off !f
      | off >= len = Right f
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do (v, off'') <- getVarint bs off' len; go off'' f { orcHeaderLength = v }
            (2, 0) -> do (v, off'') <- getVarint bs off' len; go off'' f { orcContentLength = v }
            (3, 2) -> do (v, off'') <- getLenDelim bs off' len
                         si <- decodeStripeInfo v
                         go off'' f { orcStripes = V.snoc (orcStripes f) si }
            (4, 2) -> do (v, off'') <- getLenDelim bs off' len
                         t <- decodeORCType v
                         go off'' f { orcTypes = V.snoc (orcTypes f) t }
            (5, 2) -> do (v, off'') <- getLenDelim bs off' len
                         entry <- decodeMetadataEntry v
                         go off'' f { orcMetadata = V.snoc (orcMetadata f) entry }
            (6, 0) -> do (v, off'') <- getVarint bs off' len; go off'' f { orcNumberOfRows = v }
            (7, 2) -> do (v, off'') <- getLenDelim bs off' len
                         cs <- decodeColStats v
                         go off'' f { orcStatistics = V.snoc (orcStatistics f) cs }
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' f

decodeStripeInfo :: ByteString -> Either String StripeInformation
decodeStripeInfo bs = go 0 (StripeInformation 0 0 0 0 0)
  where
    !len = BS.length bs
    go !off !si
      | off >= len = Right si
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do (v, off'') <- getVarint bs off' len; go off'' si { siOffset = v }
            (2, 0) -> do (v, off'') <- getVarint bs off' len; go off'' si { siIndexLength = v }
            (3, 0) -> do (v, off'') <- getVarint bs off' len; go off'' si { siDataLength = v }
            (4, 0) -> do (v, off'') <- getVarint bs off' len; go off'' si { siFooterLength = v }
            (5, 0) -> do (v, off'') <- getVarint bs off' len; go off'' si { siNumberOfRows = v }
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' si

decodeORCType :: ByteString -> Either String ORCType
decodeORCType bs = go 0 (ORCType TKBoolean V.empty V.empty)
  where
    !len = BS.length bs
    go !off !ot
      | off >= len = Right ot
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do
              (v, off'') <- getVarint bs off' len
              case intToTypeKind (fromIntegral v) of
                Just tk -> go off'' ot { otKind = tk }
                Nothing -> Left $ "ORC.Footer: invalid TypeKind " ++ show v
            (2, 0) -> do
              (v, off'') <- getVarint bs off' len
              go off'' ot { otSubtypes = V.snoc (otSubtypes ot) (fromIntegral v) }
            (3, 2) -> do
              (v, off'') <- getLenDelim bs off' len
              case TE.decodeUtf8' v of
                Right t -> go off'' ot { otFieldNames = V.snoc (otFieldNames ot) t }
                Left _  -> Left "ORC.Footer: invalid UTF-8 in field name"
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' ot

decodeMetadataEntry :: ByteString -> Either String (T.Text, ByteString)
decodeMetadataEntry bs = go 0 (T.empty, BS.empty)
  where
    !len = BS.length bs
    go !off !entry
      | off >= len = Right entry
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 2) -> do
              (v, off'') <- getLenDelim bs off' len
              case TE.decodeUtf8' v of
                Right t -> go off'' (t, snd entry)
                Left _  -> Left "ORC.Footer: invalid UTF-8 in metadata key"
            (2, 2) -> do
              (v, off'') <- getLenDelim bs off' len
              go off'' (fst entry, v)
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' entry

decodeColStats :: ByteString -> Either String ColumnStatistics
decodeColStats bs = go 0 (ColumnStatistics Nothing Nothing Nothing)
  where
    !len = BS.length bs
    go !off !cs
      | off >= len = Right cs
      | otherwise = do
          (tag, off') <- getVarint bs off len
          let !fieldNum = fromIntegral (tag `shiftR` 3) :: Int
              !wireType = tag .&. 7
          case (fieldNum, wireType) of
            (1, 0) -> do (v, off'') <- getVarint bs off' len; go off'' cs { csNumberOfValues = Just v }
            (2, 0) -> do (v, off'') <- getVarint bs off' len; go off'' cs { csHasNull = Just (v /= 0) }
            (3, 0) -> do (v, off'') <- getVarint bs off' len; go off'' cs { csBytesOnDisk = Just v }
            _ -> skipField wireType bs off' len >>= \off'' -> go off'' cs

------------------------------------------------------------------------
-- Low-level protobuf primitives
------------------------------------------------------------------------

getVarint :: ByteString -> Int -> Int -> Either String (Word64, Int)
getVarint bs !off !len = go off 0 0
  where
    go !pos !val !shift
      | pos >= len = Left "ORC.Footer: unexpected end of varint"
      | shift >= 64 = Left "ORC.Footer: varint too long"
      | otherwise =
          let !b = fromIntegral (BSU.unsafeIndex bs pos) :: Word64
              !val' = val .|. ((b .&. 0x7F) `shiftL` shift)
          in if b .&. 0x80 == 0
               then Right (val', pos + 1)
               else go (pos + 1) val' (shift + 7)

getLenDelim :: ByteString -> Int -> Int -> Either String (ByteString, Int)
getLenDelim bs !off !len = do
  (dlen, off') <- getVarint bs off len
  let !dataLen = fromIntegral dlen :: Int
  if off' + dataLen > len
    then Left "ORC.Footer: length-delimited data exceeds buffer"
    else Right (BSU.unsafeTake dataLen (BSU.unsafeDrop off' bs), off' + dataLen)

skipField :: Word64 -> ByteString -> Int -> Int -> Either String Int
skipField wireType bs !off !len = case wireType of
  0 -> do (_, off') <- getVarint bs off len; Right off'
  1 -> if off + 8 <= len then Right (off + 8) else Left "ORC.Footer: truncated fixed64"
  2 -> do (_, off') <- getLenDelim bs off len; Right off'
  5 -> if off + 4 <= len then Right (off + 4) else Left "ORC.Footer: truncated fixed32"
  _ -> Left $ "ORC.Footer: unknown wire type " ++ show wireType

------------------------------------------------------------------------
-- Low-level protobuf encoding primitives
------------------------------------------------------------------------

encodeVarintField :: Int -> Word64 -> B.Builder
encodeVarintField fieldNum val =
  let !tag = fromIntegral fieldNum `shiftL` 3 :: Word64
  in putVarint tag <> putVarint val

encodeLengthDelim :: Int -> B.Builder -> B.Builder
encodeLengthDelim fieldNum content =
  let !tag = (fromIntegral fieldNum `shiftL` 3) .|. 2 :: Word64
      !encoded = BL.toStrict $ B.toLazyByteString content
      !contentLen = BS.length encoded
  in putVarint tag <> putVarint (fromIntegral contentLen) <> B.byteString encoded

putVarint :: Word64 -> B.Builder
putVarint = go
  where
    go !v
      | v < 0x80  = B.word8 (fromIntegral v)
      | otherwise = B.word8 (fromIntegral (v .&. 0x7F) .|. 0x80) <> go (v `shiftR` 7)
