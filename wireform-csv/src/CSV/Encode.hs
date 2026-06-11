{-# LANGUAGE BangPatterns #-}

-- | CSV/TSV encoder.
module CSV.Encode (
  encode,
  encodeRecords,
) where

import CSV.Class (ToCSV (..))
import CSV.Value
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Word (Word8)
import Wireform.Builder qualified as B


encode :: CSVConfig -> CSVDocument -> ByteString
encode cfg doc =
  BL.toStrict $ B.toLazyByteString $ buildDocument cfg doc


encodeRecords :: ToCSV a => CSVConfig -> Vector a -> ByteString
encodeRecords cfg rows =
  BL.toStrict $ B.toLazyByteString $ buildRows cfg (V.map toCSVRow rows)


buildDocument :: CSVConfig -> CSVDocument -> B.Builder
buildDocument cfg doc =
  let !newline = B.byteString (TE.encodeUtf8 (csvNewline cfg))
      headerPart = case csvHeader doc of
        Nothing -> mempty
        Just hdr -> buildRow cfg hdr <> newline
      rowsPart =
        V.ifoldl'
          ( \acc i row ->
              acc
                <> buildRow cfg row
                <> (if i < V.length (csvRows doc) - 1 then newline else mempty)
          )
          mempty
          (csvRows doc)
  in headerPart <> rowsPart


buildRows :: CSVConfig -> Vector (Vector Text) -> B.Builder
buildRows cfg rows =
  V.ifoldl'
    ( \acc i row ->
        acc
          <> buildRow cfg row
          <> ( if i < V.length rows - 1
                 then B.byteString (TE.encodeUtf8 (csvNewline cfg))
                 else mempty
             )
    )
    mempty
    rows


buildRow :: CSVConfig -> Vector Text -> B.Builder
buildRow cfg fields =
  V.ifoldl'
    ( \acc i field ->
        acc
          <> encodeField cfg field
          <> ( if i < V.length fields - 1
                 then B.word8 (fromIntegral (ord (csvDelimiter cfg)))
                 else mempty
             )
    )
    mempty
    fields


encodeField :: CSVConfig -> Text -> B.Builder
encodeField cfg field
  | needsQuoting cfg field =
      let !qByte = fromIntegral (ord (csvQuote cfg)) :: Word8
          !escaped = escapeField cfg field
      in B.word8 qByte <> B.byteString (TE.encodeUtf8 escaped) <> B.word8 qByte
  | otherwise = B.byteString (TE.encodeUtf8 field)


needsQuoting :: CSVConfig -> Text -> Bool
needsQuoting cfg field
  | T.null field = True
  | otherwise = T.any (\c -> c == csvDelimiter cfg || c == csvQuote cfg || c == '\n' || c == '\r') field


escapeField :: CSVConfig -> Text -> Text
escapeField cfg =
  T.concatMap
    ( \c ->
        if c == csvQuote cfg
          then T.pack [csvEscape cfg, csvQuote cfg]
          else T.singleton c
    )
