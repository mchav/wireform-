{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | The @application\/x-www-form-urlencoded@ content type.

A small URL-encoding implementation lives here so we don\'t drag in
@http-api-data@ for this. Values are taken from a @[(Text, Text)]@
association list; users who want a typeclass should layer one on
top.
-}
module Network.HTTP.Client.Media.FormUrlEncoded (
  FormUrlEncoded,
  Form (..),
  encodeForm,
  decodeForm,
) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word8)
import Network.HTTP.Client.Media
import Wireform.Builder qualified as WB


data FormUrlEncoded


{- | A submission form: an ordered list of key\/value 'Text' pairs.
Keys may repeat.
-}
newtype Form = Form {fromForm :: [(Text, Text)]}
  deriving stock (Eq, Show)


instance HasMediaType FormUrlEncoded where
  mediaType = "application/x-www-form-urlencoded"


instance Encode FormUrlEncoded Form where
  encode = encodeForm


instance Encode FormUrlEncoded [(Text, Text)] where
  encode = encodeForm . Form


instance Decode FormUrlEncoded Form where
  decode bs = case decodeForm bs of
    Right f -> Right f
    Left err ->
      Left
        DecodeError
          { decodeMediaType = mediaType @FormUrlEncoded
          , decodeMessage = err
          }


encodeForm :: Form -> ByteString
encodeForm (Form pairs) =
  let chunks =
        [ urlEncode (TE.encodeUtf8 k) <> "=" <> urlEncode (TE.encodeUtf8 v)
        | (k, v) <- pairs
        ]
  in BS.intercalate "&" chunks


decodeForm :: ByteString -> Either String Form
decodeForm bs = traverse splitPair (BS.split 0x26 bs) >>= pure . Form
  where
    splitPair p =
      let (k, v0) = BS.break (== 0x3D) p
      in case BS.uncons v0 of
           Just (0x3D, v) -> do
             k' <- urlDecode k
             v' <- urlDecode v
             pure (TE.decodeUtf8 k', TE.decodeUtf8 v')
           _ -> do
             k' <- urlDecode k
             pure (TE.decodeUtf8 k', T.empty)


urlEncode :: ByteString -> ByteString
urlEncode = WB.toStrictByteString . BS.foldr step mempty
  where
    step :: Word8 -> WB.Builder -> WB.Builder
    step b acc
      | unreserved b = WB.word8 b <> acc
      | b == 0x20 = WB.word8 0x2B <> acc -- space -> '+'
      | otherwise = WB.byteString (percent b) <> acc
    unreserved b =
      (b >= 0x41 && b <= 0x5A) -- A-Z
        || (b >= 0x61 && b <= 0x7A) -- a-z
        || (b >= 0x30 && b <= 0x39) -- 0-9
        || b == 0x2D -- '-'
        || b == 0x2E -- '.'
        || b == 0x5F -- '_'
        || b == 0x7E -- '~'
    percent b =
      let hex = "0123456789ABCDEF"
          hi = b `div` 16
          lo = b `mod` 16
      in BS.pack [0x25, indexHex hex hi, indexHex hex lo]
    indexHex :: ByteString -> Word8 -> Word8
    indexHex s i = BS.index s (fromIntegral i)


urlDecode :: ByteString -> Either String ByteString
urlDecode = go mempty
  where
    go acc bs = case BS.uncons bs of
      Nothing -> Right (WB.toStrictByteString acc)
      Just (0x25, rest) -- '%'
        | BS.length rest >= 2 -> case (BS.index rest 0, BS.index rest 1) of
            (a, b) -> case (hexDigit a, hexDigit b) of
              (Just hi, Just lo) ->
                go
                  (acc <> WB.word8 (fromIntegral (hi * 16 + lo)))
                  (BS.drop 2 rest)
              _ -> Left ("bad percent-escape near " <> show [chr (fromIntegral a), chr (fromIntegral b)])
        | otherwise -> Left "truncated percent-escape"
      Just (0x2B, rest) -> -- '+'
        go (acc <> WB.word8 0x20) rest
      Just (c, rest) -> go (acc <> WB.word8 c) rest
    hexDigit b
      | b >= 0x30 && b <= 0x39 = Just (fromIntegral (b - 0x30) :: Int)
      | b >= 0x41 && b <= 0x46 = Just (fromIntegral (b - 0x41 + 10) :: Int)
      | b >= 0x61 && b <= 0x66 = Just (fromIntegral (b - 0x61 + 10) :: Int)
      | otherwise = Nothing
