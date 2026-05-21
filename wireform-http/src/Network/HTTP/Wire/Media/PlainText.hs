{- | The @text\/plain; charset=utf-8@ content type. -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Network.HTTP.Wire.Media.PlainText
  ( PlainText
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE

import Network.HTTP.Wire.Media

data PlainText

instance HasMediaType PlainText where
  mediaType = "text/plain; charset=utf-8"

instance Encode PlainText Text where
  encode = TE.encodeUtf8

instance Encode PlainText ByteString where
  encode = id

instance Decode PlainText Text where
  decode bs = case TE.decodeUtf8' bs of
    Right t  -> Right t
    Left err -> Left DecodeError
      { decodeMediaType = mediaType @PlainText
      , decodeMessage   = "UTF-8 decode failure: " <> show err
      }

instance Decode PlainText ByteString where
  decode = Right

_use :: T.Text
_use = T.empty
