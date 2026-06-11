{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | The @text\/plain; charset=utf-8@ content type.
module Network.HTTP.Client.Media.PlainText (
  PlainText,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Network.HTTP.Client.Media


data PlainText


instance HasMediaType PlainText where
  mediaType = "text/plain; charset=utf-8"


instance Encode PlainText Text where
  encode = TE.encodeUtf8


instance Encode PlainText ByteString where
  encode = id


instance Decode PlainText Text where
  decode bs = case TE.decodeUtf8' bs of
    Right t -> Right t
    Left err ->
      Left
        DecodeError
          { decodeMediaType = mediaType @PlainText
          , decodeMessage = "UTF-8 decode failure: " <> show err
          }


instance Decode PlainText ByteString where
  decode = Right
