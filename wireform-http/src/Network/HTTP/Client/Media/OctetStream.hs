{- | The @application\/octet-stream@ content type.

For passing raw bytes through 'Network.HTTP.Client.Send.send' without
any decoding step. Mostly useful for binary blobs and as a fallback
decoder when the server returns an unexpected @Content-Type@.
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.Client.Media.OctetStream
  ( OctetStream
  ) where

import Data.ByteString (ByteString)

import Network.HTTP.Client.Media

data OctetStream

instance HasMediaType OctetStream where
  mediaType = "application/octet-stream"

instance Encode OctetStream ByteString where
  encode = id

instance Decode OctetStream ByteString where
  decode = Right
