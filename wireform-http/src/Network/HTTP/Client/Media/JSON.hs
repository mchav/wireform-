{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{- | The shipped @application\/json@ content-type tag.

The 'JSON' phantom type carries no values; it's a hook for
'HasMediaType', 'Encode', and 'Decode' instances. The instances
delegate to @aeson@'s 'A.ToJSON' \/ 'A.FromJSON' classes.

@
-- Encode a value as JSON:
withBody \@JSON user req

-- Decode the response as JSON:
send transport req (as \@JSON \@User)
@
-}
module Network.HTTP.Client.Media.JSON (
  JSON,
) where

import Data.Aeson qualified as A
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Network.HTTP.Client.Media


{- | Phantom tag for @application\/json@. No values — pass it via
'Data.Proxy.Proxy' or @TypeApplications@.
-}
data JSON


instance HasMediaType JSON where
  mediaType = "application/json; charset=utf-8"


instance A.ToJSON a => Encode JSON a where
  encode = BSL.toStrict . A.encode


instance A.FromJSON a => Decode JSON a where
  decode bs = case A.eitherDecodeStrict bs of
    Right a -> Right a
    Left err ->
      Left
        DecodeError
          { decodeMediaType = mediaType @JSON
          , decodeMessage =
              "JSON decode failure: "
                <> err
                <> "; body (truncated): "
                <> show (BS.take 256 bs)
          }
