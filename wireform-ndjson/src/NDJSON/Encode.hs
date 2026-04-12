-- | NDJSON (Newline-Delimited JSON) encoder.
module NDJSON.Encode
  ( encode
  , encodeRecords
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Data.Vector (Vector)
import qualified Data.Vector as V

import qualified Data.Aeson as Aeson

encode :: Vector Aeson.Value -> ByteString
encode vals = BL.toStrict $ B.toLazyByteString $ buildNDJSON vals

encodeRecords :: Aeson.ToJSON a => Vector a -> ByteString
encodeRecords = encode . V.map Aeson.toJSON

buildNDJSON :: Vector Aeson.Value -> B.Builder
buildNDJSON vals = V.ifoldl' (\acc i val ->
  acc <> B.lazyByteString (Aeson.encode val) <>
    (if i < V.length vals - 1 then B.word8 0x0A else mempty)
  ) mempty vals
