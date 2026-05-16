-- | NDJSON (Newline-Delimited JSON) encoder.
module NDJSON.Encode (
  encode,
  encodeRecords,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Vector (Vector)
import Data.Vector qualified as V
import Wireform.Builder qualified as B


encode :: Vector Aeson.Value -> ByteString
encode vals = B.toStrictByteString $ buildNDJSON vals


encodeRecords :: Aeson.ToJSON a => Vector a -> ByteString
encodeRecords = encode . V.map Aeson.toJSON


buildNDJSON :: Vector Aeson.Value -> B.Builder
buildNDJSON vals =
  V.ifoldl'
    ( \acc i val ->
        acc
          <> foldMap B.byteString (BL.toChunks (Aeson.encode val))
          <> (if i < V.length vals - 1 then B.word8 0x0A else mempty)
    )
    mempty
    vals
