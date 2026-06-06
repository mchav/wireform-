{- | Protobuf utilities

Intended for qualified import.

> import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf
-}
module Network.GRPC.Spec.Util.Protobuf (
  -- * Serialization
  parseStrict,
  parseLazy,
  buildStrict,
  buildLazy,
) where

import Data.ByteString qualified as Strict (ByteString)
import Data.ByteString.Lazy qualified as BS.Lazy
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Proto.Decode (DecodeError, MessageDecode, decodeMessage)
import Proto.Encode (MessageEncode, encodeMessage)


{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

parseStrict :: MessageDecode msg => Strict.ByteString -> Either String msg
parseStrict bs = case decodeMessage bs of
  Left err -> Left (show err)
  Right v -> Right v


{- | Parse lazy bytestring

TODO: <https://github.com/well-typed/grapesy/issues/119>.
We currently turn this into a strict bytestring before parsing.
-}
parseLazy :: MessageDecode msg => Lazy.ByteString -> Either String msg
parseLazy = parseStrict . BS.Lazy.toStrict


buildStrict :: MessageEncode msg => msg -> Strict.ByteString
buildStrict = encodeMessage


buildLazy :: MessageEncode msg => msg -> Lazy.ByteString
buildLazy = BS.Lazy.fromStrict . encodeMessage
