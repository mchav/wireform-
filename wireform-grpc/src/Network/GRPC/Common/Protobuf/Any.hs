{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Support for the protobuf @Any@ type
--
-- Official docs at <https://protobuf.dev/programming-guides/proto3/#any>.
--
-- Intended for qualified import.
--
-- > import Network.GRPC.Common.Protobuf.Any (Any)
-- > import Network.GRPC.Common.Protobuf.Any qualified as Any
module Network.GRPC.Common.Protobuf.Any (
    Any

    -- * Packing and unpacking
  , UnpackError(..)
  , pack
  , unpack
  ) where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Data.Text qualified as Text

import Proto.Encode (encodeMessage)
import Proto.Decode (decodeMessage, DecodeError)
import Proto.Registry (IsMessage)
import Proto.Schema (ProtoMessage(..))
import Proto.Google.Protobuf.Any (Any(..))
import Proto.Google.Protobuf.Any qualified as PbAny

{-------------------------------------------------------------------------------
  Pack and unpack
-------------------------------------------------------------------------------}

typeUrlPrefix :: Text
typeUrlPrefix = "type.googleapis.com/"

pack :: forall a. IsMessage a => a -> Any
pack msg = PbAny.Any
  { anyTypeUrl       = typeUrlPrefix <> protoMessageName (Proxy @a)
  , anyValue         = encodeMessage msg
  , anyUnknownFields = []
  }

data UnpackError
    = DifferentType
        { expectedMessageType :: Text
        , actualUrl :: Text
        }
    | DecodingError Text
    deriving (Show, Eq)

unpack :: forall a. IsMessage a => Any -> Either UnpackError a
unpack any'
    | expectedName /= snd (Text.breakOnEnd "/" (anyTypeUrl any'))
        = Left DifferentType
              { expectedMessageType = expectedName
              , actualUrl = anyTypeUrl any'
              }
    | otherwise = case decodeMessage (anyValue any') of
        Left e -> Left $ DecodingError $ Text.pack (show e)
        Right x -> Right x
  where
    expectedName = protoMessageName (Proxy :: Proxy a)
