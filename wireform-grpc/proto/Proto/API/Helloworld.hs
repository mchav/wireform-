{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.Helloworld (
    -- * Greeter
    Greeter
  , SayHello
  , SayHelloStreamReply
  , SayHelloBidiStream

    -- ** Metadata
  , SayHelloMetadata(..)

    -- * Re-exports
  , module Proto.Helloworld
  ) where

import Prelude hiding (id)

import Control.Monad.Catch
import Data.ByteString qualified as Strict
import GHC.TypeLits

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf

import Proto.Helloworld

{-------------------------------------------------------------------------------
  Greeter
-------------------------------------------------------------------------------}

data Greeter

type SayHello            = Protobuf Greeter "sayHello"
type SayHelloStreamReply = Protobuf Greeter "sayHelloStreamReply"
type SayHelloBidiStream  = Protobuf Greeter "sayHelloBidiStream"

type instance Input SayHello = Proto HelloRequest
type instance Output SayHello = Proto HelloReply
instance IsRPC SayHello where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "helloworld.Greeter"
  rpcMethodName _ = "SayHello"
  rpcMessageType _ = Just "helloworld.HelloRequest"
instance SupportsClientRpc SayHello where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc SayHello where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType SayHello 'NonStreaming
instance HasStreamingType SayHello where
  type RpcStreamingType SayHello = 'NonStreaming

type instance Input SayHelloStreamReply = Proto HelloRequest
type instance Output SayHelloStreamReply = Proto HelloReply
instance IsRPC SayHelloStreamReply where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "helloworld.Greeter"
  rpcMethodName _ = "SayHelloStreamReply"
  rpcMessageType _ = Just "helloworld.HelloRequest"
instance SupportsClientRpc SayHelloStreamReply where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc SayHelloStreamReply where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType SayHelloStreamReply 'ServerStreaming
instance HasStreamingType SayHelloStreamReply where
  type RpcStreamingType SayHelloStreamReply = 'ServerStreaming

type instance Input SayHelloBidiStream = Proto HelloRequest
type instance Output SayHelloBidiStream = Proto HelloReply
instance IsRPC SayHelloBidiStream where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "helloworld.Greeter"
  rpcMethodName _ = "SayHelloBidiStream"
  rpcMessageType _ = Just "helloworld.HelloRequest"
instance SupportsClientRpc SayHelloBidiStream where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc SayHelloBidiStream where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType SayHelloBidiStream 'BiDiStreaming
instance HasStreamingType SayHelloBidiStream where
  type RpcStreamingType SayHelloBidiStream = 'BiDiStreaming

type instance RequestMetadata          (Protobuf Greeter meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf Greeter meth) = GreeterResponseInitialMetadata meth
type instance ResponseTrailingMetadata (Protobuf Greeter meth) = NoMetadata

{-------------------------------------------------------------------------------
  .. metadata
-------------------------------------------------------------------------------}

type family GreeterResponseInitialMetadata (meth :: Symbol) where
  GreeterResponseInitialMetadata "sayHelloStreamReply" = SayHelloMetadata
  GreeterResponseInitialMetadata meth                  = NoMetadata

data SayHelloMetadata = SayHelloMetadata (Maybe Strict.ByteString)
  deriving (Show)

instance BuildMetadata SayHelloMetadata where
  buildMetadata (SayHelloMetadata mVal) = concat [
        [ CustomMetadata "initial-md" val
        | Just val <- [mVal]
        ]
      ]

instance ParseMetadata SayHelloMetadata where
  parseMetadata headers =
      case headers of
        [] ->
          return $ SayHelloMetadata $ Nothing
        [md] | customMetadataName md == "initial-md" ->
          return $ SayHelloMetadata $ Just (customMetadataValue md)
        _otherwise ->
          throwM $ UnexpectedMetadata headers
