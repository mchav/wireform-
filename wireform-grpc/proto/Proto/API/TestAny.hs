{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.TestAny (
    TestAnyService
  , Reverse
  , module Proto.TestAny
) where

import Prelude hiding (id)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf

import Proto.TestAny

data TestAnyService

type Reverse = Protobuf TestAnyService "reverse"

type instance Input Reverse = Proto TestAnyMsg
type instance Output Reverse = Proto TestAnyMsg
instance IsRPC Reverse where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "TestAnyService"
  rpcMethodName _ = "Reverse"
  rpcMessageType _ = Just "TestAnyMsg"
instance SupportsClientRpc Reverse where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc Reverse where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType Reverse 'NonStreaming
instance HasStreamingType Reverse where
  type RpcStreamingType Reverse = 'NonStreaming

type instance RequestMetadata          (Protobuf TestAnyService meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf TestAnyService meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf TestAnyService meth) = NoMetadata
