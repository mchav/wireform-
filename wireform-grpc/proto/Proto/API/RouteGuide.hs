{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Proto.API.RouteGuide (
    -- * RouteGuide
    RouteGuide
  , GetFeature
  , ListFeatures
  , RecordRoute
  , RouteChat

    -- * Re-exports
  , module Proto.RouteGuide
  ) where

import Prelude hiding (id)

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf
import Network.GRPC.Spec
import Network.GRPC.Spec.Util.Protobuf qualified as Protobuf
import Network.GRPC.Server.Protobuf (ServiceMethods)

import Proto.RouteGuide

{-------------------------------------------------------------------------------
  RouteGuide
-------------------------------------------------------------------------------}

data RouteGuide

type GetFeature   = Protobuf RouteGuide "getFeature"
type ListFeatures = Protobuf RouteGuide "listFeatures"
type RecordRoute  = Protobuf RouteGuide "recordRoute"
type RouteChat    = Protobuf RouteGuide "routeChat"

type instance Input GetFeature = Proto Point
type instance Output GetFeature = Proto Feature
instance IsRPC GetFeature where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "routeguide.RouteGuide"
  rpcMethodName _ = "GetFeature"
  rpcMessageType _ = Just "routeguide.Point"
instance SupportsClientRpc GetFeature where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc GetFeature where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType GetFeature 'NonStreaming
instance HasStreamingType GetFeature where
  type RpcStreamingType GetFeature = 'NonStreaming

type instance Input ListFeatures = Proto Rectangle
type instance Output ListFeatures = Proto Feature
instance IsRPC ListFeatures where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "routeguide.RouteGuide"
  rpcMethodName _ = "ListFeatures"
  rpcMessageType _ = Just "routeguide.Rectangle"
instance SupportsClientRpc ListFeatures where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc ListFeatures where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType ListFeatures 'ServerStreaming
instance HasStreamingType ListFeatures where
  type RpcStreamingType ListFeatures = 'ServerStreaming

type instance Input RecordRoute = Proto Point
type instance Output RecordRoute = Proto RouteSummary
instance IsRPC RecordRoute where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "routeguide.RouteGuide"
  rpcMethodName _ = "RecordRoute"
  rpcMessageType _ = Just "routeguide.Point"
instance SupportsClientRpc RecordRoute where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc RecordRoute where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType RecordRoute 'ClientStreaming
instance HasStreamingType RecordRoute where
  type RpcStreamingType RecordRoute = 'ClientStreaming

type instance Input RouteChat = Proto RouteNote
type instance Output RouteChat = Proto RouteNote
instance IsRPC RouteChat where
  rpcContentType _ = "application/grpc+proto"
  rpcServiceName _ = "routeguide.RouteGuide"
  rpcMethodName _ = "RouteChat"
  rpcMessageType _ = Just "routeguide.RouteNote"
instance SupportsClientRpc RouteChat where
  rpcSerializeInput _ = Protobuf.buildLazy . getProto
  rpcDeserializeOutput _ = fmap Proto . Protobuf.parseLazy
instance SupportsServerRpc RouteChat where
  rpcDeserializeInput _ = fmap Proto . Protobuf.parseLazy
  rpcSerializeOutput _ = Protobuf.buildLazy . getProto
instance SupportsStreamingType RouteChat 'BiDiStreaming
instance HasStreamingType RouteChat where
  type RpcStreamingType RouteChat = 'BiDiStreaming

type instance RequestMetadata          (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseInitialMetadata  (Protobuf RouteGuide meth) = NoMetadata
type instance ResponseTrailingMetadata (Protobuf RouteGuide meth) = NoMetadata

type instance ServiceMethods RouteGuide = '["getFeature", "listFeatures", "recordRoute", "routeChat"]
