-- | gRPC server using Protobuf
--
-- Intended for unqualified import.
module Network.GRPC.Server.Protobuf (
    -- * Compute full Protobuf API
    ProtobufServices
  , ProtobufMethodsOf
  , ProtobufMethods
  ) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)
import Network.GRPC.Spec (Protobuf)

{-------------------------------------------------------------------------------
  Compute full Protobuf API

  These type families help compute the list of Protobuf RPC method types.

  'ProtobufMethodsOf' requires a 'ServiceMethods' type family instance
  mapping a service type to its method names. Users should provide this
  either via proto-lens generated code or by defining it manually:

  @
  type instance ServiceMethods MyService = '["Method1", "Method2"]
  @
-------------------------------------------------------------------------------}

type family ServiceMethods (serv :: Type) :: [Symbol]

type family ProtobufServices (servs :: [Type]) :: [[Type]] where
  ProtobufServices '[]             = '[]
  ProtobufServices (serv ': servs) = ProtobufMethodsOf serv
                                  ': ProtobufServices servs

type family ProtobufMethodsOf (serv :: Type) :: [Type] where
  ProtobufMethodsOf serv = ProtobufMethods serv (ServiceMethods serv)

type family ProtobufMethods (serv :: Type) (methds :: [Symbol]) :: [Type] where
  ProtobufMethods serv '[]             = '[]
  ProtobufMethods serv (meth ': meths) = Protobuf serv meth
                                      ': ProtobufMethods serv meths
