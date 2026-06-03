{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-orphans #-}

-- | gRPC service bindings for @routeguide.RouteGuide@.
--
-- The per-method @Protobuf@ RPC types (@RouteGuide@, @GetFeature@,
-- @ListFeatures@, @RecordRoute@, @RouteChat@) and all their instances are
-- generated from @proto/route_guide.proto@ by 'loadProtoServices'; the
-- message types are re-exported from "Proto.RouteGuide".
module Proto.API.RouteGuide (
    module Proto.API.RouteGuide
  , module Proto.RouteGuide
  ) where

import Network.GRPC.Protobuf.TH (loadProtoServices)

import Proto.RouteGuide

$(loadProtoServices "proto/route_guide.proto")
