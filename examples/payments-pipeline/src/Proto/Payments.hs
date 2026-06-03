{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-export-lists -Wno-unused-imports -Wno-orphans #-}

-- |
-- Module      : Proto.Payments
-- Description : @payments.v1@ message + enum types, generated at compile time.
--
-- The whole @payments.proto@ schema is spliced in by 'loadProto' at compile
-- time: every message becomes a record with @MessageEncode@ \/
-- @MessageDecode@ \/ @ProtoMessage@ \/ @HasField@ instances, and every enum
-- becomes a proto-faithful sum type. The @IsLabel@ instance below wires the
-- @HasField@ machinery to @OverloadedLabels@ so downstream code can read and
-- write fields with the @#field@ / @^.@ / @.~@ lens idiom (the same one the
-- gRPC handlers in @wireform-grpc@ use).
module Proto.Payments where

import Proto.TH (loadProto)

import Data.Reflection (Given (..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)

import GHC.OverloadedLabels (IsLabel (..))
import Proto.Lens (field)
import Proto.Schema (HasField)

instance Given ExtensionRegistry where
  given = emptyExtensionRegistry

instance (HasField msg name a, Functor f) => IsLabel name ((a -> f a) -> msg -> f msg) where
  fromLabel = field @name

$(loadProto "proto/payments.proto")
