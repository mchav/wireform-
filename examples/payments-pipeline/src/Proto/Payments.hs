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
-- time: every message becomes a plain record (with a @default\<Type\>@ value
-- and @MessageEncode@ \/ @MessageDecode@ \/ @ProtoMessage@ instances) and every
-- enum becomes a proto-faithful sum type. Downstream code uses ordinary record
-- syntax — the prefixed field selectors for reads and record-update on the
-- @default\<Type\>@ value for writes — rather than any lens idiom.
module Proto.Payments where

import Proto.TH (loadProto)

import Data.Reflection (Given (..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)

-- The TH-generated proto3 JSON instances carry a @Given ExtensionRegistry@
-- constraint (for proto2 extensions); this schema has none, so satisfy it
-- with the empty registry.
instance Given ExtensionRegistry where
  given = emptyExtensionRegistry

$(loadProto "proto/payments.proto")
