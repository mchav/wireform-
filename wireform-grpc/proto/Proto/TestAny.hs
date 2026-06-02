{-# LANGUAGE DuplicateRecordFields #-}
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
module Proto.TestAny where

import Prelude hiding (id)

import Proto.TH (loadProto)

import Data.Reflection (Given(..))
import Proto.Internal.JSON.Extension (ExtensionRegistry, emptyExtensionRegistry)

import GHC.OverloadedLabels (IsLabel(..))
import Proto.Lens (Lens', field)
import Proto.Schema (HasField)

import Proto.Google.Protobuf.Any qualified



$(loadProto "proto/test-any.proto")
