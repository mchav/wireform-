{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE BangPatterns #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE DataKinds #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE DerivingStrategies #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE FlexibleContexts #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE FlexibleInstances #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE MagicHash #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE MultiParamTypeClasses #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE OverloadedStrings #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE PatternSynonyms #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE ScopedTypeVariables #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE TypeApplications #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE TypeFamilies #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE UndecidableInstances #-}
{- This file was auto-generated from bench.proto by the proto-lens-protoc program. -}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Proto.Bench_Fields where

import Data.ProtoLens.Runtime.Data.ByteString qualified as Data.ByteString
import Data.ProtoLens.Runtime.Data.ByteString.Char8 qualified as Data.ByteString.Char8
import Data.ProtoLens.Runtime.Data.Int qualified as Data.Int
import Data.ProtoLens.Runtime.Data.Map qualified as Data.Map
import Data.ProtoLens.Runtime.Data.Monoid qualified as Data.Monoid
import Data.ProtoLens.Runtime.Data.ProtoLens qualified as Data.ProtoLens
import Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Bytes qualified as Data.ProtoLens.Encoding.Bytes
import Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Growing qualified as Data.ProtoLens.Encoding.Growing
import Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Parser.Unsafe qualified as Data.ProtoLens.Encoding.Parser.Unsafe
import Data.ProtoLens.Runtime.Data.ProtoLens.Encoding.Wire qualified as Data.ProtoLens.Encoding.Wire
import Data.ProtoLens.Runtime.Data.ProtoLens.Field qualified as Data.ProtoLens.Field
import Data.ProtoLens.Runtime.Data.ProtoLens.Message.Enum qualified as Data.ProtoLens.Message.Enum
import Data.ProtoLens.Runtime.Data.ProtoLens.Service.Types qualified as Data.ProtoLens.Service.Types
import Data.ProtoLens.Runtime.Data.Text qualified as Data.Text
import Data.ProtoLens.Runtime.Data.Text.Encoding qualified as Data.Text.Encoding
import Data.ProtoLens.Runtime.Data.Vector qualified as Data.Vector
import Data.ProtoLens.Runtime.Data.Vector.Generic qualified as Data.Vector.Generic
import Data.ProtoLens.Runtime.Data.Vector.Unboxed qualified as Data.Vector.Unboxed
import Data.ProtoLens.Runtime.Data.Word qualified as Data.Word
import Data.ProtoLens.Runtime.Lens.Family2 qualified as Lens.Family2
import Data.ProtoLens.Runtime.Lens.Family2.Unchecked qualified as Lens.Family2.Unchecked
import Data.ProtoLens.Runtime.Prelude qualified as Prelude
import Data.ProtoLens.Runtime.Text.Read qualified as Text.Read


active
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "active" a)
  => Lens.Family2.LensLike' f s a
active = Data.ProtoLens.Field.field @"active"


count
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "count" a)
  => Lens.Family2.LensLike' f s a
count = Data.ProtoLens.Field.field @"count"


description
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "description" a
     )
  => Lens.Family2.LensLike' f s a
description = Data.ProtoLens.Field.field @"description"


enabled
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "enabled" a)
  => Lens.Family2.LensLike' f s a
enabled = Data.ProtoLens.Field.field @"enabled"


id
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "id" a)
  => Lens.Family2.LensLike' f s a
id = Data.ProtoLens.Field.field @"id"


inner
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "inner" a)
  => Lens.Family2.LensLike' f s a
inner = Data.ProtoLens.Field.field @"inner"


items
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "items" a)
  => Lens.Family2.LensLike' f s a
items = Data.ProtoLens.Field.field @"items"


label
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "label" a)
  => Lens.Family2.LensLike' f s a
label = Data.ProtoLens.Field.field @"label"


maybe'inner
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "maybe'inner" a
     )
  => Lens.Family2.LensLike' f s a
maybe'inner = Data.ProtoLens.Field.field @"maybe'inner"


name
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "name" a)
  => Lens.Family2.LensLike' f s a
name = Data.ProtoLens.Field.field @"name"


payload
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "payload" a)
  => Lens.Family2.LensLike' f s a
payload = Data.ProtoLens.Field.field @"payload"


ratio
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "ratio" a)
  => Lens.Family2.LensLike' f s a
ratio = Data.ProtoLens.Field.field @"ratio"


score
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "score" a)
  => Lens.Family2.LensLike' f s a
score = Data.ProtoLens.Field.field @"score"


tags
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "tags" a)
  => Lens.Family2.LensLike' f s a
tags = Data.ProtoLens.Field.field @"tags"


timestamp
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "timestamp" a
     )
  => Lens.Family2.LensLike' f s a
timestamp = Data.ProtoLens.Field.field @"timestamp"


title
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "title" a)
  => Lens.Family2.LensLike' f s a
title = Data.ProtoLens.Field.field @"title"


values
  :: forall f s a
   . (Prelude.Functor f, Data.ProtoLens.Field.HasField s "values" a)
  => Lens.Family2.LensLike' f s a
values = Data.ProtoLens.Field.field @"values"


vec'items
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "vec'items" a
     )
  => Lens.Family2.LensLike' f s a
vec'items = Data.ProtoLens.Field.field @"vec'items"


vec'tags
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "vec'tags" a
     )
  => Lens.Family2.LensLike' f s a
vec'tags = Data.ProtoLens.Field.field @"vec'tags"


vec'values
  :: forall f s a
   . ( Prelude.Functor f
     , Data.ProtoLens.Field.HasField s "vec'values" a
     )
  => Lens.Family2.LensLike' f s a
vec'values = Data.ProtoLens.Field.field @"vec'values"
