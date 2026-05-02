{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Splice site for the oneof end-to-end regression. Loading
-- @test\/data\/oneof_regression.proto@ exercises the bridge\'s
-- @FKOneof@ path through 'Proto.TH.loadProto':
--
-- 1. 'Proto.TH.mkOneofDataDecs' synthesises the @Envelope'Choice@
--    sum type with one constructor per oneof arm.
-- 2. 'Proto.TH.fieldSpecToProtoField' translates the @FSOneof@
--    'FieldSpec' into a 'Proto.Derive.Internal.FKOneof'
--    'OneofVariant' list so the bridge can emit codecs.
-- 3. 'messageCodecsViaBridge' produces @MessageEncode@ \/
--    @MessageSize@ \/ @MessageDecode@ instances that pattern-
--    match on the sum and dispatch on the variant tags.
--
-- The actual round-trip + variant-overwrite assertions live in
-- 'Test.Proto.Derive.Oneof'.
module Test.Proto.Derive.OneofInstances
  ( -- * loadProto-generated types
    Envelope (..)
  , Inner (..)
  , Envelope'EnvelopeChoice (..)
  , defaultEnvelope
  , defaultInner
  ) where

import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Vector as V

import Proto.TH (loadProto)

-- Keep the imports the loadProto splice transitively needs from
-- being optimised away.
_unused :: (V.Vector Int, T.Text, Int32)
_unused = (V.empty, T.empty, 0)

-- The splice creates: @Envelope@, @Inner@, @Envelope'EnvelopeChoice@
-- (the oneof sum), default values, and the wire-codec instance
-- triple via 'messageCodecsViaBridge'.
$(loadProto "test/data/oneof_regression.proto")
