{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans -Wno-missing-signatures #-}

-- | All splice sites for the protobuf conformance harness.
--
-- This module centralises the @loadProto@ calls so the rest of
-- the harness can import the generated types without fighting
-- TH stage restrictions. Two schemas are spliced in:
--
--   * @conformance.proto@ — the wire protocol the upstream
--     @conformance_test_runner@ uses to drive any test program.
--     Defines @ConformanceRequest@ \/ @ConformanceResponse@ \/
--     @WireFormat@ \/ @TestStatus@ \/ @FailureSet@.
--
--   * @test_messages_proto3.proto@ — a pruned copy of the
--     upstream @TestAllTypesProto3@ schema. Carries every shape
--     wireform-proto generates code for (scalars, repeated
--     packed and unpacked, maps, oneofs, nested message and
--     enum, foreign message and enum, recursive submessage).
--     Well-known-types arms (Timestamp, Duration, Any, Struct,
--     Wrappers, FieldMask, Empty, Value) are omitted because
--     @loadProto@ doesn't currently follow proto @import@s; the
--     wire-format round-trip still survives those fields via
--     unknown-field preservation.
--
-- The two splices are kept on separate top-level pragmas so a
-- compile error in one doesn't blow up the entire module — the
-- error message points straight at the offending @.proto@ file.
module Test.Conformance.Schema
  ( -- Re-exports the generated types under their proto names.
    -- Listing them here forces GHC to link them in even when
    -- the compile-pipeline strips otherwise-unused TH outputs.
    module Test.Conformance.Schema
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

import Proto.TH (loadProto)

-- These imports look unused; in fact they bring 'Generic' /
-- 'NFData' / 'Hashable' into scope for the 'deriving anyclass'
-- the splice emits, plus the Vector / Map / ByteString types
-- the generated record fields use.
import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Hashable (Hashable)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

-- The wire protocol the upstream conformance_test_runner uses.
$(loadProto "test-conformance/protos/conformance.proto")

-- The TestAllTypesProto3 message schema (subset; see haddock above).
$(loadProto "test-conformance/protos/test_messages_proto3.proto")

-- Force the generated types into the export list so a downstream
-- module can `import Test.Conformance.Schema` without naming any
-- type explicitly.
_keepImports :: (Map.Map Int Int, V.Vector Int, Seq.Seq Int)
_keepImports = (Map.empty, V.empty, Seq.empty)
