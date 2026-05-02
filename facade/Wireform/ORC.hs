-- | Convenience re-exports for Apache ORC.
--
-- @
-- import qualified Wireform.ORC as ORC
-- @
--
-- == Quick start
--
-- The high-level API in "ORC.HighLevel" consolidates the writer's
-- variants behind a single record of options:
--
-- @
-- case ORC.'encodeORC' ORC.'defaultWriteOptions' types stripesWithRows of
--   Right bytes -> ...
--   Left  err   -> ...
-- @
--
-- @types@ is a @V.Vector ORCType@ (the ORC schema).
-- @stripesWithRows@ is @[(V.Vector (Word64, Word64, ByteString), Word64)]@:
-- one entry per stripe, pairing @(streamKind, columnId,
-- payload)@ triples (in stream-emission order) with the stripe's
-- row count. 'WriteOptions' carries the (currently small) set of
-- file-level toggles — encryption is opt-in via 'writeEncryption'.
--
-- If you want to pick Arrow or Parquet instead without
-- rewriting your data layout, use "Wireform.Columnar" — a
-- single 'Wireform.Columnar.encode' / 'Wireform.Columnar.decode'
-- routed through the format of your choice.
--
-- Reading is currently lazy / type-dispatched (see
-- 'ORC.HighLevel.decodeORC' returning an 'ORCFooter'); per-stripe
-- per-column data is decoded via the specialised readers in
-- "ORC.Read".
module Wireform.ORC
  ( -- * High-level API (most callers want this)
    module ORC.HighLevel
    -- * Schema + footer types
  , module ORC.Types
  , module ORC.Footer
  , module ORC.Stripe
    -- * Lower-level reader / writer
  , module ORC.Read
  , module ORC.Write
  ) where

import ORC.Footer
import ORC.HighLevel
import ORC.Read
import ORC.Stripe
import ORC.Types
import ORC.Write
