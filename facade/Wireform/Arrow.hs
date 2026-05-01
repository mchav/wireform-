-- | Convenience re-exports for Apache Arrow IPC.
--
-- @
-- import qualified Wireform.Arrow as Arrow
-- @
--
-- == Quick start
--
-- The high-level API in "Arrow.Stream" mirrors pyarrow:
--
-- @
-- -- Encode
-- let bytes = Arrow.'encodeArrowStream' schema batches
--
-- -- Decode
-- case Arrow.'decodeArrowStream' bytes of
--   Right (schema', batches') -> ...
--   Left  err                 -> ...
-- @
--
-- @batches@ is a @['V.Vector' 'ColumnArray']@; one entry per
-- record batch, each entry one 'ColumnArray' per schema field.
-- 'ColDictionary' columns are handled automatically — the writer
-- emits a 'DictBatch' per unique id and the reader resolves them
-- transparently. Use 'encodeArrowFile' / 'decodeArrowFile' for the
-- @ARROW1@-wrapped file format with the same input shape.
--
-- == When to drop down
--
-- "Arrow.FlatBufferIPC" exposes the underlying primitives
-- ('writeArrowStreamFB', 'buildRecordBatchBytes', 'DictBatch',
-- etc.) for callers that need:
--
-- * custom dictionary ids / delta dictionaries,
-- * pre-built 'RecordBatchDef' values (e.g. for slicing),
-- * direct access to the FlatBuffer envelope.
module Wireform.Arrow
  ( -- * High-level API (most callers want this)
    module Arrow.Stream
    -- * Column data
  , module Arrow.Column
    -- * Schema + record batch types
  , module Arrow.Types
    -- * Lower-level FlatBuffers building blocks
  , module Arrow.FlatBufferIPC
    -- * Legacy simplified-IPC framing
  , module Arrow.IPC
  , module Arrow.File
  , module Arrow.Write
  ) where

import Arrow.Column
import Arrow.IPC
import Arrow.Types
import Arrow.File
import Arrow.Write
import Arrow.Stream
import Arrow.FlatBufferIPC
