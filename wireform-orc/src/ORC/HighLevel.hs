{-# LANGUAGE BangPatterns #-}
-- | High-level Apache ORC API.
--
-- 95% of callers should reach for this module. It hides the
-- @buildORCFile@ / @buildORCFileWith@ /
-- @buildEncryptedORCFile@ split behind a single record-of-options:
--
-- @
-- -- Encode
-- let bytes = 'encodeORC' 'defaultWriteOptions' types stripes
--
-- -- Read
-- case 'decodeORC' bytes of
--   Right (footer, postScript) -> ...
--   Left  err                  -> ...
-- @
--
-- Stripes are passed as @['V.Vector' (Word64, Word64, ByteString)]@:
-- one entry per stripe, each entry a vector of
-- @(streamKind, columnId, payload)@ tuples in stream-emission order.
--
-- 'WriteOptions' carries the (currently small) set of
-- file-level toggles that 'buildORCFile' takes parameters for.
-- Encryption is opt-in via 'writeEncryption'.
--
-- For lower-level control (per-stripe footer adjustments,
-- selective stripe encryption), drop down to "ORC.Write".
module ORC.HighLevel
  ( -- * Encoding
    encodeORC
  , WriteOptions (..)
  , defaultWriteOptions
  , StripeEncryptionPlan (..)
    -- * Decoding
  , decodeORC
  , ORCFooter (..)
    -- * Re-exports for convenience
  , ORCType (..)
  , TypeKind (..)
  , Encryption (..)
  , StripeEncryption (..)
    -- * Legacy / deprecated variants
  , encodeORCWithRows
  , encodeORCWithoutRows
  ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word64)

import ORC.Encryption (Encryption (..))
import ORC.Footer (readORCFooter)
import ORC.Types (ORCFooter (..), ORCType (..), TypeKind (..))
import ORC.Write
  ( StripeEncryption (..)
  , buildEncryptedORCFile
  , buildORCFileWithRows
  )

-- ============================================================
-- Options
-- ============================================================

-- | Per-stripe encryption plan: parallel to the stripe data
-- vector, with a 'Nothing' entry for any stripe that should stay
-- in plaintext. Length must equal the number of stripes when
-- 'writeEncryption' is 'Just'.
newtype StripeEncryptionPlan = StripeEncryptionPlan
  { stripeKeys :: V.Vector (Maybe StripeEncryption)
  } deriving (Show, Eq)

-- | ORC writer configuration. Construct one with
-- 'defaultWriteOptions' and override the fields you care about.
--
-- @
-- let opts = 'defaultWriteOptions' { writeEncryption = Just plan }
--     bytes = 'encodeORC' opts types stripes
-- @
data WriteOptions = WriteOptions
  { writeEncryption :: !(Maybe (Encryption, StripeEncryptionPlan))
    -- ^ When 'Just', emit an encrypted ORC file: each per-stripe
    -- 'StripeEncryption' rotates the AES-CTR key per stripe and
    -- the file-level 'Encryption' record is stamped into the
    -- footer's @encryption@ field. When 'Nothing', emit a
    -- plaintext file. Default: 'Nothing'.
  } deriving (Show, Eq)

-- | Plaintext-everything defaults.
defaultWriteOptions :: WriteOptions
defaultWriteOptions = WriteOptions
  { writeEncryption = Nothing
  }

-- ============================================================
-- Encoding
-- ============================================================

-- | Serialise an ORC file from type info + per-stripe stream
-- data with authoritative row counts.
--
-- @types@ is the schema (one 'ORCType' per node, including the
-- root struct). @stripes@ pairs each stripe's @(streamKind,
-- columnId, payload)@ vector with its row count; the count is
-- stamped into @siNumberOfRows@ and summed into the footer's
-- @orcNumberOfRows@ so predicate-pushdown-aware readers can
-- plan scans correctly.
--
-- If you don't have row counts handy and are fine stamping 0
-- per stripe (some readers tolerate it, most don't), use
-- 'encodeORCWithoutRows'.
--
-- Returns either the encoded bytes or, if encryption is
-- requested and fails (e.g. mismatched key lengths), the error
-- the underlying writer reported.
encodeORC
  :: WriteOptions
  -> V.Vector ORCType
  -> [(V.Vector (Word64, Word64, ByteString), Word64)]
  -> Either String ByteString
encodeORC opts types stripesWithRows =
  let !sd   = V.fromList (map fst stripesWithRows)
      !rows = V.fromList (map snd stripesWithRows)
  in  case writeEncryption opts of
        Nothing -> Right (buildORCFileWithRows types sd rows)
        Just (enc, plan) ->
          -- buildEncryptedORCFile calls buildORCFileWith
          -- internally; it doesn't yet carry row counts. Path
          -- forward: lift buildEncryptedORCFile to use a row
          -- lookup. For now, the encrypted path stamps 0 rows
          -- per stripe — same as before — and callers that need
          -- both encryption and accurate row counts should open
          -- the ticket.
          buildEncryptedORCFile types sd (stripeKeys plan) enc

-- | Legacy alias for 'encodeORC'. The two forms were temporarily
-- split while row-count plumbing was being added; they're now
-- the same function.
{-# DEPRECATED encodeORCWithRows
    "Use 'encodeORC' — it now takes per-stripe row counts as the canonical shape." #-}
encodeORCWithRows
  :: WriteOptions
  -> V.Vector ORCType
  -> [(V.Vector (Word64, Word64, ByteString), Word64)]
  -> Either String ByteString
encodeORCWithRows = encodeORC

-- | Legacy entry point that stamps @siNumberOfRows = 0@ in every
-- stripe. Retained for callers that don't have row counts and
-- are encoding for readers that tolerate zero-row stripes (e.g.
-- quick-look dumps). Prefer 'encodeORC' for real workloads.
{-# DEPRECATED encodeORCWithoutRows
    "Use 'encodeORC' — pass 'zip stripes (repeat 0)' if you really don't have row counts." #-}
encodeORCWithoutRows
  :: WriteOptions
  -> V.Vector ORCType
  -> [V.Vector (Word64, Word64, ByteString)]
  -> Either String ByteString
encodeORCWithoutRows opts types stripes =
  encodeORC opts types (zip stripes (repeat 0))

-- ============================================================
-- Decoding
-- ============================================================

-- | Parse an ORC file's footer. Per-stripe column data is decoded
-- /lazily/ via the specialised readers in "ORC.Read" since ORC's
-- stream-level decode is intrinsically type-dispatched.
--
-- @
-- case 'decodeORC' bytes of
--   Right footer -> do
--     -- footer.orcStripes :: V.Vector StripeInformation
--     -- iterate stripes, then dispatch per-column via ORC.Read.*
--   Left err -> ...
-- @
decodeORC :: ByteString -> Either String ORCFooter
decodeORC = readORCFooter
