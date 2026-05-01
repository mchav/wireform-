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
  , Encryption (..)
  , StripeEncryption (..)
  ) where

import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word64)

import ORC.Encryption (Encryption (..))
import ORC.Footer (readORCFooter)
import ORC.Types (ORCFooter (..), ORCType (..))
import ORC.Write
  ( StripeEncryption (..)
  , buildEncryptedORCFile
  , buildORCFile
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

-- | Serialise an ORC file from type info + stripe stream data,
-- applying the supplied options.
--
-- @types@ is the schema (one 'ORCType' per node, including the
-- root struct). @stripes@ is one 'V.Vector' of @(streamKind,
-- columnId, payload)@ per stripe, in emission order (the writer
-- doesn't reorder streams; ordering across columns is the
-- caller's concern, governed by ORC's stream-layout rules).
--
-- Returns either the encoded bytes or, if encryption is
-- requested and fails (e.g. mismatched key lengths), the error
-- the underlying writer reported.
encodeORC
  :: WriteOptions
  -> V.Vector ORCType
  -> [V.Vector (Word64, Word64, ByteString)]
  -> Either String ByteString
encodeORC opts types stripes =
  let !sd = V.fromList stripes
  in  case writeEncryption opts of
        Nothing -> Right (buildORCFile types sd)
        Just (enc, plan) ->
          buildEncryptedORCFile types sd (stripeKeys plan) enc

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
