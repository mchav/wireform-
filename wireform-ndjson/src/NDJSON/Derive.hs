{-# LANGUAGE TemplateHaskell #-}

-- | Annotation-driven Template Haskell deriver for NDJSON.
--
-- NDJSON is just \"one JSON value per line\", so the per-record codec
-- is a 'Data.Aeson.ToJSON' \/ 'Data.Aeson.FromJSON' instance. This
-- module re-exports the Aeson deriver under an NDJSON-specific name
-- so user splices read intent-fully:
--
-- @
-- {-\# ANN myField (rename \"my-field\") \#-}
-- data Event = Event { … } deriving stock 'GHC.Generics.Generic'
--
-- 'deriveNDJSON' \''Event
-- @
--
-- The result composes with 'NDJSON.Encode.encodeRecords' and
-- 'NDJSON.Decode.decodeRecords' for streaming a 'Vector' of records.
--
-- All standard 'Wireform.Derive.Modifier.Modifier's apply (renames
-- via 'Wireform.Derive.NameStyle.Idiomatic' resolve to camelCase via
-- 'backendNDJSON', mirroring 'backendJSON').
module NDJSON.Derive
  ( deriveNDJSON
  , deriveToNDJSON
  , deriveFromNDJSON
  ) where

import Language.Haskell.TH (Dec, Name, Q)

import qualified Wireform.Derive.Aeson as A

-- | Derive both 'Data.Aeson.ToJSON' and 'Data.Aeson.FromJSON' for a
-- record / sum / enum / newtype, suitable for use with
-- 'NDJSON.Encode.encodeRecords' \/ 'NDJSON.Decode.decodeRecords'.
deriveNDJSON :: Name -> Q [Dec]
deriveNDJSON = A.deriveJSON

-- | Derive only 'Data.Aeson.ToJSON'.
deriveToNDJSON :: Name -> Q [Dec]
deriveToNDJSON = A.deriveToJSON

-- | Derive only 'Data.Aeson.FromJSON'.
deriveFromNDJSON :: Name -> Q [Dec]
deriveFromNDJSON = A.deriveFromJSON
