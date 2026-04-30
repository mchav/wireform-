-- | Pure update operations over Iceberg view metadata.
--
-- Mirrors 'Iceberg.Update' for tables: small composable functions that
-- return a new 'ViewMetadata' value, leaving I\/O to the caller.
module Iceberg.View
  ( -- * Construction
    newViewMetadata
    -- * Updates
  , addViewVersion
  , setCurrentViewVersion
  , addViewSchema
  , setViewProperty
  ) where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V

import Iceberg.Types

-- | Bootstrap a fresh 'ViewMetadata' value. Use 'addViewVersion' afterwards
-- to add the first version definition.
newViewMetadata
  :: Text          -- ^ View UUID.
  -> Text          -- ^ Storage location.
  -> Schema        -- ^ Initial schema.
  -> ViewMetadata
newViewMetadata uuid loc schema = ViewMetadata
  { vmViewUuid          = uuid
  , vmFormatVersion     = 1
  , vmLocation          = loc
  , vmSchemas           = V.singleton schema
  , vmCurrentVersionId  = 0
  , vmVersions          = V.empty
  , vmVersionLog        = V.empty
  , vmProperties        = Map.empty
  }

-- | Append a new view version, optionally making it the current one.
addViewVersion
  :: ViewVersion
  -> Bool {- make current -}
  -> Int64 {- timestamp -}
  -> ViewMetadata
  -> ViewMetadata
addViewVersion v makeCurrent ts vm =
  let withVersion = vm
        { vmVersions   = V.snoc (vmVersions vm) v
        , vmVersionLog = V.snoc (vmVersionLog vm)
                          (ViewHistoryEntry { vheTimestampMs = ts
                                            , vheVersionId   = vvVersionId v })
        }
   in if makeCurrent
      then withVersion { vmCurrentVersionId = vvVersionId v }
      else withVersion

setCurrentViewVersion :: Int -> Int64 -> ViewMetadata -> ViewMetadata
setCurrentViewVersion vid ts vm = vm
  { vmCurrentVersionId = vid
  , vmVersionLog       = V.snoc (vmVersionLog vm)
                          (ViewHistoryEntry { vheTimestampMs = ts, vheVersionId = vid })
  }

addViewSchema :: Schema -> ViewMetadata -> ViewMetadata
addViewSchema s vm = vm { vmSchemas = V.snoc (vmSchemas vm) s }

setViewProperty :: Text -> Text -> ViewMetadata -> ViewMetadata
setViewProperty k v vm = vm { vmProperties = Map.insert k v (vmProperties vm) }
