-- | User-extensible custom type registration for Avro.
--
-- Avro's extension mechanism is logical types. This module opens up the
-- closed set by allowing users to register custom 'LogicalTypeHandler's
-- and 'PropHandler's that drive code generation, encoding, and decoding.
module Avro.Registry
  ( -- * Registry
    AvroRegistry (..)
  , defaultAvroRegistry
    -- * Logical type handlers
  , LogicalTypeHandler (..)
  , registerLogicalType
    -- * Property handlers
  , PropHandler (..)
  , registerPropHandler
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

-- | Handler for a custom Avro logical type.  When a field has
-- @AvroLogical@ with a logical type name found in the registry,
-- code generation uses the handler's Haskell type, imports, and
-- encode\/decode function names instead of the base type.
data LogicalTypeHandler = LogicalTypeHandler
  { lthHaskellType :: !Text
  , lthImports     :: ![Text]
  , lthEncode      :: !Text
  , lthDecode      :: !Text
  }

-- | Handler for a custom Avro field property.  When a field carries
-- a property key matching a registered handler, the handler's
-- 'phCodeGen' function is called to emit extra lines of code.
data PropHandler = PropHandler
  { phCodeGen :: !(Text -> Text -> [Text])
  }

-- | Registry of custom logical type handlers and property handlers
-- that users can populate to affect Avro code generation.
data AvroRegistry = AvroRegistry
  { arLogicalTypes :: !(Map Text LogicalTypeHandler)
  , arCustomProps  :: !(Map Text PropHandler)
  }

instance Semigroup AvroRegistry where
  a <> b = AvroRegistry
    { arLogicalTypes = arLogicalTypes a <> arLogicalTypes b
    , arCustomProps  = arCustomProps a  <> arCustomProps b
    }

instance Monoid AvroRegistry where
  mempty = AvroRegistry Map.empty Map.empty

-- | Default registry with built-in Avro logical types:
-- @timestamp-millis@, @timestamp-micros@, @date@, @time-millis@,
-- @time-micros@, @decimal@, @uuid@, @duration@.
defaultAvroRegistry :: AvroRegistry
defaultAvroRegistry = AvroRegistry
  { arLogicalTypes = Map.fromList
      [ ("timestamp-millis", LogicalTypeHandler
          { lthHaskellType = "UTCTime"
          , lthImports     = ["Data.Time (UTCTime)"]
          , lthEncode      = "encodeTimestampMillis"
          , lthDecode      = "decodeTimestampMillis"
          })
      , ("timestamp-micros", LogicalTypeHandler
          { lthHaskellType = "UTCTime"
          , lthImports     = ["Data.Time (UTCTime)"]
          , lthEncode      = "encodeTimestampMicros"
          , lthDecode      = "decodeTimestampMicros"
          })
      , ("date", LogicalTypeHandler
          { lthHaskellType = "Day"
          , lthImports     = ["Data.Time (Day)"]
          , lthEncode      = "encodeDate"
          , lthDecode      = "decodeDate"
          })
      , ("time-millis", LogicalTypeHandler
          { lthHaskellType = "TimeOfDay"
          , lthImports     = ["Data.Time (TimeOfDay)"]
          , lthEncode      = "encodeTimeMillis"
          , lthDecode      = "decodeTimeMillis"
          })
      , ("time-micros", LogicalTypeHandler
          { lthHaskellType = "TimeOfDay"
          , lthImports     = ["Data.Time (TimeOfDay)"]
          , lthEncode      = "encodeTimeMicros"
          , lthDecode      = "decodeTimeMicros"
          })
      , ("decimal", LogicalTypeHandler
          { lthHaskellType = "Scientific"
          , lthImports     = ["Data.Scientific (Scientific)"]
          , lthEncode      = "encodeDecimal"
          , lthDecode      = "decodeDecimal"
          })
      , ("uuid", LogicalTypeHandler
          { lthHaskellType = "UUID"
          , lthImports     = []
          , lthEncode      = "encodeUUID"
          , lthDecode      = "decodeUUID"
          })
      , ("duration", LogicalTypeHandler
          { lthHaskellType = "AvroDuration"
          , lthImports     = []
          , lthEncode      = "encodeDuration"
          , lthDecode      = "decodeDuration"
          })
      ]
  , arCustomProps = Map.empty
  }

-- | Register a custom logical type handler in the registry.
registerLogicalType :: Text -> LogicalTypeHandler -> AvroRegistry -> AvroRegistry
registerLogicalType name handler reg =
  reg { arLogicalTypes = Map.insert name handler (arLogicalTypes reg) }

-- | Register a custom property handler in the registry.
registerPropHandler :: Text -> PropHandler -> AvroRegistry -> AvroRegistry
registerPropHandler name handler reg =
  reg { arCustomProps = Map.insert name handler (arCustomProps reg) }
