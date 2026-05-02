-- | Identifier for a serialization backend.
--
-- A 'Backend' is a textual tag attached to per-backend modifier overrides
-- (e.g. @forBackend backendProto (rename "n")@) and consulted by each
-- per-format deriver to decide which modifiers apply.
--
-- New backends can be defined by downstream packages: just construct a
-- 'Backend' with a fresh identifier. The set of \"standard\" backends
-- defined here is open-ended and exists only as a convenience so that
-- common cases compose cleanly.
module Wireform.Derive.Backend
  ( Backend (..)
    -- * Standard backends
  , backendProto
  , backendCBOR
  , backendMsgPack
  , backendThrift
  , backendJSON
  , backendEDN
  , backendTOML
  , backendYAML
  , backendXML
  , backendCSV
  , backendBinary
  , backendTextFormat
  ) where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Open identifier for a serialization backend.
--
-- 'Backend' has an 'IsString' instance, so backends can be written
-- inline as string literals, e.g. @forBackend "proto" (rename "n")@.
-- Prefer the named constants below where they exist so that typos are
-- caught at compile time.
newtype Backend = Backend { unBackend :: Text }
  deriving stock (Eq, Ord, Show, Data, Generic)
  deriving newtype (IsString, Hashable, NFData)

-- | Protocol Buffers wire encoding.
backendProto :: Backend
backendProto = Backend "proto"

-- | RFC 8949 CBOR.
backendCBOR :: Backend
backendCBOR = Backend "cbor"

-- | MessagePack.
backendMsgPack :: Backend
backendMsgPack = Backend "msgpack"

-- | Apache Thrift binary protocol.
backendThrift :: Backend
backendThrift = Backend "thrift"

-- | JSON (used by both Aeson-style and Proto3 JSON mappings;
-- Proto-JSON-specific overrides should additionally tag with
-- 'backendProto').
backendJSON :: Backend
backendJSON = Backend "json"

-- | Extensible Data Notation (Clojure / EDN).
backendEDN :: Backend
backendEDN = Backend "edn"

-- | TOML.
backendTOML :: Backend
backendTOML = Backend "toml"

-- | YAML.
backendYAML :: Backend
backendYAML = Backend "yaml"

-- | XML.
backendXML :: Backend
backendXML = Backend "xml"

-- | CSV / TSV header rows.
backendCSV :: Backend
backendCSV = Backend "csv"

-- | Catch-all for hand-rolled binary formats that do not fit one of the
-- standard backends above.
backendBinary :: Backend
backendBinary = Backend "binary"

-- | Protobuf text format (@google.protobuf.TextFormat@ / @pbtxt@).
backendTextFormat :: Backend
backendTextFormat = Backend "textformat"
