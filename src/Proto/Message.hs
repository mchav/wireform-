-- | Core message identity typeclass.
--
-- 'IsMessage' is the unified interface combining:
--
-- * Wire encoding/decoding ('MessageEncode', 'MessageDecode')
-- * Schema metadata ('ProtoMessage')
-- * Type identity for Any packing
--
-- All generated message types implement this.
module Proto.Message
  ( IsMessage (..)
  ) where

import Data.Proxy (Proxy)
import Data.Text (Text)

import Proto.Encode (MessageEncode)
import Proto.Decode (MessageDecode)

-- | Typeclass for proto message types that carry type identity.
--
-- The 'messageTypeName' must match the fully-qualified proto type name
-- (e.g. @"google.protobuf.Timestamp"@).
class (MessageEncode a, MessageDecode a) => IsMessage a where
  messageTypeName :: Proxy a -> Text
