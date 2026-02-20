-- | Core message identity typeclass.
--
-- 'IsMessage' associates a Haskell type with its fully-qualified
-- proto type name. This is used by 'Proto.Google.Protobuf.Any' for
-- type-safe packing/unpacking, and can also be used for message
-- registries and reflection.
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
